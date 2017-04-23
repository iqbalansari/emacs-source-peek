;;; source-peek.el --- Display function definitions inline  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Iqbal Ansari

;; Author: Iqbal Ansari <iqbalansari02@yahoo.com>
;; Keywords: convenience, extensions, help, tools
;; Version: 0.4
;; Package-Requires: ((quick-peek "1.0") (seq "1.11") (emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds the command `source-peek' which fetches the definition of
;; the function at point (using different backends) and displays them inline in
;; the current buffer.
;;
;; It allows limited interaction with the source definition, currently limited
;; to only scrolling them



;;; Code:

(require 'quick-peek)
(require 'seq)
(require 'compile)



(cl-defstruct source-peek-location
  "Location of source definition."
  file
  start-pos
  buffer
  end-pos
  line
  nlines
  definition)

(defun source-peek-jump-to-location (location)
  "Jump the LOCATION."
  (find-file (source-peek-location-file location))
  (goto-char (source-peek-location-start-pos location)))



;;; Backends for fetching definitions

;; Xref backend

(require 'xref nil :noerror)

(defun source-peek--xref--get-location (xref)
  (message "Fetching definition ...")
  (let ((inhibit-message t)
        (display-buffer-alist (list (cons ".*" #'ignore))))
    (save-excursion
      (let ((marker (xref-location-marker (xref-item-location xref))))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (prog1 (make-source-peek-location :buffer (current-buffer)
                                               :file (buffer-file-name)
                                               :start-pos (point))
                    (message nil))))))))

(defun source-peek-xref-get-locations (callback)
  (let* ((backend (or (xref-find-backend)
                      (user-error "No xref backend found for current buffer")))
         (identifier (or (xref-backend-identifier-at-point backend)
                         (user-error "No identifier found at current point")))
         (xrefs (xref-backend-definitions backend identifier)))
    (funcall callback (mapcar #'source-peek--xref--get-location xrefs))))

;; Jedi backend

(require 'deferred nil :noerror)
(require 'jedi nil :noerror)

(defun source-peek--jedi--get-location (location)
  (message "Fetching definition ...")
  (let ((inhibit-message t)
        (display-buffer-alist (list (cons ".*" #'ignore)))
        (file (plist-get location :module_path))
        (line (plist-get location :line_nr)))

    (make-source-peek-location :file file
                               :start-pos (with-temp-buffer
                                            (insert-file-contents-literally file)
                                            (goto-char (point-min))
                                            (forward-line (1- line))
                                            (point))
                               :line line)))

(defun source-peek-jedi-get-locations (callback)
  (deferred:nextc (jedi:call-deferred 'goto)
    (lambda (reply)
      (funcall callback (mapcar #'source-peek--jedi--get-location reply)))))

;; Tern backend

(require 'tern nil :noerror)

(defun source-peek--tern-get-location (result)
  (let* ((rel-file-name (cdr (assoc 'file result)))
         (abs-file-name (expand-file-name rel-file-name (tern-project-dir)))
         (start-pos (cdr (assoc 'start result))))
    (make-source-peek-location :file abs-file-name
                               :start-pos (with-temp-buffer
                                            (insert-file-contents-literally abs-file-name)
                                            (goto-char start-pos)
                                            (line-beginning-position)))))

(defun source-peek-tern-get-locations (callback)
  (tern-run-request
   (lambda (err data)
     (funcall callback (unless err
                         (list (source-peek--tern-get-location data)))))
   `((query (end . ,(point))
            (file . ,(tern-project-relative-file))
            (type . "definition")))))

;; Dumb Jump backend

(require 'dumb-jump nil :noerror)

(defun source-peek-use-dump-jump-p ()
  (when (and (featurep 'dumb-jump) buffer-file-name)
    (member (or (dumb-jump-get-language-by-filename buffer-file-name)
                (dumb-jump-get-language-from-mode))
            (mapcar (lambda (rule)
                      (plist-get rule :language))
                    dumb-jump-find-rules))))

(defun source-peek--dumb-jump--get-location (location)
  (message "Fetching definition ...")
  (let* ((inhibit-message t)
         (display-buffer-alist (list (cons ".*" #'ignore)))
         (file (plist-get location :path))
         (line (plist-get location :line)))

    (make-source-peek-location :file (expand-file-name file default-directory)
                               :start-pos (with-temp-buffer
                                            (insert-file-contents-literally file)
                                            (goto-char (point-min))
                                            (forward-line (1- line))
                                            (point))
                               :line line)))

(defun source-peek-dumb-jump-get-locations (callback)
  (let* ((results (dumb-jump-get-results))
         (locations (sort (plist-get results :results)
                          (lambda (x y)
                            (< (plist-get x :diff)
                               (plist-get y :diff)))))
         (issue (plist-get results :issue)))
    (funcall callback
             (unless issue
               (mapcar #'source-peek--dumb-jump--get-location
                       locations)))))

(defvar source-peek-backends
  '((jedi-mode . source-peek-jedi-get-locations)
    (tern-mode . source-peek-tern-get-locations)
    ((not (member (xref-find-backend) '(etags nil))) . source-peek-xref-get-locations)
    ((source-peek-use-dump-jump-p) . source-peek-dumb-jump-get-locations)))



;; Fetching definitions

(defvar-local source-peek--backend 'unknown)

(defun source-peek--get-backend ()
  (when (equal source-peek--backend 'unknown)
    (setq source-peek--backend
          (cdr (seq-find (lambda (backend)
                           (ignore-errors (eval (car backend))))
                         source-peek-backends))))
  source-peek--backend)

(defun source-peek-fetch-locations (callback)
  (let ((backend (or (source-peek--get-backend)
                     (user-error "Could not find a backend for current buffer"))))
    (funcall backend
             ;; Always to invoke callback asynchronously even for async source
             (lambda (locations)
               (run-at-time 0 nil callback locations)))))

(defun source-peek--open-buffer (location)
  (when (source-peek-location-file location)
    (find-file-noselect (source-peek-location-file location) t)))

(defun source-peek--extract-definition (location)
  (with-current-buffer (source-peek-location-buffer location)
    (let* ((start (save-excursion
                    (goto-char (source-peek-location-start-pos location))
                    (line-beginning-position)))
           (end (save-excursion
                  (goto-char (source-peek-location-end-pos location))
                  (line-beginning-position))))
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (font-lock-fontify-region start end)
          (buffer-string))))))

(defun source-peek--get-line-number (location)
  (with-current-buffer (source-peek-location-buffer location)
    (save-excursion
      (goto-char (source-peek-location-start-pos location))
      (line-number-at-pos))))

(defun source-peek--get-end-pos (location)
  (with-current-buffer (source-peek-location-buffer location)
    (save-excursion
      (goto-char (source-peek-location-start-pos location))
      (end-of-defun)
      (point))))

(defun source-peek--get-nlines (location)
  (with-current-buffer (source-peek-location-buffer location)
    (count-lines (source-peek-location-start-pos location)
                 (source-peek-location-end-pos location))))

(defun source-peek-fill-location (location)
  (unless (source-peek-location-buffer location)
    (setf (source-peek-location-buffer location)
          (source-peek--open-buffer location)))
  (unless (source-peek-location-line location)
    (setf (source-peek-location-line location)
          (source-peek--get-line-number location)))
  (unless (source-peek-location-end-pos location)
    (setf (source-peek-location-end-pos location)
          (source-peek--get-end-pos location)))
  (unless (source-peek-location-definition location)
    (setf (source-peek-location-definition location)
          (source-peek--extract-definition location)))
  (unless (source-peek-location-line location)
    (setf (source-peek-location-line location)
          (source-peek--get-line-number location)))
  (unless (source-peek-location-nlines location)
    (setf (source-peek-location-nlines location)
          (source-peek--get-nlines location)))
  location)



;; Popup displays

(cl-defstruct source-peek-popup
  (scroll-start 1)
  location)

(cl-defstruct source-peek-popup-root
  position
  popups
  (height 10)
  (current-popup 0))

(defvar-local source-peek-popup-root nil)

(defun source-peek-create-popups (position locations)
  (let ((popups (mapcar (lambda (location)
                          (make-source-peek-popup :location location))
                        locations)))
    (make-source-peek-popup-root :position position
                                 :popups popups)))

(defun source-peek--current-popup (popup-root)
  (let ((popup-index (source-peek-popup-root-current-popup popup-root)))
    (nth popup-index (source-peek-popup-root-popups popup-root))))

(defun source-peek-destroy ()
  "Hide the display source peek popup."
  (when source-peek-popup-root
    (quick-peek-hide (source-peek-popup-root-position source-peek-popup-root))
    (setq source-peek-popup-root nil)))

(defun source-peek-jump-to-definition (&rest ignored)
  "Jump the definition currently being displayed."
  (interactive)
  (when source-peek-popup-root
    (let* ((current-popup (source-peek--current-popup source-peek-popup-root))
           (location (source-peek-popup-location current-popup)))
      (source-peek-jump-to-location location))))

(defvar source-peek-mouse-map (let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1] #'source-peek-jump-to-definition)
                                (define-key map [down-mouse-1] #'source-peek-jump-to-definition)
                                (define-key map [drag-mouse-1] #'source-peek-jump-to-definition)
                                map))

(defun source-peek--truncate-definition (location &optional start-line n-lines ellipsis)
  "Return a truncated version of `defintion' slot of LOCATION.

The returned text starts at the START-LINE in the definition and contains upto
N-LINES number of lines.  ELLIPSIS is added at the end of the definition was
truncated."
  (let ((start-line (or start-line 1))
        (n-lines (or n-lines 10))
        (ellipsis (or ellipsis " …")))
    (with-temp-buffer
      (insert (source-peek-location-definition location))
      (goto-char (point-min))
      (forward-line (1- (min start-line
                             (max (1+ (- (count-lines (point-min) (point-max)) n-lines)) 1))))
      (delete-region (point-min) (point))
      (forward-line n-lines)
      (unless (eobp)
        (delete-region (point) (point-max))
        (forward-line -1)
        (goto-char (point-at-eol))
        (insert ellipsis))
      (buffer-string))))

(defun source-peek--popup-contents (popup height &optional ellipsis)
  "Return the contents for POPUP.

POPUP should be an instance of `source-peek-popup'.  The contents are basically
the source code at the location identified by the `location' slot of the POPUP.
It truncated so that the total number of lines displayed are less than or equal
to the HEIGHT.  ELLIPSIS is added at end of the text to indicate truncation."
  (let ((location (source-peek-popup-location popup))
        (scroll-start (source-peek-popup-scroll-start popup))
        (ellipsis (or ellipsis " …")))
    (with-temp-buffer
      (insert (propertize (format "\n%s:%d"
                                  (source-peek-location-file location)
                                  (source-peek-location-line location))
                          'face 'link
                          'pointer 'hand
                          'keymap source-peek-mouse-map
                          'location location))
      (insert "\n\n")
      (insert (source-peek--truncate-definition location scroll-start
                                                height ellipsis))
      (buffer-string))))

(defun source-peek--add-header (popup-contents selected-pop n-popups)
  "Add the a header displaying the number of definition being displayed.

POPUP-CONTENTS is the raw contents that are about to be displayed, it is
prepended with the header.  SELECTED-POP and N-POPUPS are position of the popup
about to be displayed and the total number of popups respectively."
  (if  (= 1 n-popups)
      popup-contents
    (with-temp-buffer
      (insert popup-contents)
      (goto-char (point-min))
      (insert (propertize (format "[%d/%d]\n" selected-pop n-popups)
                          'face 'compilation-line-number))
      (buffer-string))))

(defun source-peek-after-command ()
  (unless (source-peek-persist-p)
    (source-peek-destroy)
    (remove-hook 'post-command-hook #'source-peek-after-command t)))

(defun source-peek-render-popup (popup-root)
  "Display the POPUP-ROOT.

POPUP-ROOT should be an instance of `source-peek-popup-root'."
  (let* ((popups (source-peek-popup-root-popups popup-root))
         (current-popup (source-peek-popup-root-current-popup popup-root))
         (height (source-peek-popup-root-height popup-root))
         (popup-contents (source-peek--popup-contents (nth current-popup popups) height))
         (n-popups (length popups)))
    (quick-peek-show (source-peek--add-header popup-contents
                                                    (1+ current-popup)
                                                    n-popups)
                     (source-peek-popup-root-position popup-root)
                     'none
                     'none)
    (set-transient-map source-peek-keymap #'source-peek-persist-p)
    (add-hook 'post-command-hook #'source-peek-after-command t t)))

(defun source-peek--calculate-scroll-start (popup amount height)
  "Calculate the new start position for POPUP after scrolling it by AMOUNT.

The start is calculated so that at-least HEIGHT number of lines are visible,
unless the number of lines available for given location are less than HEIGHT."
  (let* ((current-start (source-peek-popup-scroll-start popup))
         (location (source-peek-popup-location popup))
         (new-start (+ current-start amount))
         ;; The new start cannot be less than difference between the total
         ;; number of lines for current definition and
         (start-max (1+ (- (source-peek-location-nlines location) height))))
    ;; If scroll start goes below 1 stop at 1
    (max (min new-start start-max) 1)))

(defun source-peek-scroll (&optional amount)
  "Scroll the currently visible popup by AMOUNT.

A negative value of AMOUNT causes the popup to scroll up."
  (when source-peek-popup-root
    (let* ((popup-root source-peek-popup-root)
           (height (source-peek-popup-root-height popup-root))
           (current-popup (source-peek--current-popup popup-root))
           (amount (or amount 1)))

      (setf (source-peek-popup-scroll-start current-popup)
            (source-peek--calculate-scroll-start current-popup amount height))
      (source-peek-render-popup source-peek-popup-root))))

(defun source-peek-cycle (&optional amount)
  "Move to the popup displaying AMOUNTth next definition.

A negative value of AMOUNT means to select a previous definition."
  (when source-peek-popup-root
    (let* ((n-popups (length (source-peek-popup-root-popups source-peek-popup-root)))
           (current-popup (source-peek-popup-root-current-popup source-peek-popup-root)))
      (setf (source-peek-popup-root-current-popup source-peek-popup-root)
            (mod (+ current-popup amount) n-popups))
      (source-peek-render-popup source-peek-popup-root))))

(defun source-peek-scroll-up ()
  "Scroll up the currently displayed definitions."
  (interactive)
  (source-peek-scroll -1))

(defun source-peek-scroll-down ()
  "Scroll down the currently displayed definitions."
  (interactive)
  (source-peek-scroll +1))

(defun source-peek-cycle-next ()
  "Display the next definition from current set of definitions."
  (interactive)
  (source-peek-cycle +1))

(defun source-peek-cycle-previous ()
  "Display the previous definition from current set of definitions."
  (interactive)
  (source-peek-cycle -1))

(defun source-peek-persist-p ()
  "Return t if the transient keymap should stay active.

More precisely return t, of the last command was one of source-peek commands"
  (member this-command '(source-peek
                         source-peek-scroll-up
                         source-peek-scroll-down
                         source-peek-cycle-next
                         source-peek-cycle-previous
                         recenter)))

(defvar source-peek-keymap
  (let ((map (make-keymap)))
    (define-key map (kbd "C-l") #'recenter)
    (define-key map (kbd "<left>") #'source-peek-cycle-previous)
    (define-key map (kbd "<right>") #'source-peek-cycle-next)
    (define-key map (kbd "<down>") #'source-peek-scroll-down)
    (define-key map (kbd "<up>") #'source-peek-scroll-up)
    (define-key map (kbd "M-.") #'source-peek-jump-to-definition)
    map)
  "Keymap used in while the source-peek popup is being displayed.")

(defun source-peek-display-locations (locations)
  "Display the LOCATIONS in a popup.

The LOCATIONS are instances of the `source-peek-location' struct.  This function
assumes that all the attributes of `source-peek-location' are available."
  (let* ((position (copy-marker (line-beginning-position)))
         (popups (source-peek-create-popups position locations)))
    (setq source-peek-popup-root popups)
    (source-peek-render-popup popups)))



;; User interface

;;;###autoload
(defun source-peek ()
  "Fetch and display the definitions for symbol at point inline."
  (interactive)
  (source-peek-fetch-locations
   (lambda (locations)
     (if locations
         (source-peek-display-locations (mapcar #'source-peek-fill-location
                                                locations))
       (message "[source-peek] Could not find any definitions for symbol at point!")))))



(provide 'source-peek)
;;; source-peek.el ends here

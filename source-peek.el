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



(cl-defstruct source-peek-location
  "Location of source definition."
  file
  start-pos
  buffer
  end-pos
  line
  nlines
  definition)



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
  (deferred:nextc (jedi:call-deferred 'get_definition)
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
    (let* ((start (source-peek-location-start-pos location))
           (end (source-peek-location-end-pos location)))
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
  (height 10)
  location)

(cl-defstruct source-peek-popups
  position
  popups
  (current-popup 0))

(defvar-local source-peek-popups nil)

(defun source-peek-create-popups (position locations)
  (let ((popups (mapcar (lambda (location)
                          (make-source-peek-popup :location location))
                        locations)))
    (make-source-peek-popups :position position
                             :popups popups)))

(defun source-peek--truncate-definition (location &optional start-at lines ellipsis)
  (let ((start-at (or start-at 1))
        (lines (or lines 10))
        (ellipsis (or ellipsis " …")))
    (with-temp-buffer
      (insert (source-peek-location-definition location))
      (goto-char (point-min))
      (forward-line (1- (min start-at
                             (max (1+ (- (count-lines (point-min) (point-max)) lines)) 1))))
      (delete-region (point-min) (point))
      (forward-line lines)
      (unless (eobp)
        (delete-region (point) (point-max))
        (forward-line -1)
        (goto-char (point-at-eol))
        (insert ellipsis))
      (buffer-string))))

(defun source-peek--popup-contents (popup &optional ellipsis)
  (let ((location (source-peek-popup-location popup))
        (scroll-start (source-peek-popup-scroll-start popup))
        (height (source-peek-popup-height popup))
        (ellipsis (or ellipsis " …")))
    (with-temp-buffer
      (insert (propertize (format "\n%s:%d"
                                  (source-peek-location-file location)
                                  (source-peek-location-line location))
                          'face 'link
                          'location location))
      (insert "\n\n")
      (insert (source-peek--truncate-definition location scroll-start
                                                height ellipsis))
      (buffer-string))))

(defun source-peek--add-popup-number (popup-contents selected-pop n-popups)
  (if  (= 1 n-popups)
      popup-contents
    (with-temp-buffer
      (insert popup-contents)
      (goto-char (point-min))
      (insert (format "[%d/%d]\n" selected-pop n-popups))
      (buffer-string))))

(defun source-peek-render-popup (popup)
  (let* ((popups (source-peek-popups-popups popup))
         (n-popups (length popups))
         (current-popup (source-peek-popups-current-popup popup))
         (popup-contents (source-peek--popup-contents (nth current-popup popups))))
    (quick-peek-show (source-peek--add-popup-number popup-contents
                                                    (1+ current-popup)
                                                    n-popups)
                     (source-peek-popups-position popup)
                     'none
                     'none)
    (set-transient-map source-peek-keymap #'source-peek-keep-keymap-p)))

(defun source-peek--calculate-scroll-start (popup amount)
  (let* ((current-start (source-peek-popup-scroll-start popup))
         (location (source-peek-popup-location popup))
         (new-start (if (< (+ current-start amount) 1)
                        1
                      (+ current-start amount)))
         (start-max (max (1+ (- (source-peek-location-nlines location)
                                (source-peek-popup-height popup)))
                         1)))
    (min new-start start-max)))

(defun source-peek-scroll (&optional amount)
  (when source-peek-popups
    (let* ((popups source-peek-popups)
           (current-popup (nth (source-peek-popups-current-popup popups)
                               (source-peek-popups-popups popups)))
           (amount (or amount 1)))

      (setf (source-peek-popup-scroll-start current-popup)
            (source-peek--calculate-scroll-start current-popup amount))
      (source-peek-render-popup source-peek-popups))))

(defun source-peek-cycle (&optional amount)
  (when source-peek-popups
    (let* ((n-popups (length (source-peek-popups-popups source-peek-popups)))
           (current-pop (source-peek-popups-current-popup source-peek-popups)))
      (setf (source-peek-popups-current-popup source-peek-popups)
            (mod (+ current-pop amount) n-popups))
      (source-peek-render-popup source-peek-popups))))

(defun source-peek-scroll-up ()
  (interactive)
  (source-peek-scroll -1))

(defun source-peek-scroll-down ()
  (interactive)
  (source-peek-scroll +1))

(defun source-peek-cycle-next ()
  (interactive)
  (source-peek-cycle +1))

(defun source-peek-cycle-previous ()
  (interactive)
  (source-peek-cycle -1))

(defun source-peek-quit ()
  (interactive)
  (when source-peek-popups
    (quick-peek-hide (source-peek-popups-position source-peek-popups))
    (setq source-peek-popups nil)))

(defun source-peek-keep-keymap-p ()
  (member this-command '(source-peek source-peek-scroll-up source-peek-scroll-down)))

(defvar source-peek-keymap
  (let ((map (make-keymap)))
    (define-key map [t] #'source-peek-quit)
    (define-key map (kbd "<left>") #'source-peek-cycle-previous)
    (define-key map (kbd "<right>") #'source-peek-cycle-next)
    (define-key map (kbd "<down>") #'source-peek-scroll-down)
    (define-key map (kbd "<up>") #'source-peek-scroll-up)
    map)
  "Keymap used in `source-peek-keymap'.")

(defun source-peek-display-locations (locations)
  (let* ((position (copy-marker (line-beginning-position)))
         (popups (source-peek-create-popups position locations)))
    (setq source-peek-popups popups)
    (source-peek-render-popup popups)))



;; User interface

;;;###autoload
(defun source-peek ()
  (interactive)
  (source-peek-fetch-locations (lambda (locations)
                                 (source-peek-display-locations (mapcar #'source-peek-fill-location
                                                                        locations)))))



(provide 'source-peek)
;;; source-peek.el ends here

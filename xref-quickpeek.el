(require 'quick-peek)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'cl-lib)

(cl-defstruct source-peek-location
  file
  start-pos
  buffer
  end-pos
  line
  nlines
  definition)

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

(defun source-peek-xref-get-locations (&optional identifier)
  (let* ((backend (or (xref-find-backend)
                      (user-error "No xref backend found for current buffer")))
         (identifier (or identifier
                         (xref-backend-identifier-at-point backend)
                         (user-error "No identifier found at current point")))
         (xrefs (xref-backend-definitions backend identifier)))
    (mapcar #'source-peek--xref--get-location xrefs)))

(defvar source-peek-backends '((t . source-peek-xref-get-locations)))

(defvar-local source-peek--backend 'unknown)

(defun source-peek--get-backend ()
  (when (equal source-peek--backend 'unknown)
    (setq source-peek--backend
          (cdr (seq-find (lambda (backend)
                           (or (and (functionp (car backend))
                                    (funcall (car backend)))
                               (equal (car backend) t)))
                         source-peek-backends))))
  source-peek--backend)

(defun source-peek-fetch-locations (&optional identifier)
  (let ((backend (or (source-peek--get-backend)
                     (user-error "Could not find a backend for current buffer"))))
    (funcall backend identifier)))

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

(cl-defstruct source-peek-popup
  pos
  scroll-start
  height
  current-location
  locations)

(defvar-local source-peek-popup nil)

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

(defun source-peek-display-locations (locations)
  (setq source-peek-popup (make-source-peek-popup :pos (copy-marker (line-beginning-position))
                                                  :scroll-start 1
                                                  :height 10
                                                  :current-location (car locations)
                                                  :locations locations))
  (quick-peek-show (with-temp-buffer
                     (let ((location (car locations)))
                       (insert (propertize (format "\n%s:%d"
                                                   (source-peek-location-file location)
                                                   (source-peek-location-line location))
                                           'face 'link
                                           'location location))
                       (insert "\n\n")
                       (insert (source-peek--truncate-definition location)))
                     (buffer-string))
                   (line-beginning-position)
                   'none
                   'none)
  (set-transient-map source-peek-keymap #'source-peek-keep-keymap-p))

(defun source-peek--get-scroll-start (popup amount)
  (let* ((current-start (source-peek-popup-scroll-start popup))
         (location (source-peek-popup-location popup))
         (new-start (if (< (+ current-start amount) 0)
                        0
                      (+ current-start amount)))
         (start-max (max (1+ (- (source-peek-location-nlines location)
                                (source-peek-popup-height popup)))
                         1)))
    (min new-start start-max )))

(defun source-peek-scroll-up ()
  (interactive)
  (source-peek-scroll -1))

(defun source-peek-scroll-down ()
  (interactive)
  (source-peek-scroll +1))

(defun source-peek-quit ()
  (interactive)
  (when source-peek-popup
    (quick-peek-hide (source-peek-popup-pos source-peek-popup))
    (setq source-peek-popup nil)))

(defun source-peek-keep-keymap-p ()
  (member last-command '(source-peek source-peek-scroll-up source-peek-scroll-down)))

(defvar source-peek-keymap
  (let ((map (make-keymap)))
    (define-key map [t] #'source-peek-quit)
    (define-key map (kbd "<down>") #'source-peek-scroll-down)
    (define-key map (kbd "<up>") #'source-peek-scroll-up)
    map)
  "Keymap used in `source-peek-keymap'.")

(defun source-peek ()
  (interactive)
  (source-peek-display-locations (mapcar #'source-peek-fill-location
                                         (source-peek-fetch-locations))))

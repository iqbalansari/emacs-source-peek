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
  location)

(defun source-peek--truncate-definition (location &optional lines)
  (with-temp-buffer
    (insert (source-peek-location-definition location))
    (quick-peek--truncate-buffer 0 (or lines 10))
    (buffer-string)))


(defun source-peek-cleanup ()
  (quick-peek-hide)
  (remove-hook 'post-command-hook #'source-peek-cleanup))

(defun source-peek-display-locations (locations)
  (quick-peek-show (with-temp-buffer
                     (dolist (location locations)
                       (insert (propertize (format "\n%s:%d"
                                                   (source-peek-location-file location)
                                                   (source-peek-location-line location))
                                           'face 'link
                                           'location location))
                       (insert "\n\n")
                       (insert (source-peek--truncate-definition location
                                                                 (when (> (length locations) 1)
                                                                   5))))
                     (buffer-string))
                   (line-beginning-position)
                   'none
                   'none)
  (run-at-time 0 nil
               (lambda ()
                 (add-hook 'post-command-hook #'source-peek-cleanup))))

(defun source-peek ()
  (interactive)
  (source-peek-display-locations (mapcar #'source-peek-fill-location
                                         (source-peek-fetch-locations))))

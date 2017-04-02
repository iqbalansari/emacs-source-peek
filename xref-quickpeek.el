(require 'quick-peek)
(require 'subr-x)
(require 'cl)

(defun xref-quick-peek-get-location-marker (xref)
  (let ((inhibit-message t))
    (save-excursion
      (xref-location-marker (xref-item-location xref)))))

(defun xref-quick-peek-extract-source (marker)
  (save-window-excursion
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let ((start-of-def (line-beginning-position))
              (end-of-def (save-excursion
                            (end-of-defun)
                            (point))))
          (font-lock-fontify-region start-of-def end-of-def)
          (list (or (buffer-file-name) (buffer-name))
                (line-number-at-pos)
                (buffer-substring start-of-def end-of-def)))))))

(defun xref-quick-peek-display-defs (defs)
  (dolist (def defs)
    (quick-peek-show (string-join (list
                                   (propertize (string-join (list (second def) (format "%s" (third def))) ":")
                                               'face 'link)
                                   (fourth def))
                                  "\n\n")
                     (line-beginning-position))))

(defun xref-quick-peek-at-point ()
  (interactive)
  (let* ((backend (or (xref-find-backend)
                      (user-error "No xref backend found for current buffer")))
         (identifier-at-point (or (xref-backend-identifier-at-point backend)
                                  (user-error "No identifier found at current point")))
         (xrefs (xref-backend-definitions backend identifier-at-point))
         sources)
    (if (not xrefs)
        (user-error "No definitions found for `%s'" identifier-at-point)
      (message "Fetching definitions ... ")
      (dolist (xref xrefs)
        (let ((location (xref-quick-peek-get-location-marker xref)))
          (push (cons xref
                      (xref-quick-peek-extract-source location))
                sources))))
    (xref-quick-peek-display-defs sources)))

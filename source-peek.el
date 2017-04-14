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
  (member last-command '(source-peek source-peek-scroll-up source-peek-scroll-down)))

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

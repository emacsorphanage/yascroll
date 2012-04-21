;;; yascroll.el --- Yet Another Scroll Bar Mode

;; Copyright (C) 2011, 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience

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

;; M-x global-yascroll-bar-mode RET.
;; M-x customize-group RET yascroll RET.

;;; Code:

(eval-when-compile
  (require 'cl))



;;; Utilities:

(defun yascroll:listify (object)
  (if (listp object) object (list object)))

(defun yascroll:vertical-motion (lines)
  "A portable version of `vertical-motion'."
  (cond ((>= emacs-major-version 23)
         (vertical-motion lines))
        ((consp lines)
         (prog1 (vertical-motion (cdr lines))
           (move-to-column (+ (current-column) (car lines)))))
        (t
         (vertical-motion lines))))

(defun yascroll:line-edge-position ()
  "Return \(POINT PADDING) where POINT is the most neareat
logical position to the right-edge of the window, and PADDING is
a positive number of padding to the edge."
  (save-excursion
    (let* ((window-width (window-width))
           (column-bol (progn (yascroll:vertical-motion (cons 0 0))
                              (current-column)))
           (column-eol (progn (yascroll:vertical-motion
                               (cons (- window-width 1 (if window-system 0 1)) 0))
                              (current-column)))
           (column-eol-visual (- column-eol column-bol))
           (padding (- window-width column-eol-visual (if window-system 0 1))))
      (list (point) padding))))



;;; Configurations:

(defgroup yascroll nil
  "Yet Another Scroll Bar Mode."
  :group 'convenience
  :prefix "yascroll:")

(defface yascroll:thumb-text-area
  '((t (:background "slateblue")))
  "Face for text-area scroll bar thumb."
  :group 'yascroll)

(defface yascroll:thumb-fringe
  '((t (:background "slateblue" :foreground "slateblue")))
  "Face for fringe scroll bar thumb."
  :group 'yascroll)

(defcustom yascroll:scroll-bar
  '(right-fringe left-fringe text-area)
  "Position of scroll bar. The value is:

* 'right-fringe' for rendering scroll bar in right-fringe.
* 'left-fringe' for rendering scroll bar in left-fringe.
* 'text-area' for rendering scroll bar in text area.

The value can be also a list of them. In that case, yascroll in
turn checks for a candidate of the list is available on the
system. If no candidate satsify the condition, scroll bar will
not be displayed."
  :type '(repeat (choice (const :tag "Right Fringe" right-fringe)
                         (const :tag "Left Fringe" left-fringe)
                         (const :tag "Text Area" text-area)))
  :group 'yascroll)

(defcustom yascroll:delay-to-hide 0.5
  "Delay to hide scroll bar in seconds. nil means never hide
scroll bar."
  :type '(choice (const :tag "Never Hide" nil)
                 (number :tag "Seconds"))
  :group 'yascroll)

(defcustom yascroll:enabled-window-systems
  '(nil x w32 ns pc)
  "A list of `window-system's where yascroll can work."
  :type '(repeat (choice (const :tag "Termcap" nil)
                         (const :tag "X window" x)
                         (const :tag "MS-Windows" w32)
                         (const :tag "Macintosh Cocoa" ns)
                         (const :tag "MS-DOS" pc)))
  :group 'yascroll)

(defcustom yascroll:disabled-modes
  nil
  "A list of major-modes where yascroll can't work."
  :type '(repeat symbol)
  :group 'yascroll)



;;; Scroll Bar Thumb

(defvar yascroll:thumb-overlays nil
  "Overlays for scroll bar thum.")
(make-variable-buffer-local 'yascroll:thumb-overlays)

(defun yascroll:compute-thumb-size (window-lines buffer-lines)
  "Return the proper size (height) of scroll bar thumb."
  (if (zerop buffer-lines)
      1
    (max 1 (floor (* (/ (float window-lines) buffer-lines) window-lines)))))

(defun yascroll:compute-thumb-window-line (window-lines buffer-lines scroll-top)
  "Return the line number of scroll bar thumb relative to window."
  (if (zerop buffer-lines)
      0
    (floor (* window-lines (/ (float scroll-top) buffer-lines)))))

(defun yascroll:make-thumb-overlay-text-area ()
  (destructuring-bind (edge-pos edge-padding)
      (yascroll:line-edge-position)
    (if (= edge-pos (line-end-position))
        (let ((overlay (make-overlay edge-pos edge-pos))
              (after-string
               (concat (make-string (1- edge-padding) ?\ )
                       (propertize " " 'face 'yascroll:thumb-text-area))))
          (put-text-property 0 1 'cursor t after-string)
          (overlay-put overlay 'after-string after-string)
          (overlay-put overlay 'window (selected-window))
          overlay)
      (let ((overlay (make-overlay edge-pos (1+ edge-pos)))
            (display-string
             (propertize " "
                         'face 'yascroll:thumb-text-area
                         'cursor t)))
        (overlay-put overlay 'display display-string)
        (overlay-put overlay 'window (selected-window))
        overlay))))

(defun yascroll:make-thumb-overlay-fringe (left-or-right)
  (let* ((pos (point))
         ;; If `pos' is at the beginning of line, overlay of the
         ;; fringe will be on the previous visual line.
         (pos (if (= (line-end-position) pos) pos (1+ pos)))
         (display-string `(,left-or-right filled-rectangle yascroll:thumb-fringe))
         (after-string (propertize " " 'display display-string))
         (overlay (make-overlay pos pos)))
    (overlay-put overlay 'after-string after-string)
    (overlay-put overlay 'fringe-helper t)
    (overlay-put overlay 'window (selected-window))
    overlay))

(defun yascroll:make-thumb-overlay-left-fringe ()
  (yascroll:make-thumb-overlay-fringe 'left-fringe))

(defun yascroll:make-thumb-overlay-right-fringe ()
  (yascroll:make-thumb-overlay-fringe 'right-fringe))

(defun yascroll:make-thumb-overlays (make-thumb-overlay window-line size)
  "Make overlays of scroll bar thumb at WINDOW-LINE with SIZE."
  (save-excursion
    ;; Jump to the line.
    (move-to-window-line 0)
    (vertical-motion window-line)
    ;; Make thumb overlays.
    (condition-case nil
        (loop repeat size
              do (push (funcall make-thumb-overlay)
                       yascroll:thumb-overlays)
              until (zerop (vertical-motion 1)))
      (end-of-buffer nil))))

(defun yascroll:delete-thumb-overlays ()
  "Delete overlays of scroll bar thumb."
  (when yascroll:thumb-overlays
    (mapc 'delete-overlay yascroll:thumb-overlays)
    (setq yascroll:thumb-overlays nil)))



;;; Scroll Bar

(defun yascroll:schedule-hide-scroll-bar ()
  "Hide scroll bar automatically."
  (when yascroll:delay-to-hide
    (run-with-idle-timer yascroll:delay-to-hide nil
                         (lambda (buffer)
                           (with-current-buffer buffer
                             (yascroll:hide-scroll-bar)))
                         (current-buffer))))

(defun yascroll:choose-scroll-bar ()
  (when (memq window-system yascroll:enabled-window-systems)
    (loop with (left-width right-width &rest) = (window-fringes)
          for scroll-bar in (yascroll:listify yascroll:scroll-bar)
          if (or (eq scroll-bar 'text-area)
                 (and (eq scroll-bar 'left-fringe)
                      (> left-width 0))
                 (and (eq scroll-bar 'right-fringe)
                      (> right-width 0)))
          return scroll-bar)))

(defun yascroll:show-scroll-bar ()
  "Show scroll bar in BUFFER."
  (interactive)
  (yascroll:hide-scroll-bar)
  (let ((scroll-bar (yascroll:choose-scroll-bar)))
    (when scroll-bar
      (let ((window-lines (window-height))
            (buffer-lines (count-lines (point-min) (point-max))))
        (when (< window-lines buffer-lines)
          (let* ((scroll-top (count-lines (point-min) (window-start)))
                 (thumb-window-line (yascroll:compute-thumb-window-line
                                     window-lines buffer-lines scroll-top))
                 (thumb-buffer-line (+ scroll-top thumb-window-line))
                 (thumb-size (yascroll:compute-thumb-size
                              window-lines buffer-lines))
                 (make-thumb-overlay
                  (ecase scroll-bar
                    (left-fringe 'yascroll:make-thumb-overlay-left-fringe)
                    (right-fringe 'yascroll:make-thumb-overlay-right-fringe)
                    (text-area 'yascroll:make-thumb-overlay-text-area))))
            (when (<= thumb-buffer-line buffer-lines)
              (yascroll:make-thumb-overlays make-thumb-overlay
                                            thumb-window-line
                                            thumb-size)
              (yascroll:schedule-hide-scroll-bar))))))))

(defun yascroll:hide-scroll-bar ()
  "Hide scroll bar of BUFFER."
  (interactive)
  (yascroll:delete-thumb-overlays))

(defun yascroll:scroll-bar-visible-p ()
  "Return non-nil if scroll bar is visible."
  (and yascroll:thumb-overlays t))

(defun yascroll:handle-error (&optional var)
  (message "yascroll: %s" var)
  (ignore-errors (yascroll-bar-mode -1))
  (message "yascroll-bar-mode disabled")
  var)

(defun yascroll:safe-show-scroll-bar ()
  "Same as `yascroll:show-scroll-bar' except that if errors
occurs in this function, this function will suppress the errors
and disable `yascroll-bar-mode'."
  (condition-case var
      (yascroll:show-scroll-bar)
    (error (yascroll:handle-error var))))

(defun yascroll:update-scroll-bar ()
  (when (yascroll:scroll-bar-visible-p)
    (yascroll:safe-show-scroll-bar)))

(defun yascroll:before-change (beg end)
  (yascroll:hide-scroll-bar))

(defun yascroll:after-window-scroll (window start)
  (when (eq (selected-window) window)
    (yascroll:safe-show-scroll-bar)))

(defun yascroll:after-window-configuration-change ()
  (yascroll:update-scroll-bar))

(define-minor-mode yascroll-bar-mode
  "Yet Another Scroll Bar Mode."
  :group 'yascroll
  (if yascroll-bar-mode
      (progn
        (add-hook 'before-change-functions 'yascroll:before-change nil t)
        (add-hook 'window-scroll-functions 'yascroll:after-window-scroll nil t)
        (add-hook 'window-configuration-change-hook 'yascroll:after-window-configuration-change nil t))
    (yascroll:hide-scroll-bar)
    (remove-hook 'before-change-functions 'yascroll:before-change t)
    (remove-hook 'window-scroll-functions 'yascroll:after-window-scroll t)
    (remove-hook 'window-configuration-change-hook 'yascroll:after-window-configuration-change t)))

(defun yascroll:enabled-buffer-p (buffer)
  "Return non-nil if yascroll is enabled on BUFFER."
  (with-current-buffer buffer
    (and (not (minibufferp))
         (not (memq major-mode yascroll:disabled-modes)))))

(defun yascroll:turn-on ()
  (when (yascroll:enabled-buffer-p (current-buffer))
    (yascroll-bar-mode 1)))

(define-global-minor-mode global-yascroll-bar-mode
  yascroll-bar-mode yascroll:turn-on
  :group 'yascroll)

(provide 'yascroll)
;;; yascroll.el ends here

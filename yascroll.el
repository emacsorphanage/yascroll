;;; yascroll.el --- Yet Another Scroll Bar Mode

;; Copyright (C) 2011  Tomohiro Matsuyama

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

;; 

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup yascroll nil
  "Yet Another Scroll Bar Mode."
  :group 'convenience
  :prefix "yascroll:")

(defface yascroll:thumb-face
  '((t (:background "slateblue")))
  "Face for scroll bar thumb."
  :group 'yascroll)

(defcustom yascroll:delay-to-hide 0.5
  "Delay to hide scroll bar in seconds. nil means never hide
scroll bar."
  :type '(choice (const :tag "Never Hide" nil)
                 (number :tag "Seconds"))
  :group 'yascroll)

(defcustom yascroll:disabled-modes
  nil
  "List of major-mode specifying buffers where yascroll can't
work."
  :type '(repeat symbol)
  :group 'yascroll)

(defvar yascroll:thumb-overlays nil
  "Overlays for scroll bar thum.")
(make-variable-buffer-local 'yascroll:thumb-overlays)

(defun yascroll:line-edge-position ()
  "Return POSITION and PADDING where POSITION is the most neareat
position of the right-edge of the window, and PADDING is a
positive number of padding againt the edge."
  (save-excursion
    (let* ((window-width (window-width))
           (window-hscroll (window-hscroll))
           (current-column (move-to-column (1- (+ window-width window-hscroll))))
           (padding (- window-width (- current-column window-hscroll))))
      (list (point) (max 0 padding)))))

(defun yascroll:compute-thumb-size (window-lines buffer-lines)
  "Return the size (height) of scroll bar thumb."
  (if (member 0 (list window-lines buffer-lines))
      1
    (max 1 (floor (/ (float (expt window-lines 2))
                     buffer-lines)))))

(defun yascroll:compute-thumb-line (window-lines buffer-lines scroll-top)
  "Return the line number of scroll bar thumb."
  (if (eq buffer-lines 0)
      0
    (let ((relative-in-window
           (floor (* window-lines
                     (/ (float scroll-top) buffer-lines)))))
      (+ scroll-top relative-in-window))))

(defun yascroll:make-thumb-overlay ()
  (destructuring-bind (edge-pos edge-padding)
      (yascroll:line-edge-position)
    (if (eq edge-pos (line-end-position))
        (let ((overlay (make-overlay edge-pos edge-pos))
              (after-string
               (concat (make-string (1- edge-padding) ?\ )
                       (propertize " " 'face 'yascroll:thumb-face))))
          (put-text-property 0 1 'cursor t after-string)
          (overlay-put overlay 'after-string after-string)
          overlay)
      (let ((overlay (make-overlay edge-pos (1+ edge-pos)))
            (display-string
             (propertize " "
                         'face 'yascroll:thumb-face
                         'cursor t)))
        (overlay-put overlay 'display display-string)
        overlay))))

(defun yascroll:make-thumb-overlays (line size)
  "Make overlays of scroll bar thumb at LINE with SIZE."
  (save-excursion
    ;; Jump to the line
    (goto-char (point-min))
    (forward-line line)
    ;; Make thumb overlays
    (loop repeat size
          do (push (yascroll:make-thumb-overlay)
                   yascroll:thumb-overlays)
          while (eq (forward-line) 0))))

(defun yascroll:delete-thumb-overlays ()
  "Delete overlays of scroll bar thumb."
  (when yascroll:thumb-overlays
    (mapc 'delete-overlay yascroll:thumb-overlays)
    (setq yascroll:thumb-overlays nil)))

(defun yascroll:schedule-hide-scroll-bar ()
  "Hide scroll bar automatically."
  (when yascroll:delay-to-hide
    (run-with-idle-timer yascroll:delay-to-hide nil
                         (lambda (buffer)
                           (with-current-buffer buffer
                             (yascroll:hide-scroll-bar)))
                         (current-buffer))))

(defun yascroll:show-scroll-bar ()
  "Show scroll bar in BUFFER."
  (interactive)
  (yascroll:hide-scroll-bar)
  (let ((window-lines (window-body-height))
        (buffer-lines (count-lines (point-min) (point-max))))
    (when (< window-lines buffer-lines)
      (let* ((scroll-top (count-lines (point-min) (window-start)))
             (thumb-line (yascroll:compute-thumb-line
                          window-lines buffer-lines scroll-top))
             (thumb-size (yascroll:compute-thumb-size
                          window-lines buffer-lines)))
        (when (<= thumb-line buffer-lines)
          (yascroll:make-thumb-overlays thumb-line thumb-size)
          (yascroll:schedule-hide-scroll-bar))))))

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
occured in this function, this function suppresses the errors and
disables `yascroll-bar-mode'."
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
  yascroll-bar-mode yascroll:turn-on)

(provide 'yascroll)
;;; yascroll.el ends here

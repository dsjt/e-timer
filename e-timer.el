;;; e-timer.el --- Use timer in Emacs.

;; Copyright (C) 2014  Ishida Tatsuhiro

;; Author: Ishida Tatsuhiro <toot.daiylfalaiydt@gmail.com>
;; Keywords: extensions

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

;; This program allow you to use timer in Emacs and operate it with Emacs.

;;; Code:


(defcustom e-timer-music-command nil
  "Music-command identifies a command for alert time up.")
(defcustom e-timer-alert-resources nil
  "Alert-resources is a sound file to alert you that time up")

(defvar e-timer-buffer-name "*e-timer*")
(defvar e-timer-format "%s %s")
(defvar e-timer-object nil)

(defun e-timer-alert ()
  "Play alert-sound"
  (let ((command e-timer-music-command)
        (alert-resources e-timer-alert-resources))
    (cond ((and command alert-resources)
           (start-process "*e-timer*" e-timer-buffer-name command alert-resources))
          (t (progn (message "Inappropriate command or alert-resources.")
                    nil)))))

(defun e-timer-set-timer (effort-time)
  (interactive "sSet [default: min] : \n")
  (e-timer-set-timer-1 effort-time))

(defun e-timer-set-timer-1 (effort-time)
  (let ((time (timer-duration effort-time)))
    (if (setq e-timer-object (run-with-timer time nil 'e-timer-alert))
        t
      nil)))

(defun e-timer-cancel ()
  (interactive)
  (when (y-or-n-p "End timer? ")
    (cancel-timer e-timer-object)))
;; (setq tc_timer (run-with-timer 3 nil (lambda () (message "ths"))))
;; tc_timer
;; [t 21622 45792 280000 nil (lambda nil (message "ths")) nil nil 0]
;; [nil 21622 45792 280000 nil (lambda nil (message "ths")) nil nil 0]

(provide 'e-timer)
;;; e-timer.el ends here

(load-file "~/programming/elisp/e-timer/private.el")

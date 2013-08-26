;;; scratch-message.el ---

;; Copyright (C) 2013 Sylvain Rousseau

;; Author: Sylvain Rousseau <thisirs at gmail dot com> Maintainer:
;; Sylvain Rousseau <thisirs at gmail dot com> Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code

(defvar scratch-message-function
  'scratch-message-default)

(defvar scratch-message-interval 10
  "Time in seconds to wait between two messages.")

(defvar scratch-message-invisible t
  "If non-nil, do not change message if the scratch buffer is
visible.")

(defvar scratch-message-retry 3
  "Time in seconds to wait before trying to redisplay.")

;; Internal variables
(defvar scratch-message-timer nil)
(defvar scratch-message-beg-marker (make-marker))
(defvar scratch-message-end-marker (make-marker))
(defvar scratch-message-timestamp nil)

(defun scratch-message-insert (s &optional fill)
  "Replace current scratch message or insert it at the end of the scratch buffer.

If FILL is non-nil, the string is filled."
  (with-current-buffer (get-buffer-create "*scratch*")
    (if (and (marker-position scratch-message-beg-marker)
             (marker-position scratch-message-end-marker))
        (delete-region scratch-message-beg-marker scratch-message-end-marker))
    (save-excursion
      (goto-char (if (marker-position scratch-message-beg-marker)
                     (marker-position scratch-message-beg-marker)
                   (point-max)))
      (set-marker scratch-message-beg-marker (point))
      (insert s)
      (set-marker scratch-message-end-marker (point))
      (comment-region scratch-message-beg-marker
                      scratch-message-end-marker)
      (if fill
          (let ((paragraph-start "^")
                (paragraph-separate "\n")
                (fill-prefix ";; "))
            (fill-region scratch-message-beg-marker
                         scratch-message-end-marker))))))

(defun scratch-message-default ()
  (let* ((message-buffer-name "*SCMB*")
         (message-buffer (or (get-buffer message-buffer-name)
                             (generate-new-buffer message-buffer-name)))
         (proc (start-process "SCMB" message-buffer-name "ruby"
                              "/home/sylvain/Dropbox/scripts/DTC.rb")))
    (with-current-buffer message-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (string= "finished\n" event)
         (scratch-message-insert
          (with-current-buffer "*SCMB*" (buffer-string)) 'fill))))))

(defun scratch-message-fortune ()
  (require 'fortune)
  (fortune-in-buffer t "~/.conky/english-idioms")
  (scratch-message-insert
   (with-current-buffer fortune-buffer-name
     (buffer-string))
   'fill))

(defun scratch-message-new-message ()
  "Display a new message in scratch buffer.

If `scratch-message-invisible' is non-nil and the scratch buffer
is currently displayed in one of the windows of the current
frame, wait `scratch-message-retry' seconds before giving another
try."
  (if (and scratch-message-invisible
           (get-buffer-window "*scratch*"))
      (setq scratch-message-timer
            (run-with-timer scratch-message-retry nil 'scratch-message-new-message))
    (when (or (not scratch-message-timestamp)
              (time-less-p scratch-message-timestamp
                           (buffer-local-value 'buffer-display-time
                                               (get-buffer "*scratch*"))))
      (funcall scratch-message-function)
      (setq scratch-message-timestamp (current-time)))
    (setq scratch-message-timer (run-with-timer
                                 scratch-message-interval
                                 nil
                                 'scratch-message-new-message))))

(defun scratch-message-toggle-activate (&optional arg)
  "Toggle `scratch-message'. If ARG is non-nil, activate
`scratch-message' if ARG is non-numeric or >= 0."
  (interactive "P")
  (if (timerp scratch-message-timer)
      (if (and arg (> (prefix-numeric-value arg) 0))
          (error "Already enabled!")
        (cancel-timer scratch-message-timer)
        (setq scratch-message-timer nil)
        (message "scratch-message disabled"))
    (if (and arg (< (prefix-numeric-value arg) 0))
        (error "Already disabled!")
      (scratch-message-new-message)
      (message "scratch-message enabled"))))

(provide 'scratch-message)

;;; scratch-message.el ends here

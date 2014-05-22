;;; scratch-message.el --- Changing message in your scratch buffer

;; Copyright (C) 2013 Sylvain Rousseau

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/scratch-message.git
;; Keywords: util scratch

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

;; This utility allows you to automatically insert messages in your
;; scratch buffer coming from various sources.

;;; Installation:

;; Just put the following in your .emacs:

;; (require 'scratch-message)

;; and customize the variable `scratch-message-function'.

;;; Code

(defvar scratch-message-function 'ignore
  "Function called by `scratch-message-new-message' that should
generate a message and insert it by calling
`scratch-message-insert'.")

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

(defun scratch-message-insert (message)
  "Replace or insert the message MESSAGE in the scratch buffer."
  (if (get-buffer "*scratch*")
      (with-current-buffer "*scratch*"
        (if (and (marker-position scratch-message-beg-marker)
                 (marker-position scratch-message-end-marker))
            (delete-region scratch-message-beg-marker scratch-message-end-marker))
        (save-excursion
          (goto-char (or (marker-position scratch-message-beg-marker)
                         (point-max)))
          (set-marker scratch-message-beg-marker (point))
          (insert message)
          (set-marker scratch-message-end-marker (point))
          (comment-region scratch-message-beg-marker
                          scratch-message-end-marker)))
    (error "No scratch buffer")))

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
    (when (and (get-buffer "*scratch*")
               (or (not scratch-message-timestamp)
                   (and (let ((ts (buffer-local-value 'buffer-display-time
                                                      (get-buffer "*scratch*"))))
                          (and ts (time-less-p scratch-message-timestamp ts))))))
      (with-demoted-errors
          (funcall scratch-message-function))
      (setq scratch-message-timestamp (current-time)))
    (setq scratch-message-timer (run-with-timer
                                 scratch-message-interval
                                 nil
                                 'scratch-message-new-message))))

;;;###autoload
(defun scratch-message-toggle-activate (&optional arg)
  "Toggle `scratch-message'. If ARG is non-nil, activate
`scratch-message' if ARG is non-numeric or >= 0.

With a prefix argument ARG, enable `scratch-message' if ARG is
positive, and disable it otherwise. If called from Lisp, enable
`scratch-message' if ARG is omitted or nil."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enabled (if (eq arg 'toggle)
                     (not (timerp scratch-message-timer))
                   (> (prefix-numeric-value arg) 0))))

    (if enabled
        (run-with-timer 10 nil 'scratch-message-new-message)
      (cancel-timer scratch-message-timer)
      (setq scratch-message-timer nil))

    (when (called-interactively-p 'any)
      (message "scratch-message %sabled" (if enabled "en" "dis")))))

(provide 'scratch-message)

;;; scratch-message.el ends here

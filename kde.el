;;; kde.el --- KDE interaction from Emacs

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.1.0
;; Keywords: kde
;; URL: https://github.com/lepisma/kde.el

;;; Commentary:

;; kde.el is a personal package for interacting with few KDE applications
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)

(defun kde-kmail-compose (&optional body subject attachment file address)
  "Open KMail composer window with given arguments."
  (let ((body (or body ""))
        (subject (or subject ""))
        (address (or address ""))
        (attachment (or attachment ""))
        (file (or file "")))
    (start-process "kmail-composer" nil "kmail"
                   "--body" body
                   "--subject" subject
                   "--attach" attachment
                   "--msg" file
                   address)))

;;;###autoload
(defun kde-kmail-send-file ()
  "Send current file."
  (interactive)
  (kde-kmail-compose nil nil nil buffer-file-name))

;;;###autoload
(defun kde-kmail-send-attachment ()
  "Send current file as attachment."
  (interactive)
  (kde-kmail-compose nil nil buffer-file-name))

(defun kde-kmail-send-region (start end)
  "Send current region."
  (interactive "r")
  (kde-kmail-compose (buffer-substring-no-properties start end)))

(defun kde-kmail-send-org-entry ()
  "Send current org entry as text."
  (interactive)
  (kde-kmail-compose (org-get-entry) (org-get-heading t t)))

;;;###autoload
(defun kde-kmail-send-default ()
  "Default send mode."
  (interactive)
  (cond
   ((region-active-p)
    (kde-kmail-send-region (region-beginning) (region-end)))
   ((org-get-heading)
    (kde-kmail-send-org-entry))
   ((string-match "\\.csv\\'" buffer-file-name)
    (kde-kmail-send-attachment))
   ((eq buffer-file-coding-system 'no-conversion)
    (kde-kmail-send-attachment))
   (t (kde-kmail-send-file))))

(defun kde--org-scheduled-or-deadline ()
  "Return scheduled or deadline time from current point in order of priority"
  (let ((time (org-get-scheduled-time (point))))
    (unless time
      (setq time (org-get-deadline-time (point))))
    (if time
        (format-time-string "%Y-%m-%d-%H:%M" time)
      nil)))

;;;###autoload
(defun kde-kalarm-set-org ()
  "Set an alarm for current org entry (schedule/deadline) in kalarm"
  (interactive)
  (let ((time (kde--org-scheduled-or-deadline))
        (message (substring-no-properties (org-get-heading t t))))
    (if (and time message)
        (if (eq 0 (call-process "kalarm" nil nil nil
                                "-t" time
                                message))
            (message (concat "Alarm set for : " message))
          (message "Error in setting alarm"))
      (message "Error in parsing entry"))))

;;;###autoload
(defun kde-explore ()
  "Open dolphin in current buffer's directory."
  (interactive)
  (start-process-shell-command "kde-explore" nil "dolphin ."))

(provide 'kde)

;;; kde.el ends here

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

(defun kde-kmail-send-file ()
  "Send current file."
  (interactive)
  (kde-kmail-compose nil nil nil (buffer-file-name)))

(defun kde-kmail-send-attachment ()
  "Send current file as attachment."
  (interactive)
  (kde-kmail-compose nil nil (buffer-file-name)))

(defun kde-kmail-send-region (start end)
  "Send current region."
  (interactive "r")
  (kde-kmail-compose (buffer-substring-no-properties start end)))

(defun kde-kmail-send-org-entry ()
  "Send current org entry as text."
  (interactive)
  (kde-kmail-compose (org-get-entry) (org-get-heading t t)))

(defun kde--org-scheduled-or-deadline ()
  "Return scheduled or deadline time from current point in order of priority"
  (let ((time (org-get-scheduled-time (point))))
    (unless time
      (setq time (org-get-deadline-time (point))))
    (if time
        (format-time-string "%Y-%m-%d-%H:%M" time)
      nil)))

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
          (display-warning :error "Error in setting alarm"))
      (display-warning :error "Error in parsing entry"))))

(defun kde-explore ()
  "Open dolphin in current buffer's directory."
  (interactive)
  (start-process-shell-command "kde-explore" nil "dolphin ."))

(provide 'kde)

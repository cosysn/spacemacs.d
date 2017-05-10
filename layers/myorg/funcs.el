
(when (configuration-layer/package-usedp 'org)
  (defun myorg/find-gtdfile ()
    "Edit the `gtdfile', in the current window."
    (interactive)
    (find-file-existing (org-agenda-file-gtd))))

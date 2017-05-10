;;; config.el --- Org configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar org-agenda-dir "~/myorg"
  "gtd org files location")
  
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-index (expand-file-name "index.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-someday (expand-file-name "someday.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-birthday (expand-file-name "birthday.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "index.org" org-agenda-dir))



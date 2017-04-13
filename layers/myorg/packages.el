;;; packages.el --- myorg layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <Administrator@SHINING-PC>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `myorg-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `myorg/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `myorg/pre-init-PACKAGE' and/or
;;   `myorg/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst myorg-packages
  '(
    (org :location built-in)
    alert
    org-pomodoro
    )
  "The list of Lisp packages required by the myorg layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the


 name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; For each package, define a function myorg/init-<package-name>
;;
;; (defun myorg/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun myorg/init-alert ()
  (use-package alert
    :defer t
    :config
    (progn
      (setq exec-path (append exec-path '("~/.spacemacs.d/plugins/growlforwin")))
      (defcustom alert-growlforwin-command (executable-find "growlnotify")
        "Path to the growlnotify  command. This is found in the Growl Extras: http://growl.info/extras.php."
        :type 'file
        :group 'alert)

      (message (executable-find "growlnotify"))
      (defcustom alert-growlforwin-priorities
        '((urgent   . 2)
          (high     . 2)
          (moderate . 1)
          (normal   . 0)
          (low      . -1)
          (trivial  . -2))
        "A mapping of alert severities onto Growl priority values."
        :type '(alist :key-type symbol :value-type integer)
        :group 'alert)

      (defun alert-growlforwin-notify (info)
        (if alert-growlforwin-command
            (let* ((title (alert-encode-string (plist-get info :title)))
                   (priority (number-to-string
                              (cdr (assq (plist-get info :severity)
                                         alert-growlforwin-priorities))))
                   (args
                    (case system-type
                      ('windows-nt (mapcar
                                    (lambda (lst) (apply #'concat lst))
                                    `(
                                      ;; http://www.growlforwindows.com/gfw/help/growlnotify.aspx
                                      ("/i:" ,(file-truename (concat invocation-directory "../share/icons/hicolor/48x48/apps/emacs.png")))
                                      ("/t:" ,title)
                                      ("/p:" ,priority))))
                      (t (list
                          "--appIcon"  "Emacs"
                          "--name"     "Emacs"
                          "--title"    title
                          "--priority" priority)))))
              (if (and (plist-get info :persistent)
                       (not (plist-get info :never-persist)))
                  (case system-type
                    ('windows-nt (nconc args (list "/s:true")))
                    (t (nconc args (list "--sticky")))))
              (let ((message (alert-encode-string (plist-get info :message))))
                (case system-type
                  ('windows-nt (nconc args (list message)))
                  (t (nconc args (list "--message" message)))))
              (apply #'call-process alert-growlforwin-command nil nil nil args))
          (alert-message-notify info)))

      (alert-define-style 'growlforwin :title "Notify using Growl"
                          :notifier #'alert-growlforwin-notify)

      (setq alert-default-style 'growlforwin)
      )))

(defun myorg/post-init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (message "myorg/post-init-org-pomodoro: enter function")
      (setq org-pomodoro-keep-killed-pomodoro-time t)
      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
      (message "myorg/post-init-org-pomodoro: exit function"))
    :config
    (progn
      (when (configuration-layer/package-usedp 'alert)
            (progn
              (add-hook 'org-pomodoro-finished-hook '(lambda ()
                                                       (alert "☕️ Have a break!" :title "Pomodoro Finished")))
              (add-hook 'org-pomodoro-short-break-finished-hook '(lambda ()
                                                                   (alert "🐝 Ready to Go?" :title "Short Break")))
              (add-hook 'org-pomodoro-long-break-finished-hook '(lambda ()
                                                                  (alert "💪 Ready to Go?" :title "Long Break"))))))))

(defun myorg/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (message "myorg/post-init-org: enter function")
      (spacemacs|disable-company org-mode)

      (myorg/init-myorg-capture)
      (myorg/init-myorg-clock)

      (message "myorg/post-init-org: exit function")
      ))
  )

(defun myorg/init-myorg-capture ()
  "Org capture templates"

  ;; define the refile targets
  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-dir))

  ;; the %i would copy the selected text into the template
  ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
  ;;add multi-file journal
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
           "* TODO [#B] %?\n  %i\n"
           :empty-lines 1)
          ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
           "* %?\n  %i\n %U"
           :empty-lines 1)
           ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
            "* TODO [#B] %?\n  %i\n %U"
            :empty-lines 1)
           ("s" "Code Snippet" entry
            (file org-agenda-file-code-snippet)
            "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
           ("w" "work" entry (file+headline org-agenda-file-gtd "Cocos2D-X")
            "* TODO [#A] %?\n  %i\n %U"
            :empty-lines 1)
           ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
            "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
            :empty-lines 1)
           ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
            "* TODO [#C] %?\n  %i\n %a \n %U"
            :empty-lines 1)
           ("j" "Journal Entry"
            entry (file+datetree org-agenda-file-journal)
            "* %?"
            :empty-lines 1)))
  )

(defun myorg/init-myorg-clock ()
  "Org clock"
  (progn
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (with-eval-after-load 'org
                (org-clock-persistence-insinuate))
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)

    ;; Change task state to STARTED when clocking in
    ;; (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

    ;; Show clock sums as hours and minutes, not "n days" etc.
    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    ;;; Show the clocked-in task - if any - in the header line
    (defun sanityinc/show-org-clock-in-header-line ()
      (setq-default header-line-format '((" " org-mode-line-string " "))))

    (defun sanityinc/hide-org-clock-from-header-line ()
      (setq-default header-line-format nil))

    (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
    (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
    (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

    (with-eval-after-load 'org-clock
                (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
                (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


    ))
;;; packages.el ends here

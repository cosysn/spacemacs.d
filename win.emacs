;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner sylvain.benner@gmail.com
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://emacs-china.org/t/package-package-is-unavailable-is-the-package-name-misspelled/2662
;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')


(defvar program-dir "h:/Dev/myEmacs/")

(setenv "HOME" program-dir)
(setenv "PATH" program-dir)
(setq default-directory "~/")
(load-file "~/.emacs.d/init.el")
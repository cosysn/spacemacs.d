
(when (spacemacs/system-is-mswindows)
  ;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html
  (global-set-key (kbd "<C-mouse-4>") 'spacemacs/scale-up-font)
  (global-set-key (kbd "<C-mouse-5>") 'spacemacs/scale-down-font)
  )

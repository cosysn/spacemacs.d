;; Usage

;; https://emacs-china.org/t/topic/1348
;; ====================================Themes automatically change =====================================
;;timer for automatically changing themes
(setq myui--interval-timer nil)

;;table is used to save (time themes) pair for automatically changing themes
;;time should be a string. themes should be a variant , not symbos.
(setq myui--time-themes-table nil)

(defun myui/config-time-themes-table (tt)
  "Set time . themes table for time-themes-table."
  (setq myui--time-themes-table
      ;; sort firstly, get-themes-according require a sorted table.
      (sort tt (lambda (x y) (< (string-to-int (car x)) (string-to-int (car y)))))
        )
  )

(defun myui/get-themes-according (hour-string)
  "This function return the theme according to hour-string.
Value of hour-string should be between 1 and 24(including)."
  (catch 'break
    (let (
          (now-time (string-to-int hour-string))
          ;; init current-themes to the themes of final item
          (correct-themes (cdr (car (last myui--time-themes-table))))
          (loop-list myui--time-themes-table)
          )

        ;; loop to set correct themes to correct-themes
        (while loop-list
          (let ((v (car loop-list)))
            (let ((v-time (string-to-int (car v))) (v-themes (cdr v)))
              (if (< now-time v-time)
                (throw 'break correct-themes)  ; t
                (setq correct-themes v-themes) ; nil
                )))
          (setq loop-list (cdr loop-list))
        )
        ;; This is returned for value of hour-string is bigger than or equal to car of final item
        (throw 'break correct-themes) ; t
    ))
)

(defun myui/check-time-and-modify-theme ()
  "This function will get the theme of now according to time-table-themes,
then check whether emacs should to modify theme, if so, modify it."
  (let ((new-theme (myui/get-themes-according (format-time-string "%H"))))
    (unless (eq new-theme spacemacs--cur-theme)
      (spacemacs/load-theme new-theme)
    ))
  )

(defun myui/open-themes-auto-change ()
  "Start to automatically change themes."
  (interactive)
  (myui/check-time-and-modify-theme)
  (setq
   myui--interval-timer (run-at-time 3600 3600 'myui/check-time-and-modify-theme))
  (message "themes auto change open.")
  )

(defun myui/close-themes-auto-change ()
  "Close automatically change themes."
  (interactive)
  (cancel-timer myui--interval-timer)
  (message "themes auto change close.")
  )
  
;; item of time-themes-table: ( hours-in-string . theme-name)
;; 6:00 - 17::00 use spacemacs-light, 17:00 - 24:00 use monokai, 24:00 - 6:00 use spacemacs-light
;; you could add more items.
;; (myui/config-time-themes-table '(("6" . spacemacs-light) ("17" . monokai)))
;; (myui/open-themes-auto-change)

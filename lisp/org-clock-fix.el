;;;; Fix CLOCK_MODELINE_TOTAL: today
;;;; See http://lists.gnu.org/archive/html/emacs-orgmode/2017-11/msg00291.html

(defun org-clock-get-sum-start ()
  "Return the time from which clock times should be counted.

This is for the currently running clock as it is displayed in the
mode line.  This function looks at the properties LAST_REPEAT and
in particular CLOCK_MODELINE_TOTAL and the corresponding variable
`org-clock-mode-line-total' and then decides which time to use.

The time is always returned as UTC."
  (let ((cmt (or (org-entry-get nil "CLOCK_MODELINE_TOTAL"))))
     (symbol-name org-clock-mode-line-total))
  (lr (org-entry-get nil "LAST_REPEAT")
    (cond
     ((equal cmt "current")
      (setq org--msg-extra "showing time in current clock instance")
      (current-time))
     ((equal cmt "today")
      (setq org--msg-extra "showing today's task time.")
      (let* ((dt (org-decode-time nil nil)))
       (hour (nth 2 dt))
       (day (nth 3 dt))))))
  (if (< hour org-extend-today-until) (setf (nth 3 dt) (1- day)))
  (setf (nth 2 dt) org-extend-today-until)
  (setq dt (append (list 0 0) (nthcdr 2 dt) '(t)))
  (apply #'encode-time dt
     ((or (equal cmt "all")))
    (and (or (not cmt) (equal cmt "auto"))
         (not lr)
      (setq org--msg-extra "showing entire task time.")
      nil
     ((or (equal cmt "repeat"))))
    (and (or (not cmt) (equal cmt "auto"))
         lr
      (setq org--msg-extra "showing task time since last repeat.")
      (and lr (org-time-string-to-time lr t))
     (t nil))))

;;;; Configuration for whitespace-mode
;;;; Based on https://gist.github.com/ymasory/3794723

;;; make carriage returns blue and tabs green
(custom-set-faces
 '(my-carriage-return-face ((((class color)) (:foreground "blue"))) t)
 '(my-tab-face ((((class color)) (:foreground "green"))) t)
 )
;;; add custom font locks to all buffers and all files
(add-hook
 'font-lock-mode-hook
 (function
  (lambda ()
    (setq
     font-lock-keywords
     (append
      font-lock-keywords
      '(
        ("\r" (0 'my-carriage-return-face t))
        ("\t" (0 'my-tab-face t))
        ))))))

;;; make characters after column 80 purple
(setq whitespace-style '(face trailing tab-mark))
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (setq-local whitespace-style
                        (append whitespace-style '(lines-tail)))
            (whitespace-mode 1)))
(add-hook 'find-file-hook 'whitespace-mode)

;;; transform literal tabs into an rightwards arrow (⇥, U+21E5, decimal 8677).
(setq whitespace-display-mappings
      '(
        (tab-mark ?\t [?\u21E5 ?\x09] [?\xBB ?\x09] [?\\ ?\t])
                                        ;other substitutions...
        ))

(provide 'whitespace-conf)

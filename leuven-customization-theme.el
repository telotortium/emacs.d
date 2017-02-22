(deftheme leuven-customization
  "Customize leuven theme, in particular setting the comment face to the Emacs default.")

(custom-theme-set-faces
 'leuven-customization
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((((class color) (min-colors 89) (background light)) (:foreground "Firebrick")))))

(provide-theme 'leuven-customization)

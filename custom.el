(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.4)
 '(ac-use-menu-map t)
 '(ack-and-a-half-arguments (quote ("--nopager")))
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(dabbrev-case-fold-search t)
 '(evil-want-C-u-scroll t)
 '(history-length 5000)
 '(inferior-scheme-mode-hook (quote (paredit-mode evil-paredit-mode)))
 '(markdown-command "pandoc -f markdown -t html --toc -s --mathjax")
 '(mweb--major-mode (quote html-mode))
 '(mweb-filename-extensions (quote ("php" "htm" "html" "ctp" "phtml" "php4" "php5")))
 '(mweb-tags
   (quote
    ((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
     (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
     (css-mode "<style +type=\"text/css\"[^>]*>" "</style>"))))
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODO's"
      ((agenda "")
       (alltodo)))
     ("u" "Unscheduled TODOs" todo ""
      ((org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline)
          (quote regexp)
          "
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (expand-file-name "agenda_files" user-emacs-directory))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 7)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-capture-templates
   (quote
    (("t" "Tasks" entry
      (file+headline
       (concat org-directory "/todo.org")
       "Tasks")
      "* TODO %?
  %u")
     ("n" "Notes" entry
      (file+headline org-default-notes-file "Notes")
      "* %u %?"))))
 '(org-clock-idle-time 15)
 '(org-columns-default-format
   "%60ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %10CLOCKSUM_T")
 '(org-default-notes-file "~/Documents/org/google-org/notes.org")
 '(org-directory "~/Documents/org/google-org")
 '(org-global-properties
   (quote
    (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
     ("SYTLE_ALL" . "habit"))))
 '(org-list-allow-alphabetical t)
 '(org-pretty-entities t)
 '(org-refile-use-outline-path t)
 '(org-src-fontify-natively t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
 '(org-todo-state-tags-triggers
   (quote
    (("CANCELLED"
      ("CANCELLED" . t))
     ("WAITING"
      ("WAITING" . t))
     ("HOLD"
      ("WAITING")
      ("HOLD" . t))
     (done
      ("WAITING")
      ("HOLD"))
     ("TODO"
      ("WAITING")
      ("CANCELLED")
      ("HOLD"))
     ("NEXT"
      ("WAITING")
      ("CANCELLED")
      ("HOLD"))
     ("DONE"
      ("WAITING")
      ("CANCELLED")
      ("HOLD")))))
 '(org-use-sub-superscripts (quote {}))
 '(savehist-additional-variables
   (quote
    (kill-ring search-ring regexp-search-ring compile-history)))
 '(savehist-file
   (expand-file-name
    (concat
     (file-name-as-directory "cache")
     "savehist")
    user-emacs-directory))
 '(scheme-mode-hook
   (quote
    ((lambda nil
       (slime-mode t))
     paredit-mode evil-paredit-mode)))
 '(scheme-program-name "/opt/local/bin/csi -:c")
 '(show-paren-mode t)
 '(smex-save-file
   (expand-file-name
    (concat
     (file-name-as-directory "cache")
     "smex-items")
    user-emacs-directory))
 '(temporary-file-directory "/tmp")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 100 :width normal :foundry "apple" :family "Menlo"))))
 '(my-carriage-return-face ((((class color)) (:foreground "blue"))) t)
 '(my-tab-face ((((class color)) (:foreground "green"))) t)
 '(term-color-black ((t (:background "#000000" :foreground "#000000"))))
 '(term-color-blue ((t (:background "#4465a4" :foreground "#4465a4"))))
 '(term-color-cyan ((t (:background "#06989a" :foreground "#06989a"))))
 '(term-color-green ((t (:background "#4e9a06" :foreground "#4e9a06"))))
 '(term-color-magenta ((t (:background "#75507b" :foreground "#75507b"))))
 '(term-color-red ((t (:background "#be0000" :foreground "#be0000"))))
 '(term-color-white ((t (:background "#eeeeee" :foreground "#eeeeee"))))
 '(term-color-yellow ((t (:background "#c4a000" :foreground "#c4a000"))))
 '(term-default-bg-color ((t (:inherit term-color-black))) t)
 '(term-default-fg-color ((t (:inherit term-color-white))) t))

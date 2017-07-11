; -*- lexical-binding: t; no-byte-compile: t -*-
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.
;;;
;;; Byte compilation is disabled for this file to avoid ever loading stale
;;; bytecode. The commands in this file will ensure that stale bytecode is
;;; never loaded for other files.

;;; ---------------------------------------------------------------------------
;;;  Package loading
;;; ---------------------------------------------------------------------------
;;; Avoid loading .elc files older than their corresponding .el file.
(setq load-prefer-newer t)

; Fix TLS certificate "could not be verified" errors
; (http://emacs.stackexchange.com/a/18070).
(setq gnutls-verify-error t)

(setq tls-checktrust 'ask)

; Load all the packages that are REQUIRE'd below
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") 'append)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package bind-key)                  ; To make :bind work
(use-package diminish)                  ; To use :diminish
(setq use-package-always-ensure t)

;;; Use auto-compile to recompile bytecode whenever files are loaded or saved.
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Convenience commands to upgrade packages.
(use-package package-utils
  :ensure t
  :ensure epl)

;;; ---------------------------------------------------------------------------
;;;  Basic startup configuration
;;; ---------------------------------------------------------------------------
;;; Disable startup screen
(setq inhibit-startup-message t)

;;; Command to restart emacs from within emacs
(use-package restart-emacs)

;;; Allow access from emacsclient
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;; Leuven theme
(use-package leuven-theme
  :config
  (setq leuven-scale-outline-headlines nil)
  (load-theme 'leuven t)
  (load-theme 'leuven-customization t))


;;; ---------------------------------------------------------------------------
;;;  Helm configuration
;;; ---------------------------------------------------------------------------
(use-package helm :config (require 'helm-config))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
(diminish 'helm-mode)

(helm-autoresize-mode t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;;; ---------------------------------------------------------------------------
;;;  Evil configuration
;;; ---------------------------------------------------------------------------
;; Space as leader
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; Scroll with C-u and access universal-argument with <Leader>-u.
(custom-set-variables
 '(evil-want-C-u-scroll t))
(evil-leader/set-key "u" 'universal-argument)

(use-package evil :config (evil-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode)

;; Disable all the C-<number> keys, and give C-^ and C-6 the same behavior as
;; they have in Vim.
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(define-key evil-normal-state-map (kbd "C-^") 'switch-to-previous-buffer)
(define-key evil-normal-state-map (kbd "C-6") 'switch-to-previous-buffer)
(dolist (key (mapcar 'kbd
                     '("C-1" "C-2" "C-3" "C-4" "C-5"
                       "C-7" "C-8" "C-9" "C-0")))
  (define-key evil-normal-state-map key (lambda () (interactive))))


;; Make C-w C-{h,j,k,l} equivalent to C-w {h,j,k,l}
(define-key evil-window-map "\C-h" 'evil-window-left)
(define-key evil-window-map "\C-j" 'evil-window-down)
(define-key evil-window-map "\C-k" 'evil-window-up)
(define-key evil-window-map "\C-l" 'evil-window-right)

; Kill current buffer but keep its frame around
(define-key evil-normal-state-map "Q" 'kill-this-buffer)

(evil-leader/set-key
  ";" 'evil-ex
  ":" 'evil-ex)

;; Shortcut to M-x
(evil-ex-define-cmd     "mx" 'helm-M-x)
(define-key evil-ex-map "mx" 'helm-M-x)

;; change mode-line color by evil state
(let ((default-color (cons (face-background 'mode-line)
                           (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((and (evil-emacs-state-p)
                                       (buffer-modified-p))
                                  '("#44b3f8" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package evil-numbers
  :bind (("C-c =" . evil-numbers/inc-at-pt)
         ("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)
         ("C-c _" . evil-numbers/dec-at-pt)))

;; Make default encoding UTF-8 everywhere
(custom-set-variables
 '(current-language-environment "UTF-8"))
(prefer-coding-system 'utf-8)

;; Convenience bindings for isearch buffer
(define-key isearch-mode-map (kbd "<up>")   'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)

;; Auto-indent
(add-to-list 'load-path
             (expand-file-name
              (apply 'concat
                     (mapcar 'file-name-as-directory
                             '("site-lisp" "clean-aindent")))
              user-emacs-directory))
(if (fboundp 'clean-aindent)
    (require 'clean-aindent)
  (define-key global-map (kbd "RET") 'newline-and-indent))

;; No tabs
(setq-default indent-tabs-mode nil)

;; Automatically wrap long lines
(setq-default fill-column 79)
(defun prog-mode-wrap-hook ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode t)
  (diminish 'auto-fill-function))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'prog-mode-wrap-hook)

;;; Disable scrollbars
(scroll-bar-mode -1)

;;; Line and column numbers
(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))
(line-number-mode 1)
(column-number-mode 1)

(use-package elfeed
  :init
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://www.terminally-incoherent.com/blog/feed/"
          "https://news.ycombinator.com/rss"))
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode))

(use-package rg
  :commands (rg rg-project rg-dwin))

;;; Company
(use-package company
  :diminish company-mode
  :config
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-go
  :ensure company)

;;; Syntax highlighting for Vimscript files
(use-package vimrc-mode
  :mode ".vim\\(rc\\)?$")

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command
              "pandoc -f markdown -t html --toc -s --mathjax")
  :config
  ;; Make `<tab>` only call `indent-relative` in Evil insert mode, not cycle.
  (add-hook 'markdown-mode-hook
            (lambda ()
              (define-key markdown-mode-map (kbd "<tab>")
                (lambda (&rest args)
                  (interactive "P")
                  (cond ((and (featurep 'evil) (evil-insert-state-p))
                         (apply 'indent-relative args))
                        (t (apply 'markdown-cycle args))))))))

;; Give buffers editing files with the same basename more distinctive names
;; based on directory.
(use-package uniquify
  :ensure nil
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Save mini-buffer history
(use-package savehist
  :init
  (setq savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring compile-history))
  (setq savehist-file
        (expand-file-name (concat (file-name-as-directory "cache") "savehist")
                          user-emacs-directory))
  (setq history-length 5000)
  :config
  (savehist-mode 1))

;; Disable tool bar
(message "turn off tool bar")
(tool-bar-mode -1)

;; Rust mode
(use-package rust-mode
  :mode "\\.rs\\'")

;; Lua mode
(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

;; Java
(use-package eclim
  :ensure t
  :config
  (require 'eclimd)
  (global-eclim-mode))
(use-package company-emacs-eclim
  :ensure eclim
  :config
  (company-emacs-eclim-setup))

;; Javascript
(use-package js-mode
  :ensure nil
  :mode ("\\.js\\'" "\\.javascript\\'"))

;; Multi Web Mode - automatically switch to right major mode in HTML files
(use-package multi-web-mode
  :init
  (setq mweb--major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  :config
  (multi-web-global-mode 1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paren
  :config (show-paren-mode 1))

;;; Go
(use-package go-mode
  :config
  (use-package company-go)
  (defun my-go-mode-settings ()
    (setq-local whitespace-line-column 99)
    ;; Use 4-space tabs since gofmt formats with tabs by default
    (setq-local tab-width 4)
    (setq-local tab-stop-list (number-sequence 4 200 4))
    ;; See https://github.com/dominikh/go-mode.el/issues/119. This regexp isn't
    ;; completely reliable, since it leaves comment markers in the middle of
    ;; the line and fails to handle other cases, but at least it comments the
    ;; beginning of each paragraph.
    (setq-local adaptive-fill-regexp
                "[   ]*\\(//+\\|\\**\\)[     ]*\\([  ]*\\([-–!|#%;>*·•‣⁃◦]+[  ]*\\)*\\)"))
  (add-hook 'go-mode-hook 'my-go-mode-settings)
  (defun my-go-guru-xref ()
    (evil-local-set-key 'normal (kbd "C-]") #'go-guru-definition))
  (add-hook 'go-mode-hook #'my-go-guru-xref 'append))

;;; Enable escaping from yasnippet snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

;;; Don't put yanks into X clipboard buffer by default
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
(setq mouse-drag-copy-region t)


;;; Copy and paste from macOS pasteboard (from
;;; http://apple.stackexchange.com/a/127082).
(when (eq system-type 'darwin)
  (defun pbcopy ()
    (interactive)
    (call-process-region (point) (mark) "pbcopy")
    (setq deactivate-mark t))

  (defun pbpaste ()
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun pbcut ()
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end)))

  (global-set-key (kbd "s-c") 'pbcopy)
  (global-set-key (kbd "s-v") 'pbpaste)
  (global-set-key (kbd "s-x") 'pbcut))


;; Paredit mode
(use-package paredit
  :diminish paredit-mode
  :commands enable-paredit-mode
  :init
  (use-package evil-paredit
    :commands evil-paredit-mode)
  (defun activate-paredit-mode ()
    (interactive)
    (enable-paredit-mode)
    (evil-paredit-mode 1))
  (add-hook 'emacs-lisp-mode-hook       #'activate-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'activate-paredit-mode)
  (add-hook 'ielm-mode-hook             #'activate-paredit-mode)
  (add-hook 'inferior-scheme-mode-hook  #'activate-paredit-mode)
  (add-hook 'lisp-mode-hook             #'activate-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'activate-paredit-mode)
  (add-hook 'scheme-mode-hook           #'activate-paredit-mode))

;;; Slime
(use-package slime
  :config
  (add-hook 'slime-repl-mode-hook #'activate-paredit-mode)
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  (slime-setup '(slime-fancy slime-banner)))

;;; ---------------------------------------------------------------------------
;;;  Org configuration
;;; ---------------------------------------------------------------------------
(use-package org
  :ensure org-plus-contrib
  :ensure htmlize                       ; For org-publish
  :config
  (require 'org-agenda)
  (require 'org-protocol))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

;;; Kill the frame if one was created for the capture (see
;;; https://github.com/sprig/org-capture-extension#example-closins-the-frame-after-a-capture).
(defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

(defun kk/delete-frame-if-neccessary (&rest r)
  (cond
   ((= kk/delete-frame-after-capture 0) nil)
   ((> kk/delete-frame-after-capture 1)
    (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
   (t
    (setq kk/delete-frame-after-capture 0)
    (delete-frame))))

(advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
(advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
(advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

(setq org-agenda-files (expand-file-name "agenda_files" user-emacs-directory))
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)
(setq org-capture-templates
      '(("t" "Tasks" entry (file+headline (concat org-directory "/todo.org")
                                          "Refile")
         "* TODO %?%^{Title}\n  %^{Effort}p%u" :clock-in t :clock-resume t :jump-to-captured t)
        ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
         "* %u %?" :clock-in t :clock-resume t :jump-to-captured t)
        ("j" "Journal" plain (file+weektree (concat org-directory "/journal.org"))
         "%U %?")
        ("d" "Drill" entry (file+headline org-default-notes-file "Drill")
         "* Drill entry        :drill:\n :PROPERTIES:\n :DRILL_CARD_TYPE: hide1cloze\n :END:\n   %?!|2 + 2|! equals !|4|!."
         :clock-in t :clock-resume t :jump-to-captured t)
        ;; org-protocol capture templates for
        ;; https://github.com/sprig/org-capture-extension.
        ;;
        ;; The two progn expressions serve these purposes:
        ;; 1. Bring the frame in which org-capture was launched into focus.
        ;; 2. Delete the capture frame after capture is complete (or killed).
        ("p" "Link and Text" entry (file+headline org-default-notes-files "Links")
         "%(progn (x-focus-frame nil) \"\")* %^{Title}\nSource: [[%:link][%:description]]%(progn (setq kk/delete-frame-after-capture 1) \"\")\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Link" entry (file+headline org-default-notes-file "Links")
         "%(progn (x-focus-frame nil) \"\")* [[%:link][%:description]]%(progn (setq kk/delete-frame-after-capture 1) \"\")\n%:initial")))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-refile-use-outline-path t)
(setq org-alphabetical-lists t)
(setq org-src-fontify-natively t)
(setq org-pretty-entities t)
(setq org-use-sub-superscripts '{})


;; Todo settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Don't show tasks in agenda that are done
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;; Skip tasks in the global TODO list that are done or scheduled, because
;; either of these means the tasks has been considered. Tasks marked with a
;; deadline still need to be scheduled before I've truly considered them, so
;; leave them in.
(setq org-agenda-todo-ignore-scheduled 'future)

(setq org-agenda-span 1)
(unless (boundp 'org-agenda-custom-commands)
  (setq org-agenda-custom-commands '()))
(add-to-list 'org-agenda-custom-commands
             '("n" "Agenda and all TODOs" ((agenda "") (alltodo))))
(add-to-list 'org-agenda-custom-commands
             '("u" "Unscheduled TODOs" todo ""
               ((org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
                (org-agenda-overriding-header "Unscheduled TODO entries: ")
                (org-agenda-sorting-strategy '(time-down)))))
(add-to-list 'org-agenda-custom-commands
             '("Q" . "Custom queries"))
(add-to-list 'org-agenda-custom-commands
             '("Q/" "Archive occur"
               ;; Dynamically bind `org-agenda-text-search-extra-files' with the
               ;; symbol `agenda-archives' prepended to search archive files when
               ;; calling `org-occur-in-agenda-files'.
               (lambda (unused)
                 (let* ((tmp (if (boundp 'org-agenda-text-search-extra-files)
                                 org-agenda-text-search-extra-files
                               '()))
                        (org-agenda-text-search-extra-files
                         (cond ((null tmp) '(agenda-archives))
                               ((equal (car tmp) 'agenda-archives) tmp)
                               (t (cons 'agenda-archives tmp)))))
                   (call-interactively 'org-occur-in-agenda-files)))
               ""))

(setq org-columns-default-format "%60ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %10CLOCKSUM_T")
(setq org-global-properties
      (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
              ("SYTLE_ALL" . "habit"))))

(require 'org)
(require 'org-agenda)
(require 'org-protocol)
(setq org-protocol-default-template-key "p")

; No one needs both `h` and `H` for holidays -- online help in org-agenda.
(org-defkey org-agenda-mode-map "h"
            (lambda ()
              (interactive)
              (info "(org) Agenda Commands")))

;; Easier-to-use alias of C-c C-^
(org-defkey org-mode-map (kbd "C-c C-6") 'org-up-element)

; Pop up org-agenda-list a few times a day
(run-at-time "08:00" 21600 'org-agenda-list)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)


;;; Add option to merge current buffer and file on disk using emerge if both
;;; buffer and disk have changed.
(defadvice ask-user-about-supersession-threat (around ediff-supersession-threat)
  "Wrap `ask-user-about-supersession-threat' to provide the option to run
`ediff-current-buffer' instead."
  (save-window-excursion
    (let ((prompt
           (format "%s changed on disk; \
really edit the buffer? (y, n, r, d or C-h) "
                   (file-name-nondirectory fn)))
          (choices '(?y ?n ?r ?d ?? ?\C-h))
          answer)
      (while (null answer)
        (setq answer (read-char-choice prompt choices))
        (cond ((memq answer '(?? ?\C-h))
               (ask-user-about-supersession-help)
               (setq answer nil))
              ((eq answer ?r)
               ;; Ask for confirmation if buffer modified
               (revert-buffer nil (not (buffer-modified-p)))
               (signal 'file-supersession
                       (list "File reverted" fn)))
              ((eq answer ?d)                     ;;- added
               (ediff-current-file))              ;;- added
              ((eq answer ?n)
               (signal 'file-supersession
                       (list "File changed on disk" fn)))))
      (message
       "File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))

(defadvice ask-user-about-supersession-help (around ediff-supersession-help)
  "Add help for wrapped version of `ask-user-about-supersession-threat'."
  (with-output-to-temp-buffer "*Help*"
    (princ "You want to modify a buffer whose disk file has changed
since you last read it in or saved it with this buffer.

If you say `y' to go ahead and modify this buffer,
you risk ruining the work of whoever rewrote the file.
If you say `r' to revert, the contents of the buffer are refreshed
from the file on disk.
If you say `d' to diff, the contents of the buffer are diffed with the contents
of the on-disk file using `ediff-current-file'.
If you say `n', the change you started to make will be aborted.

Usually, you should type `n' and then `\\[revert-buffer]',
to get the latest version of the file, then make the change again.")
    (with-current-buffer standard-output
      (help-mode))))


;; Fix org-column fonts
(defun org-column-view-uses-fixed-width-face ()
  ;; copy from org-faces.el
  (when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column
    ;; table.
    (set-face-attribute 'org-column nil
                        :height (face-attribute 'default :height)
                        :family (face-attribute 'default :family))))

(when (and (fboundp 'daemonp) (daemonp))
  (add-hook 'org-mode-hook 'org-column-view-uses-fixed-width-face))

;;; Make idle time more accurate on Linux (X idle time rather than just Emacs
;;; idle time)
(setq org-clock-idle-time 15)
(when (eq system-type 'gnu/linux)
  (let ((xprintidle (executable-find "xprintidle")))
    (if xprintidle
        (setq org-clock-x11idle-program-name xprintidle)
      (display-warning
       'environment
       "xprintidle should be installed for accurate idle time on Linux."))))

;;; Enable notifications on OS X using the terminal-notifier program.
(defvar terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier.")
(defun terminal-notifier-notify (title message &optional timeout)
  "Show a message with `terminal-notifier-command`."
  (let ((timeout (number-to-string (if timeout timeout 60))))
    (start-process "terminal-notifier"
                   "*terminal-notifier*"
                   terminal-notifier-command
                   "-title" title
                   "-message" message
                   "-activate" "org.gnu.Emacs"
                   "-timeout" timeout)))
(when terminal-notifier-command
  (setq org-show-notification-handler
        (lambda (message) (terminal-notifier-notify "Org Mode" message))))

;;; Ask for effort estimate when clocking in, but only when an effort estimate
;;; is not present. Based on http://orgmode.org/worg/org-hacks.html#sec-1-9-10
;;; but simplified by using the org-set-effort function, which is called
;;; interactively here and so provides a prompt with autocompletion.
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)
(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (when (null (org-entry-get-multivalued-property (point) "Effort"))
    (org-set-effort)))

;;; Show current task in frame title
(setq org-clock-clocked-in-display 'frame-title)
(setq org-clock-frame-title-format '("" "%b - " org-mode-line-string))

;;; Fix the very slow tangling of large Org files
(setq org-babel-use-quick-and-dirty-noweb-expansion t)

;;;; Stolen from http://doc.norang.ca/org-mode.html#Refiling

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)
;;;

;;; Org-drill
(use-package org-drill
  :ensure org-plus-contrib
  :ensure nil
  :init
  (require 'cl)                         ; org-drill uses old CL func names
  (require 'org)                        ; org variables need to be in scope
  :config
  (setq org-drill-scope 'directory)
  (setq org-drill-left-cloze-delimiter "!|")
  (setq org-drill-right-cloze-delimiter "|!")
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (setq org-drill-learn-fraction 0.3))


(use-package evil-org
  :ensure t
  :ensure org-plus-contrib
  :init
  (setq org-special-ctrl-a/e t))


;;; Org-gcal
(use-package org-gcal
  :ensure org-plus-contrib
  :ensure alert
  :ensure request-deferred
  :ensure nil
  :load-path "~/.emacs.d/lisp/org-gcal.git"
  :config
  (setq org-gcal-config-file (expand-file-name "org-gcal-config.el" user-emacs-directory))
  (when (file-exists-p org-gcal-config-file)
    (load org-gcal-config-file)))

;;;; Stolen from http://doc.norang.ca/org-mode.html#Clocking
;;;; bh/organization-task-id changed.
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "204185a9-c100-4d50-a097-df3b67eb57eb")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;;; Needed for clocking functions: http://doc.norang.ca/org-mode.html#Projects
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))
;;;;

;;; Find all inactive timestamps in tree, buffer, or all org buffers
;;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-07/msg01228.html
(defun org-find-timestamps ()
  "Find inactive timestamps within a date-range and maybe sort them.

This function can help to bring the notes, that you take within
org-mode, into a chronological order, even if they are scattered
among many different nodes. The result is somewhat like a diary,
listing your notes for each successive day. Please be aware
however: This intended usage requires, that you routinely
insert inactive timestamps into the notes that you write.

org-find-timstamps works by creating a regular expression to
match a given range of dates, doing a search for it and
displaying the results either as a sparse tree or with the help
of occur. The original buffer is not modified.
"
  (interactive)
  (let ((occur-buffer-name "*Occur*")
        (occur-header-regex "^[0-9]+ match\\(es\\)?") ;; regexp to match for header-lines in *Occur* buffer
        first-date
        last-date
        pretty-dates
        swap-dates
        (days 0)
        date-regex
        position-before-year
        collect-method
        buff
        org-buffers)
    (save-window-excursion
      ;; temporary buffer for date-manipulations
      (with-temp-buffer
        ;; ask user for date-range
        (setq last-date (org-read-date nil nil nil "End date (or start): " nil nil))
        (setq first-date (org-read-date nil nil nil "Start date (or end): " nil nil))
        ;; swap dates, if required
        (when (string< last-date first-date)
          (setq swap-dates last-date)
          (setq last-date first-date)
          (setq first-date swap-dates))
        (setq pretty-dates (concat "from " first-date " to " last-date))
        ;; construct list of dates in working buffer
        ;; loop as long we did not reach end-date
        (while (not (looking-at-p last-date))
          (end-of-buffer)
          ;; only look for inactive timestamps
          (insert "[")
          (setq position-before-year (point))
          ;; Monday is probably wrong, will be corrected below
          (insert first-date " Mo]\n")
          (goto-char position-before-year)
          ;; advance number of days and correct day of week
          (org-timestamp-change days 'day)
          (setq days (1+ days))
          )
        (end-of-buffer)
        ;; transform constructed list of dates into a single, optimized regex
        (setq date-regex (regexp-opt (split-string (buffer-string) "\n" t)))
        )
      )
    ;; ask user, which buffers to search and how to present results
    (setq collect-method
          (car (split-string (org-icompleting-read "Please choose, which buffers to search and how to present the matches: "
                                                   '("multi-occur -- all org-buffers, list" "org-occur -- this-buffer, sparse tree") nil t nil nil "occur -- this buffer, list")))
          )
    ;; Perform the actual search
    (save-window-excursion
      (cond ((string= collect-method "occur")
             (occur date-regex)
             )
            ((string= collect-method "org-occur")
             (if (string= major-mode "org-mode")
                 (org-occur date-regex)
               (error "Buffer not in org-mode"))
             )
            ((string= collect-method "multi-occur")
             ;; construct list of all org-buffers
             (dolist (buff (buffer-list))
               (set-buffer buff)
               (if (string= major-mode "org-mode")
                   (setq org-buffers (cons buff org-buffers))))
             (multi-occur org-buffers date-regex)))
      )
    ;; Postprocessing: Optionally sort buffer with results
    ;; org-occur operates on the current buffer, so we cannot modify its results afterwards
    (if (string= collect-method "org-occur")
        (message (concat "Sparse tree with matches " pretty-dates))
      ;; switch to occur-buffer and modify it
      (if (not (get-buffer occur-buffer-name))
          (message (concat "Did not find any matches " pretty-dates))
        (let ((original-inhibit-read-only inhibit-read-only))
          (unwind-protect
              (progn
                ;; next line might be risky, so we unwind-protect it
                (setq inhibit-read-only t)
                (set-buffer occur-buffer-name)
                (goto-char (point-min))
                ;; beautify the occur-buffer by replacing the potentially long original regexp
                (while (search-forward (concat " for \"" date-regex "\"") nil t)
                  (replace-match "" nil t))
                (goto-char (point-min))
                ;; Sort results by matching date ?
                (when (y-or-n-p "Sort results by date ? ")
                  (when (string= collect-method "multi-occur")
                    ;; bring all header lines ('xx matches for ..') to top of buffer, all lines with matches to bottom
                    (sort-subr t
                               'forward-line
                               'end-of-line
                               ;; search-key for this sort only differentiates between header-lines and matche-lines
                               (lambda () (if (looking-at-p occur-header-regex) 2 1))
                               nil)
                    )
                  ;; goto first line of matches
                  (goto-char (point-max))
                  (search-backward-regexp occur-header-regex)
                  (forward-line)
                  ;; sort all matches according to date, that matched the regex
                  (sort-subr t
                             'forward-line
                             'end-of-line
                             ;; search-key for this sort is date
                             (lambda () (search-forward-regexp date-regex) (match-string 0))
                             nil
                             'string<)
                  ;; pretend, that we did not modify the occur-buffer
                  )
                (insert "Searched " pretty-dates "\n")
                (goto-char (point-min))
                (set-buffer-modified-p nil)
                (message (concat "occur-buffer with matches " pretty-dates "(`C-h m' for help)"))
                )
            (setq inhibit-read-only original-inhibit-read-only)
            )
          )
        )
      ;; switch to occur-buffer
      (if (get-buffer occur-buffer-name)
          (switch-to-buffer occur-buffer-name))
      )
    )
  )

(require 'org-inlinetask)

;;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-enforce-todo-dependencies t)

(setq org-log-done (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;;; Week in review (https://emacs.stackexchange.com/a/7864)
(defvar org-timeline-files nil
  "The files to be included in `org-timeline-all-files'. Follows
  the same rules as `org-agenda-files'")

(setq org-timeline-files org-agenda-files)

(add-to-list 'org-agenda-custom-commands
             '("R" "Week in review"
                agenda ""
                ;; agenda settings
                ((org-agenda-span 'week)
                  (org-agenda-start-on-weekday 0) ;; start on Sunday
                  (org-agenda-overriding-header "Week in Review")
                  (org-agenda-files
                    (let ((org-agenda-files org-timeline-files))
                          (org-agenda-files nil 'ifmode)))
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-log-mode-items '(clock state closed))
                  (org-agenda-archives-mode t) ; include archive files
                )))

;;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(add-to-list 'org-agenda-custom-commands
             '("d" "Daily agenda with high-priority items isolated (slow)"
               ((tags "PRIORITY=\"A\""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "High-priority unfinished tasks:")))
                (agenda "" ((org-agenda-ndays 1)
                            (org-agenda-overriding-header "Schedule"))))
               ((org-agenda-compact-blocks t))))
(add-to-list 'org-agenda-custom-commands
             '("D" "Like d but include all TODOs (slow)"
               ((tags "PRIORITY=\"A\""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "High-priority unfinished tasks:")))
                (agenda "" ((org-agenda-ndays 1)
                            (org-agenda-overriding-header "Schedule")))
                (alltodo ""
                         ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                         (air-org-skip-subtree-if-priority ?A)
                                                         (org-agenda-skip-if nil '(scheduled deadline))))
                          (org-agenda-overriding-header "ALL normal priority tasks:"))))
               ((org-agenda-compact-blocks t))))
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode t))

(add-hook 'text-mode-hook 'flyspell-mode 'append)
(add-hook 'prog-mode-hook 'flyspell-prog-mode 'append)

;;; Remove trailing whitespace intelligently
(use-package ws-butler
  :diminish ws-butler-mode
  :commands ws-butler-mode
  :init
  (add-hook 'text-mode-hook (lambda () (ws-butler-mode 1)))
  (add-hook 'prog-mode-hook (lambda () (ws-butler-mode 1))))

;;; Miscellaneous
(custom-set-variables
 '(bookmark-default-file
   (expand-file-name (concat (file-name-as-directory "cache") "bookmarks")
                     user-emacs-directory)))

;;; Weechat
(use-package weechat
  :config
  (add-hook 'weechat-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'weechat-mode-hook (lambda () (linum-mode -1))))
(use-package weechat-notifications
  :ensure weechat
  :ensure nil
  :if (featurep 'dbusbind))

;;; Enable commands disabled by default
(put 'narrow-to-region 'disabled nil)

;;; Python
(defun evil-shift-width-from-guessed-python-indent-offset ()
  "Fix Evil shiftwidth based on Python indent"
  (python-indent-guess-indent-offset)
  (setq-local evil-shift-width python-indent-offset))
(add-hook 'python-mode-hook 'evil-shift-width-from-guessed-python-indent-offset)


;;; Fix newline behavior to use M-j by default.
;;; See http://stackoverflow.com/a/9060267.
(defun my-coding-config ()
  (local-set-key (kbd "RET") (key-binding (kbd "M-j")))
  (local-set-key (kbd "<S-return>") 'newline))
(add-hook 'prog-mode-hook 'my-coding-config)


;; Don't use Evil for image-mode.
(add-to-list 'evil-emacs-state-modes 'image-mode)

;;; Magit
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))


;;; https://codearsonist.com/reading-for-programmers
(use-package pdf-tools)
(pdf-tools-install)
(use-package interleave)
(use-package org-ref
  :config
  (setq org-ref-notes-directory "~/Documents/org/home-org"
      org-ref-bibliography-notes "~/Documents/org/home-org/index.org"
      org-ref-default-bibliography '("~/Documents/org/home-org/index.bib")
      org-ref-pdf-directory "~/Documents/org/home-org/lib/"))
(use-package helm-bibtex
  :config
  (setq helm-bibtex-bibliography "~/Documents/org/home-org/index.bib" ;; where your references are stored
      helm-bibtex-library-path "~/Documents/org/home-org/lib/" ;; where your pdfs etc are stored
      helm-bibtex-notes-path "~/Documents/org/home-org/index.org" ;; where your notes are stored
      bibtex-completion-bibliography "~/Documents/org/home-org/index.bib" ;; writing completion
      bibtex-completion-notes-path "~/Documents/org/home-org/index.org"
))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;----------------------------------------------------------------------------
;; Subfiles
;;----------------------------------------------------------------------------
(use-package whitespace-conf
  :diminish whitespace-mode
  :ensure nil
  :load-path "~/.emacs.d/lisp")
(use-package org-drill-cloze-enhancement
  :ensure nil
  :load-path "~/.emacs.d/lisp")

;;; Allow users to provide an optional "init-local" containing personal settings
(use-package init-local
  :ensure nil
  :load-path "~/.emacs.d/lisp")

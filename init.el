; -*- lexical-binding: t; no-byte-compile: t -*-

;;;* Introduction

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.

;;; Byte compilation is disabled for this file to avoid ever loading stale
;;; bytecode. The commands in this file will ensure that stale bytecode is
;;; never loaded for other files.

(defmacro c-setq (variable value &optional comment)
  "Exactly like setq, but handles custom.

Taken from http://lists.gnu.org/archive/html/emacs-devel/2017-11/msg00119.html."
  `(if (get ',variable 'custom-type)
       (customize-set-variable ',variable ,value ,comment)
     (set-default ',variable ,value)))

;;;* gc-cons-threshold

;;; Set this higher in the minibuffer, but don't make it too bad other wise
;;; you'll figure out quick

(c-setq gc-cons-threshold (* 4 1024 1024) "\
Set gc-cons-threshold to a higher value. This should be done before
searching for packages because package searches and installation tend to
generate a lot of garbage. Cite:
http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/.")

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 4 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;;* Package loading

;;; Avoid loading .elc files older than their corresponding .el file.
(c-setq load-prefer-newer t)

;;; Fix TLS certificate "could not be verified" errors
;;; (http://emacs.stackexchange.com/a/18070).)
(c-setq gnutls-verify-error t)

(c-setq tls-checktrust 'ask)

;;; Load all the packages that are REQUIRE'd below
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") 'append)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package bind-key)                  ; To make :bind work
(use-package diminish)                  ; To use :diminish
(c-setq use-package-always-ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Use auto-compile to recompile bytecode whenever files are loaded or saved.
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Convenience commands to upgrade packages.
(use-package package-utils
  :ensure t
  :ensure epl)


;;;* Basic startup configuration

;;; Disable startup screen
(c-setq inhibit-startup-message t)

;;; Command to restart emacs from within emacs
(use-package restart-emacs)

;;; Allow access from emacsclient
(use-package server
  :ensure nil
  :config
  (c-setq server-socket-dir "~/.emacs.d/server")
  (c-setq server-use-tcp t)
  (if (server-running-p)
      (warn "Not starting server - server with name \"%s\" already running" server-name)
    (server-start)))

;;;* Leuven theme
(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil)
  :config
  (load-theme 'leuven t)
  (load-theme 'leuven-customization t))

;;;* Ivy configuration
(use-package counsel
  :diminish ivy-mode
  :diminish counsel-mode
  :ensure t
  :ensure amx
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  (enable-recursive-minibuffers t)
  (ivy-initial-inputs-alist '((Man-completion-table . "^")
                              (woman . "^")))
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  ;; Unbind ivy-restrict-to-matches - always annoys me during org-capture.
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;;;* Evil configuration

;; Dependency of evil-mode
(use-package undo-tree
  :diminish undo-tree-mode)

(use-package evil
  :ensure undo-tree
  :init
  (c-setq evil-want-keybinding nil)
  :config (evil-mode 1))

;; Space as leader
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; Scroll with C-u and access universal-argument with <Leader>-u.
(c-setq evil-want-C-u-scroll t)
(evil-leader/set-key "u" 'universal-argument)

;; Evil magic search
(c-setq evil-magic 'very-magic)
(c-setq evil-search-module 'evil-search)

(use-package evil-collection
  :ensure t
  :after evil
  :init (evil-collection-init))

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

;; Kill current buffer but keep its frame around
(define-key evil-normal-state-map "Q" 'kill-this-buffer)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(evil-leader/set-key
  ";" 'evil-ex
  ":" 'evil-ex)

;; Shortcut to M-x
(evil-ex-define-cmd     "mx" #'counsel-M-x)
(define-key evil-ex-map "mx" #'counsel-M-x)
(evil-leader/set-key "SPC" 'counsel-M-x)

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
  :bind (:map evil-normal-state-map
         ("C-c =" . evil-numbers/inc-at-pt)
         ("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)
         ("C-c _" . evil-numbers/dec-at-pt)))

;;;* Miscellaneous

;; Make default encoding UTF-8 everywhere
(c-setq current-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Convenience bindings for isearch buffer
(define-key isearch-mode-map (kbd "<up>")   'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)

;; Auto-indent
(use-package clean-aindent-mode
  :bind (:map global-map ("RET" . #'newline-and-indent))
  :config (clean-aindent-mode 1))

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
(c-setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(line-number-mode 1)
(column-number-mode 1)

(use-package rg
  :commands (rg rg-project rg-dwin))

;;; Company
(use-package company-flx
  :config
  (with-eval-after-load 'company
    (company-flx-mode +1)))
(use-package company
  :diminish company-mode
  :custom
  (company-tooltip-limit 20 "bigger popup window")
  (company-idle-delay .3 "decrease delay before autocompletion popup shows")
  (company-echo-delay 0 "remove annoying blinking")
  :config
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
  :init (c-setq markdown-command
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
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Save mini-buffer history
(use-package savehist
  :ensure nil
  :custom
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring compile-history))
  (savehist-file
   (expand-file-name (concat (file-name-as-directory "cache") "savehist")
                     user-emacs-directory))
  (history-length 5000)
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
  :ensure t
  :config
  (company-emacs-eclim-setup))

;; Javascript
(use-package js-mode
  :ensure nil
  :mode ("\\.js\\'" "\\.javascript\\'"))

;; Multi Web Mode - automatically switch to right major mode in HTML files
(use-package multi-web-mode
  :custom
  (mweb--major-mode 'html-mode)
  (mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
               (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
               (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  :config
  (multi-web-global-mode 1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paren
  :ensure nil
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
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;;; Don't put yanks into X clipboard buffer by default
(c-setq select-enable-primary nil)
(c-setq select-enable-clipboard t)
(c-setq mouse-drag-copy-region t)


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



;;; Parinfer
(use-package parinfer
  :ensure t
  :ensure paredit
  :bind
  (("C-," . parinfer-toggle-mode))
  :hook ((clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode lisp-mode)
         . parinfer-mode)
  :init
  (c-setq parinfer-extensions
        '(defaults        ; should be included.
           pretty-parens  ; different paren styles for different modes.
           evil           ; If you use Evil.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))   ; Yank behavior depend on mode.
  (c-setq parinfer-auto-switch-indent-mode t)
  :config
  (c-setq parinfer-lighters '(" pi:I" " pi:P")))


;;; Slime
(use-package slime
  :ensure t
  :config
  (require 'slime-autoloads)
  (add-hook 'slime-repl-mode-hook #'activate-paredit-mode)
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  ;; Add contrib directory to load-path for slime-fancy.
  (add-to-list
   'load-path
   (expand-file-name
    "contrib"
    (file-name-directory (symbol-file #'slime-mode))))
  (slime-setup '(slime-fancy slime-banner)))


;;;* Org configuration
(use-package org
  :ensure nil
  :ensure htmlize                       ; For org-publish
  :load-path ("~/.emacs.d/lisp/org-mode.git/lisp"
              "~/.emacs.d/lisp/org-mode.git/contrib/lisp")
  :init
  :config
  (add-to-list 'org-modules 'org-habit)
  (require 'org-agenda)
  (require 'org-protocol))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

(defun swiper-multi-org-agenda-files (prefix)
  "Swiper-based replacement for ‘org-occur-in-agenda-files’.

With `\\[universal-argument]' prefix ARG, include archives in the search even \
if `agenda-archives' is not in `org-agenda-text-search-extra-files'."
  (interactive "p")
  (let* ((files (org-agenda-files))
         (tnames (mapcar #'file-truename files))
         (extra org-agenda-text-search-extra-files)
         (search-archives (or (eq (car extra) 'agenda-archives)
                              (> prefix 1))))
    (when search-archives
      (setq extra (cdr extra))
      (setq files (org-add-archive-files files)))
    (dolist (f extra)
      (unless (member (file-truename f) tnames)
       (unless (member f files) (setq files (append files (list f))))
       (setq tnames (append tnames (list (file-truename f))))))
    ;; First find the buffers for open agenda files.
    (setq swiper-multi-buffers
          (cl-remove-if #'null (mapcar #'get-file-buffer files)))
    ;; Set ‘swiper-multi-candidates’ as done in ‘swiper-multi-action-1’.
    (setq swiper-multi-candidates
          (swiper--multi-candidates
           (mapcar #'get-buffer swiper-multi-buffers)))
    ;; Call just ‘swiper-multi-action-2’, which does the actual search text
    ;; completion.
    (ivy-read (format "Org-files (%s) matching: " (if search-archives "with archives" "unarchived"))
              swiper-multi-candidates
              :action 'swiper-multi-action-2
              :unwind #'swiper--cleanup
              :caller 'swiper-multi)))

(global-set-key "\C-cq" #'swiper-multi-org-agenda-files)

;;;** Org capture

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

(c-setq org-agenda-files (expand-file-name "agenda_files" user-emacs-directory))
(c-setq org-agenda-span 7)
(c-setq org-agenda-start-on-weekday nil)
(c-setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(defun transform-square-brackets-to-curly-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c)
               (cond ((equal c ?\[) ?\{)
                     ((equal c ?\]) ?\})
                     (t c)))
           string-to-transform)))
(defun org-clock-report-buffer ()
  "Evaluate all the clocktables in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
        (goto-char (point-min))
        (while (re-search-forward "#\\\+BEGIN: clocktable" nil t)
            (org-clock-report)
            (forward-line 1)))))

(defcustom org-daily-log-file
  (concat org-directory "/daily-log.org")
  "The path to Org file in which daily log entries are captured."
  :type file)

(c-setq org-capture-templates
      `(("t" "Task" entry (file+headline (lambda () (concat org-directory "/todo.org"))
                                         "Refile")
         "
* TODO %?%^{Title}
%^{Effort}p%u" :clock-in t :clock-resume t :jump-to-captured t)
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "
* %u %?
" :jump-to-captured t)
        ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
         "
* %u %?REPLACE                      :IDEA:
" :clock-in t :clock-resume t)
        ("j" "Journal" plain (file+weektree (lambda () (concat org-directory "/journal.org")))
         "
* %U %^{Title}                 :journal:
:PROPERTIES:
:Effort: 9999:00
:END:

%?
" :clock-in t :clock-resume t)
        ("d" "Drill" entry (file+headline org-default-notes-file "Drill")
         "
* Drill entry        :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: hide1cloze
:Effort: 0:02
:END:
%?!|2 + 2|! equals !|4|!.
" :clock-in t :clock-resume t :jump-to-captured t)
        ("D" "Daily Log" entry (file+olp+datetree org-daily-log-file)
         "
* %u Daily log
:PROPERTIES:
:Effort: 0:05
:END:
*Summary*:%?

*Problem*:

*Insight*:

*Tomorrow*:

#+BEGIN: clocktable :maxlevel 9 :emphasize nil :scope agenda :stepskip0 t :fileskip0 t :block %<%F> :link t
#+END: clocktable
" :time-prompt t :tree-type week :clock-in t :clock-resume t)
        ;; org-protocol capture templates for
        ;; https://github.com/sprig/org-capture-extension.
        ;;
        ;; The two progn expressions serve these purposes:
        ;; 1. Bring the frame in which org-capture was launched into focus.
        ;; 2. Delete the capture frame after capture is complete (or killed).
        ("p" "Link and Text" entry (file+headline org-default-notes-file "Links")
         "
* %^{Title}
Source: [[%:link][%:description]]
#+BEGIN_QUOTE
%i
#+END_QUOTE

%?%U
%(progn (x-focus-frame nil) (setq kk/delete-frame-after-capture 1) nil)
")
        ("L" "Link" entry (file+headline org-default-notes-file "Links")
         "
* %?[[%:link][%(transform-square-brackets-to-curly-ones \"%:description\")]]
  %U
%(progn (x-focus-frame nil) (setq kk/delete-frame-after-capture 1) nil)
")))

(c-setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(c-setq org-refile-use-outline-path t)
(c-setq org-alphabetical-lists t)
(c-setq org-src-fontify-natively t)
(c-setq org-pretty-entities t)
(c-setq org-use-sub-superscripts '{})


;;;** Todo settings
(c-setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(c-setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
(c-setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
;;;** Agenda

;; Don't show tasks in agenda that are done
(c-setq org-agenda-skip-scheduled-if-done t)
(c-setq org-agenda-skip-deadline-if-done t)

;; Skip tasks in the global TODO list that are done or scheduled, because
;; either of these means the tasks has been considered. Tasks marked with a
;; deadline still need to be scheduled before I've truly considered them, so
;; leave them in.
(c-setq org-agenda-todo-ignore-scheduled 'future)

(c-setq org-agenda-span 1)
(unless (boundp 'org-agenda-custom-commands)
  (c-setq org-agenda-custom-commands '()))
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

(c-setq org-stuck-projects
      '("TODO={TODO\\|NEXT}-HOLD-CANCELLED-REFILE" ("NEXT" "HOLD") nil ""))

(c-setq org-columns-default-format "%60ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %10CLOCKSUM_T")
(c-setq org-global-properties
      (quote (("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
              ("SYTLE_ALL" . "habit"))))

(require 'org)
(require 'org-agenda)
(require 'org-protocol)
(c-setq org-protocol-default-template-key "p")

;;; No one needs both `h` and `H` for holidays -- online help in org-agenda.
(org-defkey org-agenda-mode-map "h"
            (lambda ()
              (interactive)
              (info "(org) Agenda Commands")))


;;; Keybindings to jump to org agenda entries and narrow buffer.
(defun org-agenda-switch-to-and-narrow (&optional delete-other-windows)
  (interactive)
  (org-agenda-switch-to delete-other-windows)
  (org-narrow-to-subtree))
(defun org-agenda-goto-and-narrow (&optional highlight)
  (interactive)
  (org-agenda-goto highlight)
  (org-narrow-to-subtree))
(org-defkey org-agenda-mode-map (kbd "<C-tab>") #'org-agenda-goto-and-narrow)
(org-defkey org-agenda-mode-map (kbd "<C-return>") #'org-agenda-switch-to-and-narrow)

;; Easier-to-use alias of C-c C-^
(org-defkey org-mode-map (kbd "C-c C-6") 'org-up-element)

;; This binding used to be present by default.
(org-defkey org-mode-map (kbd "C-c !") 'org-time-stamp-inactive)

;; Pop up org-agenda-list a few times a day
(run-at-time "08:00" 21600 'org-agenda-list)

;; Save and backup all Org files a few times a day using external script.
(use-package async)
(defun my-org-save-and-backup-repos ()
  (interactive)
  (org-save-all-org-buffers)
  (let ((default-directory (expand-file-name "~")))
    (async-start-process "my-org-save-and-backup-repos"
                         (substitute-env-vars "${HOME}/bin/org-git-cron")
                         'ignore)))
(run-at-time "00:59" 900 'my-org-save-and-backup-repos)


;;;** Supersession

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

;;;** Idle time

;;; Make idle time more accurate on Linux (X idle time rather than just Emacs
;;; idle time)
(c-setq org-clock-idle-time 15)
(when (eq system-type 'gnu/linux)
  (let ((xprintidle (executable-find "xprintidle")))
    (if xprintidle
        (c-setq org-clock-x11idle-program-name xprintidle)
      (display-warning
       'environment
       "xprintidle should be installed for accurate idle time on Linux."))))

;;;** Notifications

;;; Enable notifications on OS X using the terminal-notifier program.
(defcustom terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier."
  :type file)
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
  (c-setq org-show-notification-handler
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
(c-setq org-clock-clocked-in-display 'frame-title)
(c-setq org-clock-frame-title-format '("" "%b - " org-mode-line-string))

;;; Play sound when effort has expired.
(c-setq org-clock-sound
 (expand-file-name
  ;; Sound source:
  ;; http://soundbible.com/1496-Japanese-Temple-Bell-Small.html
  "Japanese Temple Bell Small-SoundBible.com-113624364.wav"
  user-emacs-directory))

;;;; org-refile settings:
;;;;
;;;; Based on http://doc.norang.ca/org-mode.html#Refiling and
;;;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(c-setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
(c-setq org-refile-use-outline-path 'buffer-name)
;;; Targets complete directly with Ivy, so no need to complete in steps.
(c-setq org-outline-path-complete-in-steps nil)
;;; Allow refile to create parent tasks with confirmation
(c-setq org-refile-allow-creating-parent-nodes 'confirm)
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(c-setq org-refile-target-verify-function 'bh/verify-refile-target)

(c-setq org-indirect-buffer-display 'current-window)

;;;** Org-drill
(use-package org-drill
  :ensure nil
  :after org
  :init
  (require 'cl)                         ; org-drill uses old CL func names
  (require 'org)                        ; org variables need to be in scope
  :config
  (c-setq org-drill-scope 'agenda-with-archives)
  (c-setq org-drill-left-cloze-delimiter "!|")
  (c-setq org-drill-right-cloze-delimiter "|!")
  (c-setq org-drill-add-random-noise-to-intervals-p t)
  (c-setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (c-setq org-drill-learn-fraction 0.3)
  (defun my-org-drill-global-visual-line-mode (org-drill &rest r)
    "Wrap `org-drill' to turn on `global-visual-line-mode' during drills.

This is needed because `org-drill-table' only works on `org-mode' tables, which
don't support wrapping."
    (let (old-global-visual-line-mode global-visual-line-mode)
      (unwind-protect
          (progn
            (global-visual-line-mode t)
            (apply org-drill r))
        (global-visual-line-mode old-global-visual-line-mode))))
  (advice-add #'org-drill :around #'my-org-drill-global-visual-line-mode))
(use-package org-drill-table
  :ensure t
  :after org)
(load-file (expand-file-name "lisp/org-clock-fix.el" user-emacs-directory))

;;;** Org-pomodoro
(use-package org-pomodoro
  :ensure t
  :ensure s
  :ensure async
  :after org
  :config
  ;; Complice.co Less Wrong study hall
  ;; (https://complice.co/room/lesswrong/interstitial). Reference:
  ;; https://www.lesswrong.com/posts/hyeDFbg8ahYAu4ZJu/#586GQr5xjWBzXWda6
  (c-setq org-pomodoro-length 32)
  (c-setq org-pomodoro-short-break-length 8)
  (c-setq org-pomodoro-long-break-length 30)
  (require 's)

  (defun org-pomodoro-end-in (minutes)
    "Force the current Pomodoro to end in MINUTES minutes."
    (interactive "nMinutes: ")
    (setq my-org-pomodoro-seconds-until-break-to-remind 60)
    (setq org-pomodoro-end-time
          (time-add (current-time) (* minutes 60))))

  (defcustom my-org-pomodoro-break-id nil
    "Task ID of task to clock into during Pomodoro breaks. Must specify manually."
    :type 'string)
  (defun my-org-pomodoro-finished-lock-screen ()
    "Lock screen at the end of each Pomodoro work session."
    (message "Locking screen in 15 seconds - post calendar event from *scratch*")
    (cond
     ((eq system-type 'darwin)
      (start-process "lock" nil "bash" "-c" "sleep 15; pmset displaysleepnow"))
     ((and (executable-find "xset")
           (not (s-blank-str? (getenv "DISPLAY"))))
      (shell-command "xdotool search 'Chrome' key --window '%@' XF86AudioPlay")
      (start-process "lock" nil "bash" "-c" "sleep 15; xset s activate"))
     (t
      (display-warning
           'my-org-pomodoro-finished-lock-screen
           "Can't lock screen"))))
  (defun my-org-pomodoro-finished-caffeinate ()
    "Prevent system from idle sleeping during Pomodoro breaks."
    (let ((countdown
           (cl-case org-pomodoro-state
             (:short-break (* 60 org-pomodoro-short-break-length))
             (:long-break (* 60 org-pomodoro-long-break-length))
             (t 0))))
      (when (> countdown 0)
        (cond
         ((eq system-type 'darwin)
          (async-start-process "my-org-pomodoro-finished-caffeinate"
                               "caffeinate" 'ignore
                               "-t" (number-to-string countdown)))
         (t
          (display-warning
           'my-org-pomodoro-finished-caffeinate
           "Can't prevent system from sleeping"))))))
  (defun my-org-pomodoro-finished-notify-hook ()
    (org-notify "Pomodoro phase finished"))
  (defun my-org-pomodoro-start-break ()
    "Start break - clock into task with ID my-org-pomodoro-break-id."
    (interactive)
    (save-excursion
      (org-id-goto my-org-pomodoro-break-id)
      (org-clock-in)))
  (defun my-org-pomodoro-start-lunch ()
    (interactive)
    (org-pomodoro-notify "Going to lunch now" "")
    (setq org-pomodoro-count 0)
    (org-pomodoro-start :long-break)
    (my-org-pomodoro-start-break)
    (my-org-pomodoro-finished-lock-screen))
  (defun my-org-pomodoro-finished-clock-in-break-hook ()
    "Clock into task with ID my-org-pomodoro-break-id during breaks if set."
    (message "%s %s" my-org-pomodoro-break-id org-pomodoro-state)
    (when my-org-pomodoro-break-id
      (message "About to start clock")
      (my-org-pomodoro-start-break)))
  (defun my-org-pomodoro-break-finished-notify-hook ()
    (let ((msg "Pomodoro break finished -- get back to work!"))
      (if (fboundp 'terminal-notifier-notify)
          ;; Try to ensure timeout is very high by skipping org-notify.
          (terminal-notifier-notify "Org Pomodoro" msg 84000)
        (org-notify msg))))
  (defun my-org-pomodoro-short-break-finished-punch-in ()
    "Run bh/punch-in when Pomodoro short breaks end."
    (message-box "Break finished - please run bh/punch-in"))
  (defun my-org-pomodoro-long-break-finished-punch-out ()
    "Run bh/punch-out when Pomodoro long breaks end."
    (bh/punch-out))

  (defcustom my-org-pomodoro-alarm-gcal-calendar-id nil
    "The Google Calendar ID on which to create alarms."
    :type 'string)
  (defcustom my-org-pomodoro-alarm-gcal-client-id nil
    "The Google Calendar API Client ID to use to create alarms."
    :type 'string)
  (defcustom my-org-pomodoro-alarm-gcal-client-secret nil
    "The Google Calendar API Client Secret to use to create alarms."
    :type 'string)
  (defcustom my-org-pomodoro-current-task-reminder-interval 60
    "Number of seconds between being notified of the current task. Set to nil to disable notifications"
    :type 'number)

  ;; Update agenda to log count and time of pomodoros elapsed today.
  (defvar my-org-pomodoro-start-time nil
    "Start time of current pomodoro.")
  (defvar my-org-pomodoro-time-today-var 0
    "Amount of time spent in pomodoro today.
DO NOT USE - contains only time logged outside of the current pomodoro.
Call (my-org-pomodoro-time-today) instead.")
  (defun my-org-pomodoro-time-today ()
    "Return amount of time spent in pomodoro today, as a floating-point
number of seconds."
    (+ my-org-pomodoro-time-today-var
       (if (eq org-pomodoro-state :pomodoro)
           (float-time (time-subtract (current-time)
                                      my-org-pomodoro-start-time))
         0)))
  (defun my-org-pomodoro-reset-today (&optional arg)
    "Resets daily org-pomodoro variables every day"
    (if (null org-pomodoro-last-clock-in)
        (setq my-org-pomodoro-time-today-var 0)
      (let* ((effective-midnight
              `(0 0 0 . ,(nthcdr 3 (decode-time (current-time))))))
        (when
            (and (<= 0
                     (float-time
                      (time-subtract (current-time)
                                     effective-midnight)))
                 (>= 0
                     (float-time
                      (time-subtract org-pomodoro-last-clock-in
                                     effective-midnight))))

          (setq my-org-pomodoro-time-today-var 0)))))
  (advice-add #'org-pomodoro :before #'my-org-pomodoro-reset-today)
  (defun my-org-pomodoro-set-start-time ()
    "Sets start time for use by my-org-pomodoro-time-today."
    (setq my-org-pomodoro-start-time (current-time)))
  (add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-set-start-time)
  (defun my-org-pomodoro-finished-update-time-today ()
    "Updates stored variable for my-org-pomodoro-time-today."
    (setq my-org-pomodoro-time-today-var
          (+ my-org-pomodoro-time-today-var
             (float-time (time-subtract (current-time)
                                        my-org-pomodoro-start-time)))))
  (add-hook 'org-pomodoro-finished-hook
            #'my-org-pomodoro-finished-update-time-today)
  (defun my-org-agenda-pomodoro-info ()
    "Add Org Pomodoro Count and Time to agenda."
    (save-excursion
      (goto-char (point-max))
      (end-of-line)
      (newline)
      (insert
       (format "Org Pomodoro - Count: %2d, Time: %s"
               org-pomodoro-count
               (org-timer-secs-to-hms
                       (round (my-org-pomodoro-time-today)))))
      (newline)
      ;; Add spaces to align with line above
      (insert "Try to get above                3:30:00")))
  (add-hook 'org-agenda-finalize-hook 'my-org-agenda-pomodoro-info 'append)

  (defun my-org-pomodoro-today-tick-hook ())
  (defvar my-org-pomodoro-seconds-until-break-to-remind)
  (defun my-org-pomodoro-tick-current-task-reminder ()
    "Prod me with reminders of my current task to stop me from being distracted."
    (let* ((x (cl-floor (float-time (time-subtract org-pomodoro-end-time
                                                   (current-time)))
                        my-org-pomodoro-current-task-reminder-interval))
           (quotient (car x))
           (remainder (car (cl-floor (cadr x)))))
      (when (and (eql org-pomodoro-state :pomodoro)
                 (not (null my-org-pomodoro-current-task-reminder-interval)))
        (when (> quotient 0)
          (setq my-org-pomodoro-seconds-until-break-to-remind 60))
        (when (and (> quotient 0) (= remainder 0))
            (org-pomodoro-notify "Pomodoro in progress" org-clock-heading))
        (when (and (= quotient 0) (> remainder 0))
          ;; Rate limit reminders in last minute to once every 5 seconds.
          (when (<= remainder my-org-pomodoro-seconds-until-break-to-remind)
            (setq my-org-pomodoro-seconds-until-break-to-remind
                  (* 5 (floor (- remainder 1) 5)))
            (org-pomodoro-notify (format "Pomodoro in progress - %ds to break"
                                        remainder)
                                org-clock-heading))))))
  (defun my-org-pomodoro-finished-create-break-end-alarm ()
    (when (and (or (eq org-pomodoro-state :short-break)
                   (eq org-pomodoro-state :long-break))
               ;; Current break has not ended yet.
               (> (float-time (time-subtract org-pomodoro-end-time (current-time)))
                  0)
               my-org-pomodoro-alarm-gcal-calendar-id
               my-org-pomodoro-alarm-gcal-client-id
               my-org-pomodoro-alarm-gcal-client-secret)
      (my-org-pomodoro--create-alarm-event
       my-org-pomodoro-alarm-gcal-calendar-id
       my-org-pomodoro-alarm-gcal-client-id
       my-org-pomodoro-alarm-gcal-client-secret
       org-pomodoro-end-time)))
  (defvar my-org-pomodoro--create-alarm-event-temp-files '()
    "Alist mapping calendar IDs to temporary files created by
my-org-pomodoro--create-alarm-event. Temporary files are deleted by
my-org-pomodoro--remove-temp-files-hook when Emacs exits.")
  (defun my-org-pomodoro--remove-temp-files-hook ()
    (cl-loop for x in my-org-pomodoro--create-alarm-event-temp-files
             do
             (delete-file (cdr x))))
  (add-hook 'kill-emacs-hook #'my-org-pomodoro--remove-temp-files-hook)
  (defun my-org-pomodoro--create-alarm-event (calendar-id client-id client-secret time)
    (let* ((temp-file
            (let ((x (assoc calendar-id
                            my-org-pomodoro--create-alarm-event-temp-files)))
              (if (null x)
                  (progn
                    (let ((f (make-temp-file "my-org-pomodoro--create-alarm-event")))
                      (add-to-list 'my-org-pomodoro--create-alarm-event-temp-files
                                   (cons calendar-id f))
                      f))
                (cdr x))))
           (buffer (find-file-noselect temp-file 'nowarn)))
      (with-current-buffer buffer
        (let* ((time-iso (format-time-string "%FT%T%z" time))
               (org-gcal-client-id client-id)
               (org-gcal-client-secret client-secret)
               (org-gcal-file-alist `((,calendar-id . ,temp-file)))
               (org-gcal-token-file (format "%s-%s" org-gcal-token-file calendar-id))
               (org-gcal-token-plist nil))
          (org-gcal--ensure-token)
          (message "current buffer: %s" (current-buffer))
          (org-gcal--post-event time-iso time-iso
                                "org-pomodoro break end -- get back to work!"
                                nil nil calendar-id
                                ;; skip import and export to avoid attempting to
                                ;; perform I/O using the NIL file in
                                ;; ORG-GCAL-FILE-ALIST.
                                nil nil 'skip-import 'skip-export)))))

  (add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-notify-hook)
  (add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-lock-screen)
  (add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-caffeinate)
  (add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-clock-in-break-hook)
  (add-hook 'org-pomodoro-finished-hook #'my-org-pomodoro-finished-create-break-end-alarm)
  (add-hook 'org-pomodoro-tick-hook #'my-org-pomodoro-tick-current-task-reminder)
  (add-hook 'org-pomodoro-break-finished-hook #'my-org-pomodoro-break-finished-notify-hook)
  (add-hook 'org-pomodoro-short-break-finished-hook #'my-org-pomodoro-short-break-finished-punch-in)
  (add-hook 'org-pomodoro-long-break-finished-hook #'my-org-pomodoro-long-break-finished-punch-out))

(defcustom distraction-id nil
  "Task ID of task to clock into for distracting tasks (Hacker News, Reddit, etc.). Must specify manually."
  :type 'string)
(defun distraction-clock-in ()
  "Start distracted time."
  (interactive)
  (save-excursion
      (org-id-goto distraction-id)
      (org-clock-in)))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode
          . (lambda () (evil-org-set-key-theme))))
  :init
  (c-setq org-special-ctrl-a/e t)
  :config
  ;; ;; Intentionally not enabling bindings for agenda for now.
  ;; (require 'evil-org-agenda)
  ;; (evil-org-agenda-set-keys)
  nil)

;;;; Make tag selection more intuitive
;;;; See https://blog.aaronbieber.com/2016/03/05/playing-tag-in-org-mode.html
(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let ((old-tags (org-get-tags-string))
        (tags (if tags
                  (concat " " tags)
                "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position) t)
      (replace-match tags)
      (org-set-tags t))))
(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (org-icompleting-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (let* ((cur-list (org-get-tags))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (air--org-swap-tags new)))
(defun air-org-set-tags-ctrl-c-ctrl-c-hook ()
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (if (or (eq type 'headline)
            (eq type 'inlinetask))
        (save-excursion (goto-char (org-element-property :begin context))
                        (call-interactively #'air-org-set-tags)
                        t)
      nil)))
(add-hook 'org-ctrl-c-ctrl-c-hook #'air-org-set-tags-ctrl-c-ctrl-c-hook)
(org-defkey org-mode-map (kbd "C-c C-q") #'air-org-set-tags)

;;;** Org-gcal
(use-package alert
  :config
  (c-setq alert-default-style
        (cond ((executable-find "notify-send")
               'libnotify)
              ((eq system-type 'darwin)
               'notifier)
              (t 'message))))
(use-package org-gcal
  :after org
  :ensure alert
  :ensure request-deferred
  :ensure nil
  :load-path "~/.emacs.d/lisp/org-gcal.git"
  :config
  (c-setq org-gcal-config-file (expand-file-name "org-gcal-config.el" user-emacs-directory))
  (when (file-exists-p org-gcal-config-file)
    (load org-gcal-config-file)))

;;;** Norang configuration

;; Stolen from http://doc.norang.ca/org-mode.html#Clocking
;; bh/organization-task-id changed.

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(c-setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(c-setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(c-setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(c-setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(c-setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(c-setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(c-setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(c-setq org-clock-persist t)
;; Do not prompt to resume an active clock
(c-setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(c-setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(c-setq org-clock-report-include-clocking-task t)
;; Create globally unique entry IDs when needed
(c-setq org-id-link-to-org-use-id t)

;; Reset day at 4 AM, just like Anki.
(c-setq org-extend-today-until 4)

;;; Recompute effort of a parent headline from the efforts of the children if
;;; they sum to a higher value.
(defun my-org-update-heading-effort-from-children (pt)
  "Compute the sum of efforts for each child of the heading at point PT. If
the sum is greater than the current effort for this heading, offer to update
it. This function is called recursively on each child, so the entire tree's
efforts may be updated by this function."
  (save-excursion
    (save-restriction
      (goto-char pt)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (outline-show-all)
      (let*
          ((current-effort
            (org-duration-to-minutes
             (or (org-entry-get (point) org-effort-property) 0)))
           (children-effort 0))
        (while (outline-next-heading)
          (let ((x (my-org-update-heading-effort-from-children (point))))
            (setq children-effort (+ children-effort (nth 0 x)))
            (goto-char (nth 1 x))))
        (goto-char pt)
        (let ((children-effort-duration
               (org-duration-from-minutes children-effort)))
          (when (and (< current-effort children-effort)
                 (y-or-n-p-with-timeout
                  (format "Update effort to children's sum (%s)?"
                          children-effort-duration)
                  60 nil))
            (org-entry-put (point) org-effort-property
                           children-effort-duration)
            (setq current-effort children-effort)))
        (list current-effort (point-max))))))
(defun my-org-effort-from-children-hook ()
  "Update effort of a heading from its children before clocking in."
  (my-org-update-heading-effort-from-children (point))
  nil)
(add-hook 'org-clock-in-prepare-hook 'my-org-effort-from-children-hook)

;;; bh clocking functions
(c-setq bh/keep-clock-running nil)

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
      ;; Find the tags on the current task
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

(defcustom bh/organization-task-id nil
  "Task ID of default Organization task (for use with bh/clock-in-organization-task-as-default. Must specify manually."
  :type 'string)

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

;; Needed for clocking functions: http://doc.norang.ca/org-mode.html#Projects
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
          (setq days (1+ days)))

        (end-of-buffer)
        ;; transform constructed list of dates into a single, optimized regex
        (setq date-regex (regexp-opt (split-string (buffer-string) "\n" t)))))


    ;; ask user, which buffers to search and how to present results
    (setq collect-method
          (car (split-string (org-icompleting-read "Please choose, which buffers to search and how to present the matches: "
                                                   '("multi-occur -- all org-buffers, list" "org-occur -- this-buffer, sparse tree") nil t nil nil "occur -- this buffer, list"))))

    ;; Perform the actual search
    (save-window-excursion
      (cond ((string= collect-method "occur")
             (occur date-regex))

            ((string= collect-method "org-occur")
             (if (string= major-mode "org-mode")
                 (org-occur date-regex)
               (error "Buffer not in org-mode")))

            ((string= collect-method "multi-occur")
             ;; construct list of all org-buffers
             (dolist (buff (buffer-list))
               (set-buffer buff)
               (if (string= major-mode "org-mode")
                   (setq org-buffers (cons buff org-buffers))))
             (multi-occur org-buffers date-regex))))

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
                               nil))

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
                             'string<))
                ;; pretend, that we did not modify the occur-buffer
                (insert "Searched " pretty-dates "\n")
                (goto-char (point-min))
                (set-buffer-modified-p nil)
                (message (concat "occur-buffer with matches " pretty-dates "(`C-h m' for help)")))

            (setq inhibit-read-only original-inhibit-read-only))))



      ;; switch to occur-buffer
      (if (get-buffer occur-buffer-name)
          (switch-to-buffer occur-buffer-name)))))

;;; Don't insert hard spaces to indent text with heading in Org mode
(c-setq org-adapt-indentation nil)
(c-setq org-startup-indented nil)

(require 'org-inlinetask)

(c-setq org-enforce-todo-dependencies t)

(c-setq org-log-done (quote time))
(c-setq org-log-redeadline (quote time))
(c-setq org-log-reschedule (quote time))

;;; Week in review (https://emacs.stackexchange.com/a/7864)
(defcustom org-timeline-files nil
  "The files to be included in `org-timeline-all-files'. Follows
  the same rules as `org-agenda-files'"
  :type 'sexp)

(c-setq org-timeline-files org-agenda-files)

(add-to-list 'org-agenda-custom-commands
             '("R" "Week in review"
               agenda ""
               ;; agenda settings
               ((org-agenda-span 'week)
                (org-agenda-start-on-weekday 0) ;; start on Sunday
                (org-agenda-overriding-header "Week in Review (no archives)")
                (org-agenda-files
                 (let ((org-agenda-files org-timeline-files))
                   (org-agenda-files nil 'ifmode)))
                (org-agenda-log-mode-items '(clock state closed))
                ;; Ignore scheduled and deadline tasks, showing only log entries
                (org-agenda-start-with-log-mode 'only)
                ;; Don't include archive files - I won't have archived items in
                ;; the past week.
                (org-agenda-archives-mode nil))))

(use-package org-super-agenda
  :ensure t
  :config
  (c-setq org-super-agenda-groups
          '(;; Each group has an implicit boolean OR operator between its selectors.
            (:name "Today"  ; Optionally specify section name
                   :time-grid t  ; Items that appear on the time grid
                   :todo "TODAY")  ; Items that have this TODO keyword
            (:name "Important"
                   ;; Single arguments given alone
                   :priority "A")
            ;; Set order of multiple groups at once
            (:order-multi (2 (:name "Shopping in town"
                                    ;; Boolean AND group matches items that match all subgroups
                                    :and (:tag "shopping" :tag "@town"))
                             (:name "Food-related"
                                    ;; Multiple args given in list with implicit OR
                                    :tag ("food" "dinner"))
                             (:name "Personal"
                                    :habit t
                                    :tag "personal")
                             (:name "Space-related (non-moon-or-planet-related)"
                                    ;; Regexps match case-insensitively on the entire entry
                                    :and (:regexp ("space" "NASA")
                                                  ;; Boolean NOT also has implicit OR between selectors
                                                  :not (:regexp "moon" :tag "planet")))))
            ;; Groups supply their own section names when none are given
            (:todo "WAITING" :order 8)  ; Set order of this section
            (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                   ;; Show this group at the end of the agenda (since it has the
                   ;; highest number). If you specified this group last, items
                   ;; with these todo keywords that e.g. have priority A would be
                   ;; displayed in that group instead, because items are grouped
                   ;; out in the order the groups are listed.
                   :order 9)
            (:priority<= "B"
                         ;; Show this section after "Today" and "Important", because
                         ;; their order is unspecified, defaulting to 0. Sections
                         ;; are displayed lowest-number-first.
                         :order 1)))
            ;; After the last group, the agenda will display items that didn't
            ;; match any of these groups, with the default order position of 99

  (org-super-agenda-mode 1))

;;; List tasks that should be archived
;;; http://doc.norang.ca/org-mode.html#Archiving
(add-to-list 'org-agenda-custom-commands
             '("A" "Archivable tasks" tags "-REFILE"
               ((org-agenda-skip-function #'bh/skip-non-archivable-tasks)
                (org-tags-match-list-sublevels nil)
                (org-agenda-archives-mode nil))))
(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;; Use sticky agenda's so they persist
(c-setq org-agenda-sticky t)

(c-setq org-list-allow-alphabetical t)

;;;* Autorevert
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :config (global-auto-revert-mode t))

;;;* Flyspell
(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode 'append)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode 'append))

;;;* Remove trailing whitespace intelligently
(use-package ws-butler
  :diminish ws-butler-mode
  :commands ws-butler-mode
  :init
  (add-hook 'text-mode-hook (lambda () (ws-butler-mode 1)))
  (add-hook 'prog-mode-hook (lambda () (ws-butler-mode 1))))

;;;* Miscellaneous
(c-setq bookmark-default-file
        (expand-file-name "cache/bookmarks" user-emacs-directory))

;;; Enable commands disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)

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


;;; Split window vertically if possible -- this will split vertically if the
;;; window is wide enough for the contents of both buffers.
(c-setq split-height-threshold nil)

;;;* Magit
(use-package magit
  :init
  (c-setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (c-setq vc-handled-backends (delq 'Git vc-handled-backends)))

;;;* git-auto-commit-mode
(use-package git-auto-commit-mode
  :ensure nil
  :load-path "~/.emacs.d/lisp/git-auto-commit-mode/")

;;;* https://codearsonist.com/reading-for-programmers
(use-package pdf-tools)
(pdf-tools-install)
(use-package interleave)
(use-package org-ref
  :after org
  :config
  (c-setq org-ref-notes-directory "~/Documents/org/home-org")
  (c-setq org-ref-bibliography-notes "~/Documents/org/home-org/index.org")
  (c-setq org-ref-default-bibliography '("~/Documents/org/home-org/index.bib"))
  (c-setq org-ref-pdf-directory "~/Documents/org/home-org/lib/"))
(use-package helm-bibtex
  :config
  (c-setq helm-bibtex-bibliography "~/Documents/org/home-org/index.bib") ;; where your references are stored
  (c-setq helm-bibtex-library-path "~/Documents/org/home-org/lib/") ;; where your pdfs etc are stored
  (c-setq helm-bibtex-notes-path "~/Documents/org/home-org/index.org") ;; where your notes are stored
  (c-setq bibtex-completion-bibliography "~/Documents/org/home-org/index.bib") ;; writing completion
  (c-setq bibtex-completion-notes-path "~/Documents/org/home-org/index.org"))

;;;* Preserve scratch file across sessions
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;;;* which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;;;* Folding
(defun my-fold-setup ()
  (interactive)
  (hs-minor-mode)
  (hs-hide-all)
  (diminish 'hs-minor-mode))
(add-hook 'prog-mode-hook #'my-fold-setup)

;;;* Storage for variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;;* Subfiles
(use-package whitespace-conf
  :ensure nil
  :load-path "~/.emacs.d/lisp")
(use-package org-drill-cloze-enhancement
  :ensure nil
  :after org
  :load-path "~/.emacs.d/lisp")

;;;* Local configuration

;;; Allow users to provide an optional "init-local" containing personal settings
(use-package init-local
  :ensure nil
  :load-path "~/.emacs.d/lisp")

;;; Local Variables:
;;; outline-regexp: ";;;\\*+\\|\\`"
;;; End:

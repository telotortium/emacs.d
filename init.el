; -*- lexical-binding: t; no-byte-compile: t -*-

;;;* Introduction

;;; This file bootstraps the configuration, which is divided into a number of
;;; other files.

;;; Byte compilation is disabled for this file to avoid ever loading stale
;;; bytecode. The commands in this file will ensure that stale bytecode is
;;; never loaded for other files.

(defmacro c-setq (variable value &optional comment)
  "Exactly like setq, but handles custom.

If VARIABLE was declared by `defcustom', then use `customize-set-variable' to
set it.  Otherwise, use just `set-default'.  Taken from http://lists.gnu.org/archive/html/emacs-devel/2017-11/msg00119.html."
  `(if (get ',variable 'custom-type)
       (customize-set-variable ',variable ,value ,comment)
     (set-default ',variable ,value)))

;;;* Storage for variables configured via the interactive 'customize' interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;;* gc-cons-threshold

;;; Set this higher in the minibuffer, but don't make it too bad other wise
;;; you'll figure out quick

(c-setq gc-cons-threshold (* 16 1024 1024) "\
Set gc-cons-threshold to a higher value. This should be done before
searching for packages because package searches and installation tend to
generate a lot of garbage. Cite:
http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/.")

(defun my-minibuffer-setup-hook ()
  "Disable GC inside minibuffer.

See also `my-minibuffer-exit-hook'."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Re-enable GC upon exiting minibuffer.

See also `my-minibuffer-setup-hook'."
  (setq gc-cons-threshold (* 16 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;;* Package loading

;;; Avoid loading .elc files older than their corresponding .el file.
(c-setq load-prefer-newer t)

;;; Fix TLS certificate "could not be verified" errors
;;; (http://emacs.stackexchange.com/a/18070).)
(c-setq gnutls-verify-error t)

(c-setq tls-checktrust 'ask)

(c-setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)     ; Enable :straight in ‘use-package’

(use-package bind-key :straight t)      ; To make :bind work
(use-package diminish :straight t)      ; To use :diminish

(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; For logging messages prefixed with function name.
(use-package call-log
  :straight (:host github :repo "jordonbiondo/call-log"
             :fork (:host nil :repo "git@github.com:telotortium/call-log")))

;;; Use auto-compile to recompile bytecode whenever files are loaded or saved.
(use-package auto-compile
  :straight t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;;* Basic startup configuration

;;; Disable startup screen
(c-setq inhibit-startup-message t)

;;; Command to restart emacs from within emacs
(use-package restart-emacs :straight t)

;;; Allow access from emacsclient
(use-package server
  :straight (:type built-in)
  :config
  (c-setq server-name "server")
  (c-setq server-socket-dir "~/.emacs.d/server")
  (c-setq server-use-tcp t)
  (defun warn-server-name-changed (original-server-name)
    (when (not (string= server-name original-server-name))
      (warn "server-name = \"%s\", should be \"%s\""
            server-name original-server-name)))
  (run-at-time nil 30 #'warn-server-name-changed server-name)
  (unless (eq (server-running-p) t) (server-start)))

;;;* Leuven theme
(use-package leuven-theme
  :straight t
  :custom
  (leuven-scale-outline-headlines nil)
  :config
  (load-theme 'leuven t)
  (load-theme 'leuven-customization t))

;;;* Ivy configuration
(use-package amx :straight t)
(use-package counsel
  :diminish ivy-mode
  :diminish counsel-mode
  :straight t
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
  (defun counsel-rg-org (search-archives)
    "Specialize ‘counsel-rg’ for Org-mode files.

  Unless ‘\\[universal-argument]’ prefix ARG is used, don’t include archives in
  the search. Saves all Org buffers beforehand so that ‘counsel-rg’ sees the
  contents of all Org-mode buffers."
    (interactive "P")
    (org-save-all-org-buffers)
    (let* ((extra-rg-args (concat "--smart-case"
                                  " --type-add 'org:*.org'"
                                  " --type-add 'org:*.org_archive'"
                                  " --type org")))
      (when (not search-archives)
        (setq extra-rg-args (concat extra-rg-args " '-g!*.org_archive'")))
      (counsel-rg nil "~/Documents/org/" extra-rg-args nil)))
  (global-set-key (kbd "C-c q") #'counsel-rg-org)
  ;; Unbind ivy-restrict-to-matches - always annoys me during org-capture.
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;;;* Evil configuration

;; Dependency of evil-mode
(use-package undo-tree
  :straight t
  :diminish undo-tree-mode)

(use-package evil
  :straight t
  :init
  (c-setq evil-want-keybinding nil)
  :config (evil-mode 1))

;; Space as leader
(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; Scroll with C-u and access universal-argument with <Leader>-u.
(c-setq evil-want-C-u-scroll t)
(evil-leader/set-key "u" 'universal-argument)
(evil-leader/set-key "s" (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; Evil magic search
(c-setq evil-magic 'very-magic)
(c-setq evil-search-module 'evil-search)

(use-package evil-collection
  :straight t
  :init (evil-collection-init)
  :config
  ;; Avoid conflict with evil-org
  (setq evil-collection-outline-bind-tab-p nil))

(defun switch-to-previous-buffer ()
  "Switch to previously current buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
;; Disable all the C-<number> keys, and give C-^ and C-6 the same behavior as
;; they have in Vim.
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
  :straight t
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
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :straight t
  :config
  (global-evil-visualstar-mode))

(use-package evil-numbers
  :straight t
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
  :straight t
  :bind (:map global-map ("RET" . #'newline-and-indent))
  :config (clean-aindent-mode 1))

;; No tabs
(setq-default indent-tabs-mode nil)

;; Automatically wrap long lines
(setq-default fill-column 79)
(defun prog-mode-wrap-hook ()
  "Set auto-fill for comments only in `prog-mode'."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode t)
  (diminish 'auto-fill-function))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'prog-mode-wrap-hook)

;;; Disable scrollbars
(scroll-bar-mode -1)

;;; Line and column numbers
(c-setq display-line-numbers-type 'visual)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(line-number-mode 1)
(column-number-mode 1)

(use-package rg
  :straight t
  :commands (rg rg-project rg-dwim)
  :config
  (c-setq rg-custom-type-aliases
          '(("org" . "*.org *.org_archive")))
  (rg-define-search rg-org
    "Run rg on my Org files"
    :query ask
    :files "org"
    :dir "~/Documents/org"
    :confirm prefix
    :flags ("--smart-case")             ; Emacs default search is smart case
    :menu ("Custom" "o" "Org-mode files"))
  (defun rg-org-save-files (&rest unused)
    "Run ‘org-save-all-org-buffers’ so ‘rg-org’ searches all file contents."
    (org-save-all-org-buffers))
  (advice-add 'rg-org :before #'rg-org-save-files))


;;; Company
(use-package company-flx
  :straight t
  :config
  (with-eval-after-load 'company
    (company-flx-mode +1)))
(use-package company
  :straight t
  :diminish company-mode
  :custom
  (company-tooltip-limit 20 "bigger popup window")
  (company-idle-delay .3 "decrease delay before autocompletion popup shows")
  (company-echo-delay 0 "remove annoying blinking")
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-go :straight t)

;;; Flycheck
(use-package flycheck
  :straight t                           ; May want to use built-in version at Google
  :defer t                              ; init-local-google will locate this
  :hook (after-init . global-flycheck-mode)
  :custom
  ;; Inherit Emacs load-path from current session - prevents annoying errors
  ;; from custom packages.
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Don't re-run Flycheck syntax checkers on inserting new lines, to save
  ;; performance.
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 4))


;;; syntax highlighting for vimscript files
(use-package vimrc-mode
  :straight t
  :mode ".vim\\(rc\\)?$")

;;; Markdown
(use-package markdown-mode
  :straight t
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
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Save mini-buffer history
(use-package savehist
  :straight (:type built-in)
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
  :straight t
  :mode "\\.rs\\'")

;; Lua mode
(use-package lua-mode
  :straight t
  :mode "\\.lua$"
  :interpreter "lua")

;; Java
(use-package eclim
  :straight t
  :config
  (require 'eclimd)
  (global-eclim-mode))
(use-package company-emacs-eclim
  :straight t
  :config
  (company-emacs-eclim-setup))

;; Javascript
(use-package js-mode
  :straight (:type built-in)
  :mode ("\\.js\\'" "\\.javascript\\'"))

;; Multi Web Mode - automatically switch to right major mode in HTML files
(use-package multi-web-mode
  :straight t
  :custom
  (mweb--major-mode 'html-mode)
  (mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
               (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
               (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  :config
  (multi-web-global-mode 1))

(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package rainbow-identifiers
  :straight t
  :hook (prog-mode . rainbow-identifiers-mode)
  :config
  :custom
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  (rainbow-identifiers-cie-l*a*b*-saturation 30)
  (rainbow-identifiers-cie-l*a*b*-color-count 64)
  (rainbow-identifiers-face-count 64)
  ;; Disable distinctive variable name face so that rainbow-identifiers-mode
  ;; highlights the variable at its declaration the same way as its use.
  (rainbow-identifiers-faces-to-override '(font-lock-variable-name-face)))

(use-package paren
  :straight (:type built-in)
  :config (show-paren-mode 1))

;;; Go
(use-package go-mode
  :straight t
  :config
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
  :straight t
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
(use-package paredit :straight t)
(use-package parinfer
  :straight t
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
  :straight t
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
(use-package htmlize :straight t)      ; For org-publish
(straight-use-package 'org-plus-contrib)
(use-package org
  :ensure nil                           ; Package downloaded by org-plus-contrib
  :init
  :config
  (add-to-list 'org-modules 'org-habit)
  (require 'org-agenda)
  (require 'org-attach)                 ; Needed for attachment: links to work
  (require 'org-protocol)
  (require 'org-tempo))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "S-<f11>") #'org-clock-goto)
(global-set-key (kbd "C-<f11>") #'my-org-clock-in)
(global-set-key (kbd "C-S-<f11>") #'my-org-goto-heading)
(defun my-org-clock-in ()
  "Select a recently clock-in task to clock into.  See `org-clock-in'."
  (interactive) (org-clock-in '(4)))
(defun my-org-goto-heading ()
  "Run C-u org-refile to list all headings."
  (interactive)
  ;; org-file doesn't work unless it's run from within an Org buffer, so find
  ;; an arbitrary one.
  (with-current-buffer
    (save-excursion
      (catch 'aaa
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (derived-mode-p 'org-mode)
              (throw 'aaa buffer))))))
    (org-refile '(4))))

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


;;;** Org capture

;; The following code is disabled currently because it's causing Emacs capture
;; buffers to hang. Instead I'm using this configuration from Doom.
(load (expand-file-name "lisp/auto/org-capture-doom.el" user-emacs-directory))

;; ;;; Kill the frame if one was created for the capture
;; (defun my-org-capture-finalize-delete-frame-from-org-protocol (oldfun &rest args)
;;   "Delete frames created by external org-protocol capture scripts.

;; This hook tests the presence of a frame parameter `org-protocol-capture' on
;; the created frame. This can be set using the `--frame-parameters' flag to
;; `emacsclient'."
;;   ;; First save the current frame before the capture is finalized.
;;   (let* ((frame (window-frame (get-buffer-window (current-buffer) t)))
;;          ;; Ensure org-capture-refile prompt appears in foreground frame
;;          (default-minibuffer-frame frame)
;;          (minibuffer-auto-raise t))
;;     (apply oldfun args)
;;     ;; After the capture is finalized, delete the frame.
;;     (when (frame-parameter frame 'org-protocol-capture)
;;       ;; Use condition-case to avoid useless error if attempting to close last frame.
;;       (condition-case nil
;;           (delete-frame frame)
;;         (error nil)))))

;; (defun my-org-capture-refile-delete-frame-from-org-protocol (oldfun &rest args)
;;   "Delete frames created by external org-protocol capture scripts.

;; Advice around `org-capture-refile' to temporarily remove advice
;; `my-org-capture-delete-frame-from-org-protocol' around `org-capture-finalize'
;; while calling `org-capture-refile'.  This is needed because `org-capture-refile'
;; calls `org-capture-finalize' internally.  Without removing the advice, the frame
;; is closed before I have a chance to refile it.

;; Takes an UNUSED argument."
;;   (unwind-protect
;;       (progn
;;         (advice-remove 'org-capture-finalize
;;                        #'my-org-capture-finalize-delete-frame-from-org-protocol)
;;         (apply #'my-org-capture-finalize-delete-frame-from-org-protocol
;;                oldfun args))
;;     (advice-add 'org-capture-finalize
;;                 :around
;;                 #'my-org-capture-finalize-delete-frame-from-org-protocol)))
;; (defalias 'my-org-capture-kill-delete-frame-from-org-protocol
;;   #'my-org-capture-refile-delete-frame-from-org-protocol)

;; (defun my-org-capture-steal-focus (&rest unused)
;;   "Steal focus for frames created by external org-protocol capture scripts.

;; Stealing focus here ensures that the newly-created frame is the one from which
;; prompts are read.  Otherwise, the prompt would appear on the existing frame.

;; This hook tests the presence of a frame parameter `org-protocol-capture' on the
;; newly-created frame.  This can be set using the `--frame-parameters' flag to
;; `emacsclient'.

;; Takes an UNUSED argument."
;;   (when (frame-parameter nil 'org-protocol-capture)
;;     (x-focus-frame nil)))

;; (defun my-org-capture-buffer-sole-window ()
;;   "Delete all windows containing the current Org Capture buffer except for the
;; one launched by a org-protocol Emacsclient.

;; This hook tests the presence of a frame parameter `org-protocol-capture' on
;; the newly-created frame. This can be set using the `--frame-parameters' flag to
;; `emacsclient'."
;;   (run-at-time 1 nil #'my-org-capture-buffer-sole-window--inner (current-buffer)))

;; (defun my-org-capture-buffer-sole-window--inner (buf)
;;   "Function scheduled from `my-org-capture-capture-buffer-sole-window' to run
;; after org-capture-mode is entered."
;;   (require 'call-log)
;;   (dolist (frame (frame-list))
;;     (clog/msg "frame %S" frame)
;;     (when (frame-parameter frame 'org-protocol-capture)
;;       (clog/msg "my-org-capture-capture-buffer-sole-window: %S"
;;                 (get-buffer-window-list nil nil t))
;;       (let* ((wnd (frame-root-window frame)))
;;         (with-current-buffer buf
;;           ;; Set the newly-created frame's window to the capture buffer.
;;           (set-window-buffer wnd buf)
;;           ;; Delete all windows except `wnd' containing the capture buffer.
;;           (dolist (w (get-buffer-window-list buf nil t))
;;             (clog/msg "gbwl: %S, %S : %S"
;;                       wnd w (eq wnd w))
;;             (unless (or (eq wnd w))
;;               (clog/msg "gbwl: deleting window %S" w)
;;               (delete-window w)))
;;           ;; Ensure newly-created frame is in the foreground.
;;           (select-frame-set-input-focus frame))))))

;; (advice-add 'org-capture :before #'my-org-capture-steal-focus)
;; (add-hook 'org-capture-mode-hook #'my-org-capture-buffer-sole-window)
;; (advice-add 'org-capture-finalize
;;             :around #'my-org-capture-finalize-delete-frame-from-org-protocol)
;; (advice-add 'org-capture-kill
;;             :around #'my-org-capture-kill-delete-frame-from-org-protocol)
;; (advice-add 'org-capture-refile
;;             :around #'my-org-capture-refile-delete-frame-from-org-protocol)

(c-setq org-agenda-files (expand-file-name "agenda_files" user-emacs-directory))
(c-setq org-agenda-span 7)
(c-setq org-agenda-start-on-weekday nil)
(c-setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(defun transform-square-brackets-to-curly-ones (string-to-transform)
  "Transforms [ into ( and ] into ) in STRING-TO-TRANSFORM.

Other chars left unchanged."
  (concat
   (mapcar #'(lambda (c)
               (cond ((equal c ?\[) ?\{)
                     ((equal c ?\]) ?\})
                     (t c)))
           string-to-transform)))
(defun org-clock-report-buffer (&optional no-narrow)
  "Evaluate all the clocktables in the buffer.

By default, evaluate only the clocktables in the current Org subtree, in order
to avoid recomputing all the clock tables in the buffer, which will take a
while in my daily-log.org file.  With a prefix arg NO-NARROW, evaluate all the
clocktables in the currently visible portion of the buffer."
  (interactive "P")
  (save-restriction
    (unless no-narrow
      (org-narrow-to-subtree))
    (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "#\\\+BEGIN: clocktable" nil t)
            (org-clock-report)
            (forward-line 1)))))

(defcustom org-daily-log-file
  (concat org-directory "/daily-log.org")
  "The path to Org file in which daily log entries are captured."
  :type 'file)

(c-setq org-capture-templates
      `(("t" "Task" entry (file (lambda () (concat org-directory "/inbox.org")))
         "
* TODO %?%^{Title}
%^{Effort}p%u
" :clock-in t :clock-resume t :jump-to-captured t)
        ("n" "Note" entry (file (lambda () (concat org-directory "/inbox.org")))
         "
* %u %?
" :jump-to-captured t)
        ("i" "Idea" entry (file (lambda () (concat org-directory "/inbox.org")))
         "
* %u %?REPLACE_ME                      :IDEA:
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
        ("W" "GTD weekly review" entry (file+olp+datetree org-daily-log-file)
         "
* NEXT %u GTD weekly review
SCHEDULED: <%<%Y-%m-%d %a 13:00-14:00>>
:PROPERTIES:
:Effort:   1:00
:END:
Follow:

- [[https://gettingthingsdone.com/wp-content/uploads/2014/10/Weekly_Review_Checklist.pdf][Weekly Review Checklist]]
- \"Weekly Review\" section in Getting Things Done.

  In 2015 edition: Chapter 8: \"Reflecting: Keeping It All Fresh and
  Functional\", section \"The Power of the Weekly Review\".

Checklist:

- GET CLEAR
  - [ ] Collect Loose Papers and Materials \\\\
    Gather all accumulated business cards, receipts, and miscellaneous
    paper-based materials into your in-tray.
  - [ ] Get “IN” to Zero \\\\
    Process completely all outstanding paper materials, journal and meeting
    notes, voicemails, dictation, and emails.
    - [ ] inbox.org files
    - [ ] Personal email
    - [ ] Personal tasks (use ~sync-tasks~)
    - [ ] Corp email
    - [ ] Corp tasks (use ~sync-tasks~)
    - [ ] [[https://b.corp.google.com/savedsearches/5024171][Buganizer: Org-mode assigned but not captured]]
    - [ ] [[https://critique.corp.google.com/#search/&q=reviewer:me+-is:submitted+-starred:me][Critique: CLs to review]]
    - [ ] Close all browser tabs (use org-capture-extension to capture)
    - [ ] Chats
  - [ ] Empty Your Head \\\\
    Put in writing and process any uncaptured new projects, action items,
    waiting for’s, someday maybe’s, etc.
- GET CURRENT
  - [ ] Review Action Lists \\\\
    Mark off completed actions. Review for reminders of further action steps to
    record.
  - [ ] Review Previous Calendar Data \\\\
    Review past calendar in detail for remaining action items, reference data,
    etc., and transfer into the active system.
  - [ ] Review Upcoming Calendar \\\\
    Review upcoming calendar events–long and short term. Capture actions
    triggered.
  - [ ] Review Waiting For List \\\\
    Record appropriate actions for any needed follow-up. Check off received
    ones.
  - [ ] Review Project (and Larger Outcome) Lists \\\\
    Evaluate status of projects, goals, and outcomes, one by one, ensuring at
    least one current action item on each.  Browse through project plans,
    support material, and any other work-in-progress material to trigger new
    actions, completions, waiting for’s, etc.
  - [ ] Review Any Relevant Checklists \\\\
    Use as a trigger for any new actions.
- GET CREATIVE
  - [ ] Review Someday Maybe List \\\\
    Review for any projects which may now have become active, and transfer to
    “Projects.” Delete items no longer of interest.
  - [ ] Be Creative and Courageous \\\\
    Any new, wonderful, hare-brained, creative, thought-provoking, risk-taking
    ideas to add into your system???
" :time-prompt t :tree-type week :clock-in t :clock-resume t :jump-to-captured t)
        ("p" "Link and Text" entry (file+headline org-default-notes-file "Links")
         "
* %?REPLACE_ME
Source: [[%:link][%:description]]
#+BEGIN_QUOTE
%i
#+END_QUOTE

%U
")
        ("L" "Link" entry (file+headline org-default-notes-file "Links")
         "
* %?[[%:link][%(transform-square-brackets-to-curly-ones \"%:description\")]]
  %U
" :jump-to-captured t)))

(defun org-today-overridden ()
  (if (null org-overriding-default-time)
      (org-today)
    (let* ((ot
            (decode-time org-overriding-default-time))
           (cgreg (list (nth 4 ot) (nth 3 ot) (nth 5 ot))))
      (calendar-absolute-from-gregorian cgreg))))

(defun my-org-daily-log--find-daily-log ()
   (re-search-forward
    (rx-to-string
     `(and
       line-start
       (repeat 4 "*")
       " "
       (0+ not-newline)
       ,(let ((d (calendar-gregorian-from-absolute (org-today-overridden))))
         (format "[%04d-%02d-%02d "
                 (calendar-extract-year d)
                 (calendar-extract-month d)
                 (calendar-extract-day d)))
       (0+ not-newline)
       "] Daily log"))))

(defun my-org-daily-log--find-today ()
   (re-search-forward
    (rx-to-string
     `(and
       line-start
       (repeat 3 "*")
       ,(let ((d (calendar-gregorian-from-absolute (org-today-overridden))))
         (format " %04d-%02d-%02d "
                 (calendar-extract-year d)
                 (calendar-extract-month d)
                 (calendar-extract-day d)))))))

(defun my-org-daily-log--goto-daily-log-headline ()
  (condition-case nil
    (save-excursion
      (with-current-buffer (get-file-buffer org-daily-log-file)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (my-org-daily-log--find-today)
          (org-narrow-to-subtree)
          (my-org-daily-log--find-daily-log)
          (point-marker))))
    (error nil)))

(defun my-org-daily-log-goto-today ()
  "Go to today's default log, or create it if not created yet."
  (interactive)
  (require 'call-log)
  (let ((daily-log-marker (my-org-daily-log--goto-daily-log-headline)))
    (if daily-log-marker
        (progn
          (clog/msg "in marker branch")
          (switch-to-buffer (marker-buffer daily-log-marker))
          (widen)
          (goto-char (marker-position daily-log-marker))
          (org-narrow-to-subtree)
          (set-marker daily-log-marker nil))
      (progn
        (let ((org-overriding-default-time
               (or org-overriding-default-time (current-time))))
          (org-capture nil "D")
          (org-capture-finalize 'stay-with-capture)
          (org-narrow-to-subtree))))))

(c-setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(c-setq org-refile-use-outline-path t)
(c-setq org-alphabetical-lists t)
(c-setq org-src-fontify-natively t)
(c-setq org-pretty-entities t)
(c-setq org-use-sub-superscripts '{})
(c-setq org-hide-macro-markers t)
(defun org-toggle-hide-macro-markers ()
  "Toggle the literal or descriptive display of macros."
  (interactive)
  (setq org-hide-macro-markers (not org-hide-macro-markers))
  (org-restart-font-lock))


;;;** Todo settings
(c-setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n@/!)" "|" "DONE(d)")
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

(defcustom my-org-agenda-active-days 14
  "Number of days in the past to search for active projects.")
(cl-defun my-org-agenda-someday-maybe (&optional buffer)
  "Show agenda for Someday/Maybe tasks.

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo)
      (tags "HOLD")
      (not (tags "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :buffer (or buffer org-ql-view-buffer)
    :title "Someday/Maybe tasks"))
(cl-defun my-org-agenda-waiting (&optional buffer)
  "Show agenda for NEXT steps in org-mode projects

Use `org-ql-search' to search for all WAITING tasks."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo)
      (tags "WAITING")
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :buffer (or buffer org-ql-view-buffer)
    :sort 'date
    :title "WAITING for tasks"))
(cl-defun my-org-agenda-loose-todos (&optional buffer)
  "Show agenda for Loose TODOs (those not part of projects)

Use `org-ql-search' to search for all loose TODOs."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo "TODO")
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (not (bh/skip-subprojects)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :title "Loose TODOs (not part of projects)"
    :buffer (or buffer org-ql-view-buffer)))
(cl-defun my-org-agenda-stuck-projects (&optional buffer)
  "Show agenda for projects with stuck tasks

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (not (done))
      (not (todo "NEXT"))
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (not (bh/skip-non-subprojects)))
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :title "Tasks making project stuck"
    :buffer (or buffer org-ql-view-buffer)))
(cl-defun my-org-agenda-next-tasks (&optional buffer)
  "Show agenda for NEXT steps in org-mode projects

Use `org-ql-search' to search for all NEXT steps for projects.  Show only the
NEXT steps that have a timestamp within the last `my-org-agenda-active-days'
days."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (todo "NEXT")
      (not (tags "HOLD" "CANCELLED" "ARCHIVED"))
      (not (scheduled :from 1))
      (or (not (children))
          (children (not (todo))))
      (ts :from ,(- my-org-agenda-active-days)))
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :sort 'date
    :title (format
            "NEXT (grouped by parent, except scheduled for future, %d-day active)"
            my-org-agenda-active-days)))
(cl-defun my-org-agenda-archivable-tasks (&optional buffer)
  "Show agenda for Archivable tasks

Consider these types of headlines for archiving:

- Headlines with a *done* todo keyword.
- Headlines with *no* todo keyword tagged with \"gcal\" - these are
  entries created by org-gcal. If I'm actively managing such a task,
  I'll always add a todo keyword of some kind to the heading, so these
  tasks will be saved from archiving unless they're marked done.

Only consider top-level tasks in project trees - don't individually archive
tasks that are part of an ongoing project. Only archive projects that have been
done for at least 30 days.

Daily log entries (marked by the \"dailylog\" tag) should never be
archived.

Use `org-ql-search' to search."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    `(and
      (not (tags "REFILE" "dailylog" "ARCHIVE"))
      (not (ts :from -30))
      (or (done)
          (and (tags "gcal")
               (not (todo))))
      (or (not (parent))
          (parent (and (not (todo)) (not (done)))))
      (or (not (children))
          (descendants
           (and (not (todo))
                (not (ts :from -30))))))
    :buffer (or buffer org-ql-view-buffer)
    :super-groups '((:auto-map my-org-super-agenda-group-by-project-or-task-group))
    :sort 'date
    :title "Archivable tasks"))
(defmacro my-org-agenda-ql-wrapper (wrapper-name wrapped-func-name)
  "Defines a wrapper for use in `org-agenda-custom-commands'.

Calling this macro will define a function named WRAPPER-NAME that wraps
WRAPPED-FUNC-NAME in order to be called by
`org-agenda-custom-commands'.  WRAPPED-FUNC-NAME must be passed an unused
argument when called in `org-agenda-custom-commands'."
  `(cl-defun ,wrapper-name (unused)
     ,(format "Wrap `%s' for `org-agenda'" wrapped-func-name)
     (with-current-buffer org-agenda-buffer-name
       (,wrapped-func-name (current-buffer)))))
(my-org-agenda-ql-wrapper my-org-agenda-someday-maybe-agenda-command
                          my-org-agenda-someday-maybe)
(my-org-agenda-ql-wrapper my-org-agenda-waiting-agenda-command
                          my-org-agenda-waiting)
(my-org-agenda-ql-wrapper my-org-agenda-loose-todos-agenda-command
                          my-org-agenda-loose-todos)
(my-org-agenda-ql-wrapper my-org-agenda-stuck-projects-agenda-command
                          my-org-agenda-stuck-projects)
(my-org-agenda-ql-wrapper my-org-agenda-next-tasks-agenda-command
                          my-org-agenda-next-tasks)
(my-org-agenda-ql-wrapper my-org-agenda-archivable-tasks-agenda-command
                          my-org-agenda-archivable-tasks)

(c-setq org-agenda-span 1)
(setq my-org-agenda-export-options
      ;; Use defaults for now, but leave available for future customization
      '())
(c-setq org-agenda-custom-commands '())
(add-to-list 'org-agenda-custom-commands
             '("H" "Someday/Maybe"
               ((my-org-agenda-someday-maybe-agenda-command ""))))
(add-to-list 'org-agenda-custom-commands
             `("W" "WAITING"
               ((my-org-agenda-waiting-agenda-command ""))))
(add-to-list 'org-agenda-custom-commands
             `("U" "Loose TODOs (not part of projects)"
               ((my-org-agenda-loose-todos-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "Loose TODOs (not part of projects)")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-U-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             '("u" "Tasks making projects stuck"
               ((my-org-agenda-stuck-projects-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "Tasks making projects stuck")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-u-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             `("n" "NEXT (active, grouped by parent, except scheduled for future)"
               ((my-org-agenda-next-tasks-agenda-command ""))
               ((org-agenda-write-buffer-name
                 "NEXT (active, grouped by parent, except scheduled for future)")
                (org-agenda-exporter-settings
                  my-org-agenda-export-options))
               "~/Downloads/agenda-n-export.pdf"))
(add-to-list 'org-agenda-custom-commands
             '("A" "Archivable tasks"
               ((my-org-agenda-archivable-tasks-agenda-command ""))
               ((org-tags-match-list-sublevels nil)
                (org-agenda-archives-mode nil))))
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
(c-setq org-agenda-custom-commands org-agenda-custom-commands)
(defvar my-org-agenda-combined-output-file
  "~/Downloads/agenda-export.pdf"
  "Output PDF of ‘my-org-agenda-write-combined’.")
(defun my-org-agenda--get-export-file (key)
  (nth 3 (alist-get key org-agenda-custom-commands nil nil #'string=)))
(defun my-org-agenda-write-combined ()
  "Combine several agenda views into one PDF suitable for printing"
  (interactive)
  (require 'call-log)
  ;; (org-store-agenda-views)
  (call-process "pdfnup" nil (get-buffer-create "*Async Shell Command*") nil
                "--nup" "2x2" "--no-landscape"
                "-o" (expand-file-name my-org-agenda-combined-output-file)
                (expand-file-name (my-org-agenda--get-export-file "n"))
                (expand-file-name (my-org-agenda--get-export-file "u"))
                (expand-file-name (my-org-agenda--get-export-file "U")))
  (clog/msg "Wrote %s" my-org-agenda-combined-output-file))

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
(org-defkey org-mode-map (kbd "C-c C-1") 'org-time-stamp-inactive)

;; TODO: re-enable once this can be ordered using use-package to run after
;; Org-mode is loaded.
;; ;; Pop up org-agenda-list a few times a day
;; (run-at-time "08:00" 21600 'org-agenda-list)

;; Save and backup all Org files a few times a day using external script.

;;;** Supersession

;;; Add option to merge current buffer and file on disk using emerge if both
;;; buffer and disk have changed.
(defun ediff-supersession-threat (unused-orig-fn &optional fn)
  "Wrap ‘ask-user-about-supersession-threat’ to provide the option to run \
‘ediff-current-buffer’ instead.

UNUSED-ORIG-FN is unused.  Uses argument FN from original function."
  (require 'call-log)
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
      (clog/msg
       "File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))
(advice-add 'ask-user-about-supersession-threat :around #'ediff-supersession-threat)

(defun ediff-supersession-help (unused-original-fn)
  "Add help for wrapped version of ‘ask-user-about-supersession-threat’.

UNUSED-ORIGINAL-FN is unused."
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
(advice-add 'ask-user-about-supersession-help :around #'ediff-supersession-help)


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
  :type 'file)
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

;; Display images inline, but not too wide by default.
(c-setq org-startup-with-inline-images t)
(defun org-resize-inline-images-hook (frame)
  "Hook to update Org-mode image width in resized Org-mode windows.

Iterates over all buffers in FRAME."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (org-resize-inline-images)))))
(defvar-local org-resize-inline-images--timer nil)
(defun org-redisplay-inline-images-in-buffer (buffer)
  "Redisplay inline images in Org-mode buffer BUFFER."
  (setq org-resize-inline-images--timer nil)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
        (org-redisplay-inline-images))))
(defun org-resize-inline-images ()
  "Update Org-mode image size in current buffer after window is resized."
  (when
      (and (eq major-mode 'org-mode)
           (not (= (window-pixel-width)
                   (window-pixel-width-before-size-change))))
    (when org-resize-inline-images--timer
        (cancel-timer org-resize-inline-images--timer))
    (setq org-resize-inline-images--timer
          (run-at-time
           1 nil #'org-redisplay-inline-images-in-buffer (current-buffer)))
    (setq-local org-image-actual-width (list (window-pixel-width)))))
(add-hook 'window-size-change-functions #'org-resize-inline-images-hook)
(add-hook 'org-mode-hook #'org-resize-inline-images)
(c-setq org-image-actual-width '(800))

;;;** Org-drill
(use-package org-drill
  :straight t
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
(use-package org-drill-table :straight t)

;;;** Org-pomodoro
(use-package org-pomodoro
  :straight (:host github :repo "marcinkoziej/org-pomodoro"
             :fork (:host nil :repo "git@github.com:telotortium/org-pomodoro"))
  :config
  ;; Complice.co Less Wrong study hall
  ;; (https://complice.co/room/lesswrong/interstitial). Reference:
  ;; https://www.lesswrong.com/posts/hyeDFbg8ahYAu4ZJu/#586GQr5xjWBzXWda6
  (c-setq org-pomodoro-length 32)
  (c-setq org-pomodoro-short-break-length 8)
  (c-setq org-pomodoro-long-break-length 30)
  (c-setq org-pomodoro-ticking-sound-p t)
  (c-setq org-pomodoro-ticking-sound-states '(:pomodoro))
  (require 's)

  (defun org-pomodoro-end-in (minutes)
    "Force the current Pomodoro to end in MINUTES minutes."
    (interactive "nMinutes: ")
    (setq my-org-pomodoro-current-task-reminder-next-time nil)
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
  (defun my-org-pomodoro-started-notify-hook ()
    (org-notify "Pomodoro started - snooze notifications in Hangouts Chat."))
  (defun my-org-pomodoro-finished-notify-hook ()
    (org-notify "Pomodoro phase finished"))
  (defun my-org-pomodoro-start-break ()
    "Start break - clock into task with ID my-org-pomodoro-break-id."
    (interactive)
    ;; Set org-clock-idle-time to nil to disable it during Pomodoro breaks -
    ;; sometimes Emacs will hang after the break.
    (setq my-org-pomodoro-clock-idle-time org-clock-idle-time)
    (setq org-clock-idle-time nil)
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
    (require 'call-log)
    (clog/msg "%s %s" my-org-pomodoro-break-id org-pomodoro-state)
    (when my-org-pomodoro-break-id
      (clog/msg "About to start clock")
      (my-org-pomodoro-start-break)))
  (defun my-org-pomodoro-break-finished-notify-hook ()
    (let ((msg "Pomodoro break finished -- get back to work!"))
      (if (fboundp 'terminal-notifier-notify)
          ;; Try to ensure timeout is very high by skipping org-notify.
          (terminal-notifier-notify "Org Pomodoro" msg 84000)
        (org-notify msg))))
  (defun my-org-pomodoro-short-break-finished-punch-in ()
    "Run bh/punch-in when Pomodoro short breaks end."
    (setq org-clock-idle-time my-org-pomodoro-clock-idle-time)
    (message-box "Break finished - please run bh/punch-in"))
  (defun my-org-pomodoro-long-break-finished-punch-out ()
    "Run bh/punch-out when Pomodoro long breaks end."
    (bh/punch-out))

  (defcustom my-org-pomodoro-alarm-gcal-calendar-id nil
    "The Google Calendar ID on which to create alarms."
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
  (defun my-org-pomodoro-time-today-set ()
    "Manually prompt for elapsed pomodoro time for today to set."
    (interactive)
    (require 'call-log)
    (let* ((input
            (read-from-minibuffer "Org Pomodoro Time Elapsed Today: ")))
      (clog/msg "Setting elapsed time to %s" input)
      (setq my-org-pomodoro-time-today-var
            (* 60 (org-duration-to-minutes input)))))
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
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let* ((search-for "Org Pomodoro - Count:")
               (match (search-forward search-for nil 'noerror)))
          (if match
            (let ((start-match (- (point) (length search-for))))
                (goto-char start-match)
                (delete-region start-match (point-max)))
            (progn
              (end-of-line)
              (newline)))
          (insert
           (format "Org Pomodoro - Count: %2d, Time: %s"
                   org-pomodoro-count
                   (org-timer-secs-to-hms
                           (round (my-org-pomodoro-time-today)))))
          (newline)
          ;; Add spaces to align with line above
          (insert "Try to get above                3:30:00")))))
  (add-hook 'org-agenda-finalize-hook 'my-org-agenda-pomodoro-info 'append)

  (defun my-org-pomodoro-today-tick-hook ())
  (defvar my-org-pomodoro-current-task-reminder-next-time nil)
  (defun my-org-pomodoro-tick-current-task-reminder ()
    "Prod me with reminders of my current task to stop me from being distracted."
    (when (or (null my-org-pomodoro-current-task-reminder-next-time)
              (> (float-time) my-org-pomodoro-current-task-reminder-next-time))
      (let* ((x (cl-floor
                 (float-time (time-subtract org-pomodoro-end-time (current-time)))
                 60))
             (quotient (car x))
             (remainder (car (cl-floor (cadr x)))))
        (when (and (eql org-pomodoro-state :pomodoro)
                   (not (null my-org-pomodoro-current-task-reminder-interval)))
          (cond
           ((> quotient 0)
            ;; Rate limit reminders in last minute to once every
            ;; ‘my-org-pomodoro-current-task-reminder-interval’ seconds.
            (setq my-org-pomodoro-current-task-reminder-next-time
                  (min (car (cl-floor (+ (float-time)
                                         my-org-pomodoro-current-task-reminder-interval)))
                       (float-time (time-subtract org-pomodoro-end-time 60))))
            (org-pomodoro-notify "Pomodoro in progress" org-clock-heading))
           (t
            ;; Rate limit reminders in last minute to once every 5 seconds.
            (setq my-org-pomodoro-current-task-reminder-next-time
                  (car (cl-floor (+ (float-time) 5))))
            (org-pomodoro-notify (format "Pomodoro in progress - %ds to break"
                                         remainder)
                                 org-clock-heading)))))))
  (defun my-org-pomodoro-finished-create-break-end-alarm ()
    (interactive)
    (when (and (or (eq org-pomodoro-state :short-break)
                   (eq org-pomodoro-state :long-break))
               ;; Current break has not ended yet.
               (> (float-time (time-subtract org-pomodoro-end-time (current-time)))
                  0)
               my-org-pomodoro-alarm-gcal-calendar-id)
      (my-org-pomodoro--create-alarm-event
       my-org-pomodoro-alarm-gcal-calendar-id
       org-pomodoro-end-time)))
  (defun my-org-pomodoro--create-alarm-event (calendar-id time)
    (call-process
     (expand-file-name "pomodoro_schedule_alarm.py"
                       user-emacs-directory)
     nil (get-buffer-create "*pomodoro_schedule_alarm.py*") nil
     "--calendar_id" calendar-id
     "--timestamp" (format-time-string "%FT%T%z" time)
     "--title" "org-pomodoro break end -- get back to work!"))

  (add-hook 'org-pomodoro-started-hook #'my-org-pomodoro-started-notify-hook)
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
  :straight t
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
  (let* ((old-tags (org-get-tags-string))
         (tags (if tags
                   (concat " " tags)
                 "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position)
       t)
      (replace-match tags)
      (org-set-tags tags))))
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
  (let* ((cur-list (org-get-tags nil t))
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
  :straight t
  :config
  (c-setq alert-default-style
        (cond ((executable-find "notify-send")
               'libnotify)
              ((eq system-type 'darwin)
               'notifier)
              (t 'message))))
(use-package org-gcal
  :straight (:host github :repo "kidd/org-gcal.el"
             :fork (:host nil :repo "git@github.com:telotortium/org-gcal.el"))
  :config
  (c-setq org-gcal-config-file (expand-file-name "org-gcal-config.el" user-emacs-directory))
  ;; Disable Auto Archive - my gcal.org_archive is so big that this majorly
  ;; slows down every fetch. Instead, I'll just archive old entries once a
  ;; month along with the rest of the entries to be archived.
  (c-setq org-gcal-auto-archive nil)
  (when (file-exists-p org-gcal-config-file)
    (load org-gcal-config-file)))

(defun my-org-gcal-schedule ()
  "\
Suggest a default schedule time for the event at point and create/update it
using ‘org-gcal-post-at-point’. Default suggestions (in the absence of existing
data in the entry):

- Calendar ID: first entry in ‘org-gcal-file-alist’
- Start time: tomorrow at 10 AM
- End time: start time plus effort. Prompt for effort if not already present.
"
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((elem (org-element-at-point))
           (tobj (org-element-property :scheduled elem))
           (duration (org-element-property :EFFORT elem))
           (calendar-id
            (org-entry-get (point) "calendar-id")))
      (unless calendar-id
        (setq calendar-id
              (read-from-minibuffer "Calendar ID: "
                                    (caar org-gcal-file-alist)))
        (org-entry-put (point) org-gcal-calendar-id-property calendar-id))
      ;; Set SCHEDULED time if not already present.
      (unless (plist-get (cadr tobj) :hour-start)
        (org-schedule nil "+1d 10:00")
        (org-schedule nil))
      (unless duration
        (org-set-effort))
      (setq elem (org-element-at-point))
      (setq tobj (org-element-property :scheduled elem)
            ;; By default, set duration to effort minus clocked time with
            ;; adjustments.
            duration
            (let ((min-duration 5)      ; Minimum event duration
                  (resolution 5))       ; Event resolution
              (org-duration-from-minutes
               (max
                 min-duration
                 ;; Round up to the nearest multiple of ‘resolution’ minutes.
                 (* resolution
                    (ceiling
                      (/ (- (org-duration-to-minutes (org-element-property
                                                         :EFFORT elem))
                            (org-clock-sum-current-item))
                         resolution)))))))
      (when (and (= (plist-get (cadr tobj) :hour-start)
                    (plist-get (cadr tobj) :hour-end))
                 (= (plist-get (cadr tobj) :minute-start)
                    (plist-get (cadr tobj) :minute-end)))
        (let* ((duration (read-from-minibuffer "Duration: " duration))
               (duration-minutes (org-duration-to-minutes duration))
               (duration-seconds (* 60 duration-minutes))
               (end-time (org-timestamp-from-time
                          (time-add (org-timestamp-to-time tobj)
                                    duration-seconds)
                          'with-time)))
          ;; Add SCHEDULED time with start and end times filled out.
          (org-add-planning-info
           'scheduled
           (concat
            (org-timestamp-format tobj "%Y-%m-%d %a %H:%M")
            "-"
            (org-timestamp-format end-time "%H:%M")))))
      ;; Finally, create/update event with information added to entry.
      (org-gcal-post-at-point 'skip-import))))

;;;** Org-gtasks
(use-package org-gtasks
  :ensure nil
  :load-path "~/.emacs.d/lisp/org-gtasks.git/")

;;;** norang configuration

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

(c-setq org-html-htmlize-output-type 'css)

;;; Recompute effort of a parent headline from the efforts of the children if
;;; they sum to a higher value.
(defun my-org-update-heading-effort-from-children (marker)
  "Compute the sum of efforts for each child of the heading at MARKER.

If the sum is greater than the current effort for this heading, offer to update
it.  This function is called recursively on each child, so the entire tree's
efforts may be updated by this function."
  (require 'call-log)                   ; For clog/msg
  (let*
      ((abort-at-marker)
       (ret
        (catch
            'break
          (org-with-point-at marker
            (clog/msg "At %S (%s)" (point-marker) (org-get-heading))
            (org-narrow-to-subtree)
            (outline-show-all)
            (let*
                ((current-effort
                  (org-duration-to-minutes
                   (or (org-entry-get marker org-effort-property) 0)))
                 (children-effort 0))
              (save-excursion
                (save-restriction
                  (when (org-goto-first-child)
                    ;; Use while loop with empty body to simulate a C do-while
                    ;; loop - in other words, we test at the end of the loop
                    ;; "body" whether a next sibling exists.
                    (while
                        (let ((x (my-org-update-heading-effort-from-children (point-marker))))
                         (clog/msg "x = %S" x)
                         (setq children-effort (+ children-effort (nth 0 x)))
                         (org-get-next-sibling))))))
              (let ((children-effort-duration
                     (org-duration-from-minutes children-effort)))
                (when (< current-effort children-effort)
                  (pcase (read-char-choice
                          (format
                           "Update effort in \"%s\" to children's sum (%s)? (y,n,j) "
                           (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)
                           children-effort-duration)
                          '(?y ?n ?j))
                    (?n nil)
                    (?y
                     (org-entry-put
                      marker org-effort-property children-effort-duration)
                     (setq current-effort children-effort))
                    (?j
                     (setq abort-at-marker marker)
                     (throw 'break 'abort-at-marker)))))
              (list current-effort (point-max-marker)))))))
    (pcase ret
      ('abort-at-marker
       (clog/msg "%S" abort-at-marker)
       (pop-to-buffer-same-window (marker-buffer abort-at-marker))
       (set-buffer (marker-buffer abort-at-marker))
       (goto-char (marker-position abort-at-marker))
       'abort)
      ('abort 'abort)
      (_ ret))))
(defun my-org-effort-from-children-hook ()
  "Update effort of a heading from its children before clocking in."
  (pcase (my-org-update-heading-effort-from-children (point-marker))
    ('abort 'abort)
    (_ nil)))
(add-hook 'org-clock-in-prepare-hook 'my-org-effort-from-children-hook)

(defun my-org-update-heading-effort-from-children-all ()
  "Run over all projects, updating their efforts from their children.

Pressing ‘j’ will abort the run, leaving the point at the heading we were at
when ‘j’ was pressed."
  (interactive)
  (require 'call-log)
  (org-map-entries
   (lambda ()
     (display-buffer (current-buffer) '(display-buffer-same-window))
     (recenter nil)
     (pcase (my-org-effort-from-children-hook)
       ('abort
        (clog/msg "'abort")
        (setq org-map-continue-from (point))
        (let ((debug-on-quit nil))
          (signal 'quit nil)))
       (x x)))
   nil 'agenda #'bh/skip-tasks)
  (clog/msg "Updating efforts complete."))

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
           (bh/is-project-with-active-tasks-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
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
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (and is-a-task (not (bh/is-task-p))))))

(defun bh/is-project-with-active-tasks-p ()
  "Any task with a non-done todo keyword subtask."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (and is-a-task
           (let ((has-non-done-subtask))
             (save-excursion
               (when (org-goto-first-child)
                 (while (and (not has-non-done-subtask)
                             (org-goto-sibling))
                   (when (member (org-get-todo-state) org-not-done-keywords)
                     (setq has-non-done-subtask t)))))
             has-non-done-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
          (top-level (org-current-level)))
      (save-excursion
        (while (and
                ;; is-a-task never changes - use it for early exit.
                is-a-task
                (not has-subtask)
                (outline-next-heading)
                (> (org-current-level) top-level))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/skip-subprojects ()
  "Skip subprojects (including both projects and leaf tasks)."
  (save-restriction
    (widen)
    (cond
     ((not (bh/is-subproject-p)) nil)
     (t
      (let ((next-headline
             (save-excursion (or (outline-next-heading) (point-max)))))
        next-headline)))))

(defun bh/skip-non-tasks ()
  "Show leaf tasks.  Skip projects (including subprojects)."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (cond
       ((and is-a-task (bh/is-task-p)) nil)
       (t
        (let ((next-headline
               (save-excursion (or (outline-next-heading) (point-max)))))
          next-headline))))))

(defun bh/skip-tasks ()
  "Show projects (including subprojects).  Skip leaf tasks."
  (save-restriction
    (widen)
    (let ((is-a-task
           (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (cond
       ((and is-a-task (bh/is-project-p)) nil)
       (t
        (let ((next-headline
               (save-excursion (or (outline-next-heading) (point-max)))))
          next-headline))))))


(defun bh/skip-non-subprojects ()
  "Show subprojects (including both projects and leaf tasks)."
  (save-restriction
    (widen)
    (cond
     ((bh/is-subproject-p) nil)
     (t
      (let ((next-headline
             (save-excursion (or (outline-next-heading) (point-max)))))
        next-headline)))))

(defun my-org-super-agenda-group-by-project-or-task-group (item)
  "Output the name of the parent headline of the current headline.

In order to ensure that tasks that are part of projects are sorted before loose
tasks (tasks not part of projects), the name of the parent headline is prefixed
with “P: ” if it contains a TODO keyword and “TG: ” (for “task group”)
otherwise."
  (org-super-agenda--when-with-marker-buffer
    (org-super-agenda--get-marker item)
    (let* ((parent-title)
           (parent-has-todo)
           (parent-outline-path))
      (save-excursion
        (when (org-up-heading-safe)
          (setq parent-title (org-get-heading 'notags 'notodo))
          (setq parent-outline-path
                (let ((p (org-get-outline-path)))
                  (if p
                      (format " | %s" (string-join p "/"))
                    "")))
          (setq parent-has-todo
                (member (nth 2 (org-heading-components)) org-todo-keywords-1))))
      (when parent-title
        (if parent-has-todo
            (format "P: %s%s" parent-title parent-outline-path)
          (format "TG: %s%s" parent-title parent-outline-path))))))
;;;;


(defun my-org-archive-dwim (&optional find-done)
  "Tag heading with ARCHIVE tag if it's not the top-level of a project.
Otherwise, archive the subtree to a file.

FIND-DONE has the same meaning "
  (interactive "P")
  (if (bh/is-subproject-p)
      (org-toggle-archive-tag find-done)
    (org-archive-subtree find-done)))
(c-setq org-archive-default-command #'my-org-archive-dwim)
(org-defkey org-agenda-mode-map "$"        'org-agenda-archive-default)

;;; Find all inactive timestamps in tree, buffer, or all org buffers
;;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-07/msg01228.html
(defun org-find-timestamps ()
  "Find inactive timestamps within a date-range and maybe sort them.

This function can help to bring the notes, that you take within `org-mode',
into a chronological order, even if they are scattered among many different
nodes.  The result is somewhat like a diary, listing your notes for each
successive day.  Please be aware however: This intended usage requires, that
you routinely insert inactive timestamps into the notes that you write.

`org-find-timstamps' works by creating a regular expression to match a given
range of dates, doing a search for it and displaying the results either as a
sparse tree or with the help of occur.  The original buffer is not modified.
"
  (interactive)
  (require 'call-log)
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
        (clog/msg (concat "Sparse tree with matches " pretty-dates))
      ;; switch to occur-buffer and modify it
      (if (not (get-buffer occur-buffer-name))
          (clog/msg (concat "Did not find any matches " pretty-dates))
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
                (clog/msg (concat "occur-buffer with matches " pretty-dates "(`C-h m' for help)")))

            (setq inhibit-read-only original-inhibit-read-only))))



      ;; switch to occur-buffer
      (if (get-buffer occur-buffer-name)
          (switch-to-buffer occur-buffer-name)))))

;;; Don't insert hard spaces to indent text with heading in Org mode
(c-setq org-adapt-indentation nil)
(c-setq org-startup-indented nil)

(require 'org-inlinetask)

(use-package org-randomnote
  :straight t
  :bind ("C-c r" . org-randomnote)
  :config
  (c-setq org-randomnote-candidates
          (remove-if
           (lambda (x)
             (string-match-p "/gcal.org$" x))
           (org-agenda-files))))

(c-setq org-agenda-dim-blocked-tasks t)
(c-setq org-enforce-todo-dependencies t)
(c-setq org-enforce-todo-checkbox-dependencies t)

(c-setq org-log-done (quote time))
(c-setq org-log-redeadline (quote time))
(c-setq org-log-reschedule (quote time))

;;; Week in review (https://emacs.stackexchange.com/a/7864)
(defcustom org-timeline-files nil
  "The files to be included in `org-timeline-all-files'.

Follows the same rules as `org-agenda-files'"
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
  :straight t
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
(use-package org-ql
  :straight t)

;; Use sticky agenda's so they persist
(c-setq org-agenda-sticky t)

(c-setq org-list-allow-alphabetical t)

;;;* Autorevert
(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode
  :config (global-auto-revert-mode t)
  ;; Throwing a lot of errors in Emacs 27.0, at least.
  :disabled t)

;;;* Backup files
(add-to-list 'backup-directory-alist
  '(".*" . "~/.cache/emacs-backup/")
  'append)

;;;* Flyspell
(use-package flyspell
  :straight t
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode 'append)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode 'append))

;;;* Remove trailing whitespace intelligently
(use-package ws-butler
  :straight t
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
  "Fix Evil shiftwidth based on Python indent."
  (python-indent-guess-indent-offset)
  (setq-local evil-shift-width python-indent-offset))
(add-hook 'python-mode-hook 'evil-shift-width-from-guessed-python-indent-offset)


;;; Fix newline behavior to use M-j by default.
(defun my-coding-config ()
  "Set RET key to use the behavior of M-j by default.

See http://stackoverflow.com/a/9060267."
  (let ((ret-binding (key-binding (kbd "M-j"))))
    (cond
     ;; Work around Emacs 27 binding, which isn't working for me in Elisp mode.
     ((eq ret-binding #'default-indent-new-line)
      (local-set-key (kbd "RET") #'indent-new-comment-line))
     (t
      (local-set-key (kbd "RET") (key-binding (kbd "M-j"))))))
  (local-set-key (kbd "<S-return>") 'newline))
(add-hook 'prog-mode-hook 'my-coding-config)


;; Don't use Evil for image-mode.
(add-to-list 'evil-emacs-state-modes 'image-mode)
(add-to-list 'evil-emacs-state-modes 'Custom-mode)
(delete 'git-commit-mode evil-emacs-state-modes)


;;; Split window vertically if possible -- this will split vertically if the
;;; window is wide enough for the contents of both buffers.
(c-setq split-height-threshold nil)

;;;* Magit
(use-package magit
  :straight t
  :init
  (c-setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (c-setq vc-handled-backends (delq 'Git vc-handled-backends)))

;;;* git-auto-commit-mode
(use-package git-auto-commit-mode
  :straight (:host github :repo "ryuslash/git-auto-commit-mode"
             :fork (:repo "git@github.com:telotortium/git-auto-commit-mode"
                    :host nil :branch "gac-merge")))

;;;* https://codearsonist.com/reading-for-programmers
(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))
(use-package interleave :straight t)
(use-package org-ref
  :straight t
  :config
  (c-setq org-ref-notes-directory "~/Documents/org/home-org")
  (c-setq org-ref-bibliography-notes "~/Documents/org/home-org/index.org")
  (c-setq org-ref-default-bibliography '("~/Documents/org/home-org/index.bib"))
  (c-setq org-ref-pdf-directory "~/Documents/org/home-org/lib/"))
(use-package helm-bibtex
  :straight t
  :config
  (c-setq helm-bibtex-bibliography "~/Documents/org/home-org/index.bib") ;; where your references are stored
  (c-setq helm-bibtex-library-path "~/Documents/org/home-org/lib/") ;; where your pdfs etc are stored
  (c-setq helm-bibtex-notes-path "~/Documents/org/home-org/index.org") ;; where your notes are stored
  (c-setq bibtex-completion-bibliography "~/Documents/org/home-org/index.bib") ;; writing completion
  (c-setq bibtex-completion-notes-path "~/Documents/org/home-org/index.org"))

;;;* Preserve scratch file across sessions
(use-package persistent-scratch
  :straight t
  :config
  (persistent-scratch-setup-default))

;;;* which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

;;;* browse-kill-ring
(use-package browse-kill-ring
  :straight t
  :config
  (browse-kill-ring-default-keybindings))

;;;* Folding
(defun my-fold-setup ()
  "Set up folding in current buffer."
  (interactive)
  (require 'call-log)
  (hs-minor-mode)
  (condition-case nil
    (hs-hide-all)
    (scan-error
     (clog/msg "scan-error: not folding")
     nil))
  (diminish 'hs-minor-mode))
(defun my-fold-setup-hook ()
  "Set up folding when buffer becomes visible, and then remove this hook."
  ;; Check whether buffer is visible - see
  ;; https://emacs.stackexchange.com/a/2979/17182
  (when (get-buffer-window (current-buffer) 'visible)
    (my-fold-setup)
    ;; Remove hook after running fold the first time.
    (remove-hook 'post-command-hook #'my-fold-setup-hook 'local)))
(defun my-fold-major-mode-hook ()
  "Add hook to set up folding when buffer becomes visible."
  (add-hook 'post-command-hook #'my-fold-setup-hook nil 'local))
(add-hook 'prog-mode-hook #'my-fold-major-mode-hook)

;;;* so-long - increase performance in buffers with long lines
(use-package so-long
  :ensure nil
  :load-path "~/.emacs.d/lisp"
  :config
  (global-so-long-mode 1))

;;;* Automatic indentation detection
(use-package dtrt-indent
  :straight t
  :custom
  (dtrt-indent-mode t))

;; ;;;* Faster GDB-MI interface
;; (use-package gdb-mi :quelpa (gdb-mi :fetcher git
;;                                     :url "https://github.com/weirdNox/emacs-gdb.git"
;;                                     :files ("*.el" "*.c" "*.h" "Makefile"))
;;   :init
;;   (fmakunbound 'gdb)
;;   (fmakunbound 'gdb-enable-debug))


;;;* Org-roam
(use-package org-roam
  :straight t
  :hook (after-init . org-roam-mode)
  :config
  (require 'org-roam-protocol)
  :custom
  (org-roam-directory "~/Documents/org/home-org/roam")
  (org-roam-link-title-format "§%s")
  (org-roam-completion-system 'ivy)
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n g" . org-roam-show-graph)
  (:map org-mode-map
        ("C-c n i" . org-roam-insert)))
(use-package company-org-roam
  :straight t
  :config
  (push 'company-org-roam company-backends))

;;;* Useful packages suggested by
;;;* https://blog.jethro.dev/posts/zettelkasten_with_org/.
(use-package org-download
  :straight t
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-backend "curl \"%s\" -o \"%s\""))
(use-package org-cliplink :straight t)

;;; Update environment - https://emacs.stackexchange.com/a/6107
(defun my-update-env (fn &optional unset)
  "Update environment variables from file FN containing variables.

Format is \"VAR=VAL\", NUL-separated. If UNSET is non-nil, the variables are
instead unset."
  (let ((str
         (with-temp-buffer
           (insert-file-contents fn)
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (if unset
            (setenv var nil)
          (setenv var value)))
      (setq lst (cdr lst)))))
(defun my-update-env-unset (fn)
  "Unset environment variables from file FN containing variables.

Format is \"VAR=VAL\", NUL-separated.  Everything starting at \"=\" is
ignored."
  (my-update-env fn 'unset))

;;;* Subfiles
(use-package whitespace-conf
  :ensure nil
  :load-path "~/.emacs.d/lisp")
(use-package org-drill-cloze-enhancement
  :ensure nil
  :load-path "~/.emacs.d/lisp")

;;;* Local configuration

;;; Allow users to provide an optional "init-local" containing personal settings
(use-package init-local
  :ensure nil
  :load-path "~/.emacs.d/lisp")

;;; Local Variables:
;;; outline-regexp: ";;;\\*+\\|\\`"
;;; End:

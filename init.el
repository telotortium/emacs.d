;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

; Load all the packages that are REQUIRE'd below
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


;; Evil keybindings
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

; Disable all the C-<number> keys, and give C-^ and C-6 the same behavior as
; they have in Vim.
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(define-key evil-normal-state-map (kbd "C-^") 'switch-to-previous-buffer)
(define-key evil-normal-state-map (kbd "C-6") 'switch-to-previous-buffer)
(dolist (key (mapcar 'kbd
                     '("C-1" "C-2" "C-3" "C-4" "C-5"
                       "C-7" "C-8" "C-9" "C-0")))
  (define-key evil-normal-state-map key (lambda () (interactive))))

; Kill current buffer but keep its frame around
(define-key evil-normal-state-map "Q" 'kill-this-buffer)

; Space to get to ex mode
(define-key evil-motion-state-map " " nil)
(define-key evil-normal-state-map " " 'evil-ex)
(define-key evil-visual-state-map " " 'evil-ex)

;; Shortcut to M-x
(evil-ex-define-cmd     "mx" 'execute-extended-command)
(define-key evil-ex-map "mx" 'execute-extended-command)

; RET to break line - undo in one step
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-normal-state-map (kbd "RET")
  (lambda ()
    (interactive)
    (progn
      (evil-start-undo-step)
      (delete-horizontal-space)
      (execute-kbd-macro [?i return]))))
(define-key evil-visual-state-map (kbd "RET")
  (lambda ()
    (interactive)
    (execute-kbd-macro [?c return])))

; C-c as general purpose escape key sequence (from EmacsWiki Evil page).
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return
   ;; [escape] so as Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; Don't override C-c for Emacs Evil state.
   ((evil-emacs-state-p) (kbd "C-c"))
   ;; This is the best way I could infer for now to have C-c work during
   ;; evil-read-key.  Note: As long as I return [escape] in normal-state, I
   ;; don't need this.  ((eq overriding-terminal-local-map evil-read-key-map)
   ;; (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
; Works around the fact that Evil uses read-event directly when in operator
; state, which doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
; Not sure what behavior this changes, but might as well set it, seeing the
; Elisp manual's documentation of it.
(set-quit-char "C-c")


;; Make default encoding UTF-8 everywhere
(setq current-language-environment "UTF-8")
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
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; Highlight too-long columns
(dolist (hook '(c-mode-hook c++-mode-hook python-mode-hook))
  (add-hook hook '(lambda () (font-lock-set-up-width-warning 80))))

;; Disable scrollbars
(scroll-bar-mode -1)

;; Line and column numbers
(require 'linum)
(require 'linum-relative)
(line-number-mode 1)
(column-number-mode 1)
(linum-relative 1)
(global-linum-mode)

;; auto-complete
(setq dabbrev-case-fold-search t)
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-dabbrev)
(add-to-list 'ac-sources 'ac-source-dabbrev)
;
(setq ac-auto-show-menu 0.4)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(setq ack-and-a-half-arguments '("--nopager"))

;; Syntax highlighting for Vimscript files
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

(require 'markdown-mode)
(setq markdown-command "pandoc -f markdown -t html --toc -s --mathjax")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Give buffers editing files with the same basename more distinctive names
;; based on directory.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Save mini-buffer history
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring compile-history))
(setq savehist-file
      (expand-file-name (concat (file-name-as-directory "tmp") "savehist")
			user-emacs-directory))
(setq history-length 5000)
(savehist-mode 1)

;; Disable tool bar
(message "turn off tool bar")
(tool-bar-mode -1)

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

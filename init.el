; -*- lexical-binding: t -*-
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

; Load all the packages that are REQUIRE'd below
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") 'append)

(require 'cl-lib)
(defun packages-install (packages)
  "Given a list of packages, this will install them from the standard locations."
  (let ((to-install (cl-remove-if 'package-installed-p packages)))
    (when to-install
      (package-refresh-contents)
      (dolist (it to-install)
          (package-install it)
      (delete-other-windows)))))

(packages-install '(
                    ac-helm
                    ac-slime
                    ack-and-a-half
                    auto-complete
                    evil
                    evil-leader
                    evil-numbers
                    evil-surround
                    evil-visualstar
                    helm
                    linum-off
                    linum-relative
                    lua-mode
                    org-plus-contrib
                    rainbow-delimiters
                    rust-mode
                    vimrc-mode
                    weechat
                    ws-butler
                    ))

;;; ---------------------------------------------------------------------------
;;;  Helm configuration
;;; ---------------------------------------------------------------------------
(require 'helm)
(require 'helm-config)

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

(helm-autoresize-mode t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;;; ---------------------------------------------------------------------------
;;;  Helm configuration
;;; ---------------------------------------------------------------------------
;; Space as leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(custom-set-variables
 '(evil-want-C-u-scroll t))
(require 'evil)
(evil-mode 1)

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

; Kill current buffer but keep its frame around
(define-key evil-normal-state-map "Q" 'kill-this-buffer)

(evil-leader/set-key
  ";" 'evil-ex
  ":" 'evil-ex)

;; Shortcut to M-x
(evil-ex-define-cmd     "mx" 'helm-M-x)
(define-key evil-ex-map "mx" 'helm-M-x)

; C-c as general purpose escape key sequence (from EmacsWiki Evil page).
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return
   ;; [escape] so as Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; Otherwise keep at default.
   (t (kbd "C-c"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
; Works around the fact that Evil uses read-event directly when in operator
; state, which doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
; Only changes anything in terminal Emacs. Rebind it to the ASCII value of C-c.
(set-quit-char ?\C-c)

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

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-visualstar)
(global-evil-visualstar-mode)

(require 'evil-numbers)
(global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-c _") 'evil-numbers/dec-at-pt)

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
  (auto-fill-mode t))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'prog-mode-wrap-hook)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Line and column numbers
(require 'linum)
(require 'linum-relative)
(line-number-mode 1)
(column-number-mode 1)
(linum-relative 1)
(global-linum-mode)
(defun no-linum () (linum-mode -1))

;; Disable linum-mode for large files
(require 'linum-off)

;; auto-complete
(custom-set-variables
 '(dabbrev-case-fold-search t))
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-dabbrev)
(add-to-list 'ac-sources 'ac-source-dabbrev)
;
(custom-set-variables
 '(ac-auto-show-menu 0.4)
 '(ac-use-menu-map t))
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(custom-set-variables
 '(ack-and-a-half-arguments (quote ("--nopager"))))

;; Syntax highlighting for Vimscript files
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

(require 'markdown-mode)
(custom-set-variables
 '(markdown-command "pandoc -f markdown -t html --toc -s --mathjax"))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Make `<tab>` only call `indent-relative` in Evil insert mode, not cycle.
(add-hook 'markdown-mode-hook
          (lambda ()
            (if (and
                 (featurep 'auto-complete-config)
                 global-auto-complete-mode)
                (auto-complete-mode t))
            (define-key markdown-mode-map (kbd "<tab>")
              (lambda (&rest args)
                (interactive "P")
                (cond ((and (featurep 'evil) (evil-insert-state-p))
                       (apply 'indent-relative args))
                      (t (apply 'markdown-cycle args)))))))

;; Give buffers editing files with the same basename more distinctive names
;; based on directory.
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Save mini-buffer history
(custom-set-variables
 '(savehist-additional-variables
      (quote (kill-ring search-ring regexp-search-ring compile-history)))
 '(savehist-file
      (expand-file-name (concat (file-name-as-directory "cache") "savehist")
                        user-emacs-directory))
 '(history-length 5000))
(savehist-mode 1)

;; Disable tool bar
(message "turn off tool bar")
(tool-bar-mode -1)

;; Rust mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.javascript\\'" . js-mode))

;; Multi Web Mode - automatically switch to right major mode in HTML files
(require 'multi-web-mode)
(custom-set-variables
 '(mweb--major-mode 'html-mode)
 '(mweb-tags (quote ((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                     (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                     (css-mode "<style +type=\"text/css\"[^>]*>" "</style>"))))
 '(mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5")))
(multi-web-global-mode 1)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(show-paren-mode 1)

;; Go mode - use 4-space tabs since gofmt formats with tabs by default
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local whitespace-line-column 99)
            (setq-local tab-width 4)
            (setq-local tab-stop-list (number-sequence 4 200 4))))

;;; Don't put yanks into X clipboard buffer by default
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

;; Paredit mode
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(defun activate-paredit-mode ()
  (interactive)
  (enable-paredit-mode)
  (evil-paredit-mode 1))
(add-hook 'emacs-lisp-mode-hook       #'activate-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'activate-paredit-mode)
(add-hook 'ielm-mode-hook             #'activate-paredit-mode)
(add-hook 'lisp-mode-hook             #'activate-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'activate-paredit-mode)
(add-hook 'scheme-mode-hook           #'activate-paredit-mode)

;; Slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook #'activate-paredit-mode)
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(slime-setup '(slime-fancy slime-banner))

;; Org mode
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(custom-set-variables
 '(org-agenda-files (expand-file-name "agenda_files" user-emacs-directory))
 '(org-agenda-span 7)
 '(org-agenda-start-on-weekday nil)
 '(org-capture-templates
    '(("t" "Tasks" entry (file+headline (concat org-directory "/todo.org")
                                          "Refile")
       "* TODO %?\n  %u")
      ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
       "* %u %?")))
 '(org-refile-use-outline-path t)
 '(org-alphabetical-lists t)
 '(org-src-fontify-natively t)
 '(org-pretty-entities t)
 '(org-use-sub-superscripts '{})

 ;; Todo settings
 '(org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
 '(org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
           ("NEXT" :foreground "blue" :weight bold)
           ("DONE" :foreground "forest green" :weight bold)
           ("WAITING" :foreground "orange" :weight bold)
           ("HOLD" :foreground "magenta" :weight bold)
           ("CANCELLED" :foreground "forest green" :weight bold)
           ("MEETING" :foreground "forest green" :weight bold)
           ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("HOLD" ("WAITING") ("HOLD" . t))
           (done ("WAITING") ("HOLD"))
           ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
           ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
           ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

 ;; Don't show tasks in agenda that are done
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t)

 ;; Skip tasks in the global TODO list that are done or scheduled, because
 ;; either of these means the tasks has been considered. Tasks marked with a
 ;; deadline still need to be scheduled before I've truly considered them, so
 ;; leave them in.
 '(org-agenda-todo-ignore-scheduled 'future)

 '(org-agenda-custom-commands
   (quote (("n" "Agenda and all TODOs" ((agenda "") (alltodo)))
           ("u" "Unscheduled TODOs" todo ""
            ((org-agenda-skip-function
              '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-columns-default-format "%60ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %10CLOCKSUM_T")
 '(org-global-properties
   (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
           ("SYTLE_ALL" . "habit")))))

(require 'org)
(require 'org-agenda)

; No one needs both `h` and `H` for holidays -- online help in org-agenda.
(org-defkey org-agenda-mode-map "h"
            (lambda ()
              (interactive)
              (info "(org) Agenda Commands")))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

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
(custom-set-variables '(org-clock-idle-time 15))
(let ((xprintidle (executable-find "xprintidle")))
  (when (and (eq system-type 'gnu/linux) xprintidle)
    (custom-set-variables `(org-clock-x11idle-program-name ,xprintidle))))

;;; Ask for effort estimate when clocking in.
;;; http://orgmode.org/worg/org-hacks.html#sec-1-9-10
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)
(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))


(global-auto-revert-mode t)

(add-hook 'text-mode-hook 'flyspell-mode 'append)
(add-hook 'prog-mode-hook 'flyspell-prog-mode 'append)

;;; Remove trailing whitespace intelligently
(require 'ws-butler)
(add-hook 'text-mode-hook (lambda () (ws-butler-mode 1)))
(add-hook 'prog-mode-hook (lambda () (ws-butler-mode 1)))


;;; Weechat
(require 'weechat)
(require 'weechat-notifications)
;; Enable visual-line-mode and disable linum-mode.
(add-hook 'weechat-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'weechat-mode-hook (lambda () (linum-mode -1)))

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
;; Subfiles
;;----------------------------------------------------------------------------
(require 'whitespace-conf nil t)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

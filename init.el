; -*- lexical-binding: t; -*-
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

; Load all the packages that are REQUIRE'd below
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
                    ack-and-a-half
                    company
                    company-go
                    evil
                    evil-leader
                    evil-numbers
                    evil-paredit
                    evil-surround
                    evil-visualstar
                    geiser
                    helm
                    helm-company
                    htmlize
                    linum-off
                    linum-relative
                    lua-mode
                    magit
                    markdown-mode
                    multi-web-mode
                    org-plus-contrib
                    package-utils
                    paredit
                    rainbow-delimiters
                    restart-emacs
                    rust-mode
                    slime
                    vimrc-mode
                    weechat
                    ws-butler
                    yasnippet
                    ))

;;; Convenience commands to upgrade packages.
(require 'package-utils)

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

;; Scroll with C-u and access universal-argument with <Leader>-u.
(custom-set-variables
 '(evil-want-C-u-scroll t))
(evil-leader/set-key "u" 'universal-argument)

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
(linum-on)
(linum-relative-on)
(defun no-linum () (linum-mode -1))

;; Disable linum-mode for large files
(require 'linum-off)
(add-to-list 'linum-disabled-modes-list 'markdown-mode)

(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(custom-set-variables
 '(ack-and-a-half-arguments (quote ("--nopager"))))

;;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Syntax highlighting for Vimscript files
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;; Autoload markdown-mode, as recommended in the
;; [documentation](http://jblevins.org/projects/markdown-mode/).
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(custom-set-variables
 '(markdown-command "pandoc -f markdown -t html --toc -s --mathjax"))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Make `<tab>` only call `indent-relative` in Evil insert mode, not cycle.
(add-hook 'markdown-mode-hook
          (lambda ()
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

;;; Enable escaping from yasnippet snippets
(require 'yasnippet)
(yas-global-mode 1)

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
(add-hook 'slime-repl-mode-hook #'activate-paredit-mode)
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
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
       "* TODO %?\n  %u" :clock-in t :clock-resume t)
      ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
       "* %u %?" :clock-in t :clock-resume t)))
 '(org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
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
             (org-agenda-overriding-header "Unscheduled TODO entries: ")))
           ("Q" . "Custom queries")
           ("Q/" "Archive occur"
            ;; Dynamically bind `org-agenda-text-search-extra-files' with the
            ;; symbol `agenda-archive' prepended to search archive files when
            ;; calling `org-occur-in-agenda-files'.
            ;;
            ;; TODO: Override both this and "/" to use `helm-multi-occur'
            ;; instead of the Emacs built-in multi-occur, which provides
            ;; incremental search.
            (lambda (unused)
              (let* ((tmp (if (boundp org-agenda-text-search-extra-files)
                              org-agenda-text-search-extra-files
                            '()))
                     (org-agenda-text-search-extra-files
                      (cons 'agenda-archives tmp)))
                (call-interactively 'org-occur-in-agenda-files)))
            ""))))

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

; Pop up org-agenda-list a few times a day
(run-at-time "08:00" 21600 'org-agenda-list)

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

;;; Enable notifications on OS X using the terminal-notifier program.
(defvar terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier.")
(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))
(when terminal-notifier-command
  (setq org-show-notification-handler
        (lambda (message) (terminal-notifier-notify "Org Mode" message))))

;;; Ask for effort estimate when clocking in.
;;; http://orgmode.org/worg/org-hacks.html#sec-1-9-10
(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)
(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (or (memq 'org-capture-mode minor-mode-list)
              (org-entry-get (point) "Effort"))
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

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
(require 'org-drill)
(setq org-drill-scope 'directory)

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
;;;;

(global-auto-revert-mode t)

(add-hook 'text-mode-hook 'flyspell-mode 'append)
(add-hook 'prog-mode-hook 'flyspell-prog-mode 'append)

;;; Remove trailing whitespace intelligently
(require 'ws-butler)
(add-hook 'text-mode-hook (lambda () (ws-butler-mode 1)))
(add-hook 'prog-mode-hook (lambda () (ws-butler-mode 1)))

;;; Miscellaneous
(custom-set-variables
 '(bookmark-default-file
   (expand-file-name (concat (file-name-as-directory "cache") "bookmarks")
                     user-emacs-directory)))

;;; Weechat
(require 'weechat)
(when (featurep 'dbusbind) (require 'weechat-notifications))
;; Enable visual-line-mode and disable linum-mode.
(add-hook 'weechat-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'weechat-mode-hook (lambda () (linum-mode -1)))

;;; Enable commands disabled by default
(put 'narrow-to-region 'disabled nil)

;;; Python
;; Fix Evil shiftwidth for Python indent
(add-hook 'python-mode-hook
  (function (lambda ()
          (setq evil-shift-width python-indent-offset))))


;;; Fix newline behavior to use M-j by default.
;;; See http://stackoverflow.com/a/9060267.
(defun my-coding-config ()
  (local-set-key (kbd "RET") (key-binding (kbd "M-j")))
  (local-set-key (kbd "<S-return>") 'newline))
(add-hook 'prog-mode-hook 'my-coding-config)


;; Don't use Evil for image-mode.
(add-to-list 'evil-emacs-state-modes 'image-mode)

;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;;; Disable startup screen
(setq inhibit-startup-message t)

;;; Command to restart emacs from within emacs
(require 'restart-emacs)

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

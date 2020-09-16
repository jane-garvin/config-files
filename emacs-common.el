;; My emacs configuration across all machines. A lot of it is adapted from
;; Howard Abrams's excellent setup: https://github.com/howardabrams/dot-files

;;;; ----- memory -----

;; Give emacs the megabytes it's always wanted
(setq gc-cons-threshold (* 100 1024 1024))

;;;; ----- server -----

;; start emacs-server if not already running
(if (not (boundp 'server-process))
    (server-start))

;;;; ----- packages -----

;; Don't load an old emacs file if there's a newer one.
(setq-default load-prefer-newer t)

;; grab emacs packages from repositories
(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://melpa.org/packages/")))
(setq-default package-enable-at-startup t)
(package-initialize)

;; Use use-package to automatically install certain packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Install packages if necessary
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically refresh package contents on a regular basis
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;; ----- directories and buffers -----

;; go home!
(setq default-directory (concat (getenv "HOME") "/"))

;; auto-revert in dired
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Dired-du-mode is buggy and hangs at 50% even on an empty directory.
;; Try it again later.
;;(use-package dired-du
;;  :init
;;  ;; Use human-readable size formats.
;;  (setq dired-du-size-format t))

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; put backup files in a separate directory
(setq my-backup-directory (expand-file-name (concat user-emacs-directory "backups")))
;; Create backup directory if necessary
(when (not (file-exists-p my-backup-directory))
      (message "Making directory %s" my-backup-directory)
      (make-directory my-backup-directory))
(setq-default backup-directory-alist `(("." . ,my-backup-directory)))
(setq-default vc-make-backup-files t)

;; I save about every ten seconds, and I revert a file almost never. Set it to
;; automatically save everything.
(auto-save-visited-mode)

;;;; ----- keys and shortcuts -----

;; listen to modified arrows from terminal
(define-key function-key-map "\e[1;2A" [S-up])
(define-key function-key-map "\e[1;2B" [S-down])
(define-key function-key-map "\e[1;2C" [S-right])
(define-key function-key-map "\e[1;2D" [S-left])
(define-key function-key-map "\e[5A" [C-up])
(define-key function-key-map "\e[5B" [C-down])
(define-key function-key-map "\e[5C" [C-right])
(define-key function-key-map "\e[5D" [C-left])
(define-key function-key-map "\e[1;5A" [C-up])
(define-key function-key-map "\e[1;5B" [C-down])
(define-key function-key-map "\e[1;5C" [C-right])
(define-key function-key-map "\e[1;5D" [C-left])
(define-key function-key-map "\e[1;6A" [S-C-up])
(define-key function-key-map "\e[1;6B" [S-C-down])
(define-key function-key-map "\e[1;6C" [S-C-right])
(define-key function-key-map "\e[1;6D" [S-C-left])

;; I want C-x k to kill the current buffer without asking
(global-set-key "\C-xk" 'kill-this-buffer)
;; I use bury-buffer a lot; add a shortcut
(global-set-key "\C-xy" 'bury-buffer)

;; use C-? as the help prefix key as well as C-h
(global-set-key (kbd "C-?") (lookup-key global-map (kbd "C-h")))

;; f1-f4: start shells
(defun my-numbered-shell (n)
  (shell (get-buffer-create (format "*shell*<%d>" n))))
(global-set-key [f1] (lambda () (interactive) (my-numbered-shell 1)))
(global-set-key [f2] (lambda () (interactive) (my-numbered-shell 2)))
(global-set-key [f3] (lambda () (interactive) (my-numbered-shell 3)))
(global-set-key [f4] (lambda () (interactive) (my-numbered-shell 4)))
;; compile program
(global-set-key [f5] 'compile)
;; open a new eshell terminal
(defun eshell-new () (interactive) (eshell 1))
(global-set-key [f6] 'eshell-new)
;; toggle colors
(global-set-key [f7] 'font-lock-mode)

;; use control-arrow keys to move between windows
(use-package windmove
  :init (windmove-default-keybindings 'control))

;; Use buffer-move: use ctrl-shift-arrows to move buffers between windows
(use-package buffer-move
  :bind (([C-S-up] . 'buf-move-up)
	 ([C-S-down] . 'buf-move-down)
	 ([C-S-right] . 'buf-move-right)
	 ([C-S-left] . 'buf-move-left)
	 ))

;; use ace-jump to navigate within windows
(use-package ace-jump-mode
  :bind
    ("C-c <SPC>" . ace-jump-word-mode)
  :config
    (set-face-attribute
     'ace-jump-face-foreground nil
     :foreground "Yellow"
     :weight 'bold))

;; use ace-window to jump between windows
(use-package ace-window
  :init
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode
  :config
    (set-face-attribute
     'aw-leading-char-face nil
     :foreground "Yellow"
     :weight 'bold
     :height 3.0))

;; make ⌥-up and ⌥-down move lines up and down
(defun transpose-line-up () (interactive)
  (progn
    (transpose-lines 1)
    (forward-line -2)))
(defun transpose-line-down () (interactive)
  (progn
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))
(global-set-key (kbd "M-<up>") 'transpose-line-up)
(global-set-key (kbd "M-<down>") 'transpose-line-down)

;; make tab first indent, then complete (instead of just indenting)
(setq-default tab-always-indent 'complete)

;; By default fn-up and ⌥-up are both mapped scroll-up-command. Give ⌘-up
;; and ⌘-down more useful bindings.
(global-set-key [s-up] 'scroll-up-line)
(global-set-key [s-down] 'scroll-down-line)

(use-package which-key
  :defer 10
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode 1))

(defun insert-single-quotes (p)
  "Insert a pair of unicode rounded quotes, `SINGLE TURNED COMMA
QUOTATION MARK' and `SINGLE COMMA QUOTATION MARK'."
  (interactive "P")
  (insert-pair p 8216 8217))

(defun insert-double-quotes (p)
  "Insert a pair of unicode double rounded quotes."
  (interactive "P")
  (insert-pair p 8220 8221))

(bind-key "M-C-'"  #'insert-single-quotes)
(bind-key "M-C-\"" #'insert-double-quotes)

;; From Howard: fancy newline
(defun newline-at-end-of-current-line ()
  "Insert a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "M-RET") 'newline-at-end-of-current-line)

;; Let emacs map the left option key as Meta;
;; keep the Mac meaning of the right option key to make symbols like π
(setq-default mac-right-option-modifier 'none)

;; Make a shortcut for toggling whitespace-mode, and make the indicators
;; prettier.
(use-package whitespace
  :bind ("C-c w" . whitespace-mode)
  :init
  (setq whitespace-line-column nil
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9])))
  :config
  (set-face-attribute 'whitespace-space       nil :foreground "#cccccc" :background nil)
  (set-face-attribute 'whitespace-newline     nil :foreground "#cccccc" :background nil)
  (set-face-attribute 'whitespace-indentation nil :foreground "#cccccc" :background nil)
  :diminish whitespace-mode)

;; Use ibuffer
(add-hook 'ibuffer-hook 'ibuffer-auto-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Stolen from Howard: make C-a move to beginning of stuff on the line. Hit C-a
;; again to move to the actual beginning of the line.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to ‘smarter-move-beginning-of-line’
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; By default ⌘-left and ⌘-right map to ns-next-frame and ns-previous-frame,
;; which is way too easy to mistype. If I want to switch frames, that's what s-`
;; is for.
(global-unset-key [s-left])
(global-unset-key [s-right])

;; More sensible binding for ⌘-n
(defun my-new-buffer () (interactive)
       (let ((buf (generate-new-buffer "untitled")))
         (switch-to-buffer buf)
         (funcall initial-major-mode)
         (setq buffer-offer-save t)))
(global-set-key (kbd "s-n") 'my-new-buffer)

;;;; ----- display -----

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; my favorite emacs colors
(if (or (not (eq window-system nil))
        (string= (getenv "TERM") "xterm-256color"))
    (progn
      ;; These three were originally used at the computers at the Zoo computer
      ;; cluster at Yale. I always liked them.
      (set-face-background 'default "DarkSlateGray")
      (set-face-foreground 'default "Wheat")
      (set-face-background 'cursor "Orchid")
      ;; Most font-lock colors are nice, they just need a few tweaks. First,
      ;; change the comment and string colors from the clashing orangey defaults.
      (set-face-foreground 'font-lock-comment-face "Thistle")
      (set-face-foreground 'font-lock-string-face "LightPink")
      ;; I want function names to be more noticeable than keywords, so switch
      ;; those two colors. Especially useful in org-mode.
      (set-face-foreground 'font-lock-keyword-face "LightSkyBlue")
      (set-face-foreground 'font-lock-function-name-face "Cyan")
      ;; I find the default link color too distracting.
      (set-face-foreground 'link "CornflowerBlue")
      ))

;; enable emoji
(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
;; Use my favorite monospace font if it's available
(let ((my-font-name "Inconsolata"))
  (if (member my-font-name (font-family-list))
      (set-face-attribute 'default nil
                          :font my-font-name
                          :height 140
                          :weight 'light
                          :width 'normal
                          )))

;; make the background indicate whether emacs is in focus
(add-hook 'focus-out-hook (lambda () (set-face-background 'default "DimGray")))
(add-hook 'focus-in-hook (lambda () (set-face-background 'default "DarkSlateGray")))

;; By default emacs gives up and shows "??" if lines are too long. I want emacs
;; to try harder to find line numbers.
(setq-default line-number-display-limit-width 200000)
;; no startup screen
(setq-default inhibit-startup-screen t)
;; no tool bar or scroll bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Shhhhhh. Don't disturb the neighbors
(setq-default ring-bell-function nil)
(setq-default visible-bell t)
;; always show line number and column number in mode line
(line-number-mode 1)
(column-number-mode 1)
;; show the time in the mode line, but don't show the load average. emacs
;; display-time includes load average by default for some reason
(setq-default display-time-default-load-average nil)
(display-time-mode 1)
;; Use linum-mode by default for programming modes
(use-package linum
  :init
  (add-hook 'prog-mode-hook 'linum-mode)

  :config
  (defun linum-fringe-toggle ()
    "Toggles the line numbers as well as the fringe."    (interactive)
    (cond (linum-mode (fringe-mode '(0 . 0))
                      (linum-mode -1))
          (t          (fringe-mode '(8 . 0))
                      (linum-mode 1)))))
;; show trailing whitespace for code buffers
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;;;; ----- terminal and shells -----

;; We're in emacs; no need for programs like less
(setenv "PAGER" "cat")

;; By default, eshell doesn't know about the nice executables installed by
;; MacPorts in /opt/local/bin. Use what I already have in .zshrc.
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; sudo-edit is useful for editing system files
(use-package sudo-edit)

;; use case-insensitive file completion
(setq-default pcomplete-ignore-case t)

;; don't limit terminal output
(setq-default term-buffer-maximum-size 0)
;; Use Emacs terminfo, not system terminfo. This prevents problems with things
;; not displaying properly in ansi-term.
(setq-default system-uses-terminfo nil)
;; When running M-x compile, jump to first error
(setq-default compilation-auto-jump-to-first-error t)
;; don't allow me to clobber prompt
(setq-default comint-prompt-read-only t)
;; scroll to bottom on input
(setq-default comint-scroll-to-bottom-on-input t)
;; follow output
(setq-default comint-move-point-for-output t)
;; In comint, keep the same windmove bindings of C-up and C-down used everywhere
;; else; use M-up and M-down for previous and next input instead
(define-key comint-mode-map (kbd "M-<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-<down>") 'comint-next-input)
(define-key comint-mode-map (kbd "C-<up>") 'windmove-up)
(define-key comint-mode-map (kbd "C-<down>") 'windmove-down)
;; Make M-. do insert-last-word like it does in terminal zsh
(define-key comint-mode-map (kbd "M-.") 'comint-insert-previous-argument)

;;;; ----- saving desktop and configuration -----

;; don't bug me about running processes when exiting emacs
(setq-default confirm-kill-processes nil)
;; save active buffers periodically
(desktop-save-mode 1)
;; always use the home desktop no matter where the current directory is
(setq-default desktop-path (list "~"))
;; save active minibuffer history periodically
(savehist-mode 1)
;; If emacs didn't exit cleanly because of a crash, a power outage, etc., it
;; will leave a stale desktop lock file. When emacs starts again, it will think
;; the desktop file is locked, even though the PID it thinks is locking the file
;; doesn't exist anymore. Delete the lock file on startup if the owning PID is
;; no longer alive.
(defun my-remove-stale-lock-file (dir)
  (let ((pid (desktop-owner dir)))
    (when pid
      (let ((infile nil)
            (destination nil)
            (display nil))
        (unless (= (call-process "ps" infile destination display "-p"
                                 (number-to-string pid)) 0)
          (let ((lock-fn (desktop-full-lock-name dir)))
            (delete-file lock-fn)))))))
(my-remove-stale-lock-file desktop-path)
;; Make sure files are saved if Emacs goes out of focus
(defun save-all ()
  "Save all dirty buffers without asking for confirmation."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)
;; Also remember where my cursor was
(save-place-mode 1)

;;;; ----- code -----

;; rtags is throwing "Got Diagnostics Error" during init. Remove until I fix it.
;;(require 'rtags)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
;; (defun setup-flycheck-rtags ()
;;   (interactive)
;;   (flycheck-select-checker 'rtags)
;;   ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))

;; ;; only run this if rtags is installed
;; (when (require 'rtags nil :noerror)
;;   ;; make sure you have company-mode installed
;;   (require 'company)
;;   (define-key c-mode-base-map (kbd "M-.")
;;     (function rtags-find-symbol-at-point))
;;   (define-key c-mode-base-map (kbd "M-,")
;;     (function rtags-find-references-at-point))
;;   ;; install standard rtags keybindings. Do M-. on the symbol below to
;;   ;; jump to definition and see the keybindings.
;;   (rtags-enable-standard-keybindings)
;;   ;; comment this out if you don't have or don't use helm
;;   (setq rtags-use-helm t)
;;   ;; company completion setup
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   (push 'company-rtags company-backends)
;;   (global-company-mode)
;;   (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;   ;; use rtags flycheck mode -- clang warnings shown inline
;;   (require 'flycheck-rtags)
;;   ;; c-mode-common-hook is also called by c++-mode
;;   (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))

(require 'magit)
(use-package magit
  :commands magit-status magit-blame
  :init
    (magit-auto-revert-mode)
  :config
    (setq magit-branch-arguments nil
          ;; don't put "origin-" in front of new branch names by default
          magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
          magit-push-always-verify nil
          ;; Get rid of the previous advice to go into fullscreen
          magit-restore-window-configuration t)
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  :bind
    ("C-c g d" . magit-diff-unstaged)
    ("C-c g f" . magit-fetch-all)
    ("C-c g l" . magit-log)
    ("C-c g s" . magit-status))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package haskell-mode
  :init
  (setq-default haskell-process-auto-import-loaded-modules t)
  (setq-default haskell-process-log t)
  (setq-default haskell-process-suggest-remove-import-lines t)
  (setq-default haskell-tags-on-save t)
  :bind (:map haskell-mode-map
  ("C-c C-l" . haskell-process-load-or-reload)
  ("C-c C-t" . haskell-process-do-type)
  ("C-c C-i" . haskell-process-do-info))
  :hook
  (haskell-mode . interactive-haskell-mode))
(require 'haskell-interactive-mode)
(require 'haskell-process)
(use-package hlint-refactor
  :init
  (setq-default hs-lint-replace-with-suggestions nil)
  (setq-default hs-lint-replace-without-ask t)
  :bind (:map haskell-mode-map ("C-c l" . hs-lint)))

;; ediff fixes
(use-package ediff
  :config
  ;; don't make a separate ediff frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Fix a few background colors that don't work
  (set-face-background 'ediff-even-diff-A "gray30")
  (set-face-background 'ediff-even-diff-B "gray30")
  (set-face-background 'ediff-even-diff-C "gray30")
  (set-face-background 'ediff-even-diff-Ancestor "gray30")
  (set-face-background 'ediff-odd-diff-A "gray40")
  (set-face-background 'ediff-odd-diff-B "gray40")
  (set-face-background 'ediff-odd-diff-C "gray40")
  (set-face-background 'ediff-odd-diff-Ancestor "gray40"))

;;;; ----- projects and spaces -----

(use-package projectile
  :init (projectile-global-mode 1))

(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

;; disabling perspective because it doesn't work properly with desktop-save; on
;; init it's showing 'Error (frameset): Wrong type argument: perspective,
;; "Unprintable entity"'

;; (use-package perspective
;;   :bind ("C-x x x" . persp-switch-last)
;;   :init (persp-mode +1)

;;   (use-package persp-projectile))


;;;; ----- misc -----

;; expand-region enlarges the region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; C'mon, emacs, there's only one space between sentences.
(setq-default sentence-end-double-space nil)
;; use spaces to indent, not tabs
(setq-default indent-tabs-mode nil)
;; 80 columns, not 70
(setq-default fill-column 80)
;; automatically update files when changed, quietly
(global-auto-revert-mode)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)
;; large files are OK
(setq-default large-file-warning-threshold nil)
;; find files case-insensitively
(setq-default read-file-name-completion-ignore-case t)
;; find buffer names case-insensitively
(setq-default completion-ignore-case t)
;; store more undo data
(setq-default undo-limit 300000)
(setq-default undo-strong-limit 600000)
;; I don't want to have to type "yes"
(fset 'yes-or-no-p 'y-or-n-p)
;; keep point in the middle when scrolling
(setq-default scroll-preserve-screen-position t)
;; enable emacs to use the Mac system clipboard for cut, copy, and paste
(defun paste-to-osx (text &optional push)
  (interactive)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
;; Use linux indent style with 2-space indent
(setq-default c-default-style "linux")
(setq-default c-basic-offset 2)
;; 2-space indent for shells, too
(setq-default sh-basic-offset 2)
;; When I use C-k to kill the whole line, I want to include the trailing newline
(setq-default kill-whole-line t)

;; rebalance window sizes automatically
(setq-default window-combination-resize t)  ; this is for Emacs 24+
;; For Emacs <24, we need something more elaborate:
(if (or (string-match "GNU Emacs 22" (version))
        (string-match "GNU Emacs 23" (version)))
    (progn
      (defadvice split-window-horizontally (after rebalance-windows activate)
        (balance-windows))
      (ad-activate 'split-window-horizontally)
      (defadvice delete-window
          (after rebalance-windows activate)
        (balance-windows))
      (ad-activate 'delete-window)))

;; use the mouse
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

;; use objc-mode for Objective-C++ files (close enough)
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
;; use c++-mode for Metal files (close enough)
(add-to-list 'auto-mode-alist '("\\.metal\\'" . c++-mode))

(use-package org
  :bind
    ("C-c a" . org-agenda)
    ("C-c c" . org-capture)
  (:map org-mode-map
        ;; In org-mode, don't steal M-left and M-right; I want them to be
        ;; left-word and right-word like everywhere else
        ("M-<right>" . right-word)
        ("M-<left>" . left-word)
        ;; Map ⌘-arrows to what is normally meta-arrows
        ("s-<up>" . org-metaup)
        ("s-<down>" . org-metadown)
        ("s-<right>" . org-metaright)
        ("s-<left>" . org-metaleft)
        ;; Switch return and M-return
        ("<return>" . org-meta-return)
        ("M-<return>" . org-return)
        ;; Use ctrl-t for changing todo state (I never use it for
        ;; transposing characters)
        ("C-t" . org-todo))
  :init
  (setq-default org-special-ctrl-a/e t
                org-return-follows-link t
                org-indent-mode t
                org-indent-indentation-per-level 2
                org-hide-emphasis-markers t
                org-replace-disputed-keys t
                org-support-shift-select t
                org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE"))
                org-log-done 'time
                org-startup-folded nil
                org-directory "~/Stuff/org"
                org-default-notes-file (concat org-directory "/inbox.org")
                org-capture-templates
                '(("t" "Todo" entry (file+headline "~/Stuff/org/inbox.org" "Inbox")
                   "* TODO %i%?")
                  ("l" "Link" entry (file+headline "~/Stuff/org/inbox.org" "Inbox")
                   "* %? %i\n  %a"))
                org-agenda-prefix-format
                ;; I want to show the project each todo is part of. The %b in
                ;; todo means "breadcrumbs," i.e., ancestor items.
                '((agenda . " ghi %i %-12:c%?-12t% s jkl")
                  (todo . " %i %b")
                  (tags . " %i %-12:c")
                  (search . " %i %-12:c"))
                ))

;; Use pretty org bullets
;; default: ◉ ○ ✸ ✿
;; others: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶ ► • ★ ▸
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init (setq org-bullets-bullet-list
              '("●" "○" "◆" "◇" "▸" "▹")))

;; ;; old org-mode
;; (if (string-match "GNU Emacs 22" (version))
;;   (progn
;;     (add-to-list 'load-path "~/Dropbox/software/emacs-packages/org-7.7/lisp")
;;     (require 'org-install)
;;     (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;     (global-set-key "\C-cl" 'org-store-link)
;;     (global-set-key "\C-cc" 'org-capture)
;;     (global-set-key "\C-ca" 'org-agenda)
;;     (global-set-key "\C-cb" 'org-iswitchb)
;; ;;    (setq-default org-replace-disputed-keys t)
;; ;;    (setq-default org-startup-indented t)
;;     (add-hook 'org-mode-hook
;;       (lambda ()
;;         (local-set-key [A-up] 'org-move-subtree-up)
;;         (local-set-key [A-down] 'org-move-subtree-down)))
;;     ; tell org-mode not to steal S-C-arrows
;;     (defun org-remove-bindings ()
;;       (define-key org-mode-map [S-C-up] nil)
;;       (define-key org-mode-map [S-C-down] nil)
;;       (define-key org-mode-map [S-C-right] nil)
;;       (define-key org-mode-map [S-C-left] nil))
;;     (eval-after-load "org" '(org-remove-bindings))
;;     ))

;; use ido-mode and related extras
;; I don't like ido
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (setq ido-use-filename-at-point 'guess)
;; (use-package ido-vertical-mode
;;   :init
;;   (ido-vertical-mode 1))
;; (use-package flx-ido
;;   :init
;;   (flx-ido-mode 1)
;;   :custom
;;   (ido-use-faces nil))

;; use graphviz mode for dot files
(use-package graphviz-dot-mode)


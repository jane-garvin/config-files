;; My emacs configuration across all machines. A lot of it is adapted from
;; Howard Abrams's excellent setup: https://github.com/howardabrams/dot-files

;;;; ----- Lisp -----

;; It's only right. Yes, I am a programming language nerd.
(setq lexical-binding t)

;;;; ----- memory -----

;; Give emacs the megabytes it's always wanted
(setq gc-cons-threshold (* 100 1024 1024))

;; Turning off compacting of font caches increases memory footprint but avoids
;; serious rendering slowdown.
(setq inhibit-compacting-font-caches t)

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

;; Use use-package to automatically install certain packages
(require 'use-package)

;; Install packages if necessary
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

;; Better way of differentiating buffers with the same name
(use-package uniquify-files
  :config
  (setq-default uniquify-buffer-name-style 'forward))

;; put backup files in a separate directory
(setq my-backup-directory (expand-file-name (concat user-emacs-directory "backups")))
;; Create backup directory if necessary
(when (not (file-exists-p my-backup-directory))
      (message "Making directory %s" my-backup-directory)
      (make-directory my-backup-directory))
(setq-default backup-directory-alist `(("." . ,my-backup-directory)))
(setq-default vc-make-backup-files t)

;; Without auto-save, I save about every two to ten seconds, and I revert a file almost
;; never. Set it to automatically save everything.
(setq auto-save-visited-interval 1)
(auto-save-visited-mode)

;; direx is a kind of tree-based version of dired
(use-package direx
  :bind
  ("C-x C-j" . direx:jump-to-directory))

;; delete-current-buffer-file and rename-current-buffer-file stolen from ohai emacs
;; https://github.com/bodil/ohai-emacs/blob/master/modules/ohai-dired.el
;; A function for deleting the file being edited.
;; This one is a bit dangerous, even with the yes/no question, so
;; it's not bound to any key by default.
;; Run it using M-x delete-current-buffer-file.
(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; And a function for renaming the file being edited, bound to C-x C-r.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

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
(defmacro my-numbered-shell (n)
  "Quick shell access.
Create a shell, or switch to an existing shell, in the current
window based on the given number N."
  `(lambda () (interactive)
     (shell
      (switch-to-buffer
       (get-buffer-create
        (format "*shell*<%d>" ,n))))))
(global-set-key [f1] (my-numbered-shell 1))
(global-set-key [f2] (my-numbered-shell 2))
(global-set-key [f3] (my-numbered-shell 3))
(global-set-key [f4] (my-numbered-shell 4))
;; toggle line numbers
(global-set-key [f5] 'display-line-numbers-mode)
;; change line ending appearance
(global-set-key [f6] 'my-change-line-ending)
;; toggle colors
(global-set-key [f7] 'font-lock-mode)

;; use ⌘-arrow keys to move between windows
(use-package windmove
  :config (windmove-default-keybindings 'super))

;; Use buffer-move: use M-⌘-arrows to switch buffers between windows
(use-package buffer-move
  :bind (("M-s-<up>" . 'buf-move-up)
	 ("M-s-<down>" . 'buf-move-down)
	 ("M-s-<right>" . 'buf-move-right)
	 ("M-s-<left>" . 'buf-move-left)))

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
  (setq aw-keys '(?j ?f ?k ?d ?l ?s ?\; ?a ?h ?g
                  ?u ?r ?i ?e ?o ?w ?p ?q ?y ?t
                  ?n ?v ?m ?c ?x ?b))
    (global-set-key (kbd "M-o") 'ace-window)
    (setq aw-scope 'frame)
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

;; More sensible binding for ⌘-n instead of making a new frame
(defun my-new-buffer () (interactive)
       (let ((buf (generate-new-buffer "untitled")))
         (switch-to-buffer buf)
         (funcall initial-major-mode)
         (setq buffer-offer-save t)))
(global-set-key (kbd "s-n") 'my-new-buffer)

(define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e b") 'eval-buffer)

;; More sensible binding for ⌘-w instead of killing the frame
(global-set-key (kbd "s-w") 'kill-buffer-and-window)

;; Make ⌘-digit select the nth frame.
(defmacro my-select-numbered-frame (n)
  "Select the Nth frame, where 1 is the frame created first."
  `(lambda ()
     (interactive)
     (let ((f (nth (1- ,n) (nreverse (frame-list)))))
       (if f
         (select-frame-set-input-focus f)
         (let ((nf (length (frame-list))))
           (if (= nf 1)
             (error "Only %d frame" nf)
             (error "Only %d frames" nf)))))))
(global-set-key (kbd "s-1") (my-select-numbered-frame 1))
(global-set-key (kbd "s-2") (my-select-numbered-frame 2))
(global-set-key (kbd "s-3") (my-select-numbered-frame 3))
(global-set-key (kbd "s-4") (my-select-numbered-frame 4))
(global-set-key (kbd "s-5") (my-select-numbered-frame 5))
(global-set-key (kbd "s-6") (my-select-numbered-frame 6))
(global-set-key (kbd "s-7") (my-select-numbered-frame 7))
(global-set-key (kbd "s-8") (my-select-numbered-frame 8))
(global-set-key (kbd "s-9") (my-select-numbered-frame 9))

;; I use fit-window-to-buffer pretty often, and I almost never use the default
;; binding set-fill-column.
(global-set-key (kbd "C-x f") 'fit-window-to-buffer)

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
      ;; Some of the default ANSI terminal colors are hard to see on a dark background
      ;; default: ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"]
      (setq ansi-color-names-vector
            ["black" "tomato1" "green3" "yellow3" "RoyalBlue1" "magenta2" "cyan3" "gray90"])))
      ; I don't know why this fails with "Symbol's function definition is void: ansi-color-make-color-map"
      ; (setq ansi-color-map (ansi-color-make-color-map))))
;; For some reason the cursor color doesn't follow when you make-frame
(defun set-cursor-hook (frame)
  (modify-frame-parameters
   frame (list (cons 'cursor-color (face-attribute 'cursor :background)))))
(add-hook 'after-make-frame-functions 'set-cursor-hook)

;; enable emoji 😎
(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
;; Use my favorite monospace font if it's available
(let ((my-fonts '("Inconsolata" "Inconsolata for Powerline")))
  (dolist (font my-fonts)
    (if (member font (font-family-list))
      (set-face-attribute 'default nil
                          :font font
                          :height 120
                          :weight 'light
                          :width 'normal))))

;; Dim unselected windows
(use-package auto-dim-other-buffers
  :init
  (auto-dim-other-buffers-mode))
;; make the background indicate whether emacs is in focus
(add-hook 'focus-out-hook (lambda ()
                            (auto-dim-other-buffers-mode 0)
                            (set-face-background 'default "DimGray")))
(add-hook 'focus-in-hook (lambda ()
                           (auto-dim-other-buffers-mode)
                           (set-face-background 'default "DarkSlateGray")))

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
;; make columns one-based, following clang's example
(setq-default column-number-indicator-zero-based nil)
;; show the time in the mode line, but don't show the load average. emacs
;; display-time includes load average by default for some reason
(setq-default display-time-default-load-average nil)
(display-time-mode 1)
;; show trailing whitespace in code buffers and org-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))
;; sort completions vertically instead of horizontally
(setq-default completions-format 'vertical)

;; Set a key to switch line endings between wrapping long lines, truncating long
;; lines, and word-processor-style word wrap a.k.a. visual-line-mode.
(defun my-change-line-ending ()
  "Change line endings in the current buffer.
   default (wrap) -> truncate-lines -> visual-line-mode -> default (wrap)"
  (interactive)
  (cond
   ;; visual-line-mode -> default
   (visual-line-mode (progn
                       (setq truncate-lines nil)
                       (visual-line-mode 0)))
   ;; truncate-lines -> visual-line-mode
   (truncate-lines (progn
                     (setq truncate-lines nil)
                     (visual-line-mode)))
   ;; default -> truncate-lines
   (t (setq truncate-lines t))))
(global-set-key (kbd "C-c l") 'my-change-line-ending)

;;;; ----- terminal and shells -----

;; We're in emacs; no need for programs like less
(setenv "PAGER" "cat")

;; By default, eshell doesn't know about the nice executables installed by
;; MacPorts or Homebrew. Use what I already have in .zshrc.
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
;; ignore duplicate history entries
(setq comint-input-ignoredups t)
;; make an easy way to stop scrolling through history
(define-key comint-mode-map (kbd "C-<right>") 'comint-restore-input)
;; in comint-mode, make M-up and M-down work like C-up and C-down
(define-key comint-mode-map (kbd "M-<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "M-<down>") 'comint-next-input)
;; Make M-. do insert-last-word like it does in terminal zsh
(define-key comint-mode-map (kbd "M-.") 'comint-insert-previous-argument)

;;;; ----- saving desktop and configuration -----

;; don't bug me about running processes when exiting emacs
(setq-default confirm-kill-processes nil)
(use-package desktop
  :init
  ;; save active buffers periodically
  (desktop-save-mode 1)
  ;; save active minibuffer history periodically
  (savehist-mode 1)
  ;; Remember where my cursor was
  (save-place-mode 1))
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

(use-package magit
  :commands magit-status magit-blame
  :config
    (setq magit-branch-arguments nil
          magit-push-always-verify nil
          ;; Get rid of the previous advice to go into fullscreen
          magit-restore-window-configuration t)
  :bind
    ("C-c g d" . magit-diff-unstaged)
    ("C-c g f" . magit-fetch-all)
    ("C-c g l" . magit-log)
    ("C-c g s" . magit-status))

;;(use-package flycheck
;;  :init (global-flycheck-mode))

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
;(require haskell-interactive-mode)
;(require haskell-process)
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

;; elpy for Python
(use-package elpy
  :init
  (elpy-enable)
  ;; Remove elpy's indentation highlighting because I use
  ;; highlight-indent-guides for that.
  (delete 'elpy-module-highlight-indentation elpy-modules)
  ;; Note: the following lines require the extra executables to be installed via pip.
  (setq elpy-rpc-backend "jedi")
  (setq elpy-formatter "black")
  (setq elpy-syntax-check-command "black")
  :bind (:map elpy-mode-map
              ;; Switch elpy's C-arrows and M-arrows
              ("M-<up>" . elpy-nav-backward-block)
              ("M-<down>" . elpy-nav-forward-block)
              ("M-<right>" . elpy-nav-forward-indent)
              ("M-<left>" . elpy-nav-backward-indent)
              ("C-<up>" . elpy-nav-move-line-or-region-up)
              ("C-<down>" . elpy-nav-move-line-or-region-down)
              ("C-<right>" . elpy-nav-indent-shift-right)
              ("C-<left>" . elpy-nav-indent-shift-left)
              ;; Use jedi to goto definition
              ("M-." . elpy-goto-definition))
  :defer 2)

;; rnc-mode for RELAX NG
(use-package rnc-mode)

;; Swift
(use-package swift-mode)

;; show indentation with dotted lines
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

;; Markdown
(use-package markdown-mode)

;;;; ----- projects and spaces -----

(use-package projectile
  :init (projectile-mode 1)
  :defer 2)

(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

;; disabling perspective because it doesn't work properly with desktop-save; on
;; init it's showing 'Error (frameset): Wrong type argument: perspective,
;; "Unprintable entity"'

;; (use-package perspective
;;   :bind ("C-x x x" . persp-switch-last)
;;   :init (persp-mode +1)

;;   (use-package persp-projectile))

;;;; ----- org-mode -----

(defun my-delete-leading-stars (&optional arg)
  "Delete the org heading (one or more '*'s followed by a space)
at the beginning of the current line. If the line does not begin
with an org heading, do nothing. With argument, move forward ARG
- 1 lines first."
  (interactive "p")
  (save-mark-and-excursion
    (move-beginning-of-line arg)
    (let ((beg (point)))
      (re-search-forward "^[*]+ " (line-end-position) t)
      (delete-region beg (point)))))

(defun my-org-delete-backward-char (n)
  "If point is at org header, then delete header. Otherwise,
just call org-delete-backward-char."
  (interactive "p")
  (if (/= n 1)
    (org-delete-backward-char n)
    (let ((origin (point))
          (beginning-of-text nil))
      (save-mark-and-excursion
        (back-to-indentation)
        (when (looking-at "[*]+ ")
          (goto-char (match-end 0))
          (setq beginning-of-text (point))))
      (if (and beginning-of-text
               (= origin beginning-of-text))
        (my-delete-leading-stars)
        (org-delete-backward-char 1)))))

(defun my-org-delete-char (n)
  "If point is at the end of the line, delete both the newline
and the heading of the next line, if any. Otherwise, just call
org-delete-char."
  (interactive "p")
  (when (= (point) (line-end-position)) (my-delete-leading-stars 2))
  (org-delete-char n))

(defun my-org-kill-line (&optional arg)
  "If point is at end of line, delete indentation at beginning
of next line. Then, in any case, call org-kill-line."
  (interactive "P")
  (when (= (point) (line-end-position)) (my-delete-leading-stars 2))
  (org-kill-line arg))

(defun my-org-delete-indentation (&optional arg)
  "Delete indentation at beginning of this line, then call
org-delete-indentation."
  (interactive "P")
  (my-delete-leading-stars)
  (org-delete-indentation arg))

(use-package org
  :bind
    ("C-c a" . org-agenda)
    ("C-c c" . org-capture)
  (:map org-mode-map
        ;; In org-mode, don't steal M-left and M-right; I want them to be
        ;; left-word and right-word like everywhere else
        ("M-<right>" . right-word)
        ("M-<left>" . left-word)
        ;; Map ctrl-arrows to what is normally meta-arrows
        ("C-<up>" . org-metaup)
        ("C-<down>" . org-metadown)
        ("C-<right>" . org-metaright)
        ("C-<left>" . org-metaleft)
        ;; Use ctrl-t for changing todo state (I never use it for
        ;; transposing characters)
        ("C-t" . org-todo)
        ;; Delete stars when necessary for delete and related keys.
        ("<DEL>" . my-org-delete-backward-char)
        ("C-d" . my-org-delete-char)
        ("C-k" . my-org-kill-line)
        ("M-^" . my-org-delete-indentation))
  :config
  (setq-default org-special-ctrl-a/e t
                org-return-follows-link t
                org-replace-disputed-keys t
                org-support-shift-select t
                org-todo-keywords '((sequence
                                     "TODO(t)"
                                     "NEXT(n)"
                                     "IN PROGRESS(p)"
                                     "WAITING(w)"
                                     "|"
                                     "DONE(d)"
                                     "CANCELED(c@)"
                                     "TRASH(t)"))
                org-log-done 'time
                org-startup-folded nil
                my-org-inbox-file (concat org-directory "inbox.org")
                my-org-projects-file (concat org-directory "projects.org")
                org-default-notes-file my-org-inbox-file
                org-agenda-files (list my-org-inbox-file my-org-projects-file)
                org-capture-templates
                '(("t" "Todo" entry (file my-org-inbox-file)
                   "* TODO %i%?")
                  ("l" "Link" entry (file my-org-inbox-file)
                   "* %? %i\n  %a"))
                org-agenda-prefix-format
                ;; I want to show the project each todo is part of. The %b in
                ;; todo means "breadcrumbs," i.e., ancestor items.
                '((agenda . " %i %-12:c%?-12t% s")
                  (todo . " %i %b")
                  (tags . " %i %-12:c")
                  (search . " %i %-12:c"))
                org-pretty-entities t
                org-agenda-window-setup 'current-window
                ;; require {} in subscripts and superscripts
                org-use-sub-superscripts '{}
                org-use-fast-todo-selection 'expert
                org-hide-emphasis-markers t
                org-startup-align-all-tables t)
  :defer 2)

;; Make org-agenda buffers refresh automatically. The function
;; `org-agenda-maybe-redo' is perfect for this, but it emits a message about the
;; agenda restriction lock that I don't need to see every time.
(defun my-org-agenda-maybe-redo-silently ()
  "If there is any window showing the agenda view, update it silently."
  (when (fboundp 'org-agenda-maybe-redo)
    (let ((inhibit-message t))
      (org-agenda-maybe-redo))))
(run-with-idle-timer 1 t 'my-org-agenda-maybe-redo-silently)

;; Use pretty org bullets
;; default: ◉ ○ ✸ ✿
;; others: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶ ► • ★ ▸
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init (setq org-bullets-bullet-list '("●" "○")))

(use-package org-gtd
  :after org
  :custom
  (org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c d c" . org-gtd-capture)
  ("C-c d a" . org-agenda-list)
  ("C-c d p" . org-gtd-process-inbox)
  ("C-c d n" . org-gtd-show-all-next)
  ("C-c d s" . org-gtd-show-stuck-projects))
  :init
   ;; when you're done editing an item in the processing phase
  (bind-key "C-c f" 'org-gtd-clarify-finalize))

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

;;;; ----- company-mode -----

;; Company config mostly stolen from Ohai Emacs.
(use-package company
  :demand t
  :commands company-mode
  :config
  ;; Enable company-mode globally.
  (global-company-mode)
  ;; Except when you're in term-mode.
  (setq company-global-modes '(and (not term-mode) (not shell-mode)))
  ;; Give Company a decent default configuration.
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.5
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence))
  ;; Show documentation where available for selected completion
  ;; after a short delay.
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  ;; Add a completion source for emoji. 😸
  (use-package company-emoji
    :config
    (company-emoji-init))
  ;; Company's default colours look OK with the light scheme,
  ;; but hideous with the dark one, so let's pick something nicer.
  ;; (add-hook
  ;;  'ohai-appearance/dark-hook
  ;;  (lambda ()
  ;;    (set-face-foreground 'company-tooltip "#000")
  ;;    (set-face-background 'company-tooltip "#ddd")
  ;;    (set-face-background 'company-scrollbar-bg "#fff")
  ;;    (set-face-background 'company-scrollbar-fg "#999")
  ;;    (set-face-background 'company-tooltip-selection "#aaa")
  ;;    (set-face-foreground 'company-tooltip-common "#9a0000")
  ;;    (set-face-foreground 'company-tooltip-common-selection "#9a0000")
  ;;    (set-face-foreground 'company-tooltip-annotation "#00008e")))
  ;; Use C-\ to activate the Company autocompleter.
  ;; We invoke company-try-hard to gather completion candidates from multiple
  ;; sources if the active source isn't being very forthcoming.
  (use-package company-try-hard
    :commands company-try-hard
    :bind ("C-\\" . company-try-hard)
    :config
    (bind-keys :map company-active-map
               ("C-\\" . company-try-hard)))
  :diminish company-mode)

;;;; ----- misc -----

;; expand-region enlarges the region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; From https://github.com/howardabrams/dot-files/blob/06f1e666e78c606ab32114426b69ec0ecf9a503e/emacs-fixes.org
;; Macro that moves to beginning of line before calling a function.
(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "ha/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to the beginning of the line when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

;; Make C-k kill the whole line when called with a prefix argument.
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key (kbd "C-k") (bol-with-prefix kill-line))

;; C'mon, emacs, there's only one space between sentences.
(setq-default sentence-end-double-space nil)
;; use spaces to indent, not tabs
(setq-default indent-tabs-mode nil)
;; 80 columns, not 70
(setq-default fill-column 80)
;; automatically update files when changed, quietly
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)
(global-auto-revert-mode)
;; large files are OK
(setq-default large-file-warning-threshold nil)
;; find files case-insensitively
(setq-default read-file-name-completion-ignore-case t)
;; find buffer names case-insensitively
(setq-default read-buffer-completion-ignore-case t)
;; find everything else case-insensitively
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
;; 4-space indent for python
(setq-default python-indent-offset 4)
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

;; use ido-mode and related extras
(use-package ido
  :ensure t
  :init  (setq ido-enable-flex-matching t
               ido-ignore-extensions t
               ido-use-virtual-buffers t
               ido-everywhere t
               ido-use-filename-at-point 'guess
               ;; open a buffer in this window even if it's open in another frame
               ido-default-buffer-method 'selected-window)
  :config
  (ido-mode 1)
  (ido-everywhere 1))
(use-package ido-vertical-mode
   :config
   (ido-vertical-mode 1)
   (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
(use-package flx-ido
   :init
   (flx-ido-mode 1)
   :custom
   (ido-use-faces nil))

;; use graphviz mode for dot files
(use-package graphviz-dot-mode)

;; typo-mode for correct quotation marks, dashes, etc.
(use-package typo
  :config
  (typo-global-mode)
  :hook
  (text-mode . typo-mode))

;; ripgrep is handy for searching
(use-package rg)

;; Use C-w to kill whole line when region is not active
(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode))

(provide 'emacs-common)

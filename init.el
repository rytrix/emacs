;; Packages
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) ;; ELPA and NonGNU ELPA are default in Emacs28
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; w/o this Emacs freezes when refreshing ELPA
(package-initialize)
(setq package-enable-at-startup nil)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;; GC
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Evil mode (vim bindings)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

;; Undo tree
;; (use-package undo-fu)
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;; rainbow parenthesis
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

;; rainbow mode (ex. #ffffff).
(use-package rainbow-mode
  :hook org-mode prog-mode)

;; 5. Doom packages (theming)
(use-package doom-themes
  :defer t
  :init
  (load-theme 'doom-ayu-dark t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

;; functions needed for some keybinds
(defun  center-forward-paragraph()
  "Scroll down half a page while keeping the cursor centered."
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
	  ((= ln lmax) (recenter (window-end)))
	  (t (progn
               (move-to-window-line -1)
               (recenter))))))

(defun center-backward-paragraph ()
  "Scroll up half a page while keeping the cursor centered."
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
	  ((= ln lmax) (move-to-window-line nil))
	  (t (progn
               (move-to-window-line 0)
               (recenter))))))

;; quick switch
(defvar quick-switch-array (make-vector 10 nil)
  "Array to store buffer names for quick switching")

(defun quick-switch-to-buffer (index)
  "Switch to the buffer at the given index in quick-switch-array"
  (interactive "nIndex: ")
  (let ((buffer-name (aref quick-switch-array index)))
    (if (bufferp buffer-name)
        (switch-to-buffer buffer-name)
      (message "No buffer at index %d" index))))

(defun quick-switch-add-buffer (index)
  "Add the current buffer to the quick-switch-array at the given index"
  (interactive "nIndex: ")
  (unless (and (integerp index) (>= index 0) (< index 10))
    (error "Invalid index"))
  (aset quick-switch-array index (current-buffer))
  (message "Buffer added to index %d" index))

;; keybinds
(use-package general
  :config
  (general-evil-setup)

  (global-set-key (kbd "C-1") (lambda () (interactive) (quick-switch-to-buffer 0)))
  (global-set-key (kbd "C-2") (lambda () (interactive) (quick-switch-to-buffer 1)))
  (global-set-key (kbd "C-3") (lambda () (interactive) (quick-switch-to-buffer 2)))
  (global-set-key (kbd "C-4") (lambda () (interactive) (quick-switch-to-buffer 3)))
  (global-set-key (kbd "C-5") (lambda () (interactive) (quick-switch-to-buffer 4)))
  (global-set-key (kbd "C-6") (lambda () (interactive) (quick-switch-to-buffer 5)))
  (global-set-key (kbd "C-7") (lambda () (interactive) (quick-switch-to-buffer 6)))
  (global-set-key (kbd "C-8") (lambda () (interactive) (quick-switch-to-buffer 7)))
  (global-set-key (kbd "C-9") (lambda () (interactive) (quick-switch-to-buffer 8)))
  (global-set-key (kbd "C-0") (lambda () (interactive) (quick-switch-to-buffer 9)))

  (global-set-key (kbd "C-!") (lambda () (interactive) (quick-switch-add-buffer 0)))
  (global-set-key (kbd "C-@") (lambda () (interactive) (quick-switch-add-buffer 1)))
  (global-set-key (kbd "C-#") (lambda () (interactive) (quick-switch-add-buffer 2)))
  (global-set-key (kbd "C-$") (lambda () (interactive) (quick-switch-add-buffer 3)))
  (global-set-key (kbd "C-%") (lambda () (interactive) (quick-switch-add-buffer 4)))
  (global-set-key (kbd "C-^") (lambda () (interactive) (quick-switch-add-buffer 5)))
  (global-set-key (kbd "C-&") (lambda () (interactive) (quick-switch-add-buffer 6)))
  (global-set-key (kbd "C-*") (lambda () (interactive) (quick-switch-add-buffer 7)))
  (global-set-key (kbd "C-(") (lambda () (interactive) (quick-switch-add-buffer 8)))
  (global-set-key (kbd "C-)") (lambda () (interactive) (quick-switch-add-buffer 9)))

  (define-key evil-motion-state-map (kbd "u") 'undo-tree-undo)

  (define-key evil-motion-state-map (kbd "g c") 'comment-line)
  
  ;; set up 'SPC' as the global leader key
  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  
  (leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.el")) :wk "Edit emacs config")
    "f r" '(counsel-recentf :wk "Find recent files")
    "g c" '(comment-line :wk "Comment lines")
    "p" '(lambda () (interactive) (execute-kbd-macro (kbd "\"+p")))
    "P" '(lambda () (interactive) (execute-kbd-macro (kbd "\"+P")))
    "y" '(lambda () (interactive) (execute-kbd-macro (kbd "\"+y")))
    "f f" '(fzf :wk "fuzzy find file")
    "F" '(fzf-directory default-directory :wk "fuzzy find file")
    "f g" '(fzf-grep-in-dir :wk "grep search dir")
    "e b" '(eval-buffer :wk "evaluate buffer")
    "r c" '(projectile-run-async-shell-command-in-root :wk "shell command in project root")
    "r s" '(projectile-run-shell :wk "shell command in project root")
    "u" '(lambda () (interactive) (undo-tree-visualize)))

  (leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
	"h k" '(describe-key :wk "Describe keybind")
    "h t" '(load-theme :wk "Load theme"))

  (leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t s" '(ansi-term :wk "open a eshell")
    "t b" '(shell-command :wk "run a shell command"))

  (leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current"))

  (leader-keys
    "e" '(:ignore t :wk "Eglot")
    "e s" '(eglot :wk "Start eglot")
    "e q" '(eglot-shutdown :wk "Start eglot")
    "e a" '(eglot-code-actions :wk "Code actions")
    "e r" '(eglot-rename :wk "Rename symbol")
    "e d" '(eglot-find-declaration :wk "Find declaration")
    "e i" '(eglot-find-implementation :wk "Find implementation")
    "e f" '(eglot-format-buffer :wk "Format buffer"))
  
  (leader-keys
    "s u" '(sudo-edit-find-file :wk "Sudo find file")
    "s U" '(sudo-edit :wk "Sudo edit file"))

  (leader-keys
    "g" '(:ignore t :wk "Git")
    "g s" '(magit-status :wk "Git status")
    "g c" '(magit-commit :wk "Git commit")
    "g t" '(git-timemachine :wk "Git time machine"))

  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))

;; gives sudo edit permissions if we need it
(use-package sudo-edit
  :config)

;; fuzzy finding
(use-package fzf
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -inrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; completions 
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  )

(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

;; Projects 
(use-package projectile
  :config
  (projectile-mode 1))

;; Git stuff
(use-package magit)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-next-revision))

;; language modes/lsps
(use-package cmake-mode)
(use-package lua-mode)
(use-package rust-mode)
(use-package glsl-mode)

;; Sane defaults
;; -----------------------------------------------------------

;; Backup directories
(setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo")))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist '(("." . "~/.config/emacs/saves")))

;; fonts
(set-face-attribute 'default nil
	            :height 150
	            :font "FiraCode Nerd Font Mono")

;; hide ui stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(setq display-line-numbers 'relative)
(setq-default display-line-numbers-type 'relative)

;; Don't warn for large files (shows up when launching videos)
(setq large-file-warning-threshold nil)

;; Don't warn for following symlinked files
(setq vc-follow-symlinks t)

;; Don't warn when advice is added for functions
(setq ad-redefinition-action 'accept)

(setq inhibit-startup-screen t)

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 200 4))
(setq c-basic-offset 4) 

(defun fix-syntax-tree ()
  "treats - and _ as parts of words"
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))
;; treats "-" "_" as parts of words
(add-hook 'after-change-major-mode-hook
          (lambda () (fix-syntax-tree)))

;; frame stuff and transparency 
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" default))
 '(package-selected-packages
   '(glsl-mode git-timemachine magit rust-mode lua-mode cmake-mode company-box company rainbow-delimiters projectile all-the-icons-ivy-rich counsel ivy evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

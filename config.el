(setq elpaca-core-date '(20240221))
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			:ref nil :depth 1
			:files (:defaults "elpaca-test.el" (:exclude "extensions"))
			:build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
 (build (expand-file-name "elpaca/" elpaca-builds-directory))
 (order (cdr elpaca-order))
 (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						 ,@(when-let ((depth (plist-get order :depth)))
						     (list (format "--depth=%d" depth) "--no-single-branch"))
						 ,(plist-get order :repo) ,repo))))
		 ((zerop (call-process "git" nil buffer t "checkout"
				 (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				 "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
;;(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
;;(elpaca nil (message "deferred"))

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))
  (use-package evil-collection
    :after evil
    :config
    ;;(setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))
  (use-package evil-tutor)

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer airi/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (airi/leader-keys
      "." '(find-file :wk "Find file")
      "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
      "f r" '(counsel-recentf :wk "Find recent files")
      "TAB TAB" '(comment-line :wk "Comment lines")
      "g c" '(comment-line :wk "Comment lines"))

  (airi/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bi" '(ibuffer :wk "Switch buffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))

  (airi/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
    "ed" '(eval-defun :wk "Evaluate defun containing or after point")
    "ee" '(eval-expression :wk "Evaluate and elisp expression")
    "eh" '(counsel-esh-history :which-key "Eshell history")
    "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "er" '(eval-region :wk "Evaluate elisp in region")
    "es" '(eshell :which-key "Eshell"))

 (airi/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")
    "h r r" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config"))


  (airi/leader-keys
    "s" '(:ignore t :wk "Search")
    "SPC" '(ibuffer :wk "List Buffers")
    "sf" '(find-file :wk "Search File")
    "sg" '(grep-find :wk "Search by Grep"))

  (airi/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "te" '(eshell-toggle :wk "Toggle eshell")
    "tl" '(display-line-numbers-mode :wk "Toggle line numbers")
    "tt" '(visual-line-mode :wk "Toggle truncated lines")
    "tv" '(vterm-toggle :wk "Toggle vterm"))
)

;; Change font
(add-to-list 'default-frame-alist
             '(font . "Iosevka q SemiBold-15"))

  (column-number-mode 1)
  (global-auto-revert-mode)
  (global-display-line-numbers-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default whitespace-style
		'(face
		  tabs
		  spaces
		  trailing
		  lines-tail
		  newline
		  missing-newline-at-eof
		  space-before-tab
		  indentation
		  empty
		  space-after-tab
		  space-mark
		  tab-mark
		  newline-mark))

  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Global Keybinding
  (global-set-key (kbd "C-M-j") 'buffer-menu)

  ;; Set Backup Directory
  (setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
	backup-by-copying      t  ; Don't de-link hard links
	version-control        t  ; Use version numbers on backups
	delete-old-versions    t  ; Automatically delete excess backups:
	kept-new-versions      10 ; how many of the newest versions to keep
	kept-old-versions      5) ; and how many of the old

    ;; Stop creating backup and autosave files
  (setq make-backup-files nil
	auto-save-default nil)

  ;; Default is 4k, which is too low for LSP.
  (setq read-process-output-max (* 1024 1024))

  ;; Improved handling of clipboard
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Avoid noisy bell.
(setq visible-bell t
      ring-bell-function #'ignore)

;; Long text goes below
(global-visual-line-mode t)

;; Trailing white space are banned!
(setq-default show-trailing-whitespace t)

;; Use one space to end sentences.
(setq sentence-end-double-space nil)

;; I typically want to use UTF-8.
(prefer-coding-system 'utf-8)

;; Nicer handling of regions.
(transient-mark-mode 1)

;; Don't show the splash screen
(custom-set-variables
  '(inhibit-startup-screen t))

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

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

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome Home!")
  ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.config/emacs/images/cry2sleep.png")  ;; use custom image as banner
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package diminish)

(use-package direnv
 :config
 (direnv-mode))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

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
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-nix
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-nix-nil-formatter ["alejandra"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(require 'org-tempo)

(use-package projectile
  :config
  (projectile-mode 1))

(use-package rainbow-mode
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))

  ;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
  ;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
  ;; eshell-aliases-file -- sets an aliases file for the eshell.

  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish"))

(use-package vterm
:ensure t
:after elpaca
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000
      vterm-always-compile-module t))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-minor-modes t)
  :init (doom-modeline-mode 1))

(add-to-list 'default-frame-alist '(alpha-background . 90))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

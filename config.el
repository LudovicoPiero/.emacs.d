(setq elpaca-core-date '(20240221))
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                               :ref nil
                               :depth 1
                               :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                               :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
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

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :init
  ;; tweak evil's configuration before loading it
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

;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  ;; Unset keys in evil-motion-state-map
  (mapc (lambda (key) (define-key evil-motion-state-map (kbd key) nil))
        '("SPC" "RET" "TAB")))
;; Setting RETURN key in org-mode to follow links
(setq org-return-follows-link t)

;; Remembering minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; Prevent using UI dialogs for prompts
(setq use-dialog-box nil)

;; Disable lock files (.#filenameblabla)
(setq create-lockfiles nil)

;; Automatically revert buffers when files change on disk
(global-auto-revert-mode t)

;; You can select text and delete it by typing.
(delete-selection-mode 1)

;; Set default indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Configure whitespace display style
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

;; Replace yes-or-no-p with y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Set backup directory and options
(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups
      kept-new-versions      10 ; How many of the newest versions to keep
      kept-old-versions      5) ; How many of the old versions to keep

;; Disable creation of backup and autosave files
(setq make-backup-files nil
      auto-save-default nil)

;; Improved handling of clipboard
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Disable noisy bell
(setq visible-bell t
      ring-bell-function #'ignore)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Use one space to end sentences
(setq sentence-end-double-space nil)

;; Prefer UTF-8 coding system
(prefer-coding-system 'utf-8)

;; Enable transient mark mode for better region handling
(transient-mark-mode 1)

;; Disable line numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Enable automatic parens pairing
(electric-pair-mode 1)

;; Prevent auto-pairing of <>
(add-hook 'org-mode-hook (lambda ()
                            (setq-local electric-pair-inhibit-predicate
                                        `(lambda (c)
                                           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package all-the-icons
  :ensure t)

(use-package company
  :defer 2
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome Home!")
  ;; (setq dashboard-startup-banner 'logo) ;; use standard Emacs logo as banner
  (setq dashboard-startup-banner "~/.config/emacs/images/cry2sleep.png") ;; use custom image as banner
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5)
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dired-open
  :config
  ;; Customize file associations for opening files in Dired
  (setq dired-open-extensions '(("gif" . "imv")
                                ("jpg" . "imv")
                                ("png" . "imv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  ;; Customize key bindings for peep-dired
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(use-package diminish)

(use-package direnv
  :config
  ;; Enable direnv mode globally
  (direnv-mode))

(use-package elcord
  :config
  (setq elcord-quiet t
        elcord-editor-icon "doom_cute_icon"
        elcord-use-major-mode-as-main-icon nil)
  (elcord-mode)

  (defun elcord--disable-elcord-if-no-frames (f)
    "Disable elcord mode if there are no frames left after deleting F from visible-frame-list."
    ;; (declare (ignore f))
    (when (let ((frames (delete f (visible-frame-list))))
            (or (null frames)
                (and (null (cdr frames))
                     (eq (car frames) terminal-frame))))
      (elcord-mode -1)
      (add-hook 'after-make-frame-functions 'elcord--enable-on-frame-created)))

  (defun elcord--enable-on-frame-created (f)
    "Enable elcord mode when a new frame F is created."
    ;; (declare (ignore f))
    (elcord-mode +1))

  (defun my/elcord-mode-hook ()
    "Hook to manage elcord mode activation and deactivation."
    (if elcord-mode
        (add-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)
      (remove-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)))

  (add-hook 'elcord-mode-hook 'my/elcord-mode-hook))

(use-package flycheck
  :after lsp-mode
  :diminish flycheck-mode
  :init (global-flycheck-mode))

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
      "fc" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
      "fr" '(counsel-recentf :wk "Find recent files")
      "ff" '(lsp-format-buffer :wk "Format Buffer") ;; TODO: move this somewhere
      "TAB TAB" '(comment-line :wk "Comment lines"))

  (airi/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "bb" '(switch-to-buffer :wk "Switch to buffer")
    "bc" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "bC" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "bd" '(bookmark-delete :wk "Delete bookmark")
    "bi" '(ibuffer :wk "Ibuffer")
    "bk" '(kill-current-buffer :wk "Kill current buffer")
    "bK" '(kill-some-buffers :wk "Kill multiple buffers")
    "bl" '(list-bookmarks :wk "List bookmarks")
    "bm" '(bookmark-set :wk "Set bookmark")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    "bR" '(rename-buffer :wk "Rename buffer")
    "bs" '(basic-save-buffer :wk "Save buffer")
    "bS" '(save-some-buffers :wk "Save multiple buffers")
    "bw" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (airi/leader-keys
    "d" '(:ignore t :wk "Dired")
    "dd" '(dired :wk "Open dired")
    "dj" '(dired-jump :wk "Dired jump to current")
    "dp" '(peep-dired :wk "Peep-dired"))


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
    "g" '(:ignore t :wk "Git")
    "g/" '(magit-displatch :wk "Magit dispatch")
    "g." '(magit-file-displatch :wk "Magit file dispatch")
    "gb" '(magit-branch-checkout :wk "Switch branch")
    "gc" '(:ignore t :wk "Create")
    "gcb" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "gcc" '(magit-commit-create :wk "Create commit")
    "gcf" '(magit-commit-fixup :wk "Create fixup commit")
    "gC" '(magit-clone :wk "Clone repo")
    "gf" '(:ignore t :wk "Find")
    "gfc" '(magit-show-commit :wk "Show commit")
    "gff" '(magit-find-file :wk "Magit find file")
    "gfg" '(magit-find-git-config-file :wk "Find gitconfig file")
    "gF" '(magit-fetch :wk "Git fetch")
    "gg" '(magit-status :wk "Magit status")
    "gi" '(magit-init :wk "Initialize git repo")
    "gl" '(magit-log-buffer-file :wk "Magit buffer log")
    "gr" '(vc-revert :wk "Git revert file")
    "gs" '(magit-stage-file :wk "Git stage file")
    "gt" '(git-timemachine :wk "Git time machine")
    "gu" '(magit-stage-file :wk "Git unstage file"))

 (airi/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")
    "hrr" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config"))


  (airi/leader-keys
    "s" '(:ignore t :wk "Search")
    "SPC" '(ibuffer :wk "List Buffers")
    "sf" '(find-file :wk "Search File")
    "sg" '(helm-projectile-grep :wk "Search by Grep"))

  (airi/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "td" '(treemacs :wk "Toggle treemacs")
    "te" '(eshell-toggle :wk "Toggle eshell")
    "tl" '(display-line-numbers-mode :wk "Toggle line numbers")
    "tr" '(rainbow-mode :wk "Toggle rainbow mode")
    "tt" '(visual-line-mode :wk "Toggle truncated lines")
    "tv" '(vterm-toggle :wk "Toggle vterm"))

  (airi/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "wc" '(evil-window-delete :wk "Close window")
    "wn" '(evil-window-new :wk "New window")
    "ws" '(evil-window-split :wk "Horizontal split window")
    "wv" '(evil-window-vsplit :wk "Vertical split window")
    ;;Window motions
    "wh" '(evil-window-left :wk "Window left")
    "wj" '(evil-window-down :wk "Window down")
    "wk" '(evil-window-up :wk "Window up")
    "wl" '(evil-window-right :wk "Window right")
    "ww" '(evil-window-next :wk "Goto next window"))
)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
  ;; Define key bindings for Git Time Machine mode
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
  (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package magit)

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :bind
  (("C-c C-r" . ivy-resume)     ;; Resume the last Ivy-based completion
   ("C-x B" . ivy-switch-buffer-other-window))  ;; Switch buffer in another window
  :diminish
  :custom
  (ivy-use-virtual-buffers t)    ;; Enable virtual buffers
  (ivy-count-format "(%d/%d) ")  ;; Format for displaying count
  (enable-recursive-minibuffers t)  ;; Allow recursive minibuffers
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)  ;; Enable Ivy rich mode for descriptions in M-x
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-headerline-breadcrumb-enable t
        lsp-diagnostics-provider 'flycheck
        lsp-completion-provider 'company
        lsp-log-io nil)
  :hook
  ;; Attach LSP mode to the appropriate major mode
  ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-nix
  :ensure nil
  :after lsp-mode
  :custom
  (lsp-nix-nil-formatter ["alejandra"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

;; (use-package css-mode
;;   :mode (("\\.css\\'" . css-mode)))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  (setq web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package js-mode
  :ensure nil
  :mode (("\\.js?\\'" . js-mode)
         ("\\.jsx?\\'" . js-mode))
  :config
  (setq javascript-indent-level 2
        js-indent-level 2))

(use-package typescript-mode
  :mode (("\\.ts?\\'" . typescript-mode)
         ("\\.tsx?\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2
        typescript-auto-indent-flag t))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)
            (setq org-edit-src-content-indentation 0)))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(require 'org-tempo)

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)

  ;; Set your preferred key bindings here
  :bind (("C-c p" . projectile-command-map))

  ;; Additional settings
  :custom
  ;; Define your project root files/directories here
  (projectile-project-root-files '(".projectile" ".git" ".svn" ".hg" "Makefile" "package.json"))

  ;; Enable caching to improve performance
  (projectile-enable-caching t)

  ;; Configure indexing method (default is 'alien for faster indexing)
  (projectile-indexing-method 'alien
)

  ;; Display project name in the modeline
  (projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name)))))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

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

(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands '("bash" "fish"))

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

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :after tree-sitter
  :ensure t
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;(use-package tree-sitter-indentation
;;  :after tree-sitter
;;  :hook (tree-sitter-after-on . tree-sitter-indentation-mode))


(defun my/enable-tree-sitter ()
  "Enable Tree-sitter in `prog-mode'."
  (interactive)
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (tree-sitter-mode)
  (tree-sitter-hl-mode))

(add-hook 'prog-mode-hook #'my/enable-tree-sitter)

(use-package treemacs
  :ensure t
  :config
  ;; Use icons from all-the-icons package
  (setq treemacs-icons-theme 'all-the-icons)
  ;; Adjust icon size (optional)
  (treemacs-resize-icons 14))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode)
  (setq evil-undo-system 'undo-tree))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

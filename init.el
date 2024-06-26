;; -*- lexical-binding: t; -*-
;; init.el --- Emacs Configuration File
;;
;; Copyright (C) 2024 Ludovico Piero
;; Author: Ludovico Piero <lewdovico@gnuweeb.org>
;; URL: https://github.com/ludovicopiero/.emacs.d
;; Git-Repository: git://github.com/ludovicopiero/.emacs.d.git
;; Created: 2024-03-27

;;; Commentary:

;; Configuration files for Emacs.

;;; Code:

;;; config.el -*- lexical-binding: t; -*-
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

  ;; ----- Setting cursor colors
  (setq evil-emacs-state-cursor    '("#649bce" box))
  (setq evil-normal-state-cursor   '("#d9a871" box))
  (setq evil-operator-state-cursor '("#ebcb8b" hollow))
  (setq evil-visual-state-cursor   '("#677691" box))
  (setq evil-insert-state-cursor   '("#eb998b" box))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#ad8beb" box))
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;;(setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil)

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

(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :config (global-centered-cursor-mode))

;; we recommend using use-package to organize your init.el
(use-package codeium
    :ensure nil
    ;; if you use straight
    ;; :elpaca '(:host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(use-package company
  :defer 0.1
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .5)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  :config
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
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

(use-package deadgrep)

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

(use-package flycheck
  :after lsp-mode
  :diminish flycheck-mode
  :init (global-flycheck-mode))

(use-package flycheck-eglot
  :after flycheck
  :config (global-flycheck-eglot-mode))

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
      "fc" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Edit emacs config")
      "fr" '(recentf :wk "Find recent files")
      "ff" '(lsp-format-buffer :wk "Format Buffer") ;; TODO: move this somewhere
      "TAB TAB" '(evilnc-comment-or-uncomment-lines :wk "Comment lines"))

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
    "o" '(:ignore t :wk "ORG Stuff")
    "oa" '(org-agenda :wk "ORG Agenda")
    "oT" '(org-babel-tangle :wk "Tangle ORG File")
    "ot" '(org-todo :wk "Toggle TODO"))

  (airi/leader-keys
    "s" '(:ignore t :wk "Search")
    "SPC" '(ibuffer :wk "List Buffers")
    "sf" '(find-file :wk "Search File")
    "/" '(deadgrep :wk "Search by Grep in the current buffer")
    "sg" '(deadgrep :wk "Search by Grep in the whole project"))

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

(use-package eglot
  :ensure t
)

(use-package lsp-nix
  :ensure nil
  :after lsp-mode
  :custom
  (lsp-nix-nil-formatter ["alejandra"])
)

(use-package nix-mode
  :ensure t
  :hook (nix-mode . eglot-ensure)
  :config
  ;; Ensure `nil` is in your PATH.
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
)

(use-package python-mode
  :ensure nil
  :hook (python-mode .eglot-ensure))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(use-package css-mode
  :ensure nil ;; built-in
  :mode (("\\.css\\'" . css-mode)))

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

(use-package org-modern
  :ensure t
  :config
  (set-face-attribute 'default nil :family "Iosevka q SemiBold")
  (set-face-attribute 'variable-pitch nil :family "Iosevka q")
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka SemiBold")
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)
            (setq org-edit-src-content-indentation 0)))

(use-package org-tempo
  :ensure nil)

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

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 5)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package yasnippet
  :ensure t
  :diminish
  :init
  (yas-global-mode 1)
  :hook (term-mode . (lambda () (yas-minor-mode -1)))
  :config
  (setq yas-snippet-dir (expand-file-name "snippets" user-emacs-directory))
)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package competitive-programming-snippets
  :ensure t
  :after yasnippet
  :config (competitive-programming-snippets-init))

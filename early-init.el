;;; early-init.el --- Emacs early initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; This file, early-init.el, is run before init.el, prior to package and UI initialization.
;; It is used to configure settings that should be set up as early as possible in the Emacs startup process.

;;; Code:

(setq package-enable-at-startup nil)

;; Make ~/.config/emacs clean
(require 'no-littering)
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
(no-littering-theme-backups)
;; Disable GUI elements early to improve startup time
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(dolist (mode '(tool-bar-mode scroll-bar-mode menu-bar-mode blink-cursor-mode))
  (funcall mode 0))

;; Change font early to ensure proper display
(add-to-list 'default-frame-alist '(font . "Iosevka q SemiBold-15"))

;; Set transparency
(add-to-list 'default-frame-alist '(alpha-background . 90))


;; Enable column number mode
(column-number-mode 1)

;; Enable display of line numbers in buffers
(global-display-line-numbers-mode 1)

;; Enable visual line mode globally
(global-visual-line-mode t)

;; Increase process output max for LSP performance
(setq read-process-output-max (* 1024 1024))

;; Disable native-compilation warnings/errors during async compilation
(setq native-comp-async-report-warnings-errors nil)

;; Disable visual bell and startup screen
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

;; Increase GC threshold and percentage to reduce GC pauses during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

;; Function to run GC when Emacs frame loses focus
(defun +gc-after-focus-change ()
  "Run garbage collection when Emacs frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

;; Function to reset initialization values after Emacs initialization
(defun +reset-init-values ()
  "Reset initialization values after Emacs startup."
  ;; Initialize default-file-name-handler-alist if it's not defined
  (unless (boundp 'default-file-name-handler-alist)
    (setq default-file-name-handler-alist file-name-handler-alist))
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

;; Hook to reset initialization values after Emacs Lisp Package Archive (ELPA) initialization
(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; Call the reset function after startup (in case ELPA is already initialized)
(+reset-init-values)

(provide 'early-init)

;;; early-init.el ends here

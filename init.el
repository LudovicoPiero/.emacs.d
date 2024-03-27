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


;; This needs to be before loading config.org so I don't have to give
;; permission to load the file when it is symlinked.
(customize-set-variable
 'vc-follow-symlinks t "Follow Symlinks without asking")

(org-babel-load-file (locate-user-emacs-file "config.org"))

;;; init.el ends here

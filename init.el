;; -*- emacs-lisp -*-

;;; set load path
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/helm")
(add-to-list 'load-path "~/.emacs.d/plugins/git-emacs")
(add-to-list 'load-path "~/.emacs.d/plugins/git-modes")
(add-to-list 'load-path "~/.emacs.d/plugins/neotree")
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-calfw")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16-theme")

(require 'basic-cfg)
(require 'ui-cfg)
(require 'prog-cfg)
(require 'org-cfg)
;;(require 'mail-cfg)
(require 'other-cfg)
(require 'keybind-cfg)
;;(require 'mac-cfg)

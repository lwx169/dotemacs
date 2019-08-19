;; -*- emacs-lisp -*-

(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-fringe-mode 0)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq system-time-locale "C")
(setq column-number-mode t)
(display-time-mode)

;;; window split
(setq split-width-threshold nil)
(setq split-height-threshold 0)
(setq linum-format "%3d ")

;;; hide dos '^M' character
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;; ansi-term color theme
(defface term-color-black 
  '((t (:foreground "#3f3f3f" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "#cc9393" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "#7f9f7f" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "#f0dfaf" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-blue 
  '((t (:foreground "#6d85ba" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-magenta 
  '((t (:foreground "#dc8cc3" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "#93e0e3" :background "#272822"))) 
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "#dcdccc" :background "#272822"))) 
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-green term-color-yellow 
    term-color-blue term-color-magenta term-color-cyan term-color-white])

(provide 'ui-cfg)

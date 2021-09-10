;; setting for macOS

;;; enable golbal menu
(menu-bar-mode 1)

;;; mac key-binding
(setq mac-option-modifier 'hyper)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'ctrl)

;;; set execute path
(setq exec-path (append '("/usr/local/bin") exec-path))
(setq exec-path (append '("/opt/homebrew/bin") exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))

(provide 'macos)

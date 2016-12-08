;; setting for macOS

;;; mac key-binding
(setq mac-option-modifier 'hyper)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'ctrl)

;;; set font
(set-default-font "Monaco-13")
(set-fontset-font "fontset-default" 'chinese-gb2312 "冬青黑体简体中文-16")

;;; set execute path
(setq exec-path (append '("/usr/local/bin") exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(provide 'macos)

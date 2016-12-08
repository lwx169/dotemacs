;; Load operate system related config
(cond
 ((string-equal system-type "gnu/linux")
  (progn
	(message "OS is Linux ...")
	(require 'linux)))
 ((string-equal system-type "darwin")
  (progn
	(message "OS is macOS ...")
	(require 'macos)))
 ((string-equal system-type "windows-nt")
  (progn
	(message "OS is Windows ...")
	(require 'windows))))

(provide 'os-cfg)

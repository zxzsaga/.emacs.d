(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(when (version< emacs-version "24.1")
  (error "Requires at least GNU Emacs 24.1"))

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs configuration.")
(defvar core-dir (expand-file-name "core" root-dir)
  "The home of core functionality.")
(defvar modules-dir (expand-file-name "modules" root-dir)
  "This directory houses all of the built-in modules.")
(defvar themes-dir (expand-file-name "themes" root-dir)
  "This directory houses themes.")
(setq custom-theme-directory themes-dir)
(defvar savefile-dir (expand-file-name "savefile" root-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar backup-dir (expand-file-name "backups" root-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar load-modules-file (expand-file-name "load-modules.el" root-dir)
  "This files contains a list of modules that will be loaded.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(defun add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
		 (not (equal f ".."))
		 (not (equal f ".")))
	(add-to-list 'load-path name)
	(add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; the core stuff
(require 'packages)
(require 'ui)
(require 'core)
(require 'mode)
(require 'editor)
(require 'global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'osx))

;; the modules
(when (file-exists-p load-modules-file)
  (load load-modules-file))

;;; init.el ends here

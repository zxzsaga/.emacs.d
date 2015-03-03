;; define var current-user and check emacs-version >= 24.1
(defvar current-user (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(when (version< emacs-version "24.1") (error "Requires at least GNU Emacs 24.1"))

;; define directories
(defvar root-dir     (file-name-directory load-file-name)   "The root dir of the Emacs configuration.")
(defvar core-dir     (expand-file-name "core" root-dir)     "The home of core functionality.")
(defvar modules-dir  (expand-file-name "modules" root-dir)  "This directory houses all of the built-in modules.")
(defvar vendor-dir   (expand-file-name "vendor" root-dir)   "This directory houses packages that are not yet availabel in ELPA (or MELPA).")
(defvar themes-dir   (expand-file-name "themes" root-dir)   "This directory houses themes.")
(defvar savefile-dir (expand-file-name "savefile" root-dir) "This folder stores all the automatically generated save/history-files.")
(defvar backup-dir   (expand-file-name "backups" root-dir)  "This folder stores all the automatically generated save/history-files.")

;; set custom-theme-directory to themes-dir
(setq custom-theme-directory themes-dir)

;; define load-modules-file
(defvar load-modules-file (expand-file-name "load-modules.el" root-dir)
  "This files contains a list of modules that will be loaded.")

;; mkdir of savefile-dir and backup-dir if they doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))
(unless (file-exists-p backup-dir)
  (make-directory backup-dir))

;; define function add-subfolders-to-load-path
(defun add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
		 (not (equal f ".."))
		 (not (equal f ".")))
	(add-to-list 'load-path name)
	(add-subfolders-to-load-path name)))))

;; add directories to Emacs's `load-path'
(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)
(add-subfolders-to-load-path vendor-dir)

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

;; if load-modules-file exists, load this file
(when (file-exists-p load-modules-file) (load load-modules-file))

;; set default directory
(cd "~/transformer/server")
;; (setq default-directory "~/transformers_server")

;; 代码折叠
(global-set-key [f5] 'hs-toggle-hiding)

;; use ascii install of icon
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width 12)
(sr-speedbar-open)

;; 窗口布局
;; (progn (split-window-horizontally)
;;       (other-window 1)
;;       (shell)
;;       (split-window-vertically)
;;       (other-window 1)
;;       (ielm)
;;       (other-window 1))

(setq helm-alive-p 1)

;; w3m exec-path
(add-to-list 'exec-path "/usr/local/Cellar/w3m/0.5.3/bin")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("dd48b24b2d19e9223be777964068b0d84afcdc21ccd77ea4af411e675c00e2e4" "324c2cfc812a894c0a7c34b7fd25a8b60c0cf40349a65c42114f8883fc724a70" "98c3e4263e11d2474a337dd04d1310911befcd0d0d66d9924408c6f20a3d21ad" "9ea60610b285322536c1245f79c17ae5158aeddf9806b60622803e877ad61059" "190edbd19a171d42c0f73ed3b542280f2e29d87930b6b569267462bb85ae552d" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(org-export-backends (quote (ascii html icalendar latex md))))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cl)
(require 'package)

(require 'sws-mode)
(require 'jade-mode)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar packages
  '(dash
    diminish
    undo-tree
    company
    sr-speedbar
    )
  "A list of packages to ensure are installed at launch.")

(defun check-packages-installed-p ()
  "Check if all packages in `packages' are installed."
  (every #'package-installed-p packages))

(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package packages)
    (add-to-list 'packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'require-package packages))

(define-obsolete-function-alias 'ensure-module-deps 'require-packages)

(defun install-packages ()
  "Install all packages listed in `packages'."
  (unless (check-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (require-packages packages)))

;; run package installation
(install-packages)

(defun list-foreign-packages ()
  "Browse third-party packages not bundled.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list packages)))

(defmacro auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))


(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-hook 'after-init-hook 'global-company-mode)


;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (auto-install extension package mode))))
 auto-install-alist)

(provide 'packages)

;;; packages.el ends here

;;; Package --- Emacs Config
;;; Commentary:
;;; This is my Emacs configuration for work and fun.

;;; Code:
(setq user-full-name "Jonathan Stefani")
(setq user-mail-address "jon.stefani@gmail.com")

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "loading %s..." load-file-name))

(setq debug-on-error t)
(setq require-final-newline t)

;; Get rid of those damn custom variables being written to my init.el file
(defun package--save-selected-packages (&rest opt)
  "Because the custom variables being written to this file piss me off.
`OPT' is optional arguments defined by the system."
  nil)

;; Modern encoding, yay
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Replace Splash Screen with org-mode scratch buffer ;;
(setq inhibit-splash-screen t
	  initial-scratch-message nil
	  initial-major-mode 'org-mode)

;; Package Managers
(defun init-package-manager ()
  "Set up Emacs package manager with use-package."
  (require 'package)
  (setq package-enable-at-startup nil)
  (if (eq system-type 'ms-dos)
          (nil)
          (add-to-list 'package-archives
                       '("marmalade" . "https://marmalade-repo.org/packages/") t))
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (setq-default package-archive-enable-alist '(("melpa" deft magit)))
  (package-initialize))

(load-file "~/.emacs.d/elispConfigFiles/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(init-package-manager)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package delight
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
	:init
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

(defun ui-settings ()
  "GUI settings for Emacs."
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (when window-system
    (scroll-bar-mode -1))
  (blink-cursor-mode 0)
  (global-hl-line-mode t)
  (line-number-mode t)
  (global-linum-mode)
  (column-number-mode t)
  (size-indication-mode t)
  (setq ring-bell-function 'ignore)
  (setq frame-title-format
        '("" (:eval (if (buffer-file-name)
                        (abbreviate-file-name (buffer-file-name))
                      "%b"))))
  (set-face-attribute 'mode-line nil :height 30))

(defun global-editor-settings ()
  "Minor settings for every editing mode."
  (show-paren-mode 1)
  (global-subword-mode 1)
  (global-prettify-symbols-mode t)
  (setq-default tab-width 2)
  (setq-default show-paren-delay 0)
  (setq-default compilation-scroll-output t)
  (setq-default x-select-enable-clipboard t)
  (setq scroll-margin 0)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position 1))

(defun set-theme ()
  "Set up theme."
  (use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-vibrant t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)))

(defun apply-theme (theme-function)
  "Takes the theme set up function and apply it to the proper environemnts.
THEME-FUNCTION: function that initializes the themes and settings."
  (when window-system
    (funcall theme-function))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (funcall theme-function)))
    (funcall theme-function)))

;; UI specific functions
(ui-settings)
(global-editor-settings)
(apply-theme 'set-theme)

(setq-default default-font "Inconsolata")
(if (eq system-type 'darwin)
    (setq-default default-font-size 17)
  (setq-default default-font-size 15))

(defun font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat default-font "-" (number-to-string default-font-size)))

(defun set-font-size ()
  "Set the font to `default-font' at `current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))
(set-font-size)

(use-package multi-term
  :ensure t
  :delight)
(defun multi-term-fish ()
  "Make multiterm use fish."
  (interactive)
  (setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
  (let ((multi-term-program "/usr/local/bin/fish"))
    (multi-term)))

;; Key Bindings ;;
(global-set-key (kbd "C-x p") 'eval-buffer)
(global-set-key (kbd "C-x C-g") 'rgrep)

;; Misc util functions
(defun split-window-below-and-switch ()
  "Split the window horizontally and then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically and then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)

(electric-pair-mode 1)

(use-package yasnippet
  :ensure t
  :delight)

(use-package company
  :ensure t
  :delight
  :config
  (setq-default company-dabbrev-code-other-buffer 'all)
	(setq-default company-code-everywhere t)
  (setq-default company-dabbrev-downcase nil)
	(setq-default company-dabbrev-other-buffers t)
  (setq company-idle-delay 0)
	(setq company-show-numbers t)
	(setq company-minimum-prefix-length 2)
	(setq company-auto-complete nil)
	(global-set-key (kbd "C-<tab>") 'company-dabbrev)
	(global-set-key (kbd "M-<tab>") 'company-complete)
	(global-set-key (kbd "C-c C-y") 'company-yasnippet)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :delight
  :config
  (company-quickhelp-mode 1))

(provide 'init-company)

;; ido part
(use-package ido-completing-read+
	:ensure t
	:init
	(ido-ubiquitous-mode)
	(setq ido-enable-flex-matching t)
	(setq ido-everywhere t)
	(setq ido-create-new-buffer 'always))

(use-package flx-ido
	:ensure t
	:init
	(flx-ido-mode 1))

(use-package ido-vertical-mode
	:ensure t
	:init
	(ido-vertical-mode 1)
	(setq ido-vertical-define-keys 'C-n-and-C-p-only))

(ido-mode 1)

(use-package smex
  :ensure t
  :commands (smex smex-major-mode-commands)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))
(smex-initialize)

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package magit
  :ensure t)

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; indentation adjustments for web-mode
(defun my-web-mode-hook ()
  "Set indentation spacing for web-mode to 2 spaces instead of 4."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; better jsx syntax highlighting
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Improves jsx highlighting for web-mode."
  (if (equal web-mode-content-type "jsx")
	  (let ((web-mode-enable-part-face nil))
		ad-do-it)
	adaad-do-it))

;; flycheck config
(use-package flycheck
	:ensure t
  :delight
	:init
	(setq-default flycheck-highlighting-mode 'lines)
	(setq-default flycheck-disabled-checkers '(javascript-jshint))
	:config
	(add-hook 'after-init-hook 'global-flycheck-mode)
	(flycheck-add-mode 'javascript-eslint 'web-mode)
	(flycheck-add-mode 'javascript-eslint 'js-mode))

(defun use-eslint-from-node-modules ()
  "Function to use local eslint executable if it is available."
  (let* ((root (locate-dominating-file
				(or (buffer-file-name) default-directory)
				"node_modules"))
		 (eslint (and root
					  (expand-file-name "node_modules/eslint/bin/eslint"
										root))))
	(when (and eslint (file-executable-p eslint))
	  (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)

(setq-default indent-tabs-mode nil)
(add-hook 'js-mode-hook
          (lambda ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . js-mode))

(use-package prettier-js
  :ensure t
  :bind ("C-c p" . prettier-js)
  :hook
  (js-mode)
  :config
  (setq-default prettier-js-args
                '("--trailing-comma" "none"
                  "--bracket-spacing" "true"
                  "--tab-width" "2"
                  "--print-width" "90"
                  "--no-semi"
                  "--single-quote"
                  "--arrow-parens" "avoid")))

(setq-default js-indent-level 2)

;; Tern Config ;;
(use-package tern
  :ensure t
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package json-mode
  :ensure t)

;; Css/Scss config ;;
(add-hook 'css-mode-hook
	  (lambda ()
	    (rainbow-mode)
	    (setq-default css-indent-offset 2)))
(add-hook 'scss-mode-hook 'rainbow-mode)
(setq-default scss-compile-at-save nil)

;; adding a less config
(use-package less-css-mode
  :ensure t)

(use-package auto-highlight-symbol
  :ensure t
  :init
  (auto-highlight-symbol-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :delight
  :init
  (global-whitespace-cleanup-mode 1))

(use-package discover
  :ensure t
  :init
  (global-discover-mode))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'gfm-mode 'flyspell-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Ivy Config
(use-package ivy
  :ensure t
  :delight
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;; Php-mode ;;
(defun php-editing-config ()
  "Php editing package configurations."
  (setq-default php-executable "/usr/bin/php")
  (use-package php-mode
    :ensure t
    :config
    (add-hook 'php-mode-hook 'flycheck-mode))
  ;; (load-file "~/.emacs.d/elispConfigFiles/php-extras-gen-eldoc.el"))
  (if (eq (memq window-system '(ms-dos)) nil)
      (use-package php-extras
        :ensure t)))

(php-editing-config)

;; Tramp Customizations ;;
(setq-default tramp-verbose 10)

;; rainbows ;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'elisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'php-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'css-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook 'rainbow-delimiters-mode))

(defun load-ssh-file ()
  "Requires file containing functions for connecting to servers over ssh."
  (load-file "~/.emacs.d/elispConfigFiles/secrets/ssh-connects.el")
  (require 'ssh-connects))

(if (file-exists-p "~/.emacs.d/elispConfigFiles/secrets/ssh-connects.el")
    (load-ssh-file))

(use-package geiser
  :ensure t)

(use-package quack
  :ensure t)

;; (use-package pomodoro
;;   :ensure t)

;; Notes for using my Emacs ;;
;; Remember, to look up a function, use C-h f. This will allow you to look up functions. ;;
;; Use C-SPC C-SPC to set a marked location, navigate away. Then hit C-u C-SPC and you will return to the marked position ;;

(provide 'init)
;;; init.el ends here

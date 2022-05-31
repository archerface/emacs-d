;;; Package --- Emacs Config
;;; Commentary:
;;; This is my Emacs configuration for work and fun.

;;; Added this to mess with pull requests.
;;; Code:
(setq user-full-name "Jonathan Stefani")
(setq user-mail-address "jon.stefani@gmail.com")

(defconst emacs-start-time (current-time))
(unless noninteractive (message "loading %s..." load-file-name))
(setq debug-on-error t)
(setq require-final-newline t)

;; Get rid of those damn custom variables being written to my init.el file
;; (defun package--save-selected-packages
;;     "Because the custom variables being written to this file piss me off. `OPT' is optional arguments defined by the system."
;;   (&rest opt)
;;     nil)

;; Modern encoding, yay
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Mac Stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq byte-compile-warnings '(cl-functions))

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; Replace Splash Screen with org-mode scratch buffer ;;
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(setq packages-to-install '(use-package))

;; Package Managers
(defun init-package-manager ()
  "Set up Emacs package manager with use-package."
  (require 'package)
  (setq package-enable-at-startup nil)
  (setq package-archives
        '(("GNU ElPA" . "https://elpa.gnu.org/packages/")
          ("MELPA" . "http://melpa.org/packages/")
          ("MELPA Stable" . "http://stable.melpa.org/packages/"))
        package-archive-priorities
        '(("GNU ELPA" . 10)
          ("MELPA Stable" . 5)
          ("MELPA". 0)))
  (setq-default package-archive-enable-alist '(("melpa" deft magit)))
  (package-initialize)
  (when (cl-find-if-not #'package-installed-p packages-to-install)
    (package-refresh-contents)
    (mapc #'package-install packages-to-install)))

(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(init-package-manager)

(use-package delight
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
	:init
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

(defun ui-settings ()
  "GUI settings for Emacs."
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (menu-bar-mode -1)
  (when window-system
    (scroll-bar-mode -1))
  (blink-cursor-mode -1)
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
  (setq-default tab-width 2
                show-paren-delay 0
                compilation-scroll-output t
                x-select-enable-clipboard t)
  (setq scroll-margin 1
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1)
  (electric-pair-mode t))

(defun set-theme ()
  "Set up theme."
  (fringe-mode 10)
  ;; (setq solarized-distinct-fringe-background t)
  ;; (setq solarized-high-contrast-mode-line t)
  (use-package beacon
    :ensure t
    :config
    (beacon-mode t))
  (use-package which-key
    :ensure t
    :config
    (which-key-mode t))
  (setq x-underline-at-descent-line t)
  (use-package dracula-theme
    :ensure t
    :config
    (load-theme 'dracula t))
  (use-package nyan-mode
    :ensure t
    :delight
    :config
    (setq nyan-animate-nyancat 't
          nyan-wavy-trail t
          nyan-animation-frame-interval 0.1
          nyan-minimum-window-width 100)
    (nyan-mode t))
  (use-package indent-guide
    :ensure t
    :config
    (setq indent-guide-char "|")
    (indent-guide-global-mode t)))

(defun set-init-frame-size ()
  "Set initial window frame size for graphical display."
  (if (display-graphic-p)
      (progn
        (setq initial-frame-alist
              '(
                (tool-bar-lines . 0)
                (width . 200) ; width in chars
                (height . 125) ; height in lines
                (background-color . "honeydew")
                (left . 50)
                (top 50)))
        (setq default-frame-alist
              '(
                (tool-bar-lines . 0)
                (width . 200)
                (height . 125)
                (background-color . "honeydew")
                (left . 50)
                (top . 50))))
    (progn
      (setq initial-frame-alist '( (tool-bar-lines . 0)))
      (setq default-frame-alist '( (tool-bar-lines . 0))))))

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

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-attractive)
  (setq-default sublimity-attractive-centering-width 110)
  (sublimity-mode t))

(setq-default default-font "Fira Code Retina")
(setq-default default-font-size 13)

(defun font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (let ((font default-font) (font-size default-font-size))
    (concat font "-" (number-to-string font-size))))

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

;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (show-smartparens-global-mode t))

(use-package yasnippet
  :ensure t
  :delight)

(use-package company
  :ensure t
  :delight
  :init
  (global-company-mode)
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
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-semantic company-backends))
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :delight
  :config
  (company-quickhelp-mode 1))

;; (use-package company-lsp
;;  :ensure t
;;  :config
;;  (push 'company-lsp company-backends))
(provide 'init-company)

(use-package cedet
  :ensure t)

(use-package semantic
  :ensure t
  :config
  (global-semanticdb-minor-mode t)
  (global-semantic-idle-scheduler-mode t)
  (global-semantic-idle-summary-mode t)
  (semantic-mode t))

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
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-enable-auto-pairing t))

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

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))
;; (setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 5)

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :ensure t
  :config
  (progn
    (define-key js-mode-map (kbd "M-.") nil)
    (add-hook 'js2-minor-mode-hook
              (lambda ()
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

(use-package tern
  :ensure t
  :config
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package indium
  :ensure t
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode))

;; flycheck config
(defun flycheck-c-variables ()
  "Set flycheck settings for c/c++."
  (setq flycheck-clang-include-path
        '((getenv("NODE_PATH"))))
  (setq flycheck-clang-language-standard "c++11"))

(use-package flycheck
	:ensure t
  :delight
	:init
	(setq-default flycheck-highlighting-mode 'lines)
	(setq-default flycheck-disabled-checkers '(javascript-jshint))
	:config
	(add-hook 'after-init-hook 'global-flycheck-mode)
	(flycheck-add-mode 'javascript-eslint 'web-mode)
	(flycheck-add-mode 'javascript-eslint 'js-mode)
	(flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-c-variables))

(use-package lsp-ui
  :requires (lsp-mode flycheck)
  :ensure t
	:config
	(add-hook 'lsp-mode-hook 'lsp-ui-mode))

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

(setq-default js-indent-level 2)

(use-package json-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package rainbow-mode
  :ensure t)

;; Css/Scss config
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

(use-package projectile
  :ensure t
  :delight
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-mode t))

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
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c++-mode-hook 'rainbow-delimiters-mode))

(use-package geiser
  :ensure t
  :config
  (add-hook 'scheme-mode-hook (lambda (run-geiser))))

;; (use-package quack
;;   :ensure t)

;; (use-package racket-mode
;;   :ensure t
;;   :config
;;   (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
;;   (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

(use-package cider
  :ensure t)

(use-package pomidor
  :ensure t
  :init
  (setq alert-default-style 'libnotify)
  :config
  (global-set-key (kbd "<f5>") #'pomidor)
  (setq pomidor-play-sound-file
        nil))

(setq-default inferior-lisp-program "~/Documents/programs/ccl/lx86cl64")
(setq slime-contribs '(slime-fancy))
(use-package slime
  :ensure t
  :config
  (add-hook 'slime-mode-hook
            (lambda ()
              (unless (slime-connected-p)
                (save-excursion (slime))))))

;; requires libclang-dev to be installed on the system
(defun custom-irony-mode-hook ()
  "Make use of irony's async options."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(use-package irony
  :ensure t
  :config
  (use-package company-irony
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package flycheck-irony
    :ensure t
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  (use-package company-irony-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'irony-mode-hook 'custom-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package clang-format
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda () (add-hook 'before-save-hook
                            'clang-format-buffer))))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; TODO: write a function that will automatically setup a GTAGS db for a project.
(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
                (ggtags-mode 1))))
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  ;; use this in conjunction with M-.
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go
  :ensure t)

(use-package lsp-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'rust-mode-hook #'lsp)
  (setq rust-format-on-save t))

;; Notes for using my Emacs ;;
;; Remember, to look up a function, use C-h f. This will allow you to look up functions. ;;
;; Use C-SPC C-SPC to set a marked location, navigate away. Then hit C-u C-SPC and you will return to the marked position ;;

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" default))
 '(package-selected-packages
   '(dracula-theme rust-mode magit yasnippet whitespace-cleanup-mode web-mode vue-mode use-package tern solarized-theme smex rainbow-delimiters racket-mode quack prettier-js pomodoro pomidor php-extras org-bullets multi-term lsp-ui less-css-mode json-mode ido-vertical-mode ido-completing-read+ geiser flx-ido exec-path-from-shell doom-themes discover diff-hl delight counsel company-quickhelp company-lsp color-theme-sanityinc-tomorrow auto-highlight-symbol)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "dark slate gray")))))

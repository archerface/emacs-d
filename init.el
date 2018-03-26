;;; Package --- Emacs Config
;;; Commentary:
;;; This is my Emacs configuration for work

;;; Code:
(setq user-full-name "Jonathan Stefani")
(setq user-mail-address "jon.stefani@gmail.com")

(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "loading %s..." load-file-name))

(setq debug-on-error t)
(setq require-final-newline t)

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
  (add-to-list 'package-archives
	       '("marmalade" . "https://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (setq package-archive-enable-alist '(("melpa" deft magit)))
  (package-initialize))

(load-file "~/.emacs.d/elispConfigFiles/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(init-package-manager)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun ui-settings ()
  "GUI settings for Emacs."
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (when window-system
    (scroll-bar-mode -1))
  (blink-cursor-mode 0)
  (global-hl-line-mode t)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (setq ring-bell-function 'ignore)
  (setq frame-title-format
        '("" (:eval (if (buffer-file-name)
                        (abbreviate-file-name (buffer-file-name))
                      "%b")))))

(defun global-editor-settings ()
  "Minor settings for every editing mode."
  (show-paren-mode 1)
  (global-subword-mode 1)
  (global-prettify-symbols-mode t)
  (setq show-paren-delay 0)
  (setq scroll-margin 0)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position 1)
  (setq compilation-scroll-output t)
  (setq x-select-enable-clipboard t)
  (setq-default tab-width 2))

(defun set-solarized-theme ()
  "Set up and run solarized theme."
  (fringe-mode 15)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic nil)
  (setq solarized-emphasize-indicators t)
  (setq solarized-scale-org-headlines nil)
  (setq x-underline-at-descent-line t)
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-dark t)))

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
(apply-theme 'set-solarized-theme)

;; A lot of font modifications, all pretty much stolen from:
;; https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org
(setq default-font "Inconsolata")
(setq default-font-size 15)
(setq current-font-size default-font-size)
(setq font-change-increment 1.1)

(defun font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat default-font "-" (number-to-string current-font-size)))

(defun set-font-size ()
  "Set the font to `default-font' at `current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun reset-font-size ()
  "Change font size back to `default-font-size'."
  (interactive)
  (setq current-font-size default-font-size)
  (set-font-size))

(defun increase-font-size ()
  "Increase current font size by a factor of `font-change-increment'."
  (interactive)
  (setq current-font-size
        (ceiling (* current-font-size font-change-increment)))
  (set-font-size))

(defun decrease-font-size ()
  "Decrease current font size by a factor of `font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq current-font-size
        (max 1
             (floor (/ current-font-size font-change-increment))))
  (set-font-size))

(define-key global-map (kbd "C-)") 'reset-font-size)
(define-key global-map (kbd "C-+") 'increase-font-size)
(define-key global-map (kbd "C-=") 'increase-font-size)
(define-key global-map (kbd "C-_") 'decrease-font-size)
(define-key global-map (kbd "C--") 'decrease-font-size)

(reset-font-size)

(mode-icons-mode 1)
(use-package all-the-icons)

(use-package multi-term)
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

(require 'req-package)
(req-package company
	     :require yasnippet
	     :config (progn (global-company-mode 1)
			    (setq company-idle-delay 0)
			    (setq company-show-numbers t)
			    (setq company-minimum-prefix-length 2)
			    (setq company-dabbrev-downcase nil)
			    (setq company-dabbrev-other-buffers t)
			    (setq company-auto-complete nil)
			    (setq company-dabbrev-code-other-buffer 'all)
			    (setq company-code-everywhere t)
			    (setq company-code-ignore-case t)
			    (global-set-key (kbd "C-<tab>") 'company-dabbrev)
			    (global-set-key (kbd "M-<tab>") 'company-complete)
			    (global-set-key (kbd "C-c C-y") 'company-yasnippet)))

(req-package company-quickhelp
	     :require company
	     :config (company-quickhelp-mode 1))

(provide 'init-company)
(add-hook 'after-init-hook 'global-company-mode)

;; ido part
(ido-mode 1)
(ido-ubiquitous-mode)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :commands (smex smex-major-mode-commands)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))
(smex-initialize)


(require 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; flycheck config
(defun flycheck-settings ()
  "Defines flycheck customizations for Emacs."
  (require 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlint))))

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

(add-hook 'web-mode-hook (lambda () (interactive) (column-marker-1 100)))
(add-hook 'js-mode-hook (lambda () (interactive) (column-marker-1 100)))

;; Tern Config ;;
(add-to-list 'load-path "~/Documents/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; Css/Scss config ;;
(add-hook 'css-mode-hook
	  (lambda ()
	    (rainbow-mode)
	    (setq css-indent-offset 2)))
(add-hook 'scss-mode-hook 'rainbow-mode)
(setq scss-compile-at-save nil)

;; adding a less config
(require 'less-css-mode)

(auto-highlight-symbol-mode 1)
(global-whitespace-cleanup-mode 1)

(require 'discover)
(global-discover-mode)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(require 'org-alert)
(setq alert-default-style 'libnotify)

;; diff hl config ;;
(require 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(add-hook 'gfm-mode 'flyspell-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Ivy Config
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 15)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-initial-inputs-alist nil)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
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
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Php-mode ;;
(load-file "~/.emacs.d/elispConfigFiles/php-mode.el")
(load-file "~/.emacs.d/elispConfigFiles/php-extras.el")
(load-file "~/.emacs.d/elispConfigFiles/php-extras-gen-eldoc.el")

(require 'php-mode)
(require 'php-extras)

(setq php-executable "/usr/bin/php")
(add-hook 'php-mode-hook 'flycheck-mode)

;; Flycheck Mode ;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Tramp Customizations ;;
(setq tramp-verbose 10)

;; rainbows ;;
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'elisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'php-mode-hook 'rainbow-delimiters-mode)
(add-hook 'css-mode-hook 'rainbow-delimiters-mode)
(add-hook 'web-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js-mode-hook 'rainbow-delimiters-mode)
(add-hook 'rust-mode-hook 'rainbow-delimiters-mode)

;; rust settings ;;
(require 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Encryption Stuff ;;
(require 'epa-file)
(epa-file-enable)

;; Language Sever Protocol ;;
(add-to-list 'load-path "~/Documents/lsp-mode/")
(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck))
(require 'lsp-mode)
(add-hook 'prog-major-mode #'lsp-mode)

;; LSP python
(require 'lsp-python)
(add-hook 'python-mode-hook #'lsp-mode)

(load-file "~/.emacs.d/elispConfigFiles/secrets/ssh-connects.el")
(require 'ssh-connects)

; ################################ ;
;;;; Emacs Config No Man's Land ;;;;
; ###############################  ;

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
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (zenburn beacon zenburn-theme yaml-mode whitespace-cleanup-mode web-mode web-beautify tern solarized-theme smooth-scrolling smex slime scss-mode restclient req-package rainbow-mode rainbow-delimiters racer prettier-js powerline phpcbf php-extras panda-theme org-bullets org-alert nord-theme neotree multi-term moe-theme mode-icons markdown-mode magit lsp-python lsp-javascript-typescript less-css-mode json-mode jsfmt js-format irony-eldoc indent-guide ido-vertical-mode ido-ubiquitous highlight-symbol highlight-indent-guides helm gruber-darker-theme git-messenger git-gutter geben flycheck-pos-tip flycheck-popup-tip flycheck-color-mode-line flx-ido exec-path-from-shell eslintd-fix eslint-fix engine-mode eclim dumb-jump dracula-theme discover diff-hl counsel-projectile company-racer company-php company-lsp company-irony company-flx column-marker color-theme-sanityinc-tomorrow cmake-ide cider base16-theme avy auto-highlight-symbol apropospriate-theme all-the-icons-ivy))))

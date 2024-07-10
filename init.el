

(setq inhibit-startup-message t)

(scroll-bar-mode -1)      ; Disable visible scrollbar
(tool-bar-mode -1)        ; Disable the toolbar
(tooltip-mode -1)         ; Disable tooltip
(set-fringe-mode 10)      ; Give some breathing room (Ränder links/rechts)

(menu-bar-mode -1)        ; Disable the menu bar

(setq visible-bell t)     ; set up the visible bell

(set-face-attribute 'default nil :font "Fira Code Retina" :height 105)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(load-theme 'tango-dark)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)
(setq use-package-always-ensure t)


;; --- line number --------------------------------------------
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; --- rainbow-delimiters -------------------------------------
;; The recognisability depends on the appearance(theme)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; --- ivy ----------------------------------------------------
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
   (ivy-mode)
   (bind-keys
    :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-linex)
    ("C-d" . ivy-reverse-i-search-kill)))


;; --- ivy-rich -----------------------------------------------
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; --- counsel ------------------------------------------------
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searching with ^


;; --- doom-modeline ------------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))


;;(use-package doom-themes)  ;;still using tango-dark


;; --- which-key ----------------------------------------------
(use-package which-key
  :init (which-key-mode)
  :diminish whick-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; --- helpful ------------------------------------------------
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes helpful counsel swiper ivy-rich which-key rainbow-delimiters command-log-mode doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

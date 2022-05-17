;;; init.el --- Where all the magic begins
;;
;; This file allows Emacs to initialize my customizations
;; in Emacs lisp embedded in *one* literate Org-mode file.


;; -----------------------------------------------------------------------------
;; User info
;;
(setq user-full-name "Adalbert Soborka"
      user-mail-address "asoborka@gmx.de")

(setq gc-cons-threshold 100000000)



;; -----------------------------------------------------------------------------
;; * General Configuration
;; ** User Interface
;; *** Clean up Emacs user interface,  make it more minimal

;;
;; Startup Messages
;;
(setq inhibit-startup-message t )

;; -----------------------------------------------------------------------------
;; Turn off unnecessary graphical features
;;
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(set-fringe-mode 10)        ; Give some breathing room


;; -----------------------------------------------------------------------------
;; Bell
;;(setq ring-bell-function 'ignore)
(setq visible-bell t)


;; -----------------------------------------------------------------------------
;; Font
;;
;;(set-face-attribute 'default nil :height 90);;93
(set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 87)
(setq-default line-spacing 3)


;; -----------------------------------------------------------------------------
;; Initialize package sources
;; Use Package
;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(setq load-prefer-newer t)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;; This sets up the load path so that we can override it
;; (package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp/")


(global-display-line-numbers-mode t)
(column-number-mode 1)

;; -----------------------------------------------------------------------------
;; Disable line numbers for some modes.
;;
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; -----------------------------------------------------------------------------
;; misc useful keybindings
;;
(global-set-key (kbd "C-d") 'kill-whole-line)           ; Zeile löschen
(global-set-key (kbd "<f12>") 'calculator)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)


;; -----------------------------------------------------------------------------
;; ivy
;;
;;   Swiper is an easy way to search through the current buffer.
;;          Very similar to Helm's swoop package.
(use-package ivy
  :diminish            ; vermindern
  :bind (("C-x s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ;; ("C-j" . ivy-next-line)
         ;; ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ;; ("C-j" . ivy-next-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ;; ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; -----------------------------------------------------------------------------
;; ivy-rich : More friendly interface for ivy
;;            https://github.com/Yevgnen/ivy-rich
;;
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; -----------------------------------------------------------------------------
;; ivy-rich : More friendly interface for ivy
;;            https://github.com/Yevgnen/ivy-rich
;;
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x f" . counsel-ag)         ;; search over the whole project-directory
         ("C-x C-f" . counsel-find-file)
         ("C-c r" . counsel-recentf)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ;; Don't start searches with ^


(use-package all-the-icons
  :if (display-graphic-p))


(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)) ;


;; -----------------------------------------------------------------------------
;; doom modeline
;;
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))


;; -----------------------------------------------------------------------------
;; Doom-themes are a modern set of themes for emacs.
;;
(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (progn
    (doom-themes-neotree-config)
    (setq doom-neotree-line-spacing 0)
    (doom-themes-org-config)))


;; -----------------------------------------------------------------------------
;; which-key - bring up help for key bindings
;;
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; -----------------------------------------------------------------------------
;; general
;; ohne evil funktioniert nicht !!! schade
;;
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer as/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    ;; :global-prefix "M-c")
    :global-prefix "C-SPC")

  (as/leader-key-def
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))


;; ----------------------------------------------------------------------------
;; Projectile
;;
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :demand t
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "c:/Projekte")
    (setq projectile-project-search-path '("c:/Projekte")))
  (setq projectile-switch-project-action #'projectile-dired))


(use-package counsel-projectile
;;  :disabled
  :after projectile
  :config
  (counsel-projectile-mode))


;; -----------------------------------------------------------------------------
;; Magit
;;
;; (use-package magit
;;   :ensure t
;;   :bind (("C-x g" . magit-status)))
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (setenv "GIT_ASKPASS" "git-gui--askpass")

;; -----------------------------------------------------------------------------
;; git-timemachine
;; Travel back and forward in git history of the current file
;;
;; p visit previous historic version
;; n visit next historic version
;; w copy the hash of the current historic version
;; q exit the time machine buffer
;;
(use-package git-timemachine
  :ensure t
  :bind (("C-x t t" . git-timemachine)))


;; -----------------------------------------------------------------------------
;;
;; Git Auto Commit Mode
(use-package git-auto-commit-mode
  :delight)


;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

;; check searchprog ag
;; check searchprog rg

;; -----------------------------------------------------------------------------
;; Detection Operating System
;;
(defun is-linux-p
    ()
  (eq system-type 'gnu/linux))

(defun is-windows-p
    ()
  (or
   (eq system-type 'ms-dos)
   (eq system-type 'windows-nt)
   (eq system-type 'cygwin)))


;; (defconst *sys/win32*
;;   (eq system-type 'windows-nt)
;;   "Are we running on a WinTel system?")

;; -----------------------------------------------------------------------------
;; Theme
;;
;;(load-theme 'tango-dark)       ;; dark background thema
;;(set-cursor-color "#77ff00")   ;; set cursor color to white

;; test csv
(setq csv-separators '(";" "    "))
(add-hook 'csv-mode-hook
          (lambda ()
            (define-key csv-mode-map (kbd "C-c C-M-a")
              (defun csv-align-visible (&optional arg)
                "Align visible fields"
                (interactive "P")
                (csv-align-fields nil (window-start) (window-end))
                )
              )
            )
          )




;; -----------------------------------------------------------------------------
;; Prettify Symbols
;;
(global-prettify-symbols-mode +1)


;; -----------------------------------------------------------------------------
;; Backups
;;
(setq delete-old-versions t
      kept-new-versions 20
      kept-old-versions 10
      version-control t
      auto-save-default nil
      backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))


;; -----------------------------------------------------------------------------
;; Yes or Not
(fset 'yes-or-no-p 'y-or-n-p)


;; -----------------------------------------------------------------------------
;; Encoding
;;
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)


;; -----------------------------------------------------------------------------
;; --- use space to indent by default
;;
(setq-default indent-tabs-mode nil)


;; -----------------------------------------------------------------------------
;; show unncessary whitespace that can mess up your diff
;;
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))




;; -----------------------------------------------------------------------------
;; Smooth scrolling (line by line) instead of jumping by half-screens.
;;
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; -----------------------------------------------------------------------------
;; show unncessary whitespace that can mess up your diff
;;
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 5)))


;; -----------------------------------------------------------------------------
;; --- use space to indent by default
(setq-default indent-tabs-mode nil)


;; -----------------------------------------------------------------------------
;; --- highlight the cursor whenever the window scrolls
;;
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t))


;; -----------------------------------------------------------------------------
;; Highlight Current Line (from emacsredux.com)
;;
(global-hl-line-mode +1)
(set-face-background 'hl-line "#3e4446")     ; change color
(set-face-foreground 'highlight nil)


;; -----------------------------------------------------------------------------
;;
(windmove-default-keybindings)

;; -----------------------------------------------------------------------------
;;
(setq ls-lisp-use-insert-directory-program t)


;; -----------------------------------------------------------------------------
;; Make C-n insert new lines if the point is at the end of the buffer.
(setq next-line-add-newlines t)


;; -----------------------------------------------------------------------------
;; CUA
;;
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands


;; -----------------------------------------------------------------------------
;; Eshell
;;
(use-package eshell
  :bind (("<f1>" . eshell))
  :hook ((eshell-mode . with-editor-export-editor)
	 (eshell-mode . setup-company-eshell-autosuggest))
  :init
  (setq eshell-banner-message "")

  (defun new-eshell ()
    (interactive)
  (eshell 'true))

  (use-package esh-autosuggest
    :hook (eshell-mode . esh-autosuggest-mode)))


;; -----------------------------------------------------------------------------
;; example of setting env var named “path” by prepending new paths to existing paths
;; REF: http://ergoemacs.org/emacs/eshell.html
;;
(when (window-system)
  (getenv "PATH")         ;; show env var named path

  (setenv "PATH"
          (concat
           "C:/home" ";"
           "C:/cygwin/usr/local/bin" ";"
           "C:/cygwin/usr/bin" ";"
           "C:/cygwin/bin" ";"
           "C:/tools/openjdk-14.0.2/bin" ";"
           "C:/Qt/Qt5.15.2/5.15.2/msvc2019_64/include" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/IDE/Extensions/Microsoft/IntelliCode/CLI" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/14.29.30133/bin/HostX64/x64" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/IDE/VC/VCPackages" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/IDE/CommonExtensions/Microsoft/TestWindow" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/MSBuild/Current/bin/Roslyn" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Team Tools/Performance Tools/x64" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Team Tools/Performance Tools" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/Tools/devinit" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/MSBuild/Current/Bin" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/IDE" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/Common7/Tools" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/Shared/Common/VSPerfCollectionTools/vs2019" ";"
           ;; "C:/Program Files (x86)/Microsoft Visual Studio/Shared/Common/VSPerfCollectionTools/vs2019/x64" ";"
           ;; "C:/Program Files (x86)/Microsoft SDKs/Windows/v10.0A/bin/NETFX 4.8 Tools/x64" ";"
           ;; "C:/Program Files (x86)/Windows Kits/10/bin/10.0.18362.0/x64" ";"
           ;; "C:/Program Files (x86)/Windows Kits/10/bin/x64" ";"
           ;; "C:/Windows/Microsoft.NET/Framework64/v4.0.30319" ";"
           ;; "C:/Qt/Qt5.15.2/5.15.2/msvc2019_64/bin" ";"
           (getenv "PATH") ; inherited from OS
           )))


;; -----------------------------------------------------------------------------
;; Dired
;;
(use-package dired
  :ensure f
  :bind (("<f2>" . dired)
	 ("C-x C-d" . dired)
	 :map dired-mode-map
	 ("C-x o" . ace-window)
	 ("<return>" . dired-find-alternate-file)
	 ("'" . wdired-change-to-wdired-mode)
	 ("s-/" . dired-filter-mode))
  :config
  (bind-key "^" (lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (put 'dired-find-alternate-file 'disabled nil)
  ;; (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-dwim-target t
	dired-recursive-deletes 'always
	dired-recursive-copies 'always
	dired-isearch-filenames t
	dired-listing-switches "-alh"
	;; dired-omit-files-p t
	;; dired-omit-files "\\|^.DS_STORE$\\|^.projectile"
	)
  ;; (use-package dired+
  ;;   :init
  ;;   (setq diredp-hide-details-initially-flag t)) ;; also automatically calls dired-x, enabling dired-jump, C-x C-j
  (use-package dired-details
    :disabled t
    :init
    (dired-details-install))
  (use-package dired-filter)
  (use-package dired-subtree
    :init
    (unbind-key "M-O" dired-mode-map) ;; to support mode-line-other-buffer in Dired
    (bind-keys :map dired-mode-map
	       :prefix "C-,"
	       :prefix-map dired-subtree-map
	       :prefix-docstring "Dired subtree map."
	       ("C-i" . dired-subtree-insert)
	       ("i" . dired-subtree-insert)
	       ("C-/" . dired-subtree-apply-filter)
	       (";" . dired-subtree-remove)
	       ("C-k" . dired-subtree-remove)
	       ("C-n" . dired-subtree-next-sibling)
	       ("C-p" . dired-subtree-previous-sibling)
	       ("C-u" . dired-subtree-up)
	       ("C-d" . dired-subtree-down)
	       ("C-a" . dired-subtree-beginning)
	       ("C-e" . dired-subtree-end)
	       ("m" . dired-subtree-mark-subtree)
	       ("u" . dired-subtree-unmark-subtree)
	       ("C-o C-f" . dired-subtree-only-this-file)
	       ("C-o C-d" . dired-subtree-only-this-directory)))
  (use-package dired-quick-sort)
;;    :init
;;    (dired-quick-sort-setup))
  (use-package dired-collapse
    :hook dired-mode))


;; -----------------------------------------------------------------------------
;; duplicate-thing - bring up help for key bindings
;;
(use-package duplicate-thing
  ;; :ensure t
  ;; :demand t
  ;; :config
  :bind ("M-<return>" . duplicate-thing))


;; -----------------------------------------------------------------------------
;; drag-stuff
;; Drag Stuff is a minor mode for Emacs that makes it possible to drag stuff
;; (words, region, lines) around in Emacs.
;; Thanks to Johan Andersson
;;
(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; -----------------------------------------------------------------------------
;; Package: undo-tree
;; C-x u: undo tree vizualization
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


;; -----------------------------------------------------------------------------
;; Package: clean-aindent-mode
;; Simple indent, if activated, will bypass the default language dependent indentation
;; of ‘newline-and-indent’.
;;
(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))


;; -----------------------------------------------------------------------------
;; Package: dtrt-indent
;;
(use-package dtrt-indent
  :init
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))


;; -----------------------------------------------------------------------------
;; PACKAGE: comment-dwim-2
;;
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2))
  )


;;(set-background-color "blue")
;; -----------------------------------------------------------------------------
;; PACKAGE: iedit
;;
(use-package iedit
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))


;; -----------------------------------------------------------------------------
;; Title Bar
(setq-default frame-title-format '("SA-EMACS - " user-login-name "@" system-name " - %b"))


;; -----------------------------------------------------------------------------
;; smartparens
;;   C-M-n: Move forward over a parenthetical group (forward-list).
;;   C-M-p: Move backward over a parenthetical group (backward-list).
;;   C-M-u: Move up in parenthesis structure (backward-up-list).
;;   C-M-d: Move down in parenthesis structure (down-list).
;;
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands smartparens-global-mode
  :config
  (require 'smartparens-config)
  :init
  (show-paren-mode 1)
  (smartparens-global-mode 1))


;; -----------------------------------------------------------------------------
;; show file name
;;
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want


;; ----------------------------------------------------------------------------
;; Semantic
;;  http://tuhdo.github.io/c-ide.html#sec-9-2
;;
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(global-semantic-idle-summary-mode 1)
;; Semantic Sticky Function minor mode displays a header line that shows
;; the declaration line of the function or tag on the topmost line in the text area.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; Semantic Idle Summary mode is a minor mode that displays a short summary of the
;; symbol at point, such as its function prototype
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(semantic-mode 1)


;; -----------------------------------------------------------------------------
;; Yasnippet
;;
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


;; -----------------------------------------------------------------------------
;; Flycheck
;;
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;XXX: check this
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-gcc-include-path
                      (list (expand-file-name "c:/Qt/Qt5.15.2/5.15.2/msvc2019_64/include/QtWidgets")))))


;; -----------------------------------------------------------------------------
;; NeoTree - A tree plugin like NerdTree for Vim
;;
(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-window-fixed-size nil))
  :bind
  ("C-p" . neotree-toggle)
  ("C-c n" . neotree-projectile-action))


;; -----------------------------------------------------------------------------
;; ORG Mode
;;
(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c M-k" . org-cut-subtree)
	 ;;	 ("<down>" . org-insert-todo-heading)
	 :map org-mode-map
	 ("C-c >" . org-time-stamp-inactive))
  :custom-face
  (variable-pitch ((t (:family "ETBembo"))))
  (org-document-title ((t (:foreground "#171717" :weight bold :height 1.5))))
  (org-done ((t (:background "#E8E8E8" :foreground "#0E0E0E" :strike-through t :weight bold))))
  (org-headline-done ((t (:foreground "#171717" :strike-through t))))
  (org-level-1 ((t (:foreground "#090909" :weight bold :height 1.3))))
  (org-level-2 ((t (:foreground "#090909" :weight normal :height 1.2))))
  (org-level-3 ((t (:foreground "#090909" :weight normal :height 1.1))))
  (org-image-actual-width '(600))
  :init
  (setq default-major-mode 'org-mode
	org-directory "~/org/"
	org-log-done t
	org-startup-indented t
	org-startup-truncated nil
	org-startup-with-inline-images t
	org-completion-use-ido t
	org-default-notes-file (concat org-directory "notes.org")
	org-image-actual-width '(300)
	org-goto-max-level 10
	org-imenu-depth 5
	org-goto-interface 'outline-path-completion
	org-outline-path-complete-in-steps nil
	org-src-fontify-natively t
	org-lowest-priority ?C
	org-default-priority ?B
	org-expiry-inactive-timestamps t
	org-show-notification-handler 'message
	org-special-ctrl-a/e t
	org-special-ctrl-k t
	org-yank-adjusted-subtrees t
	org-file-apps
	'((auto-mode . emacs)
	  ("\\.mm\\'" . default)
	  ("\\.x?html?\\'" . "firefox %s")
	  ("\\.pdf\\'" . "open %s"))
	org-todo-keywords
	'((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "SOMEDAY(.)" "MAYBE(m)" "|" "DONE(x!)" "CANCELLED(c)"))
	;; Theming
	org-ellipsis "  " ;; folding symbol
	org-pretty-entities t
	org-hide-emphasis-markers t ;; show actually italicized text instead of /italicized text/
	org-agenda-block-separator ""
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t)

  ;; (add-to-list 'org-global-properties
  ;; 	       '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

  (add-hook 'org-mode-hook
	    '(lambda ()
	       (setq line-spacing 0.2) ;; Add more line padding for readability
	       (variable-pitch-mode 1) ;; All fonts with variable pitch.
	       (mapc
		(lambda (face) ;; Other fonts with fixed-pitch.
		  (set-face-attribute face nil :inherit 'fixed-pitch))
		(list 'org-code
		      'org-link
		      'org-block
		      'org-table
		      'org-verbatim
		      'org-block-begin-line
		      'org-block-end-line
		      'org-meta-line
		      'org-document-info-keyword))))

  ;;  (custom-theme-set-faces
  ;;   'spacemacs-light
  ;;   `(org-block-begin-line ((t (:background "#fbf8ef"))))
  ;;   `(org-block-end-line ((t (:background "#fbf8ef")))))
  )


;; -----------------------------------------------------------------------------
;; company
;;
(use-package company
  :ensure t
;;  :bind ("C-<tab>" . company-complete)
  :init
  (global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  ;; (setq company-backends '(company-clang))
  (add-to-list 'company-backends 'company-gtags)
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)  ;; 11.01.2022
  (setq company-tooltip-flip-when-above t)    ;; 11.01.2022
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.5))


;; -----------------------------------------------------------------------------
;; company-quickhelp (11.01.2022)
;;
(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :ensure t))


;; -----------------------------------------------------------------------------
;; sa-c-coding-style
;;
(c-add-style "sa-c-coding-style"               ; mit C-c C-o kann man einzelnes einstellen
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
               (c-basic-offset . 3)            ; indent by two spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (case-label . +)
                                   (statement-block-intro . +)
                                   (statement-case-intro . ++)   ;14.10.15: changed in ++
                                   (statement-case-open . +))))) ;14.10.15: chnaged in +


;; -----------------------------------------------------------------------------
;; sa-c-mode-hook
;;
(defun sa-c-mode-hook ()
  (c-set-style "sa-c-coding-style")        ; use my-style defined above

  (setq c-macro-names-with-semicolon '("Q_OBJECT" "Q_PROPERTY"
                                       "Q_DECLARE" "Q_ENUMS" "Q_INTERFACES"))
  (c-make-macro-with-semi-re)

  (auto-fill-mode)
  ;;(electric-pair-mode nil)		;; turn on automatic bracket insertion by pairs. New in emacs 24
  (c-toggle-auto-hungry-state -1)       ;; chnaged 16.02.2017, from 1 -> -1
  (local-set-key (kbd "C-d") 'kill-whole-line))

(add-hook 'c-mode-common-hook 'sa-c-mode-hook)


;; -----------------------------------------------------------------------------
;; warning - ToDo, FixMe, xxx, bug, ....
;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|bla\\|TODO\\|ToDo\\|XXX\\|BUG\\)" 1 font-lock-warning-face t)))))


;; -----------------------------------------------------------------------------
;; open corresponding header file
;;
;;(global-set-key [C-tab] 'ff-find-other-file)
(define-key prog-mode-map (kbd "<C-tab>") 'ff-find-other-file)

;; -----------------------------------------------------------------------------
;; Syntax highlighting support for "Modern C++" - until C++20 and Technical Specification.
;; This package aims to provide a simple highlight of the C++ language without dependency.
;; It is recommended to use it in addition with the c++-mode major mode for extra highlighting
;; (user defined types, functions, etc.) and indentation.
;;
(use-package modern-cpp-font-lock
  
  :diminish t
  :init (modern-c++-font-lock-global-mode t)
  :ensure t)

;;(use-package cc-mode
;;  :init
  ;;(define-key c-mode-map  [(tab)] 'company-complete)
 ; ;  (define-key c++-mode-map  [(tab)] 'company-complete))

;; -----------------------------------------------------------------------------
;; ESC Key redefine
;; From:
;; http://stackoverflow.com/questions/557282/in-emacs-whats-the-best-way-for-keyboard-escape-quit-not-destroy-other-windows
;;
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; -----------------------------------------------------------------------------
;; Package: ws-butler
;; eine unaufdringliche Möglichkeit, Leerzeichen vom Zeilenende aus zu beschneiden
;;
(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))


;; -----------------------------------------------------------------------------
;; PACKAGE: anzu
;; prettier text replacement with anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
;;
(use-package anzu
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))


;; -----------------------------------------------------------------------------
;; Package: volatile-highlights
;; hebt vorübergehend Änderungen im Puffer hervor, die mit bestimmten Befehlen verbunden sind
;;
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))


;; -----------------------------------------------------------------------------
;; Google this
;;
(use-package google-this
  :ensure t
  :diminish google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap))


;; -----------------------------------------------------------------------------
;; Google translate
;;    (C-p)(C-n) change translation direction
;;
(use-package google-translate
  :ensure t
  :bind ("C-c t" . google-translate-smooth-translate)
  :init
  (setq google-translate-translation-directions-alist
        '(("en" . "de") ("de" . "en"))))


;; -----------------------------------------------------------------------------
;; Package zygospore
;; A simple package for undoing the delete-other-windows command.
;; Convenient for needing more space in one buffer temporarily
;; or not having to worry about losing a specific setup.
;;
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))


;; ------------------------------------------------------------------------------
;; Woche im calendar
;;
(setq calendar-week-start-day 1)

(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0)

(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

                                        ; Title for week number
(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 1.0)
(setq calendar-intermonth-header
      (propertize "KW"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'calendar-iso-week-header-face))


;; ------------------------------------------------------------------------------
;; Customized functions
;; 
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.
    
   If ARG is not nil or 1, move forward ARG - 1 lines first. If
   point reaches the beginning or end of the buffer, stop there."

  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-o") 'open-line)


;;------------------------------------------------------------------------------
;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


;;------------------------------------------------------------------------------
;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

;;------------------------------------------------------------------------------
;;
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)


;; Compilation
;; (global-set-key (kbd "<f5>") (lambda ()
;;                                (interactive)
;;                                (setq-local compilation-read-command nil)
;;                                (call-interactively 'compile)))

;; -----------------------------------------------------------------------------
;; bookmarks - visible bookmarks in buffer
;; https://github.com/joodland/bm
;;
(use-package bm
  :ensure t
  :demand t

  :init
  (setq bm-restore-repository-on-load t)  ;;restore on load (even before you require bm)

  :config
  (setq bm-cycle-all-buffers t)                        ;; Allow cross-buffer 'next'
  (setq bm-repository-file "~/.emacs.d/bm-repository") ;;where to store persistant files
  (setq-default bm-buffer-persistence t)               ;; save bookmarks
  (add-hook' after-init-hook 'bm-repository-load)      ;; Loading the repository from file when on start up
  (add-hook 'find-file-hooks 'bm-buffer-restore)       ;; Restoring bookmarks when on file find.
  (add-hook 'kill-buffer-hook 'bm-buffer-save)         ;; Saving bookmarks

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
				(bm-buffer-save-all)
				(bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("C-n" . bm-next)
         ("M-n" . bm-previous)
         ("<f5>" . bm-toggle)
         ("C-<f5>" . bm-remove-all-all-buffers)))


;; -----------------------------------------------------------------------------
;; ispell
;;
(use-package ispell
  :config
  (setq ispell-program-name "C:/tools/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
  ;;(setq ispell-local-dictionary "en_US")
  (add-to-list 'ispell-local-dictionary-alist '("deutsch-hunspell"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "de_DE"); Dictionary file name
                                              nil
                                              iso-8859-1))
  (add-to-list 'ispell-local-dictionary-alist '("english-hunspell"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "en_US")
                                              nil
                                              iso-8859-1))
  (setq ispell-dictionary   "deutsch-hunspell"))
  ;;(setq ispell-local-dictionary-alist
  ;;      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

(use-package zoom
  :init
  (setq zoom-mode t
	zoom-size '(0.618 . 0.618)))


;; Load the rest of the packages
;;(package-initialize t)
(setq package-enable-at-startup nil)    ;



(require 'org)
;;(org-babel-load-file "~/.emacs.d/config.org")
;;(org-babel-load-file "~/.emacs.d/secrets.org")

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


(use-package header2
  :load-path (lambda () (expand-file-name "elisp/header2" user-emacs-directory))
  :custom
  (header-copyright-notice (concat "Copyright (C) 2019 " (user-full-name) "\n"))
  :hook (emacs-lisp-mode . auto-make-header)
  :config
  (add-to-list 'write-file-functions 'auto-update-file-header)
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2"))


(use-package zone
  :ensure nil
  :defer 5
  :config
  ;; (zone-when-idle 600) ; in seconds
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'."
    (interactive
     (list
      (completing-read
       "Program: "
       (mapcar 'symbol-name zone-programs))))
    (let ((zone-programs (list (intern pgm))))
      (zone))))

;;; init.el ends here

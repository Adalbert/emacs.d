;;
;; Adalbert Soborka
;;


;; the toolbar is just a waste of voluable screen estate
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)


(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


;; --- highlight the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t))

;; --- use space to indent by default
(setq-default indent-tabs-mode nil)


;; --- set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)


;; --- delete the selection with a keypress
(delete-selection-mode t)


;; --- No region when it is not highlighted
(transient-mark-mode 1)


;; --- Automatically save buffers to file when losing focus
(defun my-save-buffers ()
  "Save all file-visiting buffers"
  (save-some-buffers t nil))

(add-hook 'focus-out-hook 'my-save-buffers)



(load-theme 'better-tango-dark)       ;; dark background thema
(set-cursor-color "#77ff00")          ;; set cursor color to white
(global-linum-mode)                   ;; linum-mode - Zeile nummerierung
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands

;;(set-default-font "Inconsolata-10")
(set-face-attribute 'default nil :height 90);;93
(setq-default line-spacing 0)


;; -----------------------------------------------------------------------------
;; Highlight Current Line (from emacsredux.com)
;;
(use-package hl-line
  :config
  (global-hl-line-mode +1))
;;(global-hl-line-mode +1)
(set-face-background 'hl-line "#3e4446")     ; change color
(set-face-foreground 'highlight nil)


;; -----------------------------------------------------------------------------
;; Magit
;;
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))


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
  :ensure t)


;; -----------------------------------------------------------------------------
;; NeoTree - A tree plugin like NerdTree for Vim
;;
(use-package neotree
  :ensure t
  :bind ("C-p" . neotree-toggle))


;; -----------------------------------------------------------------------------
;; duplicate-thing - bring up help for key bindings
;;
(use-package duplicate-thing
  ;; :ensure t
  ;; :demand t
  ;; :config
  :bind ("M-C-<down>" . duplicate-thing))



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
;; which-key - bring up help for key bindings
;;
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; -----------------------------------------------------------------------------
;; Package: projectile
;;
;; (use-package projectile
;;   :init
;;   (projectile-global-mode)
;;   (setq projectile-enable-caching t))

(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode))
  ;;(setq projectile-completion-system 'ivy))
;; (global-set-key [f7] 'projectile-compile-project) ; Or any other key you want



;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )


;; -----------------------------------------------------------------------------
;; auto-complete
;;
(use-package auto-complete
  :ensure t
  :disabled t
  :config
  ;; add to dictionary directories
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  ;; default configuration for auto-complete
  (require 'auto-complete-config)
  (ac-config-default)
  ;; include C headers
  (defun my:ac-c-header-init ()
    (require 'auto-complete-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'achead:include-directories '"C:/Qt/4.8.7/include/"))
  ;; call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:ac-c-header-init)
  (add-hook 'c-mode-hook 'my:ac-c-header-init))


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
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.5))


(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-c-headers-path-system "C:/Qt/4.8.7/include/"))
;;  (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9.2/"))

;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)


;; -----------------------------------------------------------------------------
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

;;(show-paren-mode 1)


;; -----------------------------------------------------------------------------
;; show file name
;;
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want


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


  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
(windmove-default-keybindings)

;; -----------------------------------------------------------------------------
;; open corresponding header file
;;
(global-set-key [C-tab] 'ff-find-other-file)


;;------------------------------------------------------------------------------
;; handle backup files
;;
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 20
 kept-old-versions 10
 version-control t)


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


(provide 'setup-general)

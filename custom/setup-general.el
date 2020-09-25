(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(load-theme 'tango-dark)       ;; dark background thema
(defalias 'yes-or-no-p 'y-or-n-p)
(set-cursor-color "#77ff00")          ;; set cursor color to white
(global-linum-mode)                   ;; linum-mode - Zeile nummerierung

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
;;(setq cua-keep-region-after-copy t)   ;; Standard Windows behaviour
(setq column-number-mode t)           ;; column number in status line


(global-set-key (kbd "C-d") 'kill-whole-line)           ; Zeile löschen
(global-set-key (kbd "<f12>") 'calculator)



;; -----------------------------------------------------------------------------
;; magit
;;
(global-set-key (kbd "C-x g") 'magit-status)

;; -----------------------------------------------------------------------------
;; Highlight Current Line (from emacsredux.com)
;;
(global-hl-line-mode +1)
(set-face-background 'hl-line "#3e4446")     ; change color
(set-face-foreground 'highlight nil)


;; -----------------------------------------------------------------------------
;; Font
;;
;;(set-default-font "Inconsolata-11")
;; (set-frame-buffer "Inconsolata-11")
;;(set-face-attribute 'default nil :height 90);;93
(setq-default line-spacing 0)


(setq truncate-lines t)         ;; lange Zeilen nicht umbrechen, sondern abschneiden



;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

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

;; company
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; Package: projejctile
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))


;; -----------------------------------------------------------------------------
;; Package zygospore
;;
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))
;;         ("RET" .   newline-and-indent)))   ;; ToDo RET braucht man?

                                        ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
(windmove-default-keybindings)


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

(provide 'setup-general)


;; -----------------------------------------------------------------------------
;; Highlight Matching Parentheses (from emacsredux.com)
;;
(require 'paren)
(setq show-paren-style 'parenthesis) ;;expression or parenthesis or mixed
(show-paren-mode +1)


;; -----------------------------------------------------------------------------
;; show file name
;;
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want


;; -----------------------------------------------------------------------------
;; NeoTree - A tree plugin like NerdTree for Vim
;;
;; (require 'neotree)
;; (global-set-key (kbd "C-p") 'neotree-toggle)
;; (setq neo-smart-open t)


;; -----------------------------------------------------------------------------
;; which-key - bring up help for key bindings
;;
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


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

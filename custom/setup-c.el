

;; -----------------------------------------------------------------------------
;; company-c-headers
;;
(use-package company-c-headers
 :init
 (add-to-list 'company-backends 'company-c-headers))


;; -----------------------------------------------------------------------------
;; HideShow - toggle
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))


;; -----------------------------------------------------------------------------
;; HideShow - toggle
;;
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-*") 'toggle-selective-display)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook   'hs-minor-mode)


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
  (electric-pair-mode 1)		;; turn on automatic bracket insertion by pairs. New in emacs 24
  (c-toggle-auto-hungry-state -1)       ;; chnaged 16.02.2017, from 1 -> -1
  (local-set-key (kbd "C-d") 'kill-whole-line))

(add-hook 'c-mode-common-hook 'sa-c-mode-hook)


;; -----------------------------------------------------------------------------
;; warning - ToDo, FixMe, xxx, bug, ....
;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|ToDo\\|XXX\\|???\\|BUG\\)"
                                       1 font-lock-warning-face t)))))


;; -----------------------------------------------------------------------------
;; (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(use-package modern-cpp-font-lock
  :ensure t)


;; -----------------------------------------------------------------------------
;; open corresponding header file
;;
(global-set-key [C-tab] 'ff-find-other-file)


;; ----------------------------------------------------------------------------
;; flycheck - on the fly syntax checking
;;
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))


;; ----------------------------------------------------------------------------
;; flycheck - on the fly syntax checking
;;
(use-package cc-mode
  :init
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete))


(provide 'setup-c)

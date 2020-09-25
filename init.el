;;; init.el --- Where all the magic begins
;;
;; This file allows Emacs to initialize my customizations
;; in Emacs lisp embedded in *one* literate Org-mode file.

;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp/")


;; User info
;;
(setq user-full-name "Adalbert Soborka"
      user-mail-address "asoborka@gmx.de")

(setq gc-cons-threshold 100000000)

;; Use Package
;;
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(setq load-prefer-newer t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


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


;; -----------------------------------------------------------------------------
;; Theme
;;
(load-theme 'tango-dark)       ;; dark background thema(defalias 'yes-or-no-p 'y-or-n-p)
(set-cursor-color "#77ff00")   ;; set cursor color to white


;; -----------------------------------------------------------------------------
;; Font
;;
(set-face-attribute 'default nil :height 90);;93
;;(set-default-font "Inconsolata-11")
(setq-default line-spacing 0)

;; -----------------------------------------------------------------------------
;; misc useful keybindings
;;
(global-set-key (kbd "C-d") 'kill-whole-line)           ; Zeile löschen
(global-set-key (kbd "<f12>") 'calculator)


;; -----------------------------------------------------------------------------
;; Prettify Symbols
;;
(global-prettify-symbols-mode +1)


;; -----------------------------------------------------------------------------
;; Turn off unnecessary graphical features
;;
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; -----------------------------------------------------------------------------
;; Startup Messages
;;
(setq inhibit-startup-message t
      initial-scratch-message ""
      inhibit-startup-echo-area-message t)


;; -----------------------------------------------------------------------------
;; Mode Line
;; [[http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html#understanding-mode-line-format][Sebastian Wiesner]] inspired me to slim down my mode line. I have a very simple mode line that has the name of the file on the left and the date and time on the right. I've found I don't really need the mode information very often - I usually know which modes are active; if I do need the mode information, I can access that with =C-h m=, =describe-mode=. Moreover, that means I don't need to diminish most packages.
;;
;; I commented out any variables that I eliminated from the mode-line, so that I can add them in later if I deem them useful.
;;
(setq-default mode-line-format
	      '("%e" ; print error message about full memory.
		mode-line-front-space
	        mode-line-mule-info
		;; mode-line-client
		column-number-mode
		;; mode-line-modified
		;; mode-line-remote
		;; mode-line-frame-identification
		mode-line-buffer-identification
		"   "
		mode-line-position
		;; (vc-mode vc-mode)
		;; "  "
		;; mode-line-modes
		"   "
		;; mode-line-misc-info
		;; battery-mode-line-string
		mode-line-end-spaces))

(setq display-time-format "%a, %b %e %R"
      battery-mode-line-format "%p%%"  ; Default: "[%b%p%%]"
      global-mode-string   (remove 'display-time-string global-mode-string)
      mode-line-end-spaces (list (propertize " "
					     'display '(space :align-to (- right 17)))
				 'display-time-string))

(display-time-mode 1)
(display-time-update)


;; -----------------------------------------------------------------------------
;; Backups
;;
(setq delete-old-versions t
      kept-new-versions 20
      kept-old-versions 10
      version-control t
      backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))


;; -----------------------------------------------------------------------------
;; Yes or Not
(fset 'yes-or-no-p 'y-or-n-p)


;; -----------------------------------------------------------------------------
;; Bell
(setq ring-bell-function 'ignore)


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
;; Linum
;;
(global-linum-mode)     ;; linum-mode - Zeile nummerierung


;; -----------------------------------------------------------------------------
;; Nice scrolling
;;
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; -----------------------------------------------------------------------------
;; show unncessary whitespace that can mess up your diff
;;
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


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
	;; dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"
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
  (use-package dired-quick-sort
    :init
    (dired-quick-sort-setup))
  (use-package dired-collapse
    :hook dired-mode))


;; -----------------------------------------------------------------------------
;; Magit
;;
(use-package magit
 :ensure t
 :bind (("C-x g" . magit-status)))
;; -----------------------------------------------------------------------------
;; Magit
;;
;; (use-package magit
;;   :bind (("C-x g" . magit-status)
;; ;;	 ("C-c g" . magit-status)
;;          :map magit-status-mode-map
;;          ("TAB" . magit-section-toggle)
;;          ("<C-tab>" . magit-section-cycle)
;;          :map magit-branch-section-map
;;          ("RET" . magit-checkout))
;;   :config
;;   (add-hook 'after-save-hook 'magit-after-save-refresh-status)
;;   (setq magit-use-overlays nil
;;         magit-section-visibility-indicator nil
;;         magit-completing-read-function 'ivy-completing-read
;;         magit-push-always-verify nil)
;; ;;	magit-repository-directories '("~/src/"))
;;   (use-package git-timemachine
;; ;; Travel back and forward in git history of the current file    
;; ;;       p visit previous historic version
;; ;;       n visit next historic version
;; ;;       w copy the hash of the current historic version
;; ;;       q exit the time machine buffer
;;     :bind (("C-x v t" . git-timemachine)))
;;   (use-package git-link
;;     :bind (("C-x v L" . git-link))
;;     :init
;;     (setq git-link-open-in-browser t))
;;   (use-package pcmpl-git)
;;   (defun visit-pull-request-url ()
;;     "Visit the current branch's PR on Github."
;;     (interactive)
;;     (browse-url
;;      (format "https://github.com/%s/pull/new/%s"
;;              (replace-regexp-in-string
;;               "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
;;               (magit-get "remote"
;;         		 (magit-get-remote)
;;         		 "url"))
;;              (cdr (magit-get-remote-branch)))))
 
;;   (bind-key "v" 'visit-pull-request-url magit-mode-map)
 
;;   ;; Do Not Show Recent Commits in status window
;;   ;; https://github.com/magit/magit/issues/3230#issuecomment-339900039
;;   (magit-add-section-hook 'magit-status-sections-hook
;;         		  'magit-insert-unpushed-to-upstream
;;         		  'magit-insert-unpushed-to-upstream-or-recent
;;         		  'replace)
;;  )


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
  :bind (("C-x t" . git-timemachine)))


;; -----------------------------------------------------------------------------
;;
;; Git Auto Commit Mode
(use-package git-auto-commit-mode
  :delight)


;; ----------------------------------------------------------------------------
;; Projectile
;; Projectile configuration adapted from [[http://endlessparentheses.com/improving-projectile-with-extra-commands.html][Improving Projectile with extra commands on Endless Parentheses]].
;;
(use-package projectile
  :bind ("C-c p" . projectile-switch-project)
  :init
  (projectile-global-mode)
  (use-package ibuffer-projectile
    :bind (("C-x C-b" . ibuffer)
	   :map ibuffer-mode-map
	   ("c" . clean-buffer-list)
	   ("n" . ibuffer-forward-filter-group)
	   ("p" . ibuffer-backward-filter-group))
    :init
    (add-hook 'ibuffer-hook
	      (lambda ()
		(ibuffer-projectile-set-filter-groups)
		(unless (eq ibuffer-sorting-mode 'alphabetic)
		  (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq projectile-enable-caching t
	projectile-create-missing-test-files t
	projectile-completion-system 'ivy
	projectile-use-git-grep t
	projectile-switch-project-action #'projectile-commander
	;; I'm redefining a lot of bindings, so unset pre-defined methods
	;; and define everyting here.
	projectile-commander-methods nil)


  (def-projectile-commander-method ?? "Commander help buffer."
    (ignore-errors (kill-buffer projectile-commander-help-buffer))
    (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
      (insert "Projectile Commander Methods:\n\n")
      (dolist (met projectile-commander-methods)
	(insert (format "%c:\t%s\n" (car met) (cadr met))))
      (goto-char (point-min))
      (help-mode)
      (display-buffer (current-buffer) t))
    (projectile-commander))
  (def-projectile-commander-method ?a
    "Run ag on project."
    (counsel-projectile-ag))
  (def-projectile-commander-method ?b
    "Open an IBuffer window showing all buffers in the current project."
    (counsel-projectile-switch-to-buffer))
  (def-projectile-commander-method ?B
    "Display a project buffer in other window."
    (projectile-display-buffer))
  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?D
    "Find a project directory in other window."
    (projectile-find-dir-other-window))
  (def-projectile-commander-method ?e
    "Open an eshell buffer for the project."
    ;; This requires a snapshot version of Projectile.
    (projectile-run-eshell))
  (def-projectile-commander-method ?f
    "Find a project directory in other window."
    (projectile-find-file))
  (def-projectile-commander-method ?F
    "Find project file in other window."
    (projectile-find-file-other-window))
  (def-projectile-commander-method ?g
    "Open project root in vc-dir or magit."
    (projectile-vc))
  (def-projectile-commander-method ?G
    "Run grep on project."
    (projectile-grep))
  (def-projectile-commander-method ?i
    "Open an IBuffer window showing all buffers in the current project."
    (projectile-ibuffer))
  (def-projectile-commander-method ?j
    "Jack in to CLJ or CLJS depending on context."
    (let* ((opts (projectile-current-project-files))
	   (file (ido-completing-read
		  "Find file: "
		  opts
		  nil nil nil nil
		  (car (cl-member-if
			(lambda (f)
			  (string-match "core\\.clj\\'" f))
			opts)))))
      (find-file (expand-file-name
		  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (if (derived-mode-p 'clojurescript-mode)
	  (cider-jack-in-clojurescript)
	(cider-jack-in))))
  (def-projectile-commander-method ?r
    "Find recently visited file in project."
    (projectile-recentf))
  (def-projectile-commander-method ?s
    "Switch project."
    (counsel-projectile-switch-project))
  (def-projectile-commander-method ?t
    "Find test file in project."
    (projectile-find-test-file))
  (def-projectile-commander-method ?\C-?
    "Go back to project selection."
    (projectile-switch-project)))


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


;; test1
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t))

;; test2
;; (use-package doom-themes
;;   :init
;;   (load-theme 'doom-one t)
;;   :config
;;   (progn
;;     (doom-themes-neotree-config)
;;     (setq doom-neotree-line-spacing 0)
;;     (doom-themes-org-config)))

;; test3
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; test4
;; (use-package all-the-icon
;;   :ensure t)

;; -----------------------------------------------------------------------------
;; Package: undo-tree
;; C-x u: undo tree vizualization
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


;; -----------------------------------------------------------------------------
;; PACKAGE: iedit
;;
(use-package iedit
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))


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


;; -----------------------------------------------------------------------------
;; HELM
;;
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    ;; To fix error at compile:
    ;; Error (bytecomp): Forgot to expand macro with-helm-buffer in
    ;; (with-helm-buffer helm-echo-input-in-header-line)
    (if (version< "26.0.50" emacs-version)
        (eval-when-compile (require 'helm-lib)))

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line t

          ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-mode-fuzzy-match t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          helm-display-header-line nil)

    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c r") 'helm-recentf)
    (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h o") 'helm-occur)

;;    (global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
;;    (global-set-key (kbd "C-c h g") 'helm-google-suggest)

    (global-set-key (kbd "C-c h x") 'helm-register)
    ;; (global-set-key (kbd "C-x r j") 'jump-to-register)

    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;; show minibuffer history with Helm
    (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
    (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

    (define-key global-map [remap find-tag] 'helm-etags-select)

    (define-key global-map [remap list-buffers] 'helm-buffers-list)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PACKAGE: helm-swoop                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Locate the helm-swoop folder to your path
    (use-package helm-swoop
      :bind (("C-c h o" . helm-swoop)
             ("C-c s" . helm-multi-swoop-all))
      :config
      ;; When doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows t)

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t))

    (helm-mode 1)

    (use-package helm-projectile
      :init
      (helm-projectile-on)
      (setq projectile-completion-system 'helm)
      (setq projectile-indexing-method 'alien)
      (setq projectile-switch-project-action 'helm-projectile))))


;; -----------------------------------------------------------------------------
;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

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
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.5))


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
                                    '(("\\<\\(FIXME\\|TODO\\|ToDo\\|XXX\\|???\\|BUG\\)" 1 font-lock-warning-face t)))))



;; -----------------------------------------------------------------------------
;; open corresponding header file
;;
(global-set-key [C-tab] 'ff-find-other-file)


;; -----------------------------------------------------------------------------
;; Syntax highlighting support for "Modern C++" - until C++20 and Technical Specification.
;; This package aims to provide a simple highlight of the C++ language without dependency.
;; It is recommended to use it in addition with the c++-mode major mode for extra highlighting
;; (user defined types, functions, etc.) and indentation.
;;
(use-package modern-cpp-font-lock
  :ensure t)


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
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))




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

;;; init.el ends here



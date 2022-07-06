;; Useful stuff
;; :diminish -> do not appear on modeline
;; :ensure -> ensure package is available always (download if not available)
;; :custom or :config -> add customization
;; :bind -> bind key commands
;; remap -> remap keybinding to a new function
;; :bind-keymap -> add command prefix -> get to commands easier from a lead key
;; # -> use a function instead of the defined (default) function

;; If some package fails to load "M-x list-package" to refresh package list  

;; M-x check-parens: check where are the parenthesis are unbalanced -> check Emacs lisp faster
;; if error in package pull: M-x list-packages to refresh packages

;; C-u: Universal argument: augment (change) behaviour of emacs functions,
;; the more you use it the more augmentation levels the functions will get,
;; with different numbers you get different behaviour
;; No it is rebound to vim scroll

;; TODO: create eshell binding in general
;; TODO: create hydra go to next buffer binding in general 

;; vim mode: C-w for buffer management

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1) ;; Disable tollbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable the menubar

;; visible bell instead of sound bell at errors 
(setq visible-bell t)
;; add font
(set-face-attribute 'default nil :font "Fira Code" :height 120)
;; Add nicer theme
(load-theme 'doom-dracula t)

;; set new keyboard shortcuts to call emacs functions
;; Make <esc> quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; initilize use-package on non linux platforms
(unless (package-installed-p 'use-package) (package-install 'use-package))

;; Always ensure that the package will be available (download it if it is not available)
(require 'use-package)
(setq use-package-always-ensure t)

;; Show line numebrs
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; show command log 
(use-package command-log-mode)

;; Fuzzy search

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Add new icons and fonts for emacs, defaults to is it can't find it then uses the system provided ones
;; first run: M-x all-the-icons-install fonts
(use-package all-the-icons)

;; Nicer mode bar
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package doom-themes)

;; Delimiters for parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows what the consecutive button combination will do
(use-package which-key
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describle-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Pull in general package
(use-package general
;; Make swithch buffer a single combo instead of a double
  :config
  ;;(general-evil-setup t)
  (general-create-definer rune/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose-theme")))

(defun sedon/evil-hook ()
   (dolist (mode '(Buffer-menu-mode))
     (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook
  (sedon/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)
(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

; (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body))
(rune/leader-keys
  "b" '(hydra-buffer-menu/body :which-key "buffer menu"))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))


(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package markdown-mode)
;; Check why this fails
(use-package flymd)

;; Easier handling of Projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~")
    (setq projectile-project-search-path '("~")))
  (setq projectile-switch-project-action #'projectile-dired)
  )

;; Counsel for projectile
;; important command: M-x counsel-projectile-rg -> fast fuzzy search in projects 
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  ;; put diff buffer in the same buffer as the code
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

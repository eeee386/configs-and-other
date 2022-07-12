;; Useful stuff
;; :diminish -> do not appear on modeline
;; :ensure -> ensure package is available always (download if not available)
;; :custom or :config -> add customization
;; :bind -> bind key commands
;; remap -> remap keybinding to a new function
;; :bind-keymap -> add command prefix -> get to commands easier from a lead key
;; :custom and :config is the same but with config you use function calls, and with custom you use (<key> <value>)

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
(global-set-key (kbd "M-p") 'eshell)

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
  (sedon/evil-hook t)
  :config
  (turn-on-evil-mode)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :bind-keymap ())
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

(define-key Buffer-menu-mode-map (kbd ".") 'hydra-buffer-menu/body)


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

;; Git helper for handling pull requests, issues, notifications
(use-package forge)


(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
    (visual-line-mode 1))


(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Markdown like language + project management
;; org-agenda -> see all of the org stuff (weekly todos, deadlines, schedule, logs...)
;; Check repeated syntax https://orgmode.org/manual/Repeated-tasks.html
;; Org-wild-notifier to create os notifications
;; "|" in keywords to separate active and finishe task states
;; M-Ret -> new line with the same header
;; M-S-Ret -> new line same header as a TODO Item
;; M-<arrow> -> change header type
;; S-<arrow> -> Change TODO progress
;; org-deadline (C-c C-d) -> Create daedline for task
;; org-schedule (C-c C-s) -> Create schedule for task
;; org-timestamp (C-c .) -> Create Timestamp (birthdays, events,...etc)
;; org-set-effort (C-c C-x e) ->  to set effort to items
;; org-set-property (C-c C-x p) ->  to set property (like effort) to items
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)

  ;; Log stuff happening in agenda
  (setq org-agenda-start-with-log-mode t)
  ;; Log for done items
  (setq org-log-done 'time)
  ;; Show log in drawer mode (only show what is needed, less clutter)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files '("~/todos-and-remainders/Tasks.org" "~/todos-and-remainders/Birthday.org" "~/todos-and-remainders/Habit.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; Refile: move TODO or other items to the files defined here
  ;; For example: DONE items can be refiled to Archive
  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
 ;;  Tags popup: C-c C-q to easily add tags to todos 
  (setq org-tag-alist
    '((:startgroup)
      ;; Put mutually exclusive tags here
      (:endgroup)
       ("work" . ?w)
       ("finance" . ?f)
       ("health" . ?h)
       ("professional" . ?p)
       ("book" . ?c)
       ("drawing" . ?d)
       ("optimization" . ?o)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  ;; https://orgmode.org/manual/Custom-Agenda-Views.html
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   ;; Overview mode
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "IN PROGRESS"
        ((org-agenda-overriding-header "In Progress")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
     ;; In progress view
    ("i" "In Progress Tasks"
     ((todo "IN PROGRESS"
        ((org-agenda-overriding-header "In Progress Tasks")))))

    ;; counsel-org-tag to add tags to todos
    ;; M-Ret to cycle which tag to add
    ;; To add new tags for Work Tasks "+<tag name>" to remove: "-<tag name>"
    ("W" "Work Tasks" tags-todo "+work")
    ("I" "Improvement Tasks" tags-todo "+finance+health+professional+book+drawing+idea")

    ;; Low-effort next actions
    ;; org-set-effort (C-c C-x e) ->  to add effort to items
    ;; ("e" tags-todo "+TODO=\"IN PROGRESS\"+Effort<15&+Effort>0"
    ;;  ((org-agenda-overriding-header "Low Effort Tasks")
    ;;   (org-agenda-max-todos 20)
    ;;   (org-agenda-files org-agenda-files)))

   ("w" "Workflow Status"
     ((todo "TODO"
            ((org-agenda-overriding-header "Waiting to be done")
             (org-agenda-files org-agenda-files)))
      (todo "BLOCKED"
            ((org-agenda-overriding-header "Blocked")
             (org-agenda-files org-agenda-files)))
      (todo "IN PROGRESS"
            ((org-agenda-overriding-header "In Progress")
             (org-agenda-files org-agenda-files)))
      (todo "DONE"
            ((org-agenda-overriding-header "Done")
             (org-agenda-files org-agenda-files)))
      (todo "WON'T DO"
            ((org-agenda-overriding-header "Won't do")
             (org-agenda-files org-agenda-files)))))))

  ;; Create tasks and others on the file and save it (make sure the file you save to has the defined header)
   (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/todos-and-remainders/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/todos-and-remainders/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; (dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)

      ("b" "Book Entries")
      ("bb" "Book" entry
           (file+olp+datetree "~/todos-and-remainders/BookIdeas.org")
           "\n* %<%I:%M %p> - Book :book:\n\n%?\n\n"
           ;; (dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))
  (define-key global-map (kbd "C-c t")
    (lambda () (interactive) (org-capture nil "tt")))
  (define-key global-map (kbd "C-c b")
    (lambda () (interactive) (org-capture nil "bb")))
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b)" "IN PROGRESS(i)"  "|" "DONE(d)" "WON'T DO(w)")))




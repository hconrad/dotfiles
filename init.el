;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 180)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;;Add Paths
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

;; Relative line numbers
;; (setq display-line-numbers-type 'relative)

;; Set up the visible bell
;;(setq visible-bell t)

(set-face-attribute 'default nil :font "Iosevka Fixed" :height runemacs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		cider-repl-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

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

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-nord t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun move-buffer-to-window (windownum)
  "Moves buffer to window"
  (if (> windownum (length (window-list-1 nil nil t)))
      (message "No window numbered %s" windownum)
    (let ((b (current-buffer))
	  (w1 (selected-window))
	  (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
	(set-window-buffer w2 b)
	(switch-to-prev-buffer)
	(unrecord-window-buffer w1 b)
	(select-window (winum-get-window-by-number windownum))))))

(defun window-split-2 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows))

(defun window-split-3 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))
    
(dotimes (i 4)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
	     (move-buffer-to-window ,n)
             ))))
(global-unset-key (kbd "C-c o"))
(use-package org-modern)
(global-org-modern-mode)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/Users/hans/org"))
  :bind (("C-c o h" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o g" . org-roam-graph)
         ("C-c o i" . org-roam-node-insert)
         ("C-c o c" . org-roam-capture)
         ;; Dailies
         ("C-c o t" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package general
  :config
 (general-create-definer my/leader-keys 
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
(general-create-definer clojure-kb
  :keymaps '(clojure-mode-map)
  :states 'normal
  :prefix ",")
  (my/leader-keys
    "SPC" '(counsel-M-x :which-key "Execute")
    ":" '(eval-expression :which-key "Evaluate Expr")
    "e" '(:ignore t :which-key "Emacs")
    "ef" '(eval-defun :which-key "Eval defun")
    "eb" '(eval-buffer :which-key "Eval buffer")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "f" '(:ignore t :which-key "Files")
    "fs" '(save-buffer :which-key "Save")))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(defun kill-current-buffer ()
(interactive)
(kill-buffer (current-buffer)))

(my/leader-keys
  "b" '(:ignore t :which-key "Buffer")
  "bb" '(counsel-ibuffer :which-key "List Buffers")
  "bd" '(kill-current-buffer :which-key "Kill Buffer")
  "bn" '(next-buffer :which-key "Next Buffer")
  "bp" '(previous-buffer :which-key "Previous Buffer")
  "b1" '(buffer-to-window-1 :which-key "Move Buffer to 1")
  "b2" '(buffer-to-window-2 :which-key "Move Buffer to 2")
  "b3" '(buffer-to-window-3 :which-key "Move Buffer to 3")
  "b4" '(buffer-to-window-4 :which-key "Move Buffer to 4"))

(use-package ace-window)
(use-package winum)
(winum-mode)
  
(my/leader-keys
  "1" '(winum-select-window-1 :which-key "Select 1st Window")
  "2" '(winum-select-window-2 :which-key "Select 2nd Window")
  "3" '(winum-select-window-3 :which-key "Select 3rd Window")
  "4" '(winum-select-window-4 :which-key "Select 4th Window")
  "w" '(:ignore w :which-key "Window")
  "wb" '(balance-windows :which-key "Balance Windows")
  "w1" '(delete-other-windows :which-Key "Single Column")
  "w2" '(window-split-2 :which-key "2 Columns")
  "w3" '(window-split-3 :which-key "3 Columns")
  "w-" '(split-window-vertically :which-key "Split Window -")
  "w|" '(split-window-horizontally :which-key "Split Window |")
  "wd" '(delete-window :which-key "Del Window")
  "q" '(save-buffers-kill-terminal :which-key "Quit Emacs"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

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


(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(my/leader-keys
"p" '(projectile-command-map :which-key "Projects")
"o" '(projectile-add-known-project :which-key "Open Unknown Project"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(my/leader-keys
"g" '(:ignore t :which-key "Git")
"gs" '(magit-status :which-key "Status")
"o" '(projectile-add-known-project :which-key "Open Unknown Project"))

(use-package smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(my/leader-keys
  "k" '(:ignore t :which-key "Smart Parens!")
  "kr" '(sp-raise-sexp :which-key "Raise")
  "kw" '(sp-wrap-round :which-key "Wrap"))

;; CLOJURE
(use-package clojure-mode)
(use-package cider)
(defun cider-repl-new-line-prompt (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "%s\n:) " namespace))
(setq cider-repl-prompt-function 'cider-repl-new-line-prompt)

(clojure-kb
  "ef" '(cider-eval-defun-at-point :which-key "Eval defun")
  "e;" '(cider-eval-defun-to-comment :which-key "Eval defun to comment")
  "e(" '(cider-eval-list-at-point :which-key "Eval List")
  "eb" '(cider-eval-buffer :which-key "Eval buffer")
  "scj" '(cider-connect-clj :which-key "Connect to REPL")
  "sjj" '(cider-jack-in-clj :which-key "Jack in CLJ"))

(add-hook 'clojure-mode-hook #'smartparens-mode)
;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-roam org-modern cider winum ace-window forge evil-magit magit counsel-projectile projectile which-key use-package rainbow-delimiters ivy-rich hydra helpful general evil-collection doom-themes doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

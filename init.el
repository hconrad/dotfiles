;;; Commentary: N/A

;;; Code:
(defvar runemacs/default-font-size 140)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
(show-paren-mode)

;;Add Paths
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

;; Relative line numbers
;; (setq display-line-numbers-type 'relative)

;; Set up the visible bell
;(setq visible-bell t)

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

(use-package vterm)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		cider-repl-mode-hook
                term-mode-hook
		vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package flycheck)
(global-flycheck-mode)
(use-package flycheck-clj-kondo :ensure t)

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
(use-package flx)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
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
  :init (load-theme 'doom-challenger-deep t))

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

(defun window-split (n)
  (interactive)
  (let ((win-list (window-list-1 nil nil t))
	(win-cnt (length (window-list-1 nil nil t))))
    (cond ((= n win-cnt) (message "Nothing to do!"))
	  ((> n win-cnt) (progn
			   (dotimes (w-idx (- n win-cnt)) (split-window-horizontally))
			   (balance-windows)))
          ((< n win-cnt) (progn (dotimes (w-idx (- win-cnt n) ) (delete-window (elt win-list w-idx) ))) (balance-windows)))))

(defun window-split-2 ()
  (interactive)
  (window-split 2))

(defun window-split-3 ()
  (interactive)
  (window-split 3))
    
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

(defun open-terminal ()
  (interactive)
  (vterm))

(use-package general
  :config
 (general-create-definer my/leader-keys 
    :keymaps 'override
    :states '(normal visual emacs)
    :prefix "SPC" )
(general-create-definer clojure-kb
  :states 'normal
  :keymaps '(clojure-mode-map cider-repl-mode-map)
  :prefix ",")
(general-create-definer clojure-repl-kb
  :states 'normal
  :keymaps '(cider-repl-mode-map)
  :prefix ",")
  (my/leader-keys
    "SPC" '(counsel-M-x :which-key "Execute")
    "pt" '(projectile-dired :which-key "Dired")
    "/" '(counsel-projectile-ag :which-key "Search Project")
    ":" '(eval-expression :which-key "Evaluate Expr")
    ";" '(open-terminal :which-key "Terminal")
    "e" '(:ignore t :which-key "Emacs")
    "ef" '(eval-defun :which-key "Eval defun")
    "eb" '(eval-buffer :which-key "Eval buffer")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "f" '(:ignore t :which-key "Files")
    "fs" '(save-buffer :which-key "Save")
    "v" '(:ignore t :which-key "View")
    "vs" '(window-configuration-to-register :which-key "Store View")
    "vl" '(jump-to-register :which-key "Load View")))

(my/leader-keys
  "o" '(:ignore t :which-key "Org Roam")
  "oh" '(org-roam-buffer-toggle :which-key "Buffer Toggle" )
  "of" '(org-roam-node-find :which-key "Find")
  "og" '(org-roam-graph :which-key "Graph")
  "oi" '(org-roam-graph :which-key "Insert")
  "oc" '(org-roam-capture :which-key "Capture")
  "ot" '(org-roam-dailies-capture-today :which-key "Capture Today"))



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
  "b" '(:ignore t :which-key "buffer")
  "br" '(rename-buffer :which-key "rename")
  "bb" '(counsel-ibuffer :which-key "list")
  "bd" '(kill-current-buffer :which-key "kill")
  "bn" '(next-buffer :which-key "next")
  "bp" '(previous-buffer :which-key "previous")
  "b1" '(buffer-to-window-1 :which-key "move -> 1")
  "b2" '(buffer-to-window-2 :which-key "move -> 2")
  "b3" '(buffer-to-window-3 :which-key "move -> 3")
  "b4" '(buffer-to-window-4 :which-key "move -> 4"))

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
  (when (file-directory-p "~/repos/crossbeam")
    (setq projectile-project-search-path '("~/repos/crossbeam")))
  (setq projectile-switch-project-action #'projectile-dired))

(my/leader-keys
"p" '(projectile-command-map :which-key "Projects")
"a" '(projectile-add-known-project :which-key "Open Unknown Project"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(my/leader-keys
"g" '(:ignore t :which-key "Git")
"gs" '(magit-status :which-key "Status"))

(defun wrap-around-and-insert (&optional arg)
  (interactive)
  (sp-wrap-round)
  (evil-insert 1))

(use-package smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(my/leader-keys
  "k" '(:ignore t :which-key "Smart Parens!")
  "k$" '(sp-end-of-sexp :which-key "Jump to End")
  "k^" '(sp-beginning-of-sexp :which-key "Jump to Beginning")
  "kr" '(sp-raise-sexp :which-key "Raise")
  "kb" '(sp-forward-barf-sexp :which-key "Forward barf")
  "ks" '(sp-forward-slurp-sexp :which-key "Forward slurp")
  "kw" '(wrap-around-and-insert :which-key "Wrap"))

;; CLOJURE
(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
  like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
                          `(("(\\(fn\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "λ")
                                       nil)))
                            ("(\\(partial\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "Ƥ")
                                       nil)))
                            ("(\\(comp\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∘")
                                       nil)))
                            ("\\(#\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))
                            ("\\(#\\){"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∈")
                                       nil))))))

(use-package clojure-mode
  :config
  (progn (clojure/fancify-symbols 'clojure-mode)
	 (require 'flycheck-clj-kondo)))

(use-package cider
  :config
  (progn
    (clojure/fancify-symbols 'cider-repl-mode)
    (clojure/fancify-symbols 'cider-clojure-interaction-mode)))

(defun cider-repl-new-line-prompt (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "%s\n:) " namespace))
(setq cider-repl-prompt-function 'cider-repl-new-line-prompt)
(setq clojure-toplevel-inside-comment-form t)
;; Play with this setting 
;;(add-hook 'cider-repl-mode-hook '(lambda () (setq scroll-conservatively 101)))
;;(setq cider-invert-insert-eval-p t)                        ;; 1
;;(setq cider-switch-to-repl-after-insert-p nil)             ;; 2
(use-package company)
(global-company-mode)

(defun cider-eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-connection)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return)
      (with-selected-window (get-buffer-window (cider-current-connection))
        (goto-char (point-max))))))

(defun cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing the focus."
  (interactive)
  (cider-eval-in-repl-no-focus (cider-defun-at-point)))

(defun cider-send-region-to-repl (start end)
  "START Capture Region.
END End Region.
Send Region to Repl." 
  (interactive "r")
  (cider-eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

(defun cider-send-ns-form-to-repl ()
  (interactive)
  (cidern-send-function-to-repl (cider-ns-form)))

(defun cider-send-ns-form-to-repl-focus ()
  (interactive)
  (cider-insert-ns-form-in-repl t)
  (evil-insert-state))

(clojure-repl-kb
 "sa" '(cider-switch-to-last-clojure-buffer :which-key "toggle repl"))
(general-define-key :keymaps 'cider-repl-mode-map
		    "C-k" 'cider-repl-previous-input
		    "C-j" 'cider-repl-next-input)
(clojure-kb
  "bc" '(cider-repl-clear-buffer :which-key "clear")
  "e" '(:ignore t :which-key "eval")
  "en" '(cider-eval-ns-form :which-key "eval Ns")
  "ef" '(cider-eval-defun-at-point :which-key "eval defun")
  "e;" '(cider-eval-defun-to-comment :which-key "eval defun to comment")
  "e(" '(cider-eval-list-at-point :which-key "eval list")
  "eb" '(cider-eval-buffer :which-key "eval buffer")
  "gg" '(cider-find-var :which-key "go to def")
  "hh" '(cider-doc :which-key "doc at point")
  "hH" '(cider-clojuredocs :which-key "clojure docs at point")
  "s" '(:ignore t :which-key "send")
  "scj" '(cider-connect-clj :which-key "connect")
  "sjj" '(cider-jack-in-clj :which-key "jack in CLJ")
  "sq" '(cider-quit :which-key "quit")
  "sn" '(cider-send-ns-form-to-repl :which-key "ns no focus")
  "sN" '(cider-send-ns-form-to-repl-focus :which-key "ns focus")
  "sf" '(cider-send-function-to-repl :which-key "defn")
  "sr" '(cider-send-region-to-repl :which-key "region")
  "sa" '(cider-switch-to-repl-buffer :which-key "toggle")
  "tn" '(cider-test-run-ns-tests :which-key "namespace")
  "tt" '(cider-test-run-test :which-key "single")
  "tr" '(cider-test-rerun-test :which-key "rerun")
  "tf" '(cider-test-rerun-failed-tests :which-key "rerun failed"))


(add-hook 'clojure-mode-hook #'smartparens-mode)
;;Zprint
(load-file "/home/hans/repos/zprint.el/zprint.el")
(add-hook 'clojure-mode-hook #'zprint-mode)
;;TREEMACS
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
;;END Treemacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons flx org-roam org-modern cider winum ace-window forge evil-magit magit counsel-projectile projectile which-key use-package rainbow-delimiters ivy-rich hydra helpful general evil-collection doom-themes doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

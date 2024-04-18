;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Hans Conrad"
      user-mail-address "hans@crossbeam.com")
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq doom-localleader-key ",")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka SS04" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Iosevka SS04" :size 20))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; previous was doom-challenger-deep
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:*" :size 0.5 :vslot -4 :select t :quit nil :ttl 0 :side 'right)
  (evil-set-initial-state 'vterm-mode 'insert)
  (add-hook 'vterm-mode-hook 'evil-insert-state)
  (setq evil-insert-state-cursor '(bar "#00FF00")
        evil-visual-state-cursor '(box "#FF00FF")
        evil-normal-state-cursor '(box "#E2E8EF"))
  )

(after! company
  (setq company-idle-delay 1.0))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  :custom-face (flycheck-posframe-error-face ((t (:background "DarkSlateBlue"))))
  :custom-face (flycheck-posframe-warning-face ((t (:background "DarkSlateBlue"))))
  :custom-face (flycheck-posframe-border-face ((t (:background "DarkBlue")))))

(set-face-attribute 'flycheck-posframe-border-width 5)
(after! exec-path-from-shell
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(defun get-vterm-project-name ()
  (concat "*vterm-" (projectile-project-name) "*"))

(defun create-project-vterm ()
  (setq vterm-buffer-name (get-vterm-project-name))
  (let ((vbuf (+vterm/here nil)) ) (setq vterm-buffer-name "*vterm*") vbuf))

(defun find-or-create-vterm ()
  "Finds or creates a vterm buffer"
  (interactive)
  (let ((vterm-buffer  (get-buffer (get-vterm-project-name))))
    (if (null vterm-buffer) (create-project-vterm) (set-window-buffer (selected-window) vterm-buffer))))

(defun find-or-create-ldb ()
  "Finds or creates a Local DB Conection"
  (interactive)
  (let ((ldb-buffer (get-buffer "*ldb*")))
    (if (null ldb-buffer) (progn (vterm "*ldb*")
                                 (with-current-buffer "*ldb*"
                                                   (vterm-send-string "ldb")
                                                   (vterm-send-return)))
      (switch-to-buffer "*ldb*"))))

(map! :leader (:prefix "t" :desc "Find..." "f" #'find-or-create-vterm))
(map! :leader (:prefix "t" :desc "Find DB Term..." "d" #'find-or-create-ldb))
(map! :leader (:prefix "b" :desc "Kill Buffer by Name" "X" #'doom/kill-matching-buffers))
(map! :leader (:prefix "w" :desc "Maximize" "m" #'doom/window-maximize-buffer))

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

(dotimes (i 4)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
             ,(format "Move buffer to the window with number %i." n)
             (interactive "P")
     (move-buffer-to-window ,n)
             ))))

(map! :leader (:prefix "b"
               :desc "Kill Matching Buffers" "a" #'doom/kill-matching-buffers
               :desc "To Win 1" "1" #'buffer-to-window-1
               :desc "To Win 2" "2" #'buffer-to-window-2
               :desc "To Win 3" "3" #'buffer-to-window-3
               :desc "To Win 4" "4" #'buffer-to-window-4))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-log-done 'time)

(defun wrap-around-and-insert (&optional arg)
  (interactive)
  (sp-wrap-round)
  (evil-insert 1))



(after! smartparens
  (smartparens-global-mode)
  (map! :leader
        (:prefix ("k" . "parens")
         :desc "move param right" "l" #'sp-transpose-sexp
         :desc "jump to end" "$" #'sp-end-of-sexp
         :desc "jump to beginning" "^" #'sp-beginning-of-sexp
         :desc "raise" "r" #'sp-raise-sexp
         :desc "forward barf" "b" #'sp-forward-barf-sexp
         :desc "forward slurp" "s" #'sp-forward-slurp-sexp
         :desc "wrap" "w" #'wrap-around-and-insert)))

(map! :leader :desc "Exec" "SPC" #'execute-extended-command
      :desc "Server Edit" "d" #'server-edit
      :desc "Win 1" "1" #'winum-select-window-1
      :desc "Win 2" "2" #'winum-select-window-2
      :desc "Win 3" "3" #'winum-select-window-3
      :desc "Win 4" "4" #'winum-select-window-4)

(after! evil
  (map! :leader :desc "Search Project" "/" #'+vertico/project-search))

(vertico-posframe-mode 1)
(setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
(setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)

(map! :leader (:prefix "s" :desc "find references" "r" #'lsp-find-references))
(map! :leader (:prefix "c" :desc "lsp ui menu" "m" #'lsp-ui-imenu))

(after! org
  (setq org-roam-directory "~/org")
  (setq org-roam-dailies-directory ".")
  (setq org-agenda-files (list "~/org")))

(map! :leader (:prefix ("r" . "org-roam")
                 :desc "toggle buffer" "h" #'org-roam-buffer-toggle
                 :desc "find" "f" #'org-roam-node-find
                 :desc "capture" "c" #'org-roam-capture
                 :desc "capture today" "t" #'org-roam-dailies-goto-today
                 :desc "capture manana" "m" #'org-roam-dailies-goto-tomorrow))

;; JS
(after! web-mode
  (map! :map web-mode-map "C-k" #'cider-repl-previous-input "C-j" #'cider-repl-next-input)
  (map! :map web-mode-map :localleader (:prefix "g"
                                        :desc "go to definition" "g" #'lsp-find-definition
                                        :desc "find references" "r" #'lsp-find-references)))

(after! js2-mode
(map! :map js2-mode-map :localleader (:prefix "c"
                                         :desc "Create NodeJS REPL" "c" #'nodejs-repl
                                         :desc "Send Region" "r" #'nodejs-repl-send-region
                                         :desc "Send buffer" "f" #'nodejs-repl-send-buffer
                                         :desc "Send Line" "l" #'nodejs-repl-send-line
                                         :desc "Go to REPL" "b" #'nodejs-repl-switch-to-repl))
  (map! :map js2-mode-map :localleader (:prefix "f"
                                         :desc "format Region" "r" #'lsp-format-region
                                         :desc "format Buffer" "b" #'lsp-format-buffer))
  (map! :map js2-mode-map :localleader (:prefix "g"
                                         :desc "go to definition" "g" #'lsp-find-definition
                                         :desc "find references" "r" #'lsp-find-references)))

;; SQL Config
(add-hook 'sql-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'server-edit nil t)))
(setq sqlformat-command 'sqlformat)
;;CLOJURE
;;
;; CROSSBEAM CLOJURE
(defun clean-ns ()
  (interactive)
  (cider-interactive-eval "(user/clean-ns)")
  (message "Namespace Cleaned"))


(after! clojure-mode
  (map! :map clojure-mode-map :localleader (:prefix "e"
                                            :desc "clean ns" "c" #'clean-ns)))

;;
(setq cider-show-error-buffer nil)
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

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

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
        (goto-char (point-max)))
      )))

(defun cider-quit-when-disconnect () "Disconnect CIDER When we're disconnected" (cider-quit) )


(defun cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing
the focus."
  (interactive)
  (cider-eval-in-repl-no-focus (cider-defun-at-point)))

(defun cider-send-region-to-repl (start end)
  "Send region to REPL and evaluate it without changing
the focus."
  (interactive "r")
  (cider-eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

(after! clojure-mode
        (progn (clojure/fancify-symbols 'clojure-mode)
               (require 'flycheck-clj-kondo)))

(after! cider
  (progn
    (set-popup-rule! "*cider-repl*" :ignore t)
    (setq cider-repl-display-in-current-window nil)
    (clojure/fancify-symbols 'cider-repl-mode)
               (clojure/fancify-symbols 'cider-clojure-interaction-mode)
  ;; Quit Cider when disconnected
  (add-hook 'cider-disconnected-hook #'cider-quit-when-disconnected)
  ;; include cider buffer into current workspace
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (persp-add-buffer (current-buffer))))

  ;; include test-report buffer into current workspace
  (add-hook 'cider-test-report-mode-hook
            (lambda ()
              (persp-add-buffer (current-buffer))))

  ;; include temp buffers created by cider into current workspace
  (add-hook 'cider-popup-buffer-mode-hook
            (lambda ()
              (persp-add-buffer (current-buffer))))

               ))



(after! clojure-mode
  (map! :map cider-repl-mode-map "C-k" #'cider-repl-previous-input "C-j" #'cider-repl-next-input)
  (map! :map cider-repl-mode-map :localleader (:prefix "r" :desc "last clojure buffer" "b" #'cider-switch-to-last-clojure-buffer))
  (map! :map cider-repl-mode-map :localleader (:prefix "h"
                                               :desc "Describe func" "d" #'cider-doc
                                               :desc "Descirbe ClojureDoc" "c" #'cider-docview-clojuredocsd))
  (map! :map clojure-mode-map :localleader (:prefix "e"
                                            :desc "show clerk" "s" #'clerk-show
                                            :desc "eval ns" "n" #'cider-eval-ns-form
                                            :desc "eval func" "f" #'cider-eval-defun-at-point
                                            :desc "eval list" "(" #'cider-eval-list-at-point
                                            :desc "eval defun to comment" ";" #'cider-eval-defun-to-comment))
(map! :map clojure-mode-map :localleader (:prefix "g"
                                            :desc "go to other window" "G" #'cider-find-dwim-other-window
                                            :desc "find references" "r" #'lsp-find-references))
   (map! :map clojure-mode-map :localleader (:prefix "r"
                                             :desc "send func to repl" "f" #'cider-send-function-to-repl
                                             :desc "send region to repl" "r" #'cider-send-region-to-repl)))
(setq lsp-enable-file-watchers nil)
(setq cider-save-file-on-load t)


(add-hook! (js-mode js2-mode) #'eslint-fix-auto-mode)

(defun cider-repl-new-line-prompt (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "%s\n:) " namespace))
(setq cider-repl-prompt-function 'cider-repl-new-line-prompt)
(setq clojure-toplevel-inside-comment-form t)

(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

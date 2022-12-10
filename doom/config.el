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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
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
)

(after! exec-path-from-shell
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

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

(map! :leader :desc "Exec" "SPC" #'execute-extended-command)

(map! :leader (:prefix "s" :desc "find references" "r" #'lsp-find-references))

(after! org
  (setq org-roam-directory "/home/hans/org"))

(map! :leader (:prefix ("r" . "org-roam")
                 :desc "toggle buffer" "h" #'org-roam-buffer-toggle
                 :desc "find" "f" #'org-roam-node-find
                 :desc "capture" "c" #'org-roam-capture
                 :desc "capture today" "t" #'org-roam-dailies-capture-today
                 :desc "capture manana" "m" #'org-roam-dailies-capture-tomorrow))



;;CLOJURE
;;
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

(after! clojure-mode
(progn (clojure/fancify-symbols 'clojure-mode)))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))


(after! clojure-mode
  (progn
    (clojure/fancify-symbols 'cider-repl-mode)
    (clojure/fancify-symbols 'cider-clojure-interaction-mode))
  (map! :map cider-repl-mode-map "C-k" #'cider-repl-previous-input "C-j" #'cider-repl-next-input)
  (map! :map cider-repl-mode-map :localleader (:prefix "r" :desc "last clojure buffer" "b" #'cider-switch-to-last-clojure-buffer))
  (map! :map clojure-mode-map :localleader (:prefix "e"
                                            :desc "show clerk" "s" #'clerk-show
                                            :desc "eval ns" "n" #'cider-eval-ns-form
                                            :desc "eval func" "f" #'cider-eval-defun-at-point
                                            :desc "eval list" "(" #'cider-eval-list-at-point
                                            :desc "eval defun to comment" ";" #'cider-eval-defun-to-comment)))

(set-popup-rule! "*cider-repl *" :ignore t :side 'right)

(defun cider-repl-new-line-prompt (namespace)
  "Return a prompt string that mentions NAMESPACE."
  (format "%s\n:) " namespace))
(setq cider-repl-prompt-function 'cider-repl-new-line-prompt)
(setq clojure-toplevel-inside-comment-form t)

(load "~/repos/zprint.el/zprint.el" )
(add-hook 'clojure-mode-hook 'zprint-mode)

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

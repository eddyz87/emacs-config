(setq visible-bell nil
      inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(require 'package)


(defun ensure-packages (&rest packages)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(ensure-packages
 'undo-tree
 'auto-complete
 'paredit
 'rainbow-delimiters
 'color-theme
 'color-theme-sanityinc-tomorrow)


(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list ))))

;;(global-set-key (kbd "C-/") 'auto-complete)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-r") 'query-replace-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-r") 'query-replace)

(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

(global-set-key (kbd "<f10>") 'menu-bar-mode)
(global-set-key (kbd "<f9>") 'shell)

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(require 'auto-complete)
(setq ac-use-quick-help t)
(global-auto-complete-mode)

(require 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq undo-tree-visualizer-timestamps t)
(global-undo-tree-mode)

(defun hook-name (mode)
  (intern (concat (symbol-name mode) "-mode-hook")))

(require 'paredit)
(dolist (mode '(emacs-lisp lisp clojure scheme))
  (add-hook (hook-name mode) 'paredit-mode))

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(setq custom-safe-themes t)
(load-theme 'sanityinc-tomorrow-eighties)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; Took this from:
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/starter-kit-defuns.el
(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)



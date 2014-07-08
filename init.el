(setq inhibit-startup-message t)
(setq ring-bell-function (lambda ())) ;; disable bell

(tool-bar-mode -1)
(menu-bar-mode -1)

;; CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)   ;; Standard Windows behaviour

(setq-default indent-tabs-mode nil)

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
 'color-theme-sanityinc-tomorrow
 'slime)


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

(global-set-key (kbd "C-y") 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo-tree-undo)

(defun hook-name (mode)
  (intern (concat (symbol-name mode) "-mode-hook")))

(require 'paredit)
(dolist (mode '(emacs-lisp lisp clojure scheme))
  (add-hook (hook-name mode) 'paredit-mode))

;;(global-set-key (kbd "<backspace>") 'delete-region-or-default-action)
;;;; delete selected region by backspace
;;(delete-selection-mode t)
;; TODO need hook for paredit, because it uses it's own binding on backspace

;; (define-minor-mode ez-kill-region-with-backspace-mode
;;   "Smart completion"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "<backspace>") 'ez-kill-region-with-backspace)
;;             map))

;; (defun ez-kill-region-with-backspace ()
;;   (interactive)
;;   (if (use-region-p)
;;       (delete-region (region-beginning) (region-end))
;;     (let ((ez-kill-region-with-backspace-mode nil))
;;       (call-interactively (key-binding (kbd "<backspace>"))))))

;; (ez-kill-region-with-backspace-mode t)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(setq custom-safe-themes t)
(load-theme 'sanityinc-tomorrow-bright)

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

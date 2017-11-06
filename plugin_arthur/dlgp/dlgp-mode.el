;; a simple major mode, dlgp-mode
(defvar dlgp-mode-hook nil)

(defvar dlgp-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for DLGP major mode")

(add-to-list 'auto-mode-alist '("\\.dlgp" . dlgp-mode))


(defconst dlgp-font-lock-keywords
  (list
   '("\\,\\|:-\\|\\." . font-lock-preprocessor-face)
   '("%.*" . font-lock-comment-face)
   '("not_[a-z][^(]*" . font-lock-function-name-face)
   '("[a-z][^(]*" . font-lock-function-name-face)
   '("(" . (:foreground "#FF0000"))
   '(")" . font-lock-keyword-face)
   '("[A-Z][_a-zA-Z0-9]*" . font-lock-variable-name-face)
   '("[a-z][_a-zA-Z0-9]*" . font-lock-constant-face)))

(defun dlgp-mode ()
  "DLGP major mode"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dlgp-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(dlgp-font-lock-keywords))
  (setq major-mode 'dlgp-mode)
  (setq mode-name "DLGP")
  (run-hooks 'dlgp-mode-hook))

(provide 'dlgp-mode)

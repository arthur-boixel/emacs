;; turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t)   ;; remove start message
(setq initial-scratch-message nil) ;; show nothing in *scratch* when started
(setq visible-bell t)              ;; replace bell by blink

;; disable version control and magic mode
(setq vc-handled-backends nil)
(setq magic-mode-alist nil)

;; disable local variables -*-
(setq enable-local-variables nil)

(column-number-mode 1)             ;; show column number
(defalias 'yes-or-no-p 'y-or-n-p)  ;; replace yes/no by y/n

;; keep just one window when multiple file opening
(add-hook 'emacs-startup-hook 'delete-other-windows)

;; save temporary/backup file in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; set utf-8 coding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; do not use tab (4 spaces)
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd"
      c-basic-offset 4) 

;; set background colors
(set-background-color "black")
(set-foreground-color "white")

;; set/load some paths 
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq setup-dir (expand-file-name "setup" user-emacs-directory))  
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path setup-dir)

(require 'mode-mappings)
(require 'key-bindings)

;############
;AJOUT ARTHUR
;############

;correction orthographique (F2) pour lancer
(global-set-key [f2] 'flyspell-mode)

;display line numbers on the left side
(global-linum-mode t)


;Zoom Ctrl + Roulette
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;use a color theme from color-theme.el library - download and put in load-path
;http://www.emacswiki.org/emacs/ColorTheme
;(add-to-list 'load-path "~/.emacs.d/plugin_arthur/colorTheme")
;(require 'color-theme)
;(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-solarized-20130307.1350")
;(require solarized-light-theme)

; set custom color to the modline (bottom bar)
(set-face-attribute 'mode-line nil
:foreground "ivory"
:background "#4090D9"
:overline "#4090D9"
:underline "#4090D9")

                                        ;C colors

; set preprocesor (#include) color
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "#FF4466")

; string color
(set-face-attribute 'font-lock-string-face nil :foreground "orange")

; doc color
(set-face-attribute 'font-lock-doc-face nil :foreground "darkgray" :slant 'italic)

; constant color
(set-face-attribute 'font-lock-constant-face nil :foreground "#FF00FF")

; set type color (HANDLE, int, char, etc.)
(set-face-attribute 'font-lock-type-face nil :foreground "#4dbb0a") ;4D950A

; set variable name color
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#ad7fa9")

; set function name color
(set-face-attribute 'font-lock-function-name-face nil :foreground "#729ec9")

; for comment  
(set-face-attribute 'font-lock-comment-face nil :foreground "darkgray" :slant 'italic)

; color for selected text
(set-face-attribute 'region nil :background "#243c45")

; color for if/return/sizeof...
(set-face-attribute 'font-lock-keyword-face nil :foreground "cyan" :slant 'normal)

; highlighting numbers for C code (hex, dec, etc.)
(add-hook 'c-mode-common-hook (lambda ()
                                (font-lock-add-keywords nil '(

                                        ; Valid hex number (will highlight invalid suffix though)
                                                              ("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-preprocessor-face)

                                        ; Invalid hex number
                                                              ("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)

                                        ; Valid floating point number.
                                                              ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b" (1 font-lock-preprocessor-face) (3 font-lock-preprocessor-face))

                                        ; Invalid floating point number.  Must be before valid decimal.
                                                              ("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)

                                        ; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
                                        ; will be highlighted as errors.  Will highlight invalid suffix though.
                                                              ("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-preprocessor-face)

                                        ; Valid octal number
                                                              ("\\b0[0-7]+[uUlL]*\\b" . font-lock-preprocessor-face)

                                        ; Floating point number with no digits after the period.  This must be
                                        ; after the invalid numbers, otherwise it will "steal" some invalid
                                        ; numbers and highlight them as valid.
                                                              ("\\b\\([0-9]+\\)\\." (1 font-lock-preprocessor-face))

                                        ; Invalid number.  Must be last so it only highlights anything not
                                        ; matched above.
                                                              ("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face)
                                                              ))
                                ))


                                        ; highlighting numbers for asm code (hex, dec, etc.)
(add-hook 'asm-mode-hook (lambda ()
                           (font-lock-add-keywords nil '(

                                        ; Valid hex number (will highlight invalid suffix though)
                                                         ("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-variable-name-face)

                                        ; Invalid hex number
                                                         ("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)

                                        ; Valid floating point number.
                                                         ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b" (1 font-lock-variable-name-face) (3 font-lock-variable-name-face))

                                        ; Invalid floating point number.  Must be before valid decimal.
                                                         ("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)

                                        ; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
                                        ; will be highlighted as errors.  Will highlight invalid suffix though.
                                                         ("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-variable-name-face)

                                        ; Valid octal number
                                                         ("\\b0[0-7]+[uUlL]*\\b" . font-lock-variable-name-face)

                                        ; Floating point number with no digits after the period.  This must be
                                        ; after the invalid numbers, otherwise it will "steal" some invalid
                                        ; numbers and highlight them as valid.
                                                         ("\\b\\([0-9]+\\)\\." (1 font-lock-variable-name-face))

                                        ; Invalid number.  Must be last so it only highlights anything not
                                        ; matched above.
                                                         ("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face)
                                                         ))
                           ))

; load dlgp mode
(add-to-list 'load-path "~/.emacs.d/plugin_arthur/dlgp/dlgp-mode/")
(load "dlgp-mode.el")

; load flex plugin
(add-to-list 'load-path "~/.emacs.d/plugin_arthur/flex-mode/")
(load "flex-mode.el")

; load popup plugin, required to install auto complete
(add-to-list 'load-path "~/.emacs.d/plugin_arthur/popup/")
(load "popup.el")

;julia
(add-to-list 'load-path "~/.emacs.d/plugin_arthur/julia/")
(load "julia-mode.el")

; auto complete: auto code completion
(add-to-list 'load-path "~/.emacs.d/plugin_arthur/autoComplete/")
(require 'auto-complete-config)
(ac-config-default)

; Ajout du mode Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))


                    
; start package.el with emacs
(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
; initialize package.el
(package-initialize)

; 2048
(require '2048-game)

;correction orthographe a la volée
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/.abrev")
(setq save-abbrevs t)

;loadind w3m
(require 'w3m-load)

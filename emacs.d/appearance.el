;; Theme.
(use-package labburn-theme
  :init
  (load-theme 'labburn t)
  (set-face-attribute 'fringe nil :background "#3f3f3f" :foreground "dark cyan"))

(set-face-attribute 'default nil :family "Hack" :foundry "outline" :slant 'normal :weight 'medium :height 181)

(customize-set-variable 'display-line-numbers-type nil)
;(fringe-mode nil nil (fringe))
(customize-set-variable 'global-visual-line-mode t)
(customize-set-variable 'indicate-empty-lines t)
(customize-set-variable 'show-paren-mode t)
(customize-set-variable 'visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

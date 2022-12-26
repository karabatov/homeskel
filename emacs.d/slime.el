;; SLIME

;; https://common-lisp.net/project/slime/doc/html/Installation.html#Installing-from-Git
; (add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-v2.26.1/")
; (require 'slime-autoloads)
(use-package slime
  :init
  (slime-setup '(slime-fancy))
  :config
  (if (eq system-type 'windows-nt)
      (setq inferior-lisp-program "sbcl")
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))
  :bind ("C-c s" . 'slime-selector))

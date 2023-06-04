;; Notes

;; Deft configuration
(use-package deft
  :bind (
         ([f8] . 'deft)
         ("C-<f8>" . 'deft))
  :custom
  (deft-directory "~/Documents/notes")
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-extensions '("md"))
  (deft-default-extension "md")
  (deft-new-file-format "%Y%m%d%H%M"))

;; Zetteldeft configuration
(use-package zetteldeft
  :after deft
  :custom
  (zetteldeft-id-format "%Y%m%d%H%M")
  (zetteldeft-id-regex "[0-9]\\{12\\}")
  (zetteldeft-link-indicator "§")
  (zetteldeft-link-suffix "")
  (zetteldeft-home-id "202201031224")
  (zetteldeft-title-prefix "# ")
  (zetteldeft-title-suffix "")
  :config
  (zetteldeft-set-classic-keybindings)
  (font-lock-add-keywords 'markdown-mode
                          `((,zetteldeft-id-regex . font-lock-warning-face)))
  :bind
  ("C-c d w" . 'zetteldeft-copy-id-current-file))

(defun yk-filter-query (str)
  "Ask and apply filter and show Deft."
  (interactive
   (list (read-from-minibuffer "Search: ")))
  (deft-filter str t)
  (deft))
;; (f)ilter) (q)uery
(global-set-key (kbd "C-c d b") 'yk-filter-query)

(defun yk-filter-at-point ()
  "Set Deft filter to tag at point and show Deft."
  (interactive)
  ;; A tag is most like a 'filename.
  (deft-filter (thing-at-point 'filename 'no-properties) t)
  (deft))
;; (f)ilter (p)oint
(global-set-key (kbd "C-, f p") 'yk-filter-at-point)

(defun yk-insert-date ()
  "Insert current date in ZK format."
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M")))
;; (i)nsert (d)ate
(global-set-key (kbd "C-, i d") 'yk-insert-date)

(defun yk-deft-random-note ()
  "Open a random Deft note."
  (interactive)
  (deft-open-file (seq-random-elt deft-all-files)))
;; (r)andom (n)ote
(global-set-key (kbd "C-, r n") 'yk-deft-random-note)

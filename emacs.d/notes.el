;; Notes

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

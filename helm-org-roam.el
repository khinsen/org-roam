;;
;; Work in progress
;;
;; Instructions: load this file after org-roam initialization
;; to replace the key bindings for org-roam-find-file and org-roam-insert
;; by their helm-based equivalents.
;; Change the key bindings at the end if yours are different from mine!
;;

(require 'helm)
(require 'org-roam)
(require 's)

(defun helm-org-roam---candidate-transformer (candidates source)
  (let ((prefixed-pattern (propertize
                           " " 'display
                           (propertize "[?]" 'face 'helm-ff-prefix))))
    (cons (concat prefixed-pattern " " helm-pattern)
          candidates)))

(defun helm-org-roam--read-title (completions &optional input)
  (let ((title
         (helm :sources (helm-build-sync-source "Title"
                          :candidates (-map #'car completions)
                          :filtered-candidate-transformer
                          #'helm-org-roam---candidate-transformer
                          :fuzzy-match t)
               :buffer "*org-roam titles*"
               :prompt "Title: "
               :input input)))
    (unless title
      (keyboard-quit))
    (s-trim-left title)))

(defun helm-org-roam-find-file ()
  "Find and open an org-roam file using Helm."
  (interactive)
  (org-roam--find-file-with-completion-method #'helm-org-roam--read-title))

(defun org-roam-insert (prefix)
  "Find an org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion."
  (interactive "P")
  (org-roam--insert-with-completion-method
   prefix
   #'(lambda (completions region-text)
       (completing-read "File: " completions nil nil region-text))))

(defun helm-org-roam-insert (prefix)
  "Find an org-roam file using Helm, and insert a relative org link to
it at point. If PREFIX, downcase the title before insertion."
  (interactive "P")
  (org-roam--insert-with-completion-method 
   prefix #'helm-org-roam--read-title))

(require 'helm)

(defun org-roam--find-file-with-completion-method (chooser)
  "Find and open an org-roam file using completion method CHOOSER."
  (let* ((completions (org-roam--get-title-path-completions))
         (title (funcall chooser completions))
         (file-path (cdr (assoc title completions))))
    (if file-path
        (find-file file-path)
      (let* ((org-roam--capture-info (list (cons 'title title)
                                           (cons 'slug (org-roam--title-to-slug title))))
             (org-roam--capture-context 'title))
        (org-roam-capture '(4))))))

;; (defun org-roam-find-file ()
;;   "Find and open an org-roam file."
;;   (interactive)
;;   (org-roam--find-file-with-completion-method
;;    #'(lambda (completions) (completing-read "File: " completions))))

(defun org-roam-find-file-helm ()
  "Find and open an org-roam file using Helm."
  (interactive)
  (org-roam--find-file-with-completion-method
   #'(lambda (completions)
       (helm :sources (helm-build-sync-source "Title"
                        :candidates (-map #'car completions)
                        :filtered-candidate-transformer
                          #'(lambda (candidates source) (cons helm-pattern candidates))
                 :fuzzy-match t)
      :buffer "*org-roam titles*"
      :prompt "Title: "))))

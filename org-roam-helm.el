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

(defun org-roam--helm-candidate-transformer (candidates source)
  (let ((prefixed-pattern (propertize
                           " " 'display
                           (propertize "[?]" 'face 'helm-ff-prefix))))
    (cons (concat prefixed-pattern " " helm-pattern)
          candidates)))

(defun org-roam-find-file-helm ()
  "Find and open an org-roam file using Helm."
  (interactive)
  (org-roam--find-file-with-completion-method
   #'(lambda (completions)
       (helm :sources (helm-build-sync-source "Title"
                        :candidates (-map #'car completions)
                        :filtered-candidate-transformer
                          #'org-roam--helm-candidate-transformer
                        :fuzzy-match t)
      :buffer "*org-roam titles*"
      :prompt "Title: "))))


(defun org-roam--insert-with-completion-method (prefix chooser)
  (let* ((region (and (region-active-p)
                      ;; following may lose active region, so save it
                      (cons (region-beginning) (region-end))))
         (region-text (when region
                        (buffer-substring-no-properties
                         (car region) (cdr region))))
         (completions (org-roam--get-title-path-completions))
         (title (funcall chooser completions region-text))
         (region-or-title (or region-text title))
         (target-file-path (cdr (assoc title completions)))
         (current-file-path (-> (or (buffer-base-buffer)
                                    (current-buffer))
                                (buffer-file-name)
                                (file-truename)
                                (file-name-directory)))
         (buf (current-buffer))
         (p (point-marker)))
    (unless (and target-file-path
                 (file-exists-p target-file-path))
      (let* ((org-roam--capture-info (list (cons 'title title)
                                           (cons 'slug (org-roam--title-to-slug title))))
             (org-roam--capture-context 'title))
        (setq target-file-path (org-roam-capture))))
    (with-current-buffer buf
      (when region ;; Remove previously selected text.
        (delete-region (car region) (cdr region)))
      (let ((link-location (concat "file:" (file-relative-name target-file-path current-file-path)))
            (description (format org-roam-link-title-format (if prefix
                                                                (downcase region-or-title)
                                                              region-or-title))))
        (goto-char p)
        (insert (format "[[%s][%s]]"
                        link-location
                        description))
        (setq org-roam--capture-insert-point (point))))))

;; (defun org-roam-insert (prefix)
;;   "Find an org-roam file, and insert a relative org link to it at point.
;; If PREFIX, downcase the title before insertion."
;;   (interactive "P")
;;   (org-roam--insert-with-completion-method
;;    prefix
;;    #'(lambda (completions region-text)
;;        (completing-read "File: " completions nil nil region-text))))

(defun org-roam-insert-helm (prefix)
  "Find an org-roam file using Helm, and insert a relative org link to
it at point. If PREFIX, downcase the title before insertion."
  (interactive "P")
  (org-roam--insert-with-completion-method
   prefix
   #'(lambda (completions region-text)
       (helm :sources (helm-build-sync-source "Title"
                        :candidates (-map #'car completions)
                        :filtered-candidate-transformer
                        :filtered-candidate-transformer
                          #'org-roam--helm-candidate-transformer
                        :fuzzy-match t)
             :buffer "*org-roam titles*"
             :prompt "Title: "
             :input region-text))))

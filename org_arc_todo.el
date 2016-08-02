(setq arcanist-todo--user-template "
## [subscribers]
--cc=\"%s\"")
(setq arcanist-todo--project-template "
## [project]
--project=\"%s\"")
(setq arcanist-todo--task-summary-template "
## [summary of task]
\"%s\"")

(defun arcanist-todo ()
  "Create acranist todos"
  (interactive)
  (let* (;(dir (arcanist-find-root-path))
	 ;;(config (arcanist-parse-config dir))
         (headline (replace-regexp-in-string "\\*+ " "" (org-entry-get nil "ITEM")))
         (proj_name (org-entry-get (point) "arc_project" t))
         (users (org-entry-get (point) "arc_user"))
         )
    (generate-new-buffer "*arcanist-todo*")    
    (with-current-buffer "*arcanist-todo*"  
      (erase-buffer)
      (insert (format arcanist-todo--task-summary-template (or headline "")))
      (mapc '(lambda (arg)
	       (insert (format arcanist-todo--user-template (or arg ""))))
            (split-string users ", "))
      )
    (display-buffer "*arcanist-todo*")))

(defun arcanist-todo-finish ()
  "wrap xx and finish the job"
  (interactive)
  (let* ((content (with-current-buffer "*arcanist-todo*"
                    (goto-char (point-min))
                    (flush-lines "^##")
                    (flush-lines "^$")
                    (goto-char (point-min))
                    (replace-regexp "\n" " ")
                    (buffer-string))))
    (shell-command (concat "arc todo " content))))

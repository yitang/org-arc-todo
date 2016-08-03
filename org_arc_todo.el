;;;; * org-arc-todo.el --- Helps you build up your vocabulary
;;
;; Copyright (C) 2015 Yi Tang
;;
;; Author: Yi Tang <yi.tang.uk@me.com>
;; Keywords: Phabricator
;; Created: 28th March 2015
;; Package-Requires: ((org-mode "8.2"))
;; URL: https://github.com/yitang/org-arc-todo
;; Version: 0.1.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; * Commentary:
;;
;; Eventually, org_arc_todo would help you to sync your local notes in
;; Org-mode with the project tasks in Phabricator, but it is not quite
;; there yet. At this moment, it can only send your notes to
;; Phabricator.
;;
;;; Use:
;;
;; To send current node in org-mode to Phabricator
;;   (org-arc-todo)
;;   (org-arc-todo-finish)
;;
;;; Code:

;; requires
(require 'org)

; templates, define as constant atm.
(defconst org-arc-todo--user-template "
## [subscribers]
--cc=\"%s\"")
(defconst org-arc-todo--project-template "
## [project]
--project=\"%s\"")
(defconst org-arc-todo--task-summary-template "
## [summary of task]
\"%s\"")

; main functions
(defun org-arc-todo ()
  "parse a node in org-mode for arc todo"
  (interactive)
  (let* ((headline (replace-regexp-in-string "\\*+ " "" (org-entry-get nil "ITEM")))
         (proj_name (org-entry-get (point) "arc_project" t))
         (users (org-entry-get (point) "arc_user")))
    (generate-new-buffer "*arcanist-todo*")
    (with-current-buffer "*arcanist-todo*"  
      (erase-buffer)
      (insert (format org-arc-todo--task-summary-template (or headline "")))
      (insert (format org-arc-todo--project-template (or proj_name "")))       
      (mapc '(lambda (arg)
	       (insert (format org-arc-todo--user-template (or arg ""))))
            (split-string users ", ")))
    (display-buffer "*arcanist-todo*")))

(defun org-arc-todo-finish ()
  "format *arcranist-todo* buffer as argument for arc todo
command and then execute it"
  (interactive)
  (let* ((content (with-current-buffer "*arcanist-todo*"
                    (goto-char (point-min))
                    (flush-lines "^##")  ; remove lines start with ##
                    (flush-lines "^$")   ; remove empty lines 
                    (while (search-forward "\n" nil t) (replace-match "" nil t)) ; replace newline with whitespace
                    (buffer-string))))
    (shell-command (concat "arc todo " content))))

(provide 'org-arc-todo)

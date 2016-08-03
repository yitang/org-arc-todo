;;;; * voca-builder.el --- Helps you build up your vocabulary
;;
;; Copyright (C) 2015 Yi Tang
;;
;; Author: Yi Tang <yi.tang.uk@me.com>
;; Keywords: English vocabulary 
;; Created: 28th March 2015
;; Package-Requires: ((popup "0.5.2"))
;; URL: https://github.com/yitang/voca-builder
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
;; voca-builder is an Emacs package that aimed to help you build up your
;; vocabulary by automating the most step in the process, so that you
;; actually spent time in learning new words.
;; It will do the checking, and shows the meaning as a popup above the
;; text. It also records the meaning and the sentence containing the word.
;; Finally, it can export vocabularies that have the same tags, or are
;; between two dates.
;;
;;; Use:
;;
;; To use voca-builder 
;;   (setq voca-builder/voca-file "~/.vocabulary.org")
;;   (setq voca-builder/export-file "~/.voca-builder-temp.org")
;;   (setq voca-builder/current-tag "Demo")
;;   (global-set-key (kbd "<f4>") 'voca-builder/search-popup) 
;;   
;; To export all the vocabulary tagged by TLOTR 
;;   (voca-builder/extract-by-tags "TLOTR") , get all the vocabularies tagged
;;   ;; by TLOTR,  The Lord of The Rings.
;; To export all the vocabulary recored between 2015-01-01 and 2015-03-01
;;   (voca-builder/extract-period "2015-01-01" "2015-03-01")
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

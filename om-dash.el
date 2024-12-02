;;; om-dash.el --- Building blocks for org-based dashboards -*- lexical-binding: t -*-

;; Copyright (C) 2024 Victor Gaydov and contributors

;; Author: Victor Gaydov <victor@enise.org>
;; URL: https://github.com/gavv/om-dash

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; om-dash implements a set of dynamic blocks for org-mode that you can use
;; to compose a custom dashboard for your projects.

;; The following dynamic blocks are available:
;; * om-dash-github
;;   generates a table with issues or pull requests from github repository
;; * om-dash-orgfile
;;   generates tables with top-level entries from an org file
;; * om-dash-imap
;;   generates table with unread email counters for IMAP folder
;; * om-dash-command
;;   generates a table from JSON or CSV output of a shell command
;; * om-dash-function
;;   generates a table from output of a Elisp function

;; The package also provides a minor mode (om-dash-mode) that applies
;; highlighting to the generated tables.

;; In addition, there is support for templates, which allow to create
;; reusable parameterized configurations of the above blocks.

;; Refer to README.org for examples and screenshots.

;;; Code:

(require 'cl-lib)
(require 'imap)
(require 'json)
(require 'org)
(require 'seq)

(require 'org-ql)
(require 's)
(require 'ts)

(when (featurep 'parse-csv)
  ;; https://github.com/mrc/el-csv
  (require 'parse-csv))

(defvar om-dash-todo-keywords nil
  "List of keywords considered as TODO.

If block has any of the TODO keywords, block's heading becomes TODO.
The first element from this list is used for block's heading in this case.

If a keyword from this list doesn't have a face in 'om-dash-keyword-faces',
it uses default TODO keyword face.

When nil, filled automatically from 'org-todo-keywords', 'org-done-keywords',
and pre-defined github keywords.")

(defvar om-dash-done-keywords nil
  "List of keywords considered as DONE.

If block doesn't have any of the TODO keywords, block's heading becomes DONE.
The first element from this list is used for block's heading in this case.

If a keyword from this list doesn't have a face in 'om-dash-keyword-faces',
it uses default DONE keyword face.

When nil, filled automatically from 'org-todo-keywords', 'org-done-keywords',
and pre-defined github keywords.")

(defvar om-dash-keyword-faces
  '(
    ;; org-mode
    ("TODO" . om-dash-todo-keyword)
    ("DONE" . om-dash-done-keyword)
    ;; github
    ("OPEN" . om-dash-open-keyword)
    ("MERGED" . om-dash-merged-keyword)
    ("CLOSED" . om-dash-closed-keyword)
    ;; imap
    ("NEW" . om-dash-new-keyword)
    ("UNREAD" . om-dash-unread-keyword)
    ("CLEAN" . om-dash-clean-keyword)
    )
  "Assoc list to map keywords to faces.

If some keyword is not mapped to a face explicitly, default face is selected,
using face for TODO or DONE depending on whether that keyword is in
'om-dash-todo-keywords' or 'om-dash-done-keywords'.")

(defvar om-dash-tag-map nil
  "Assoc list to remap or unmap tag names.

Defines how tags are displayed in table.
You can map tag name to a different string or to nil to hide it.")

(defvar om-dash-templates
  '(
    ;; OBSOLETE templates:
    (milestone . om-dash-github:milestone)
    (project-column . om-dash-github:project-column)
    )
  "Assoc list of expandable templates for om-dash dynamic blocks.

Each entry is a cons of two symbols: template name and template function.

When you pass \":template foo\" as an argument to a dynamic block, it finds
a function in this list by key 'foo' and uses it to \"expand\" the template.

This function is invoked with dynamic block parameters plist and should
return a new plist. The new plist is used to update the original
parameters by appending new values and overwriting existing values.

For example, if 'org-dblock-write:om-dash-github' block has parameters:
  (:repo \"owner/repo\"
   :type 'issue
   :template project-column
   :project 123
   :column \"In progress\")

Dynamic block will use 'project-column' as a key in 'om-dash-templates'
and find 'om-dash-github:project-column' function.

The function is invoked with the original parameter list, and returns
a modified parameter list:
  (:repo \"owner/repo\"
   :type 'issue
   :open (\"project:owner/repo/123\"
          \".projectCards[] | (.column.name == \\\"In progress\\\")\")
   :closed \"\"
   :headline \"issues (owner/repo \\\"1.2.3\\\")\")

Then modified parameters are interpreted by dynamic block as usual.")

(defvar om-dash-table-fixed-width nil
  "If non-nil, align tables to have given fixed width.
If nil, tables have minimum width that fits their contents.")

(define-obsolete-variable-alias
  'om-dash-table-width
  'om-dash-table-fixed-width "0.2")

(defvar om-dash-table-squeeze-empty t
  "If non-nil, automatically remove empty columns from tables.
E.g. if every row has empty tags, :tags column is removed from this table.")

(define-obsolete-variable-alias
  'om-dash-squeeze-empty-columns
  'om-dash-table-squeeze-empty "0.2")

(defvar om-dash-table-link-style :cell
  "How links are generated in om-dash tables.

Allowed values:
 - :none - no links are inserted
 - :text - only cell text becomes a link
 - :cell - whole cell becomes a link")

(define-obsolete-variable-alias
  'om-dash-link-style
  'om-dash-table-link-style "0.2")

(defvar om-dash-table-time-format "%a, %d %b %Y"
  "Format for 'format-time-string' used for times in tables.
E.g. used for :created-at, :updated-at, :closed-at github columns.")

(defvar om-dash-github-columns
  '(:state
    :number
    :author
    :title-link)
  "Column list for 'om-dash-github' tables.

Supported values:

| symbol          | example           |
|-----------------+-------------------|
| :state          | OPEN, CLOSED, ... |
| :number         | #123              |
| :title          | text              |
| :title-link     | [[link][text]]    |
| :milestone      | 1.2.3             |
| :tags           | :tag1:tag2:...:   |
| :author         | @octocat          |
| :assignee       | @octocat,@github  |
| :reviewer       | @octocat,@github  |
| :project        | text              |
| :project-status | text              |
| :created-at     | date              |
| :updated-at     | date              |
| :closed-at      | date              |
")

(defvar om-dash-orgfile-columns
  '(:state
    :title-link)
  "Column list for 'om-dash-orgfile' tables.

Supported values:

| symbol      | example         |
|-------------+-----------------|
| :state      | TODO, DONE, ... |
| :title      | text            |
| :title-link | [[link][text]]  |
| :tags       | :tag1:tag2:...: |
")

(defvar om-dash-imap-columns
  '(:state
    :new
    :unread
    :total
    :folder)
  "Column list for 'om-dash-imap' tables.

Supported values:

| symbol      | example            |
|-------------+--------------------|
| :state      | NEW, UNREAD, CLEAN |
| :new        | 10                 |
| :unread     | 20                 |
| :total      | 30                 |
| :folder     | foo/bar            |
")

(defvar om-dash-github-limit 200
  "Default limit for github queries.

E.g. if you query \"all open issues\" or \"closed issues since january\",
only last 'om-dash-github-limit' results are returned.")

(defvar om-dash-github-fields
  '(
    (pullreq
     .
     ("assignees"
      "author"
      "autoMergeRequest"
      "baseRefName"
      "body"
      "closed"
      "closedAt"
      "createdAt"
      "headRefName"
      "headRefOid"
      "headRepository"
      "headRepositoryOwner"
      "id"
      "isCrossRepository"
      "isDraft"
      "labels"
      "maintainerCanModify"
      "mergeable"
      "mergeCommit"
      "mergedAt"
      "mergedBy"
      "mergeStateStatus"
      "milestone"
      "number"
      "potentialMergeCommit"
      "state"
      "title"
      "updatedAt"
      "url"))
    (issue
     .
     ("assignees"
      "author"
      "closed"
      "closedAt"
      "createdAt"
      "id"
      "labels"
      "milestone"
      "number"
      "state"
      "title"
      "updatedAt"
      "url"
      ))
    )
  "List of json fields enabled by default in github queries.

This defines which fields are present in github responses and hence can
be used in jq selectors.

We don't enable all fields by default because some of them noticeably
slow down response times.

There is also 'om-dash-github-auto-enabled-fields', which defines fields
that are enabled automatically for a query if jq selector contains them.

In addition, 'org-dblock-write:om-dash-github' accept ':fields'
parameter, which can be used to overwrite fields list per-block.")

(defvar om-dash-github-auto-enabled-fields
  '(
    (pullreq
     .
     (
      "additions"
      "changedFiles"
      "comments"
      "commits"
      "deletions"
      "files"
      "latestReviews"
      "projectCards"
      "projectItems"
      "reactionGroups"
      "reviewDecision"
      "reviewRequests"
      "reviews"
      "statusCheckRollup"
      ))
    (issue
     .
     (
      "body"
      "comments"
      "projectCards"
      "projectItems"
      "reactionGroups"
      ))
    )
  "List of json fields automatically enabled on demand in github queries.

See 'om-dash-github-fields' for more details.")

(defvar om-dash-imap-host nil
  "Default IMAP server hostname.

Used by 'om-dash-imap' if ':host' parameter is not provided.
Host must be always set, either via ':host' or 'om-dash-imap-host'.")

(defvar om-dash-imap-port nil
  "Default IMAP server port number.

Used by 'om-dash-imap' if ':port' parameter is not provided.
If port is not set, default IMAP port is used.")

(defvar om-dash-imap-machine nil
  "Default ~/.authinfo machine for IMAP server.

Used by 'om-dash-imap' if ':machine' parameter is not provided.
If machine is not set, value of host is used.")

(defvar om-dash-imap-user nil
  "Default username for IMAP server.

Used by 'om-dash-imap' if ':user' parameter is not provided.
If user is not set, it's read from ~/.authinfo.
See also 'om-dash-imap-machine'.")

(defvar om-dash-imap-password nil
  "Default username for IMAP server.

Used by 'om-dash-imap' if ':password' parameter is not provided.
If password is not set, it's read from ~/.authinfo.
See also 'om-dash-imap-machine'.")

(defvar om-dash-imap-stream nil
  "Default STREAM parameter for 'imap-open'.

Used by 'om-dash-imap' if ':stream' parameter is not provided.
Must be one of the values from 'imap-streams'.
If nil, detected automatically.")

(defvar om-dash-imap-auth nil
  "Default AUTH parameter for 'imap-open'.

Used by 'om-dash-imap' if ':auth' parameter is not provided.
Must be one of the values from 'imap-authenticators'.
If nil, detected automatically.")

(defvar om-dash-imap-empty-folders nil
  "Whether to display empty IMAP folders.
If nil, empty folders are excluded from the table.")

(defvar om-dash-verbose nil
  "Enable verbose logging.
If non-nill, all commands and queries are logged to '*om-dash*' buffer.")

(defgroup om-dash-faces nil
  "Faces in om-dash mode.")

(defface om-dash-header-cell
  '((t (:inherit default)))
  "Face used for entire cell in om-dash table header.
You can use it so specify header background."
   :group 'om-dash-faces)

(defface om-dash-header-text
  '((t (:inherit org-table)))
  "Face used for text in om-dash table header.
You can use it so specify header font."
   :group 'om-dash-faces)

(defface om-dash-cell
  '((t (:inherit default)))
  "Face used for entire non-header cell in om-dash table.
You can use it so specify cell background."
   :group 'om-dash-faces)

(defface om-dash-text
  '((t (:inherit default)))
  "Face used for text in om-dash table non-header cell.
You can use it so specify cell font."
   :group 'om-dash-faces)

(defface om-dash-number
  '((t (:inherit org-link)))
  "Face used for issue or pull request numbers in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-username
  '((t (:inherit org-document-info)))
  "Face used for github usernames in om-dash tables."
  :group 'om-dash-faces)

(define-obsolete-face-alias
  'om-dash-author
  'om-dash-username "0.3")

(defface om-dash-todo-keyword
  '((t (:inherit org-todo :weight normal)))
  "Face used for 'TODO' keyword in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-done-keyword
  '((t (:inherit org-done :weight normal)))
  "Face used for 'DONE' keyword in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-open-keyword
  '((t (:inherit om-dash-todo-keyword)))
  "Face used for 'OPEN' keyword in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-merged-keyword
  '((t (:inherit om-dash-done-keyword)))
  "Face used for 'MERGED' keyword in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-closed-keyword
  '((t (:inherit org-warning :weight normal)))
  "Face used for 'CLOSED' keyword in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-new-keyword
  '((t (:inherit org-todo :weight normal)))
  "Face used for 'NEW' keyword in om-dash tables."
  :group 'om-dash-faces)

(defface om-dash-unread-keyword
  '((t (:inherit org-todo :weight normal)))
  "Face used for 'UNREAD' keyword in om-dash tables."
   :group 'om-dash-faces)

(defface om-dash-clean-keyword
  '((t (:inherit org-done :weight normal)))
  "Face used for 'CLEAN' keyword in om-dash tables."
   :group 'om-dash-faces)

(defun om-dash--log (msg)
  "Log line to *om-dash* buffer"
  (if om-dash-verbose
      (with-current-buffer (get-buffer-create "*om-dash*")
        (goto-char (point-max))
        (insert ">>> ")
        (insert msg)
        (insert "\n"))))

(defun om-dash--canon-type (type)
  "Map type to canonical form."
  (cond ((eq type 'pr)
         (warn "'pr is deprecated, use 'pullreq instead")
         'pullreq)
        (t type)))

(defun om-dash--kwlistp (object)
  "Check if object is keyworded plist, like (:key1 val key2 val ...)."
  (and object
       (proper-list-p object)
       (> (length object) 0)
       (keywordp (car object))))

(defun om-dash--contains-p (list &rest elems)
  "Check if list has any of the elements."
  (seq-some (lambda (e)
              (seq-contains list e 'string=))
            elems))

(defun om-dash--join (separator &rest strings-or-lists)
  "Concatenate all strings and lists of strings into one flatten list.
Remove nils and empty strings.
Join resulting list into one string using a separator and return result."
  (when-let ((seq
              (seq-remove
               's-blank-p
               (apply 'seq-concatenate 'list
                      (seq-map (lambda (elem)
                                 (if (listp elem)
                                     elem
                                   (list elem)))
                               strings-or-lists)))))
    (s-join separator seq)))

(defun om-dash--in-dblock-p ()
  (let ((case-fold-search t))
    (org-between-regexps-p "^[ \t]*#\\+BEGIN:"
                           "^[ \t]*#\\+END:"
                           (point-min)
                           (point-max))))

(defun om-dash--todo-keywords ()
  "Get list of TODO keywords."
  (unless om-dash-todo-keywords
    (setq-local om-dash-todo-keywords
                (seq-concatenate 'list
                                 ;; org
                                 (seq-difference org-todo-keywords-1
                                                 org-done-keywords)
                                 ;; github
                                 (list "OPEN")
                                 ;; imap
                                 (list "NEW"
                                       "UNREAD"))))
  om-dash-todo-keywords)

(defun om-dash--done-keywords ()
  "Get list of DONE keywords."
  (unless om-dash-done-keywords
    (setq-local om-dash-done-keywords
                (seq-concatenate 'list
                                 ;; org
                                 org-done-keywords
                                 ;; github
                                 (list "MERGED"
                                       "CLOSED")
                                 ;; imap
                                 (list "CLEAN"))))
  om-dash-done-keywords)

(defun om-dash--choose-keyword (is-todo)
  "Select TODO/DONE keyword"
  (let ((todo-keywords (om-dash--todo-keywords))
        (done-keywords (om-dash--done-keywords)))
    (if is-todo
        (if todo-keywords
            (car todo-keywords)
          "TODO")
      (if done-keywords
          (car done-keywords)
        "DONE"))))

(defun om-dash--choose-face (keyword)
  "Select face for keyword"
  (let ((om-face (cdr (assoc keyword om-dash-keyword-faces)))
        (org-face (cdr (assoc keyword org-todo-keyword-faces))))
    (cond
     ;; face from om-dash-keyword-faces
     (om-face om-face)
     ;; face from org-todo-keyword-faces
     (org-face
      `(:inherit ,org-face :weight normal))
     ;; fallback to face for "TODO"
     ((and (seq-contains-p (om-dash--todo-keywords) keyword)
           (not (string= keyword "TODO")))
      (om-dash--choose-face "TODO"))
     ;; fallback to face for "DONE"
     ((and (seq-contains-p (om-dash--done-keywords) keyword)
           (not (string= keyword "DONE")))
      (om-dash--choose-face "DONE"))
     ;; give up
     (t 'default))))

(defun om-dash--choose-level ()
  "Select default outline level for a table."
  (save-excursion
    (while (om-dash--in-dblock-p)
      (org-previous-visible-heading 1))
    (1+ (org-outline-level))))

(cl-defstruct om-dash--cell
  "Struct that represents single table cell."
  text
  link)

(defun om-dash--format-tags (tags)
  "Format list of tags into string."
  ;; remap and remove tags according to om-dash-tag-map
  (setq tags
        (seq-remove 'not
                    (seq-map (lambda (tag)
                               (let ((mapping (assoc tag om-dash-tag-map)))
                                 (if mapping
                                     (cadr mapping)
                                   tag)))
                             tags)))
  ;; fix tags names
  (setq tags (seq-map (lambda (tag)
                        (s-replace-regexp "[^[:alnum:]_@#%:]" "_" tag))
                      tags))
  ;; format tagline
  (if tags
      (s-concat ":" (s-join ":" tags) ":")
    ""))

(defun om-dash--format-cell (text link width)
  "Format cell contents into string."
  (when (> (length text) width)
    (setq text (s-truncate width text)))
  (let ((padding
         (if (< (length text) width)
             (s-repeat (- width (length text)) " ")
           "")))
    (cond ((or (not link) (eq :none (or om-dash-table-link-style :none)))
           (format "%s%s" text padding))
          ((eq :text om-dash-table-link-style)
           (format "[[%s][%s]]%s" link text padding))
          ((eq :cell om-dash-table-link-style)
           (format "[[%s][%s%s]]" link text padding))
          (t
           (error "om-dash: unknown om-dash-table-link-style %S" om-dash-table-link-style)))))

(defun om-dash--insert-heading (keyword headline level)
  "Insert org heading with given text."
  (let ((stars (s-repeat level "*")))
    (insert (format "%s %s %s\n" stars keyword headline))))

(defun om-dash--insert-indent (level)
  "Insert initial indentation for specified org level."
  (let ((indent (s-repeat (1+ level) " ")))
    (insert indent)))

(defun om-dash--insert-header (cols)
  "Insert table header according to columns spec."
  (dolist (col cols)
    (let ((text (car col))
          (width (cdr col)))
      (insert "|")
      (insert (s-concat " " (s-pad-right width " " text) " "))))
  (insert "|\n"))

(defun om-dash--insert-ruler (cols)
  "Insert table ruler according to columns spec."
  (let ((col-num 0))
    (dolist (col cols)
      (let ((text (car col))
            (width (cdr col)))
        (if (eq col-num 0)
            (insert "|")
          (insert "+"))
        (insert (s-repeat (+ 2 width) "-")))
      (setq col-num (1+ col-num))))
  (insert "|\n"))

(defun om-dash--insert-cell (cell width)
  "Insert table cell, performing truncation, padding, and formatting link."
  (let* ((text (or (om-dash--cell-text cell) ""))
         (link (om-dash--cell-link cell))
         (contents
          (om-dash--format-cell text link width)))
    (insert contents)))

(defun om-dash--insert-row (cols cells)
  "Insert table row according to columns spec and cell values."
  (let ((pos 0))
    (dolist (cell cells)
      (let ((column-width (cdr (elt cols pos))))
        (insert "| ")
        (om-dash--insert-cell cell column-width)
        (insert " ")
        (setq pos (1+ pos))))
    (insert "|\n")))

(defun om-dash--insert-table (column-names rows level)
  "Generate table from column names and list of rows."
  (let* (;; index of column to stretch - by default last one
         (stretch-col (1- (length column-names)))
         ;; list of pairs ("title" . length)
         (columns (seq-map-indexed
                   (lambda (name index)
                     (when (consp name)
                       ;; if column name is ("name" . t) instead of "name",
                       ;; it means that this is the column to stretch when
                       ;; we need to fit required table width
                       (when (cdr name)
                         (setq stretch-col index))
                       (setq name (car name)))
                     (cons name (length name)))
                   column-names)))
    ;; squeeze - remove empty columns
    (when om-dash-table-squeeze-empty
      (let ((col-count (length columns)))
        (dotimes (idx col-count)
          (let ((col-num (- col-count idx 1)))
            (unless (seq-some (lambda (row)
                                (not (s-blank-str-p (om-dash--cell-text
                                                     (elt row col-num)))))
                              rows)
              ;; this column is empty, remove it
              (setq columns
                    (seq-remove-at-position columns col-num))
              (setq rows
                    (seq-map (lambda (row)
                               (seq-remove-at-position row col-num))
                             rows))
              (when (>= stretch-col col-num)
                (setq stretch-col (1- stretch-col))))))))
    ;; adjust column widths to fit contents
    (dolist (row rows)
      (let* ((col-num 0))
        (dolist (cell row)
          (let* ((cell-text (or (om-dash--cell-text cell) ""))
                 (cell-len (length cell-text))
                 (col-name (car (elt columns col-num)))
                 (col-width (cdr (elt columns col-num))))
            (if (> cell-len col-width)
                (setcdr (assoc col-name columns) cell-len)))
          (setq col-num (1+ col-num)))))
    ;; stretch - truncate or pad one column to fit total table width
    (when om-dash-table-fixed-width
      (let ((total-width 1))
        (dolist (col columns)
          (setq total-width (+ total-width 3 (cdr col))))
        (when (not (eq total-width om-dash-table-fixed-width))
          (let ((col (elt columns stretch-col)))
            (setcdr col (- om-dash-table-fixed-width
                           (- total-width (cdr col))))))))
    ;; |--------|
    (om-dash--insert-indent level)
    (om-dash--insert-ruler columns)
    ;; | header |
    (om-dash--insert-indent level)
    (om-dash--insert-header columns)
    ;; |--------|
    (om-dash--insert-indent level)
    (om-dash--insert-ruler columns)
    ;; |  cell  |
    (dolist (row rows)
      (om-dash--insert-indent level)
      (om-dash--insert-row columns row))
    ;; |--------|
    (om-dash--insert-indent level)
    (om-dash--insert-ruler columns)))

(defun om-dash--insert-newline ()
  "If current line is not empty, insert LF."
  (unless (bolp)
    (insert "\n")))

(defun om-dash--remove-empty-line ()
  "If current line is empty, remove it."
  (when (bolp)
    (backward-delete-char 1)))

(defun om-dash--table-todo-p (table)
  "Check if a table has a cell with value from 'om-dash-todo-keywords'."
  (seq-some (lambda (row)
              (seq-some (lambda (cell)
                          (seq-contains-p (om-dash--todo-keywords)
                                          (om-dash--cell-text cell)))
                        row))
            table))

(defun om-dash--shell-quote (arg)
  "Quote argument for shell"
  (let ((str (format "%s" arg)))
    ;; minimize quoting for more readable logs
    (cond ((s-matches-p "^[a-zA-Z0-9/:.,_-]+$" str)
           str)
          ((and (not (s-contains-p "'" str))
                (eq system-type 'gnu/linux))
           (format "'%s'" str))
          (t
           (shell-quote-argument str)))))

(defun om-dash--shell-run (command capture)
  "Run shell command"
  (om-dash--log command)
  (with-output-to-string
    (let ((status
           (call-process-shell-command command nil
                                       (if capture
                                           standard-output
                                         nil))))
      (unless (eq status 0)
        (om-dash--log (format "command exited with status %s" status))
        (error "om-dash: command failed")))))

(defun om-dash--parse-json (columns raw-output)
  "Parse json output to table."
  (let ((parsed-output (json-read-from-string raw-output))
        table)
    (seq-do (lambda (json)
              (let ((col-num 0)
                    row)
                (dolist (col columns)
                  (let* ((field (format "%s"
                                        (cdr (assoc (intern col) json)))))
                    (push (make-om-dash--cell :text field)
                          row))
                  (setq col-num (1+ col-num)))
                (push (nreverse row)
                      table)))
            parsed-output)
    (nreverse table)))

(defun om-dash--parse-csv (columns raw-output)
  "Parse csv output to table."
  (unless (featurep 'parse-csv)
    (error "parse-csv package not found"))
  (let ((parsed-output (parse-csv-string-rows raw-output ?\, ?\" "\n"))
        table)
    (dolist (parsed-row parsed-output)
      (when (seq-some 's-present-p parsed-row)
        (let ((col-num 0)
              row)
          (dolist (col columns)
            (let ((field (if (< col-num (length parsed-row))
                             (elt parsed-row col-num)
                           "")))
              (push (make-om-dash--cell :text field)
                    row))
            (setq col-num (1+ col-num)))
          (push (nreverse row)
                table))))
    (nreverse table)))

(defun om-dash--parse-link (link)
  "Parse org-mode link."
  (if (string-match org-link-bracket-re (s-trim link))
      (cons (match-string 1 link) (match-string 2 link))
    nil))

(defun om-dash--parse-reltime (str)
  "Parse relative timestamp like -123d."
  (when (string-match "^-\\([[:digit:]]+\\)\\([a-z][a-z]?\\)$" str)
    (let ((count (string-to-number (match-string 1 str)))
          (unit (match-string 2 str)))
      (ts-format "%Y-%m-%d"
                 (cond
                  ;; "-123d"
                  ((string= unit "d")
                   (ts-adjust 'day (- count) (ts-now)))
                  ;; "-123w"
                  ((string= unit "w")
                   (ts-adjust 'day (- (* count 7)) (ts-now)))
                  ;; "-123mo"
                  ((string= unit "mo")
                   (ts-adjust 'month (- count) (ts-now)))
                  ;; "-123y"
                  ((string= unit "y")
                   (ts-adjust 'year (- count) (ts-now)))
                  (t
                   (error "om-dash: bad timestamp %S" str)))))))

(defun om-dash--expand-template (params)
  "Return params with expanded :template (if it's present)"
  (let ((result-params
         (if-let ((template-name (plist-get params :template)))
             (let* ((template (or (assoc template-name om-dash-templates)
                                  (error "om-dash: unknown :template %s"
                                         template-name)))
                    (expanded-params (funcall (cdr template) params))
                    (merged-params (seq-copy params)))
               (while expanded-params
                 (let ((key (car expanded-params))
                       (val (cadr expanded-params)))
                   (setq merged-params (plist-put merged-params key val))
                   (setq expanded-params (cddr expanded-params))))
               merged-params)
           (seq-copy params))))
    (if (plist-member result-params :template)
        (cl-remf result-params :template))
    result-params))

(defun om-dash--gh-quote-arg (str)
  "Quote argument of github query search term."
  (s-replace "\"" "\\\""
             (s-replace "\\" "\\\\" str)))

(defun om-dash--gh-map-fields (field-map type)
  "Get field list from map by topic type."
  (let* ((type (om-dash--canon-type type))
         (key (if (eq type 'pullreq)
                  (if (assoc 'pr field-map)
                      (progn
                        (warn "'pr is deprecated, use 'pullreq instead")
                        'pr)
                    'pullreq)
                type)))
    (cdr (assoc key field-map))))

(defun om-dash--gh-build-fields-arg (type selector fields extra-fields)
  "Construct fields list for gh command."
  ;; if user didn't provide :fields, construct them automatically
  (unless fields
    ;; add all fields enabled by default
    (setq fields
          (om-dash--gh-map-fields om-dash-github-fields type))
    (if selector
        ;; add all fields that are disabled by default but are
        ;; present in jq selector
        (dolist (field
                 (om-dash--gh-map-fields om-dash-github-auto-enabled-fields type))
          (if (s-contains-p field selector)
              (setq fields
                    (append fields (list field)))))))
  (seq-uniq (append fields
                    extra-fields)))

(defun om-dash--gh-build-search-arg (state query)
  "Construct query for gh --search option."
  (let ((sub-queries
         (list (pcase state
                 (`any "state:open state:closed")
                 (`open "state:open")
                 (`closed "-state:open")
                 (_ (error "om-dash: bad state %S" state)))
               (cond
                ;; "*"
                ((string= "*" query) "")
                ;; "-123d"
                ((when-let ((timestamp (om-dash--parse-reltime query)))
                   (format "updated:>=%s" timestamp)))
                ;; passthrough
                (t query)))))
    (s-join " " (seq-remove 's-blank-p sub-queries))))

(defun om-dash--gh-build-command (repo type state filter fields extra-fields limit)
  "Construct gh command."
  (let ((type (om-dash--canon-type type))
        (query (car filter))
        (selector (cadr filter))
        command)
    (when (s-present-p query)
      (let ((fields
             (s-join "," (om-dash--gh-build-fields-arg type selector fields extra-fields)))
            (subcmd (pcase type
                      (`pullreq "pr")
                      (`issue "issue")
                      (_ (error "om-dash: bad :type %S" type))))
            (search
             (om-dash--gh-build-search-arg state query)))
        (setq command (format
                       "gh %s -R %s list --json %s --search %s --limit %s"
                       subcmd
                       (om-dash--shell-quote repo)
                       (om-dash--shell-quote fields)
                       (om-dash--shell-quote search)
                       (om-dash--shell-quote limit))))
      (when (s-present-p selector)
        (let ((expr (format "[.[] | select(%s)]"
                            selector)))
          (setq command (format "%s | jq %s"
                                command
                                (om-dash--shell-quote expr)))))
      command)))

(defun om-dash--jq-build-command (input-files sort-by)
  "Construct jq command that joins and sorts output of gh commands."
  (let* ((merge-expr
          (s-join " + " (seq-map
                         (lambda (index) (format ".[%d]" index))
                         (number-sequence 0 (1- (length input-files))))))
         (expr
          (format "%s | sort_by(.%s)"
                  merge-expr
                  sort-by)))
    (format "jq -s %s %s"
            (om-dash--shell-quote expr)
            (s-join " " (seq-map 'om-dash--shell-quote input-files)))))

(defun om-dash--github-build-extra-fields (table-columns)
  "Determine what additional github fields to we need to request."
  (apply 'append
         (seq-remove
          'not (list
                (when (seq-contains table-columns :reviewer)
                  '("reviewRequests"))
                (when (or (seq-contains table-columns :project)
                          (seq-contains table-columns :project-status))
                  '("projectItems"
                    "projectCards"))))))

(defun om-dash--github-read-topics (repo type any-filter open-filter closed-filter
                                         sort-by fields table-columns limit)
  "Construct and run command for om-dash-github."
  (let* ((extra-fields
          (om-dash--github-build-extra-fields table-columns))
         (gh-types
          (if (eq type 'any) (list 'pullreq 'issue)
            (list type)))
         (gh-commands
          (seq-remove 'not
                      (apply 'append
                             (seq-map
                              (lambda (type)
                                (list
                                 (om-dash--gh-build-command ;  :any
                                  repo type 'any any-filter fields extra-fields limit)
                                 (om-dash--gh-build-command ;  :open
                                  repo type 'open open-filter fields extra-fields limit)
                                 (om-dash--gh-build-command ;  :closed
                                  repo type 'closed closed-filter fields extra-fields limit)))
                              gh-types))))
         (gh-outputs
          (seq-map (lambda (cmd) (make-temp-file "om-dash-"))
                   gh-commands))
         (jq-command
          (om-dash--jq-build-command gh-outputs sort-by)))
    (unwind-protect
        (progn
          ;; run gh commands, save output to temp files
          (seq-mapn (lambda (cmd out)
                      (let ((gh-command
                             (format "%s > %s" cmd out)))
                        (om-dash--shell-run gh-command nil)))
                    gh-commands
                    gh-outputs)
          ;; run jq command to merge and sort gh outputs, and return result
          (om-dash--shell-run jq-command t))
      ;; delete temp files
      (dolist (out gh-outputs)
        (delete-file out)))))

(defun om-dash--github-q-timestamp (params)
  "Build queries for :created-at, :updated-at, :closed-at"
  (let* ((created-at (plist-get params :created-at))
         (updated-at (plist-get params :updated-at))
         (closed-at (plist-get params :closed-at))
         (gh-query
          (om-dash--join
           ;; note: space-separated "created:", "updated:", and "closed:"
           ;;       github queries are ANDed
           " " (cl-mapcar
                (lambda (name query)
                  (cond
                   ;; string, e.g. "2024-02-20" or "-100d"
                   ((stringp query)
                    (let ((timestamp (or (om-dash--parse-reltime query)
                                         query)))
                      (format "%s:%s" name timestamp)))
                   ;; 2-element list, e.g. (> "2024-02-20")
                   ((and (listp query) (eq (length query) 2))
                    (let ((operator (nth 0 query))
                          (timestamp (or (om-dash--parse-reltime (nth 1 query))
                                         (nth 1 query))))
                      (pcase operator
                        (`> (format "%s:>%s" name timestamp))
                        (`>= (format "%s:>=%s" name timestamp))
                        (`< (format "%s:<%s" name timestamp))
                        (`<= (format "%s:<=%s" name timestamp))
                        (_ (error "om-dash: bad :%s-at" name)))))
                   ;; 3-element list, e.g. (range "2024-01-01" "2024-02-20")
                   ((and (listp query) (eq (length query) 3))
                    (let ((operator (nth 0 query))
                          (timestamp-1 (or (om-dash--parse-reltime (nth 1 query))
                                           (nth 1 query)))
                          (timestamp-2 (or (om-dash--parse-reltime (nth 2 query))
                                           (nth 2 query))))
                      (pcase operator
                        (`range (format "%s:%s..%s" name timestamp-1 timestamp-2))
                        (_ (error "om-dash: bad :%s-at" name)))))
                   ;; not given
                   ((not query)
                    nil)
                   ;; unrecognized
                   (t
                    (error "om-dash: bad :%s-at" name))))
                (list "created" "updated" "closed")
                (list created-at updated-at closed-at)))))
    (list gh-query nil)))

(defun om-dash--github-q-milestone (params)
  "Build queries for :milestone and :no-milestone"
  (let* ((match-list (if-let ((param (plist-get params :milestone)))
                         (cond ((listp param) param)
                               ((stringp param) (list param))
                               (t (error "om-dash: bad :milestone parameter %S" param)))))
         (ignore-list (if-let ((param (plist-get params :no-milestone)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-milestone parameter %S" param)))))
         (gh-query
          ;; :milestone
          ;; note: github doesn't support multiple "milestone:" queries,
          ;;       hence this case is handled in jq below
          (when (eq (length match-list) 1)
            (pcase (car match-list)
              ;; only include topics with non-empty milestone
              ("*" "milestone:*")
              ;; only include topics with empty milestone
              ("-" "no:milestone")
              ;; only include topics with specified milestone
              (_ (format "milestone:\"%s\""
                         (om-dash--gh-quote-arg (car match-list)))))))
         (jq-selector
          (om-dash--join
           " and "
           ;; :milestone
           (when (> (length match-list) 1)
             (format "(%s)"
                     (om-dash--join
                      " or "
                      (seq-map
                       (lambda (elem)
                         (pcase elem
                           ;; only include topics with non-empty milestone
                           ("*" "(.milestone != null)")
                           ;; only include topics with empty milestone
                           ("-" "(.milestone == null)")
                           ;; only include topics with specified milestone
                           (_ (format "(.milestone.title == \"%s\")"
                                      (om-dash--gh-quote-arg elem)))))
                       match-list))))
           ;; :no-milestone
           (seq-map
            (lambda (elem)
              (pcase elem
                ;; exclude topics with non-empty milestone
                ("*" "(.milestone == null)")
                ;; exclude topics with empty milestone
                ("-" "(.milestone != null)")
                ;; exclude topics with specified milestone
                (_ (format "(.milestone.title != \"%s\")"
                           (om-dash--gh-quote-arg elem)))))
            ignore-list))))
    (list gh-query jq-selector)))

(defun om-dash--github-q-label (params)
  "Build queries for :label and :no-label"
  (let* ((any-list (if-let ((param (plist-get params :label)))
                       (cond ((listp param) param)
                             ((stringp param) (list param))
                             (t (error "om-dash: bad :label parameter %S" param)))))
         (every-list (if-let ((param (plist-get params :every-label)))
                         (cond ((listp param) param)
                               ((stringp param) (list param))
                               (t (error "om-dash: bad :every-label parameter %S" param)))))
         (ignore-list (if-let ((param (plist-get params :no-label)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-label parameter %S" param)))))
         (gh-query
          (om-dash--join
           " "
           ;; :label
           (when (and any-list
                      (not (om-dash--contains-p any-list "*" "-")))
             ;; include topic if it has any of the labels from list
             ;; note: comma-separated label names inside "label:" query are ORed
             ;; note: github doesn't support ORing label names with "*" and "-",
             ;;       so these cases are handled in jq below
             (format "label:%s"
                     (om-dash--join
                      "," (seq-map (lambda (elem)
                                     (format "\"%s\"" (om-dash--gh-quote-arg elem)))
                                   any-list))))
           ;; :every-label
           ;; note: space-separated "label:" github queries are ANDed
           (seq-map
            (lambda (elem)
              (unless (or (string= elem "*")
                          (string= elem "-"))
                ;; include topic if it has all of the labels from list
                (format "label:\"%s\""
                        (om-dash--gh-quote-arg elem))))
            every-list)))
         (jq-selector
          (om-dash--join
           " and "
           ;; :label
           (when (and any-list
                      (om-dash--contains-p any-list "*" "-"))
             ;; include topic if it has any of the labels from list
             (format "(%s)"
                     (om-dash--join
                      " or "
                      (seq-map
                       (lambda (elem)
                         (pcase elem
                           ;; match topics with non-empty labels
                           ("*" "(.labels != [])")
                           ;; match topics with empty labels
                           ("-" "(.labels == [])")
                           ;; match topics with specified label
                           (_ (format "any(.labels[]; .name == \"%s\")"
                                      (om-dash--gh-quote-arg elem)))))
                       any-list))))
           ;; :every-label
           (seq-map
            (lambda (elem)
              (pcase elem
                ;; only include topics with non-empty labels
                ("*" "(.labels != [])")
                ;; only include topics with empty labels
                ("-" "(.labels == [])")))
            every-list)
           ;; :no-label
           (seq-map
            (lambda (elem)
              (pcase elem
                ;; exclude topics with non-empty labels
                ("*" "(.labels == [])")
                ;; exclude topics with empty labels
                ("-" "(.labels != [])")
                ;; exclude topics with specific label
                (_ (format "all(.labels[]; .name != \"%s\")"
                           (om-dash--gh-quote-arg elem)))))
            ignore-list))))
    (list gh-query jq-selector)))

(defun om-dash--github-q-author (params)
  "Build queries for :author and :no-author"
  (let* ((match-list (if-let ((param (plist-get params :author)))
                         (cond ((listp param) param)
                               ((stringp param) (list param))
                               (t (error "om-dash: bad :author parameter %S" param)))))
         (ignore-list (if-let ((param (plist-get params :no-author)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-author parameter %S" param)))))
         (gh-query
          ;; :author
          ;; note: space-separated "author:" github queries are ORed
          ;; note: github doesn't support "author:*" and "no:author", so
          ;;       hence these cases are handled in jq below
          ;;       (though they are probably impossible in practice)
          (when (and match-list
                     (not (om-dash--contains-p match-list "*" "-")))
            (om-dash--join
             " " (seq-map
                  (lambda (elem)
                    ;; only include topics with specific author
                    (format "author:\"%s\""
                            (om-dash--gh-quote-arg elem)))
                  match-list))))
         (jq-selector
          (om-dash--join
           " and "
           ;; :author
           (when (and match-list
                      (om-dash--contains-p match-list "*" "-"))
             ;; include topic if it has any of the authors from list
             (format "(%s)"
                     (om-dash--join
                      " or "
                      (seq-map
                       (lambda (elem)
                         (pcase elem
                           ;; match topics with non-empty author
                           ("*" "(.author != null)")
                           ;; match topics with non author
                           ("-" "(.author == null)")
                           ;; match topics with specified author
                           (_ (format "(.author.login == \"%s\")"
                                      (om-dash--gh-quote-arg elem)))))
                       match-list))))
           ;; :no-author
           (seq-map
            (lambda (elem)
              (pcase elem
                ;; exclude topics with non-empty author
                ("*" "(.author == null)")
                ;; exclude topics with empty author
                ("-" "(.author != null)")
                ;; exclude topics with specific author
                (_ (format "(.author.login != \"%s\")"
                           (om-dash--gh-quote-arg elem)))))
            ignore-list))))
    (list gh-query jq-selector)))

(defun om-dash--github-q-assignee (params)
  "Build queries for :assignee and :no-assignee"
  (let* ((match-list (if-let ((param (plist-get params :assignee)))
                         (cond ((listp param) param)
                               ((stringp param) (list param))
                               (t (error "om-dash: bad :assignee parameter %S" param)))))
         (ignore-list (if-let ((param (plist-get params :no-assignee)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-assignee parameter %S" param)))))
         (gh-query
          (om-dash--join
           ;; :assignee
           ;; note: space-separated "assignee:" github queries are ORed
           " " (seq-map
                (lambda (elem)
                  (pcase elem
                    ;; only include topics with non-empty assignee
                    ("*" "assignee:*")
                    ;; only include topics with empty assignee
                    ("-" "no:assignee")
                    ;; only include topics with specified assignee
                    (_ (format "assignee:\"%s\""
                               (om-dash--gh-quote-arg elem)))))
                match-list)))
         (jq-selector
          ;; :no-assignee
          (when ignore-list
            (om-dash--join
             " and " (seq-map
                     (lambda (elem)
                       (pcase elem
                         ;; exclude topics with non-empty assignee
                         ("*" "(.assignees == [])")
                         ;; exclude topics with empty assignee
                         ("-" "(.assignees != [])")
                         ;; exclude topics with specified assignee
                         (_ (format "all(.assignees[]; .login != \"%s\")"
                                    (om-dash--gh-quote-arg elem)))))
                     ignore-list)))))
    (list gh-query jq-selector)))

(defun om-dash--github-q-reviewer (params)
  "Build queries for :reviewer and :no-reviewer"
  (let* ((match-list (if-let ((param (plist-get params :reviewer)))
                         (cond ((listp param) param)
                               ((stringp param) (list param))
                               (t (error "om-dash: bad :reviewer parameter %S" param)))))
         (ignore-list (if-let ((param (plist-get params :no-reviewer)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-reviewer parameter %S" param)))))
         (match-expr
          ;; :reviewer
          (when match-list
            (om-dash--join
             " or " (seq-map
                     (lambda (elem)
                       (pcase elem
                         ;; only include topics with non-empty reviewers
                         ("*" "(.reviewRequests != [])")
                         ;; only include topics with empty reviewers
                         ("-" "(.reviewRequests == [])")
                         ;; only include topics with specified reviewer
                         (_ (format "any(.reviewRequests[]; .login == \"%s\")"
                                    (om-dash--gh-quote-arg elem)))))
                     match-list))))
         (ignore-expr
          ;; :no-reviewer
          (when ignore-list
            (om-dash--join
             " and " (seq-map
                      (lambda (elem)
                        (pcase elem
                          ;; exclude topics with non-empty reviewers
                          ("*" "(.reviewRequests == [])")
                          ;; exclude topics with empty reviewers
                          ("-" "(.reviewRequests != [])")
                          ;; exclude topics with specified reviewer
                          (_ (format "all(.reviewRequests[]; .login != \"%s\")"
                                     (om-dash--gh-quote-arg elem)))))
                      ignore-list))))
         (jq-selector
          (om-dash--join
           " and " (seq-map (lambda (expr)
                              (format "(%s)" expr))
                            (seq-remove 'not
                                        (list match-expr ignore-expr))))))
    (list nil jq-selector)))

(defun om-dash--github-q-review-status (params)
  "Build queries for :review-status and :no-review-status"
  (let* ((match-list (if-let ((param (plist-get params :review-status)))
                         (cond ((listp param) param)
                               ((symbolp param) (list param))
                               (t (error "om-dash: bad :review-status %S" param)))))
         (ignore-list (if-let ((param (plist-get params :no-review-status)))
                          (cond ((listp param) param)
                                ((symbolp param) (list param))
                                (t (error "om-dash: bad :no-review-status %S" param)))))
         ;; 2-element list:
         ;;   - elem 0: expression list for match-list
         ;;   - elem 1: expression list for ignore-list
         (expr-lists
          (seq-map
           (lambda (status-list)
             (seq-map
              (lambda (status)
                (pcase status
                  ;; undecided: no review decision, no review requests, if there are
                  ;;            reviews, then only in COMMENTED or PENDING states
                  (`undecided
                   (s-concat
                    ".reviewDecision == \"\""
                    " and "
                    ".reviewRequests == []"
                    " and "
                    "all(.reviews[]; .state | IN(\"COMMENTED\", \"PENDING\"))"))
                  ;; required: review decision is REVIEW_REQUIRED
                  (`required
                   ".reviewDecision == \"REVIEW_REQUIRED\"")
                  ;; requested: at least one review request
                  (`requested
                   ".reviewRequests != []")
                  ;; commented: at least one review with COMMENTED state
                  (`commented
                   "any(.reviews[]; .state == \"COMMENTED\")")
                  ;; approved: all reviews are either in APPROVED or COMMENTED
                  ;;           state, and at least one of them is APPROVED
                  (`approved
                   (s-concat
                    "any(.reviews[]; .state == \"APPROVED\")"
                    " and "
                    "all(.reviews[]; .state | IN(\"APPROVED\", \"COMMENTED\"))"))
                  ;; rejected: at least one review in CHANGES_REQUESTED or
                  ;;           DISMISSED state
                  (`rejected
                   "any(.reviews[]; .state | IN(\"CHANGES_REQUESTED\", \"DISMISSED\"))")
                  (_
                   (error "om-dash: bad :review-status %S" status))))
              status-list))
           (list match-list
                 ignore-list)))
         ;; :review-status
         (match-exprs (car expr-lists))
         (match-selector
          (om-dash--join " or "
                         (seq-map (lambda (expr)
                                    (format "(%s)" expr))
                                  (seq-remove 'not
                                              match-exprs))))
         ;; :no-review-status
         (ignore-exprs (cadr expr-lists))
         (ignore-selector
          (om-dash--join " and "
                         (seq-map (lambda (expr)
                                    (format "((%s) | not)" expr))
                                  (seq-remove 'not
                                              ignore-exprs))))
         ;; combined
         (jq-selector
          (om-dash--join " and "
                         (seq-map (lambda (selector)
                                    (format "(%s)" selector))
                                  (seq-remove 'not
                                              (list match-selector
                                                    ignore-selector))))))
    (list nil jq-selector)))

(defun om-dash--github-q-project (params)
  "Build query for :project, :project-status :no-project-status"
  (let* ((repo (plist-get params :repo))
         (owner (car (s-split "/" repo)))
         (project (if-let ((param (plist-get params :project)))
                      (cond ((listp param) param)
                            (t (list 'v2 param)))))
         (match-status (if-let ((param (plist-get params :project-status)))
                             (cond ((listp param) param)
                                   (t (list param)))))
         (ignore-status (if-let ((param (plist-get params :no-project-status)))
                             (cond ((listp param) param)
                                   (t (list param)))))
         (gh-query
          ;; :project
          (when project
            ;; only include topics with specified project
            (format "project:%s/%s"
                    (pcase (car project)
                      (`v2 owner)
                      (`classic repo)
                      (_ (error "om-dash: bad project type %S" project-type)))
                    (cadr project))))
         (jq-selector
          (om-dash--join
           " and "
           ;; :project-status
           (seq-map (lambda (status)
                      (unless project
                        (error "om-dash: missing :project"))
                      (pcase (car project)
                        ;; only include topics with specified status name
                        (`v2
                         (format "any(.projectItems[]; .status.name == \"%s\")"
                                 (om-dash--gh-quote-arg status)))
                        ;; only include topics with specified column name
                        (`classic
                         (format "any(.projectCards[]; .column.name == \"%s\")"
                                 (om-dash--gh-quote-arg status)))))
                    match-status)
           (seq-map (lambda (status)
                      (unless project
                        (error "om-dash: missing :project"))
                      (pcase (car project)
                        ;; exclude topics with specified status name
                        (`v2
                         (format "all(.projectItems[]; .status.name != \"%s\")"
                                 (om-dash--gh-quote-arg status)))
                        ;; exclude  topics with specified column name
                        (`classic
                         (format "all(.projectCards[]; .column.name != \"%s\")"
                                 (om-dash--gh-quote-arg status)))))
                    ignore-status))))
    (list gh-query jq-selector)))

(defun om-dash--github-build-query (params plist)
  "Build gh and jq queries from plist."
  (let* (;; build sub-queries for all supported parameters
         ;; query-list is a list of pairs (gh-query jq-selector)
         ;; some elements are nil/blank when corresponding parameters
         ;; not given, we filter them out below
         (subquery-list
          (seq-map (lambda (build-func)
                     (apply build-func (append plist params) nil))
                   (list
                    #'om-dash--github-q-timestamp
                    #'om-dash--github-q-milestone
                    #'om-dash--github-q-label
                    #'om-dash--github-q-author
                    #'om-dash--github-q-assignee
                    #'om-dash--github-q-reviewer
                    #'om-dash--github-q-review-status
                    #'om-dash--github-q-project)))
         ;; join all non-empty github sub-queries
         (gh-query
          (let ((query
                 (s-join " "
                         (seq-remove 's-blank-p
                                     (seq-map (lambda (query)
                                                (nth 0 query))
                                              subquery-list)))))
            (if (s-present-p query)
                query
              ;; default github query is "*" (fetch all)
              "*")))
         ;; join all non-empty jq sub-selectors
         (jq-selector
          (let ((selector
                 (s-join " and "
                         (seq-map (lambda (expr)
                                    (format "(%s)" expr))
                                  (seq-remove 's-blank-p
                                              (seq-map (lambda (query)
                                                         (nth 1 query))
                                                       subquery-list))))))
            (if (s-present-p selector)
                selector
              ;; default jq selector is empty (no filtering)
              ""))))
    (list gh-query jq-selector)))

(defun om-dash--github-format-assignee (json)
  "Format assignee name(s) of github topic."
  (let ((assignee-list (seq-map (lambda (user)
                                  (format "@%s" (cdr (assoc 'login user))))
                                (cdr (assoc 'assignees json)))))
    (when assignee-list
      (s-join "," assignee-list))))

(defun om-dash--github-format-reviewer (json)
  "Format reviewer name(s) of github topic."
  (let ((reviewer-list (seq-uniq
                        (seq-map (lambda (request)
                                   (format "@%s" (cdr (assoc 'login request))))
                                 (cdr (assoc 'reviewRequests json))))))
    (when reviewer-list
      (s-join "," reviewer-list))))

(defun om-dash--github-format-project (json)
  "Format project name(s)."
  (when-let ((project-list
              (seq-uniq
               (if-let ((project-items (cdr (assoc 'projectItems json))))
                   (seq-map (lambda (item)
                              (cdr (assoc 'title item)))
                            project-items)
                 (if-let ((project-cards (cdr (assoc 'projectCards json))))
                     (seq-map (lambda (card)
                                (cdr (assoc 'name (cdr (assoc 'project card)))))
                              project-cards))))))
    (s-join ", " project-list)))

(defun om-dash--github-format-project-status (json)
  "Format project status name(s)."
  (when-let ((status-list
              (seq-uniq
               (if-let ((project-items (cdr (assoc 'projectItems json))))
                   (seq-map (lambda (item)
                              (cdr (assoc 'name (cdr (assoc 'status item)))))
                            project-items)
                 (if-let ((project-cards (cdr (assoc 'projectCards json))))
                     (seq-map (lambda (card)
                                (cdr (assoc 'name (cdr (assoc 'column card)))))
                              project-cards))))))
    (s-join ", " status-list)))

(defun om-dash--github-format-timestamp (date)
  "Re-format github timestamp."
  (when date
    (if-let ((ts (ts-parse date)))
        (ts-format om-dash-table-time-format ts)
      date)))

(defun om-dash--github-format-headline (type)
  "Get headline from github type."
  (let ((type (om-dash--canon-type type)))
    (pcase type
      (`issue "issues")
      (`pullreq "pull requests")
      (`any "issues and pull requests")
      (_ (error "om-dash: bad type %S" type)))))

(defun org-dblock-write:om-dash-github (params)
  "Builds org heading with a table of github issues or pull requests.

Basic example:

  #+BEGIN: om-dash-github :repo \"owner/repo\" :type issue :open \"*\" :closed \"-1w\"
  ...
  #+END:

More complicated query using simple syntax:

  #+BEGIN: om-dash-github :repo \"owner/repo\" :type pullreq :open (:milestone \"1.2.3\" :label \"blocker\" :no-label \"triage\")
  ...
  #+END:

Same query but by providing github search query and jq selector:

  #+BEGIN: om-dash-github :repo \"owner/repo\" :type pullreq :open (\"milestone:1.2.3 label:blocker\" \".labels | (.name == \\\"triage\\\") | not\")
  ...
  #+END:

Parameters:

| parameter      | default                  | description                            |
|----------------+--------------------------+----------------------------------------|
| :repo          | required                 | github repo in form “<owner>/<repo>“   |
| :type          | required                 | topic type ('issue', 'pullreq', 'any') |
| :any           | see below                | query for topics in any state          |
| :open          | see below                | query for topics in open state         |
| :closed        | see below                | query for topics in closed state       |
| :sort          | “createdAt“              | sort results by given field            |
| :fields        | 'om-dash-github-fields'  | explicitly specify list of fields      |
| :limit         | 'om-dash-github-limit'   | limit number of results                |
| :table-columns | 'om-dash-github-columns' | list of columns to display             |
| :headline      | auto                     | text for generated org heading         |
| :heading-level | auto                     | level for generated org heading        |

Parameters ':any', ':open', and ':closed' define 'QUERY' for topics in corresponding
states. You should specify either ':any' or ':open' and/or ':close'. Not specifying
anything is equavalent to :open \"*\".

'QUERY' can have one of the following forms:

 - plist: om-dash 'SIMPLE-QUERY', e.g.:
     (:milestone \"1.2.3\" :no-author \"bob\")

 - string: standard or extended 'GITHUB-QUERY', e.g.:
     \"milestone:1.2.3\"
     \"*\"
     \"-1w\"

 - list: two-element list with 'GITHUB-QUERY' and 'JQ-SELECTOR' strings, e.g.:
     (\"milestone:1.2.3\" \".author.login != \"bob\")

You can specify different queries for ':open' and ':closed' topics, e.g. to show all
open issues but only recently closed issues, use:

  :open \"*\" :closed \"-1mo\"

Or you can use a single query regardless of topic state:

  :any \"-1mo\"

'SIMPLE-QUERY' format is a convenient way to build queries for some typical
use cases. The query should be a 'plist' with the following properties:

| property           | description                                              |
|--------------------+----------------------------------------------------------|
| :created-at        | include only topics created within given date range      |
| :updated-at        | include only topics updated within given date range      |
| :closed-at         | include only topics closed within given date range       |
| :milestone         | include only topics with any of given milestone(s)       |
| :no-milestone      | exclude topics with any of given milestone(s)            |
| :label             | include only topics with any of given label(s)           |
| :every-label       | include only topics with all of given label(s)           |
| :no-label          | exclude topics with any of given label(s)                |
| :author            | include only topics with any of given author(s)          |
| :no-author         | exclude topics with any of given author(s)               |
| :assignee          | include only topics with any of given assignee(s)        |
| :no-assignee       | exclude topics with any of given assignee(s)             |
| :reviewer          | include only topics with any of given reviewer(s)        |
| :no-reviewer       | exclude topics with any of given reviewer(s)             |
| :review-status     | include only topics with any of given review status(es)  |
| :no-review-status  | exclude topics with any of given review status(es)       |
| :project           | include only topics added to given project               |
| :project-status    | include only topics with any of given project status(es) |
| :no-project-status | exclude topics with any of given project status(es)      |

All properties are optional (but at least one should be provided). Multiple
properties are ANDed, e.g. (:author \"bob\" :label \"bug\") matches topics with
author “bob“ AND label “bug“. Most properties support list form, in which case
its elements are ORed. E.g. (:author (\"bob\" \"alice\") :label \"bug\") matches
topics with label “bug“ AND author either “bob“ OR “alice“.

':created-at', ':updated-at', ':closed-at' can have one of this forms:
 - \"TIMESTAMP\"
 - (> \"TIMESTAMP\")
 - (>= \"TIMESTAMP\")
 - (< \"TIMESTAMP\")
 - (<= \"TIMESTAMP\")
 - (range \"TIMESTAMP\" \"TIMESTAMP\")

Supported 'TIMESTAMP' formats:

| format                      | description                 |
|-----------------------------+-----------------------------|
| “2024-02-20“                | date                        |
| “2024-02-20T15:59:59Z“      | utc date and time           |
| “2024-02-20T15:59:79+00:00“ | date and time with timezone |
| “-10d“                      | 10 days before today        |
| “-10w“                      | 10 weeks before today       |
| “-10mo“                     | 10 months before today      |
| “-10y“                      | 10 years before today       |

Examples:
  :created-at \"2024-02-20\"
  :updated-at (>= \"-3mo\")

':milestone', ':label', ':author', ':assignee', and ':reviewer' properties, as
well as their ':no-xxx' counterparts, can be either a string (to match one value)
or a list of strings (to match any value from the list). Two special values are
supported: '*' matches if corresponding property (e.g. assignee) is non-empty,
and '-' matches if the property unset/empty.

Examples:
  :author \"bob\"
  :assignee \"-\"
  :no-label (\"refactoring\" \"documentation\")

':every-label' is similar to ':label', but it matches topics that have all of
the labels from the list, instead of any label from list.

':review-status' property can be a symbol or a list of symbols
(to match any status from the list).

Supported values:

| status    | description                                                           |
|-----------+-----------------------------------------------------------------------|
| undecided | review not required, not requested, there're no approvals or rejects  |
| required  | review is required by repo rules                                      |
| requested | review is explicitly requested                                        |
| commented | some reviewers commented without approval or rejection                |
| approved  | all reviewers either approved or commented, and at least one approved |
| rejected  | some reviewers requested changes or dismissed review                  |

Examples:
  :review-status (required requested)
  :review-status approved
  :no-review-status (approved rejected commented)

GitHub review state model is complicated. These statuses is an attempt to provide
a simplified view of the review state for most common needs.

Note that not all statuses are mutually exclusive, in particular 'required' can
co-exist with any status except 'undecided', and 'commented' can co-exist with
any other status. You can match multiple statuses by providing a list.

':project' property can have one of the two forms:
 - number
 - (type number)

Here, number is project id (you can see it in url), and optional type is either 'v2'
or 'classic'. (Classic projects are deprecated by GitHub but are still in use). If
type is omitted, 'v2' is assumed.

':project-status' can be a string or a list of strings. For 'v2' projects, it matches
“status“ field of the project item, which corresponds to column name if board view of
the project. For 'classic' projects, it matches “column“ property of the project card.

If you use ':project-status', you should alsp specify ':project'.

Examples:
  :project 5 :project-status \"In work\"
  :project (classic 2) :project-status (\"Backlog\" \"On hold\")

'GITHUB-QUERY' is a string using github search syntax:
https://docs.github.com/en/search-github/searching-on-github/searching-issues-and-pull-requests

Besides standard syntax, a few extended forms are supported for github query:

| form     | description                                     |
|----------+-------------------------------------------------|
| “*“      | match all topics                                |
| “-123d“  | match if topic was updated during last 123 days |
| “-123w“  | same, but weeks                                 |
| “-123mo“ | same, but months                                |
| “-123y“  | same, but years                                 |

'JQ-SELECTOR' is an optional selector to filter results using jq command:
https://jqlang.github.io/jq/

Under the hood, this block uses combination of gh and jq commands like:

  gh -R <repo> issue list \\
        --json <fields> --search <github query> --limit <limit> \\
    | jq '[.[] | select(<jq selector>)]'

Exact commands being executed are printed to '*om-dash*' buffer
if 'om-dash-verbose' is set.

By default, github query uses all fields from 'om-dash-github-fields', plus any
field from 'om-dash-github-auto-enabled-fields' if it's present in jq selector.
The latter allows to exclude fields that makes queries slower, when they're
not used. To change this, you can specify ':fields' parameter explicitly.
"
  ;; expand template
  (setq params
        (om-dash--expand-template params))
  ;; parse params
  (let* ((repo (or (plist-get params :repo) (error "om-dash: missing :repo")))
         (type (or (plist-get params :type) (error "om-dash: missing :type")))
         (any-filter (if-let ((param (plist-get params :any)))
                         (cond ((om-dash--kwlistp param)
                                (om-dash--github-build-query params param))
                               ((and (listp param) (eq (length param) 2))
                                param)
                               ((stringp param)
                                (list param nil))
                               (t (error "om-dash: bad :any parameter %S" param)))))
         (open-filter (if-let ((param (plist-get params :open)))
                          (cond ((om-dash--kwlistp param)
                                 (om-dash--github-build-query params param))
                                ((and (listp param) (eq (length param) 2))
                                 param)
                                ((stringp param)
                                 (list param nil))
                                (t (error "om-dash: bad :open parameter %S" param)))))
         (closed-filter (if-let ((param (plist-get params :closed)))
                            (cond ((om-dash--kwlistp param)
                                   (om-dash--github-build-query params param))
                                  ((and (listp param) (eq (length param) 2))
                                   param)
                                  ((stringp param)
                                   (list param nil))
                                  (t (error "om-dash: bad :closed parameter %S" param)))))
         (sort-by (or (plist-get params :sort)
                      "createdAt"))
         (fields (plist-get params :fields))
         (limit (or (plist-get params :limit)
                    om-dash-github-limit))
         (table-columns (or (plist-get params :table-columns)
                            om-dash-github-columns))
         (headline (or (plist-get params :headline)
                       (format "%s (%s)" (om-dash--github-format-headline type) repo)))
         (heading-level (or (plist-get params :heading-level)
                            (om-dash--choose-level))))
    ;; validation and defaults
    (when (and (or open-filter closed-filter)
               any-filter)
      (error "om-dash: :any can't be used together with :open or :closed"))
    (unless (or any-filter open-filter closed-filter)
      (setq open-filter (list "*" nil)))
    ;; build an run command
    (let* ((raw-output (om-dash--github-read-topics
                        repo type any-filter open-filter closed-filter
                        sort-by fields table-columns limit))
           (parsed-output (json-read-from-string raw-output))
           (column-names
            (seq-map (lambda (col)
                       (pcase col
                         (`:state "state")
                         (`:number "no.")
                         ((or `:title `:title-link) '("title" . t))
                         (`:milestone "milestone")
                         (`:tags "tags")
                         (`:author "author")
                         (`:assignee "assignee")
                         (`:reviewer "reviewer")
                         (`:project "project")
                         (`:project-status "status")
                         (`:created-at "created")
                         (`:updated-at "updated")
                         (`:closed-at "closed")
                         (_ (error "om-dash: unknown table column %S" col))))
                     table-columns))
           table
           todo-p)
      (seq-do (lambda (json)
                (let ((state (cdr (assoc 'state json)))
                      (number (format "#%s" (cdr (assoc 'number json))))
                      (milestone (cdr (assoc 'title
                                             (cdr (assoc 'milestone json)))))
                      (tags (om-dash--format-tags
                             (seq-map (lambda (label) (cdr (assoc 'name label)))
                                      (cdr (assoc 'labels json)))))
                      (author (format "@%s" (cdr (assoc 'login (cdr (assoc 'author json))))))
                      (assignee (om-dash--github-format-assignee json))
                      (reviewer (om-dash--github-format-reviewer json))
                      (project (om-dash--github-format-project json))
                      (project-status (om-dash--github-format-project-status json))
                      (title (cdr (assoc 'title json)))
                      (url (cdr (assoc 'url json)))
                      (created-at (om-dash--github-format-timestamp
                                   (cdr (assoc 'createdAt json))))
                      (updated-at (om-dash--github-format-timestamp
                                   (cdr (assoc 'updatedAt json))))
                      (closed-at (om-dash--github-format-timestamp
                                  (cdr (assoc 'closedAt json)))))
                  (if (seq-contains-p (om-dash--todo-keywords) state)
                      (setq todo-p t))
                  (push
                   (seq-map (lambda (col)
                              (pcase col
                                (`:state (make-om-dash--cell :text state))
                                (`:number (make-om-dash--cell :text number :link url))
                                (`:title (make-om-dash--cell :text title))
                                (`:title-link (make-om-dash--cell :text title :link url))
                                (`:milestone (make-om-dash--cell :text milestone))
                                (`:tags (make-om-dash--cell :text tags))
                                (`:author (make-om-dash--cell :text author))
                                (`:assignee (make-om-dash--cell :text assignee))
                                (`:reviewer (make-om-dash--cell :text reviewer))
                                (`:project (make-om-dash--cell :text project))
                                (`:project-status (make-om-dash--cell :text project-status))
                                (`:created-at (make-om-dash--cell :text created-at))
                                (`:updated-at (make-om-dash--cell :text updated-at))
                                (`:closed-at (make-om-dash--cell :text closed-at))
                                (_ (error "om-dash: unknown table column %S" col))))
                            table-columns)
                   table)))
              parsed-output)
      (om-dash--insert-heading (om-dash--choose-keyword todo-p) headline heading-level)
      (when table
        (om-dash--insert-table column-names (nreverse table) heading-level))
      (om-dash--remove-empty-line))))

(defun om-dash-github:milestone (params)
  "This template is OBSOLETE.
Use 'om-dash-github' with ':milestone' query instead.
"
  ;; parse args
  (let* ((repo (or (plist-get params :repo) (error "om-dash: missing :repo")))
         (type (or (plist-get params :type) (error "om-dash: missing :type")))
         (state (or (plist-get params :state) 'open))
         (milestone (or (plist-get params :milestone) (error "om-dash: missing :milestone")))
         (headline (or (plist-get params :headline)
                       (format "%s (%s \"%s\")"
                               (om-dash--github-format-headline type)
                               repo
                               milestone)))
         (heading-level (or (plist-get params :heading-level)
                            (om-dash--choose-level))))
    ;; build query
    (let ((query
           (om-dash--github-q-milestone (list :milestone milestone))))
      ;; return modified parameters
      (append
       (list :headline headline
             :heading-level heading-level)
       (pcase state
         (`any `(:any ,query))
         (`open `(:open ,query :closed ""))
         (`closed `(:open "" :closed ,query)))))))

(make-obsolete
 'om-dash-github:milestone
 'org-dblock-write:om-dash-github "0.3")

(defun om-dash-github:project-column (params)
  "This template is OBSOLETE.
Use 'om-dash-github' with ':project-status' query instead.
"
  ;; parse args
  (let* ((repo (or (plist-get params :repo) (error "om-dash: missing :repo")))
         (type (or (plist-get params :type) (error "om-dash: missing :type")))
         (state (or (plist-get params :state) 'open))
         (project (or (plist-get params :project) (error "om-dash: missing :project")))
         (column (or (plist-get params :column) (error "om-dash: missing :column")))
         (headline (or (plist-get params :headline)
                       (format "%s (%s \"%s\")"
                               (om-dash--github-format-headline type)
                               repo
                               column)))
         (heading-level (or (plist-get params :heading-level)
                            (om-dash--choose-level))))
    ;; build query
    (let ((query
           (om-dash--github-q-project
            (list :repo repo
                  :project project
                  :project-type 'classic
                  :project-status column))))
      ;; return modified parameters
      (append
       (list :headline headline
             :heading-level heading-level)
       (pcase state
         (`any `(:any ,query))
         (`open `(:open ,query :closed ""))
         (`closed `(:open "" :closed ,query)))))))

(make-obsolete
 'om-dash-github:project-column
 'org-dblock-write:om-dash-github "0.3")

(defun om-dash--orgfile-q-nth-parent (depth pred)
  "Build org-ql query that applies predicate to nth parent."
  (while (> depth 0)
    (setq pred (list 'parent pred))
    (setq depth (1- depth)))
  pred)

(defun om-dash--orgfile-q-and (sub-queries)
  "Build org-ql query by ANDing non-nil sub-queries."
  (let ((sub-queries (seq-remove 'not sub-queries)))
    (pcase (length sub-queries)
      (0 nil)
      (1 (car sub-queries))
      (_ (append '(and) sub-queries)))))

(defun om-dash--orgfile-q-or (sub-queries)
  "Build org-ql query by ORing non-nil sub-queries."
  (let ((sub-queries (seq-remove 'not sub-queries)))
    (pcase (length sub-queries)
      (0 nil)
      (1 (car sub-queries))
      (_ (append '(or) sub-queries)))))

(defun om-dash--orgfile-q-keyword (keywords)
  "Build org-ql query to match entry keyword."
  (append '(todo) keywords))

(defun om-dash--orgfile-q-flags (blocked habit)
  "Build org-ql query to match entry blocked and habit flag."
  (when (or blocked habit)
    (om-dash--orgfile-q-and
     (list
      (pcase blocked
        (`nil nil)
        (`any nil)
        (`yes '(blocked))
        (`no '(not (blocked)))
        (_ (error "om-dash: bad :blocked value %S" blocked)))
      (pcase habit
        (`nil nil)
        (`any nil)
        (`yes '(habit))
        (`no '(not (habit)))
        (_ (error "om-dash: bad :habit value %S" habit)))))))

(defun om-dash--orgfile-q-category (any-category no-category)
  "Build org-ql query to match entry category."
  (when (or any-category no-category)
    (om-dash--orgfile-q-and
     (list
      (when any-category
        (append '(category) any-category))
      (when no-category
        `(not ,(append '(category) no-category)))))))

(defun om-dash--orgfile-q-priority (any-priority no-priority)
  "Build org-ql query to match entry priority."
  (when (or any-priority no-priority)
    (om-dash--orgfile-q-and
     (list
      (when any-priority
        (append '(priority) any-priority))
      (when no-priority
        `(not ,(append '(priority) no-priority)))))))

(defun om-dash--orgfile-q-tag (any-tag every-tag no-tag)
  "Build org-ql query for tags."
  (when (or any-tag every-tag no-tag)
    (om-dash--orgfile-q-and
     (list
      (when any-tag
        (append '(tags) any-tag))
      (when every-tag
        (append '(tags-all) every-tag))
      (when no-tag
        `(not ,(append '(tags) no-tag)))))))

(defun om-dash--orgfile-build-subquery (plist depth keywords)
  "Construct org-ql query for TODO or DONE entries."
  ;; parse parameters
  (let* ((blocked (plist-get plist :blocked))
         (habit (plist-get plist :habit))
         (any-category (if-let ((param (plist-get plist :category)))
                           (cond ((listp param) param)
                                 ((stringp param) (list param))
                                 (t (error "om-dash: bad :category parameter %S" param)))))
         (no-category (if-let ((param (plist-get plist :no-category)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-category parameter %S" param)))))
         (any-priority (if-let ((param (plist-get plist :priority)))
                           (cond ((listp param) param)
                                 ((stringp param) (list param))
                                 (t (error "om-dash: bad :priority parameter %S" param)))))
         (no-priority (if-let ((param (plist-get plist :no-priority)))
                          (cond ((listp param) param)
                                ((stringp param) (list param))
                                (t (error "om-dash: bad :no-priority parameter %S" param)))))
         (any-tag (if-let ((param (plist-get plist :tag)))
                      (cond ((listp param) param)
                            ((stringp param) (list param))
                            (t (error "om-dash: bad :tag parameter %S" param)))))
         (every-tag (if-let ((param (plist-get plist :every-tag)))
                        (cond ((listp param) param)
                              ((stringp param) (list param))
                              (t (error "om-dash: bad :every-tag parameter %S" param)))))
         (no-tag (if-let ((param (plist-get plist :no-tag)))
                     (cond ((listp param) param)
                           ((stringp param) (list param))
                           (t (error "om-dash: bad :no-tag parameter %S" param)))))
         ;; query to match keyword of top-level entry
         (kw-query (om-dash--orgfile-q-keyword keywords))
         ;; query to match parameters of nested entries
         (filter-query (om-dash--orgfile-q-and
                        (list
                         (om-dash--orgfile-q-flags blocked habit)
                         (om-dash--orgfile-q-category any-category no-category)
                         (om-dash--orgfile-q-priority any-priority no-priority)
                         (om-dash--orgfile-q-tag any-tag every-tag no-tag))))
         ;; query to list level 1 entries
         (level-1-query
          (when (>= depth 1)
            (om-dash--orgfile-q-and `((level 1)
                                      ,kw-query
                                      ,filter-query))))
         ;; queries to list level 2..depth entries
         (level-n-queries
          (seq-map
           (lambda (depth) (om-dash--orgfile-q-and
                            `((level ,depth)
                              ,(om-dash--orgfile-q-nth-parent (1- depth) kw-query)
                              ,filter-query)))
           (number-sequence 2 depth))))
    ;; combine queries
    (om-dash--orgfile-q-or (append (list level-1-query)
                                   level-n-queries))))

(defun om-dash--orgfile-build-query (plist)
  "Construct org-ql query from plist."
  (let* ((todo-depth (or (plist-get plist :todo-depth) 2))
         (done-depth (or (plist-get plist :done-depth) 1))
         (todo-query
          (om-dash--orgfile-build-subquery plist todo-depth (om-dash--todo-keywords)))
         (done-query
          (om-dash--orgfile-build-subquery plist done-depth (om-dash--done-keywords))))
    (om-dash--orgfile-q-or (list todo-query
                                 done-query))))

(defun om-dash--orgfile-run-query (path query digest)
  "Construct and run org-ql query for om-dash-orgfile."
    (om-dash--log
     (format "%s: %S" path query))
    (let ((entries (org-ql-select path query)))
      (if digest
          (seq-map (lambda (entry)
                     (let ((entry (org-element-copy entry)))
                       (org-element-put-property
                        entry :level (1+ (org-element-property :level entry)))
                       entry))
                   entries)
        entries)))

(defun om-dash--orgfile-leveled-keyword (keyword level)
  "Format entry for keyword column in om-dash-orgfile block."
  (let ((padding
         (if (> level 2)
             (s-repeat (* (- level 2) 1) " ")
           "")))
    (s-concat padding keyword)))

(defun om-dash--orgfile-leveled-title (title level)
  "Format entry for title column in om-dash-orgfile block."
  (let ((padding
         (if (> level 2)
             (s-concat
              (s-repeat (* (- level 2) 2) " ")
              "* ")
           "")))
    (s-concat padding title)))

(defun org-dblock-write:om-dash-orgfile (params)
  "Builds org headings with tables based on another org file.

Basic usage:

  #+BEGIN: om-dash-orgfile :file \"~/my/file.org\" :query (:todo-depth 2 :done-depth 1)
  ...
  #+END:

Custom org-ql query:

  #+BEGIN: om-dash-orgfile :file \"~/my/file.org\" :query (todo \"SOMEDAY\")
  ...
  #+END:

Parameters:

| parameter      | default                       | description                            |
|----------------+-------------------------------+----------------------------------------|
| :file          | required                      | path to .org file                      |
| :query         | (:todo-depth 2 :done-depth 1) | query for org entries                  |
| :digest        | nil                           | generate single table with all entries |
| :table-columns | 'om-dash-orgfile-columns'     | list of columns to display             |
| :headline      | auto                          | text for generated org headings        |
| :heading-level | auto                          | level for generated org headings       |

By default, this block generates an org heading with a table for every
top-level (i.e. level-1) org heading in specified ':file', with nested
headings represented as table rows.

If ':digest' is t, a single table with all entries is generated instead.

':query' defines what entries to retrieve from org file and add to table.
It should have one of the following forms:

 - plist: om-dash 'SIMPLE-QUERY', e.g. (:todo-depth 2 :done-depth 1)
 - list: 'ORG-QL' sexp query, e.g. (todo \"SOMEDAY\")
 - string: 'ORG-QL' string query, e.g. \"todo:SOMEDAY\"

'SIMPLE-QUERY' format is a convenient way to build queries for some typical
use cases. The query should be a 'plist' with the following properties:

| property     | default | description                                          |
|--------------+---------+------------------------------------------------------|
| :todo-depth  | 2       | nesting level for “todo“ entries                     |
| :done-depth  | 1       | nesting level for “done“ entries                     |
| :category    | nil     | include only entries with any of given category(ies) |
| :no-category | nil     | exclide entries with any of given category(ies)      |
| :priority    | nil     | include only entries with any of given priority(ies) |
| :no-priority | nil     | exclide entries with any of given priority(ies)      |
| :tag         | nil     | include only entries with any of given tag(s)        |
| :every-tag   | nil     | include only entries with all of given tag(s)        |
| :no-tag      | nil     | exclide entries with any of given tag(s)             |
| :blocked     | any     | whether to include blocked entries                   |
| :habit       | any     | whether to include habit entries                     |

Properties ':todo-depth' and ':done-depth' limit how deep the tree is
traversed for top-level headings in “todo“ and “done“ states.

For example:

 - if ':todo-depth' is 0, then level-1 headings in “todo“ state are not
   shown at all

 - if ':todo-depth' is 1, then level-1 headings in “todo“ state are shown
   \"collapsed\", i.e. org heading is generated, but without table

 - if ':todo-depth' is 2, then level-1 headings in “todo“ state are shown
   and each has a table with its level-2 children

 - if ':todo-depth' is 3, then level-1 headings in “todo“ state are shown
   and each has a table with its level-2 and level-3 children

...and so on. Same applies to ':done-depth' parameter.

Whether a keyword is considered as “todo“ or “done“ is defined by
variables 'om-dash-todo-keywords' and 'om-dash-done-keywords'.
By default they are automatically populated from 'org-todo-keywords-1'
and 'org-done-keywords', but you can set them to your own values.

':category', ':priority', and ':tag' properties, as well as their ':no-xxx'
counterparts, can be either a string (to match one value) or a list of strings
(to match any value from the list).

Examples:
  :priority \"A\"
  :no-tag (\"wip\" \"stuck\")

':every-tag' is similar to ':tag', but it matches entries that have all of
the tags from the list, instead of any tag from list.

':blocked' and ':habit' properties should be one of the three symbols: 'any'
(ignore type), 'yes' (include only entries of this type), 'no' (exclude entries).

For 'ORG-QL' sexp and string queries, see here:
https://github.com/alphapapa/org-ql?tab=readme-ov-file#queries

':headline' parameter defines text for org headings which contains
tables. If ':digest' is t, there is only one table and ':headline'
is just a string. Otherwise, there are many tables, and ':headline'
is a format string where '%s' can be used for entry title.
"
  ;; expand template
  (setq params
        (om-dash--expand-template params))
  ;; parse params
  (let* ((file (or (plist-get params :file) (error "om-dash: missing :file")))
         (file-path (expand-file-name file))
         (file-name (file-name-nondirectory file))
         (query (cond
                 ;; :query given
                 ((when-let ((query (plist-get params :query)))
                    query))
                 ;; :todo or :done given (backwards compatibility)
                 ((let ((todo-depth (plist-get params :todo))
                        (done-depth (plist-get params :done)))
                    (when (or todo-depth done-depth)
                      (warn ":todo and :done are deprecated, use :query (:todo-depth N :done-depth K)")
                      `(:todo-depth ,todo-depth :done-depth ,done-depth))))
                 ;; nothing given (default query)
                 (t '(:todo-depth nil :done-depth nil))))
         (digest (plist-get params :digest))
         (table-columns (or (plist-get params :table-columns)
                            om-dash-orgfile-columns))
         (headline (plist-get params :headline))
         (heading-level (or (plist-get params :heading-level)
                            (om-dash--choose-level))))
    ;; parse and run query
    (let* ((sexp-query (pcase query
                         ;; user provided kwlist, build org-ql sexp query from it
                         ((pred om-dash--kwlistp) (om-dash--orgfile-build-query query))
                         ;; user provided org-ql sexp query
                         ((pred listp) query)
                         ;; user provided org-ql string string, parse it to sexp
                         ((pred stringp) (org-ql--query-string-to-sexp query))
                         (_ (error "om-dash: bad :query %S" query))))
           (entries
            (om-dash--orgfile-run-query file-path sexp-query digest))
           (column-names
            (seq-map (lambda (col)
                       (pcase col
                         (`:state "state")
                         ((or `:title `:title-link) '("title" . t))
                         (`:tags "tags")
                         (_ (error "om-dash: unknown table column %S" col))))
                     table-columns))
           table)
      (when digest
        (om-dash--insert-heading (om-dash--choose-keyword (length entries))
                                 (if headline
                                     headline
                                   (format "org tasks (%s)" file-name))
                                 heading-level))
      (dolist (entry entries)
        (let* ((level (org-element-property :level entry))
               (keyword (org-element-property :todo-keyword entry))
               (leveled-keyword (om-dash--orgfile-leveled-keyword keyword level))
               (title (s-trim (car (org-element-property :title entry))))
               (leveled-title (om-dash--orgfile-leveled-title title level))
               (line-number (with-current-buffer (org-element-property :buffer entry)
                              (goto-char (org-element-property :begin entry))
                              (line-number-at-pos)))
               (url (format "%s::%s" file-path line-number))
               (tags (om-dash--format-tags
                      (org-element-property :tags entry))))
          (cond
           ((eq level 1)
            (when table
              (om-dash--insert-table column-names (nreverse table) heading-level)
              (setq table nil))
            (om-dash--insert-heading keyword
                                     (if headline
                                         (format headline title)
                                       (format "%s (%s)" title file-name))
                                     heading-level))
           (t
            (push
             (seq-map (lambda (col)
                        (pcase col
                          (`:state (make-om-dash--cell :text leveled-keyword))
                          (`:title (make-om-dash--cell :text leveled-title))
                          (`:title-link (make-om-dash--cell :text leveled-title :link url))
                          (`:tags (make-om-dash--cell :text tags))
                          (_ (error "om-dash: unknown table column %S" col))))
                      table-columns)
             table)))))
      (when table
        (om-dash--insert-table column-names (nreverse table) heading-level))
      (om-dash--remove-empty-line))))

(defun om-dash--imap-folder-stats (host port machine user password stream auth folder)
  "Connect to IMAP server and read stats for a folder tree."
  (let ((parent-folder (if (s-present-p folder)
                           (downcase folder)
                         nil))
        folder-stats)
    (unless (and (s-present-p user)
                 (s-present-p password))
      (om-dash--log (format "reading imap credentials from ~/.authinfo for machine \"%s\""
                            machine))
      (let* ((machine (if (s-present-p machine)
                          machine
                        host))
             (credentials (netrc-credentials machine)))
        (unless credentials
          (error "om-dash: machine %s not found in ~/.authinfo" machine))
        (unless (s-present-p user)
          (setq user (nth 0 credentials)))
        (unless (s-present-p password)
          (setq password (nth 1 credentials)))))
    (om-dash--log
     (format "connecting to imap server: server \"%s:%s\" user \"%s\" stream \"%s\" auth \"%s\""
             host port user stream auth))
    (with-current-buffer (imap-open host port stream auth)
      (imap-authenticate user password)
      (dolist (folder (imap-mailbox-list (s-concat parent-folder "*")))
        (when (or (not parent-folder)
                  (s-equals-p parent-folder folder)
                  (s-starts-with-p (s-concat parent-folder "/") folder))
          (om-dash--log (format "inspecting imap folder %s"
                                folder))
          (imap-mailbox-select folder)
          (let* ((total-count (length (imap-search "ALL")))
                 (new-count (length (imap-search "NEW")))
                 (unread-count (length (imap-search "UNSEEN")))
                 (state (cond ((> new-count 0) "NEW")
                              ((> unread-count 0) "UNREAD")
                              (t "CLEAN"))))
            (push (list :folder folder
                        :state state
                        :total total-count
                        :new new-count
                        :unread unread-count)
                  folder-stats)))))
    (sort folder-stats
          (lambda (a b)
            (s-less-p (plist-get a :folder)
                      (plist-get b :folder))))))

(defun org-dblock-write:om-dash-imap (params)
  "Builds org heading with a table of IMAP folder(s) and their unread mail counters.

Usage example:
  #+BEGIN: om-dash-imap :folder \"foo/bar\"
  ...
  #+END:

| parameter      | default                                | description                     |
|----------------+----------------------------------------+---------------------------------|
| :host          | 'om-dash-imap-host'                    | IMAP server hostmame            |
| :port          | 'om-dash-imap-port' or default         | IMAP server port                |
| :machine       | 'om-dash-imap-machine' or host         | ~/.authinfo machine             |
| :user          | 'om-dash-imap-user' or ~/.authinfo     | IMAP username                   |
| :password      | 'om-dash-imap-password' or ~/.authinfo | IMAP password                   |
| :stream        | 'om-dash-imap-stream' or auto          | STREAM for imap-open            |
| :auth          | 'om-dash-imap-auth' or auto            | AUTH for imap-open              |
| :table-columns | 'om-dash-imap-columns'                 | list of columns to display      |
| :headline      | auto                                   | text for generated org heading  |
| :heading-level | auto                                   | level for generated org heading |

':host' and ':port' define IMAP server address.
Host must be always set, and port is optional.

':user' and ':password' define IMAP credentials.
If not set, 'om-dash-imap' will read them from ~/.authinfo.
If ':machine' is set, it's used to search ~/.authinfo, otherwise host is used.

':stream' and ':auth' may be used to force 'imap-open' to use specific
connection and authentification types. For example, you can use 'network'
and 'login' values to force plain-text unencrypted password.

All these parameters have corresponding variables (e.g. 'om-dash-imap-host'
for ':host') which are used if paremeter is omitted. Value is considered
unset when both parameter is omitted and variable is nil.
"
  ;; expand template
  (setq params
        (om-dash--expand-template params))
  ;; parse params
  (let* ((host (or (plist-get params :host)
                   om-dash-imap-host
                   (error "om-dash: missing :host or om-dash-imap-host")))
         (port (or (plist-get params :port)
                   om-dash-imap-port))
         (machine (or (plist-get params :machine)
                      om-dash-imap-machine))
         (user (or (plist-get params :user)
                   om-dash-imap-user))
         (password (or (plist-get params :password)
                       om-dash-imap-password))
         (stream (or (plist-get params :stream)
                     om-dash-imap-stream))
         (auth (or (plist-get params :auth)
                   om-dash-imap-auth))
         (folder (or (plist-get params :folder)
                     (error "om-dash: missing :folder")))
         (table-columns (or (plist-get params :table-columns)
                            om-dash-imap-columns))
         (headline (or (plist-get params :headline)
                       (format "emails (%s)" folder)))
         (heading-level (or (plist-get params :heading-level)
                            (om-dash--choose-level))))
    ;; get stats and format table
    (let* ((column-names
            (seq-map (lambda (col)
                       (pcase col
                         (:state "state")
                         (:new "new")
                         (:unread "unread")
                         (:total "total")
                         (:folder "folder")
                         (_ (error "om-dash: unknown table column %S" col))))
                     table-columns))
           (entries
            (om-dash--imap-folder-stats host port machine user password stream auth folder))
           (todo-p
            (seq-count (lambda (entry)
                         (not (string= (plist-get entry :state)
                                       "CLEAN")))
                       entries))
           table)
      (dolist (entry entries)
        (let* ((state (plist-get entry :state))
               (new (plist-get entry :new))
               (unread (plist-get entry :unread))
               (total (plist-get entry :total))
               (folder (plist-get entry :folder)))
          (when (or om-dash-imap-empty-folders
                    (> total 0))
            (push
             (seq-map (lambda (col)
                        (pcase col
                          (:state (make-om-dash--cell :text state))
                          (:new (make-om-dash--cell :text (number-to-string new)))
                          (:unread (make-om-dash--cell :text (number-to-string unread)))
                          (:total (make-om-dash--cell :text (number-to-string total)))
                          (:folder (make-om-dash--cell :text folder))
                          (_ (error "om-dash: unknown table column %S" col))))
                      table-columns)
             table))))
      (om-dash--insert-heading (om-dash--choose-keyword todo-p)
                               headline
                               heading-level)
      (when table
        (om-dash--insert-table column-names (nreverse table) heading-level))
      (om-dash--remove-empty-line))))

(defun org-dblock-write:om-dash-command (params)
  "Builds org heading with a table from output of a shell command.

Usage example:
  #+BEGIN: om-dash-command :command \"curl -s https://api.github.com/users/octocat/repos\" :format json :columns (\"name\" \"forks_count\")
  ...
  #+END:

| parameter      | default  | description                             |
|----------------+----------+-----------------------------------------|
| :command       | required | shell command to run                    |
| :columns       | required | column names (list of strings)          |
| :format        | 'json'   | command output format ('json' or 'csv') |
| :headline      | auto     | text for generated org heading          |
| :heading-level | auto     | level for generated org heading         |

If ':format' is 'json', command output should be a JSON array of
JSON objects, which have a value for every key from ':columns'.

If ':format' is 'csv', command output should be CSV. First column
of CSV becomes value of first column from ':columns', and so on.

Note: using CSV format requires installing 'parse-csv' package
from https://github.com/mrc/el-csv
"
  ;; expand template
  (setq params
        (om-dash--expand-template params))
  ;; parse params
  (let* ((command (or (plist-get params :command)
                      (error "om-dash: missing :command")))
         (format (or (plist-get params :format) 'json))
         (columns (or (plist-get params :columns)
                      (error "om-dash: missing :columns")))
         (headline (or (plist-get params :headline)
                       (file-name-nondirectory
                        (car (split-string-shell-command command)))))
         (heading-level (or (plist-get params :heading-level)
                            (om-dash--choose-level))))
    ;; run command
    (let* ((raw-output (om-dash--shell-run command t))
           (table (pcase format
                    (`json (om-dash--parse-json columns raw-output))
                    (`csv (om-dash--parse-csv columns raw-output))
                    (_ (error "om-dash: bad :format %S" format))))
           (is-todo (om-dash--table-todo-p table)))
      (om-dash--insert-heading (om-dash--choose-keyword is-todo)
                               headline heading-level)
      (when table
        (om-dash--insert-table columns table heading-level))
      (om-dash--remove-empty-line))))

(defun org-dblock-write:om-dash-function (params)
  "Builds org heading with a table from output of a elisp function.

Usage example:
  #+BEGIN: om-dash-function :func example-func
  ...
  #+END:

| parameter      | default  | description                     |
|----------------+----------+---------------------------------|
| :func          | required | elisp function to call          |
| :args          | nil      | optional function arguments     |
| :headline      | auto     | text for generated org heading  |
| :heading-level | auto     | level for generated org heading |

The function should return a list of tables, where each table is
a 'plist' with the following properties:

| property      | default  | description                                          |
|---------------+----------+------------------------------------------------------|
| :keyword      | 'TODO'   | keyword for generated org heading                    |
| :headline     | auto     | text for generated org heading                       |
| :level        | auto     | level for generated org heading                      |
| :column-names | required | list of column names (strings)                       |
| :rows         | required | list of rows, where row is a list of cells (strings) |

If ':headline' or ':heading-level' is provided as the block parameter, it overrides
':headline' or ':level' returned from function.

Example function that returns a single 2x2 table:

  (defun example-func ()
    ;; list of tables
    (list
     ;; table plist
     (list :keyword \"TODO\"
           :headline \"example table\"
           :column-names '(\"foo\" \"bar\")
           :rows '((\"a\" \"b\")
                   (\"c\" \"d\")))))
"
  ;; expand template
  (setq params
        (om-dash--expand-template params))
  ;; parse params
  (let* ((function (or (plist-get params :func)
                       (error "om-dash: missing :func")))
         (args (plist-get params :args))
         (default-keyword (om-dash--choose-keyword nil))
         (forced-headline (or (plist-get params :headline)
                              (symbol-name function)))
         (forced-level (or (plist-get params :heading-level)
                           (om-dash--choose-level))))
    ;; run function and build table
    (dolist (table-plist (apply function args))
      (let* ((keyword (or (plist-get table-plist :keyword)
                          default-keyword))
             (headline (or forced-headline
                           (plist-get table-plist :headline)))
             (heading-level (or forced-level
                                (plist-get table-plist :level)))
             (column-names (or (plist-get table-plist :column-names)
                               (error "om-dash: missing :column-names")))
             (rows (or (plist-get table-plist :rows)
                       (error "om-dash: missing :rows")))
             table)
        (dolist (row rows)
          (push
           (seq-map (lambda (cell-text)
                      (if-let ((link (om-dash--parse-link cell-text)))
                          (make-om-dash--cell :text (car link) :link (cdr link))
                        (make-om-dash--cell :text cell-text)))
                    row)
           table))
        (om-dash--insert-heading keyword
                                 headline
                                 heading-level)
        (when table
          (om-dash--insert-table column-names (nreverse table) heading-level))
        (om-dash--remove-empty-line)))))

(defun org-dblock-write:om-dash--readme-toc (params)
  "Dynamic block to insert table of contents into README."
  (let* ((file (buffer-file-name))
         (entries (org-ql-select file '(level 2)))
         (result
          (with-temp-buffer
            (dolist (entry entries)
              (let* ((level (org-element-property :level entry))
                     (title (car (org-element-property :title entry)))
                     (link (s-concat
                            "#" (s-downcase
                                 (s-replace " " "-" title))))
                     (stars (s-repeat (- level 2) " ")))
                (insert (format "%s- [[%s][%s]]\n" stars link title))))
            (buffer-string))))
    (insert result)
    (om-dash--remove-empty-line)))

(defun org-dblock-write:om-dash--readme-symbol (params)
  "Dynamic block to insert documentation for a symbol into README."
  (let* ((symbol (plist-get params :symbol))
         (name (s-replace "org-dblock-write:" "" (symbol-name symbol)))
         (result
          (with-temp-buffer
            (insert (format "*** %s\n" name))
            (insert (cond ((functionp symbol)
                           (documentation symbol))
                          ((facep symbol)
                           (face-documentation symbol))
                          (t
                           (documentation-property
                            symbol 'variable-documentation))))
            (om-dash--insert-newline)
            ;; escape blocks
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "#\\+\\(BEGIN\\|END\\):" nil t)
                (replace-match (s-concat "," (match-string 0)) nil t)))
            ;; replace ’xxx’ with =xxx=
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward
                      "[’‘']\\([a-zA-Z0-9*:-]+\\)[’‘']" nil t)
                (replace-match (s-concat "=" (match-string 1) "=") nil t)))
            ;; replace ’ with '
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "[’‘]" nil t)
                (replace-match "'" nil t)))
            ;; replace [[ with [_[ (escape links)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "\\[\\[" nil t)
                (replace-match "[​[" nil t)))
            ;; wrap indented code blocks
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "^  \\|^ +-" nil t)
                (beginning-of-line)
                (if (looking-at "^ +-")
                    ;; if this is a list item, skip it
                    (while (looking-at "^  \\|^ +-")
                      (forward-line))
                  (progn
                    ;; otherwise, assume it's a code block
                    (insert "#+BEGIN_EXAMPLE\n")
                    (while (looking-at "^  ")
                      (forward-line))
                    (insert "#+END_EXAMPLE\n")))))
            ;; re-align tables
            (save-excursion
              (goto-char (point-min))
              (let ((tab-width 8))
                (org-table-map-tables 'org-table-align)))
            ;; return temp buffer contents
            (buffer-string))))
    (insert result)
    (om-dash--remove-empty-line)))

(defun om-dash-update-tree ()
  "Update all dynamic blocks in current tree, starting from top-level entry.

E.g., for the following document:

  * 1.               ---o
  ** 1.1    <- cursor   |
  *** 1.1.1             | [tree]
  *** 1.1.2             |
  ** 1.2             ---o
  * 2.
  ** 2.1

the function updates all blocks inside 1., 1.1, 1.1.1, 1.1.2, 1.2."
  (interactive)
  (save-excursion
    (while (org-up-heading-safe))
    (om-dash-update-subtree)))

(defun om-dash-update-subtree ()
  "Update all dynamic blocks in current subtree, starting from current entry.

E.g., for the following document:

  * 1.
  ** 1.1    <- cursor --o
  *** 1.1.1             | [subtree]
  *** 1.1.2           --o
  ** 1.2
  * 2.
  ** 2.1

the function updates all blocks inside 1.1, 1.1.1, 1.1.2."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (let* ((start-level (org-current-level))
           (start-point (point))
           (end-point (save-excursion
                        (outline-next-heading)
                        (while (and (not (eobp))
                                    (> (org-current-level) start-level))
                          (outline-next-heading))
                        (point)))
           (num-blocks 0))
      (while (and (re-search-forward "^[ \t]*#\\+BEGIN:" nil t)
                  (< (point) end-point))
        (when (om-dash--in-dblock-p)
          (save-excursion
            (org-dblock-update))
          (setq num-blocks (1+ num-blocks))))
      (message "Updated %s dynamic block(s)." num-blocks))))

(defvar om-dash--keyword-regexp ""
  "Matches TODO and DONE keywrods.")

(defconst om-dash--number-regexp
  "#[0-9]+\\|\\[\\[[^] \t\n]+\\]\\[#[0-9]+\\]\\]"
  "Matches github issue or pr number.")

(defconst om-dash--username-regexp
  "@[[:alnum:]_-]+"
  "Matches github username.")

(defconst om-dash--userlist-regexp
  (s-concat om-dash--username-regexp
            "\\(," om-dash--username-regexp "\\)*")
  "Matches list of github usernames.")

(defconst om-dash--tagline-regexp
  "\\(:[[:alnum:]_@#%:]+\\)+:"
  "Matches org tagline.")

(defconst om-dash--text-regexp ".+"
  "Matches other cell text.")

(defconst om-dash--ruler-regexp "|-"
  "Matches table ruler.")

(defconst om-dash--cell-regexp
  "|\\([ \t]*\\([^| \t\n][^|]+[^| \t\n]\\)[ \t]*\\)|"
  "Matches table cell.")

(defconst om-dash--ruler-or-cell-regexp
  (format "\\(%s\\)\\|\\(%s\\)"
          om-dash--ruler-regexp
          om-dash--cell-regexp)
  "Matches table ruler or cell.")

(defun om-dash--rebuild-regexps ()
  "Rebuild font-lock regexps based on user-defined variables."
  (setq-local om-dash--keyword-regexp
              (s-join
               "\\|"
               (seq-map 'regexp-quote
                        (seq-concatenate 'list
                                         (om-dash--todo-keywords)
                                         (om-dash--done-keywords))))))

(defun om-dash--table-start-p ()
  "Check if we're at the very first line of the table."
  (and (org-at-table-p)
       (org-at-table-hline-p)
       (save-excursion
         (previous-line)
         (not (org-at-table-p)))))

(defun om-dash--table-field-p ()
  "Check if we're inside table header or cell."
  (and (org-at-table-p)
       (org-table-check-inside-data-field t t)))

(defun om-dash--headp (cached-table-start)
  "Check if we're inside table header.
Assumes that om-dash--table-field-p returned true."
  (if cached-table-start
      ;; don't do expensive checks if we already know
      ;; where last table starts
      (eq (line-number-at-pos)
          (1+ cached-table-start))
    (and (org-table-check-inside-data-field t t)
         (save-excursion
           (previous-line)
           (and (org-at-table-hline-p)
                (progn
                  (previous-line)
                  (not (org-at-table-p))))))))

(defun om-dash--cellp (regex)
  "Check if we're inside table cell which text matches regexp.
Assumes that om-dash--table-field-p returned true."
  (and (org-table-check-inside-data-field t t)
       (save-excursion
         (re-search-backward "|" (line-beginning-position 0) t)
         (looking-at (s-concat "|\\s-*\\(" regex "\\)\\s-*|")))))

(defun om-dash--fontify (beg end face)
  "Apply face to given region."
  (add-face-text-property beg end face)
  (add-text-properties beg end (list 'font-lock-fontified t)))

(defun om-dash--fontify-tagline (beg end)
  "Apply faces to tags in tagline."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward ":" end t)
      (let ((pos (point)))
        (om-dash--fontify (1- pos) pos 'org-tag)
        (when (< beg (1- pos))
          (let* ((tag (buffer-substring-no-properties beg (1- pos)))
                 (face (org-get-tag-face tag)))
            (om-dash--fontify beg (1- pos) face)))
        (setq beg pos)))))

(defun om-dash--font-lock-hook (limit)
  "Find tables and cells and apply faces."
  (let (cached-table-start)
    (while (re-search-forward om-dash--ruler-or-cell-regexp
                              limit t)
      (backward-char 1)
      (cond
       ;; we're looking at the beginning of the table
       ((match-beginning 1)
        (when (om-dash--table-start-p)
          (setq cached-table-start
                (line-number-at-pos))))
       ;; we're looking at a table cell
       ((match-beginning 2)
        (let ;; cell contents (everything inside "| ... |")
            ((cell-beg (match-beginning 3))
             (cell-end (match-end 3))
             ;; cell text (contents with stripped out padding)
             (text-beg (match-beginning 4))
             (text-end (match-end 4))
             (text (match-string 4)))
          (when (om-dash--table-field-p)
            (if (om-dash--headp cached-table-start)
                (progn
                  ;; apply header faces
                  (om-dash--fontify cell-beg cell-end 'om-dash-header-cell)
                  (om-dash--fontify text-beg text-end 'om-dash-header-text))
              (progn
                ;; apply cell faces
                (om-dash--fontify cell-beg cell-end 'om-dash-cell)
                (cond
                 ;; keyword (TODO, DONE, ...)
                 ((om-dash--cellp om-dash--keyword-regexp)
                  (om-dash--fontify text-beg text-end
                                    (om-dash--choose-face text)))
                 ;; github pr or issue number (#123)
                 ((om-dash--cellp om-dash--number-regexp)
                  (om-dash--fontify text-beg text-end 'om-dash-number))
                 ;; github username list (@abc[,@abc,...])
                 ((om-dash--cellp om-dash--userlist-regexp)
                  (om-dash--fontify text-beg text-end 'om-dash-username))
                 ;; tagline (:tag1:tag2:)
                 ((om-dash--cellp om-dash--tagline-regexp)
                  (om-dash--fontify-tagline text-beg text-end))
                 ;; regular text
                 ((om-dash--cellp om-dash--text-regexp)
                  (om-dash--fontify text-beg text-end 'om-dash-text))))))))))))

(define-minor-mode om-dash-mode
  "om-dash minor mode.

This minor mode for .org files enables additional highlighting inside
org tables generated by om-dash dynamic blocks.

Things that are highlighted:
 - table header and cell (text and background)
 - org-mode keywords
 - issue or pull request state, number, author, etc.
 - tags

After editing keywords list, you need to reactivate minor mode for
changes to take effect.

To activate this mode automatically for specific files, you can use
local variables, e.g. add this to the end of the file:

  # Local Variables:
  # eval: (om-dash-mode 1)
  # End:"
  :lighter " OM-Dash"
  ;; rebuild regexps from user configuration
  (om-dash--rebuild-regexps)
  ;; register font-lock hook
  (font-lock-add-keywords
   nil '((om-dash--font-lock-hook)) t))

(provide 'om-dash)
;;; om-dash.el ends here

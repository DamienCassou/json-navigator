;;; json-navigator.el --- View and navigate JSON structures

;; Copyright (C) 2017-2023 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; URL: https://github.com/DamienCassou/json-navigator
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1") (hierarchy "0.6.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; View and navigate JSON structures

;;; Code:

(require 'hierarchy)
(require 'json)
(require 'seq)

(defvar json-navigator-display-length 3
  "Number of JSON elements to print for an array or object.")

(define-derived-mode json-navigator-mode special-mode "Json Navigator"
  "Major mode for json navigator.")

(defun json-navigator-object-p (json)
  "Return non-nil if JSON is an object."
  (or (null json)
      (and
       (consp json)
       (json-navigator-pair-p (car json)))))

(defun json-navigator-pair-p (json)
  "Return non-nil if JSON is an object's pair."
  (and
   (consp json)
   (symbolp (car json))))

(defun json-navigator-array-p (json)
  "Return non-nil if JSON is an array."
  (vectorp json))

(defun json-navigator--children (json)
  "Return children of JSON as a list."
  (cond
   ;; children of an object are its name/value pairs
   ((json-navigator-object-p json) json)
   ;; children of a name/value pair is the value
   ((json-navigator-pair-p json) (list (cdr json)))
   ;; children of an array are its values
   ((json-navigator-array-p json) (seq-concatenate 'list json))
   ;; other cases have no children
   (t nil)))

(defun json-navigator-create-hierarchy (json)
  "Return a hierarchy from JSON.

JSON should respect this non-default setting for `json-read':

- `json-null' ⇒ :json-nil

JSON should respect the default settings for `json-read', namely:

- `json-object-type' ⇒ `alist'

- `json-array-type' ⇒ `vector'

- `json-key-type' ⇒ nil

- `json-false' ⇒ :json-false"
  ;; wrap all JSON element in a cons with a UID so that hierarchy does
  ;; not confuse similar elements
  (hierarchy-from-list json t #'json-navigator--children))

(defun json-navigator--read-after-point ()
  "Read json after point."
  (let ((json-null :json-null))
    (save-excursion
      (json-read))))

(defun json-navigator--read-string (string)
  "Read json STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (json-navigator--read-after-point)))

(defun json-navigator--read-region (start end)
  "Read json between START and END."
  (json-navigator--read-string (buffer-substring-no-properties start end)))

(defun json-navigator--unwrap (item)
  "Return JSON element inside ITEM, ignoring UID."
  (cdr item))

(defun json-navigator--insert-ellipsis ()
  "Insert an horizontal ellipsis in current buffer."
  (insert "…"))

(defun json-navigator--insert-sequence (json-seq insertfn)
  "Insert JSON-SEQ (array or object keys) into current buffer.

Call INSERTFN on each item of JSON-SEQ."
  (let ((first t))
    (seq-map (lambda (item)
               (if first
                   (setq first nil)
                 (insert ", "))
               (funcall insertfn item))
             (seq-take json-seq json-navigator-display-length)))
  (when (> (seq-length json-seq) json-navigator-display-length)
    (insert ", ")
    (json-navigator--insert-ellipsis)))

(defun json-navigator--insert-object (json-object &optional summarize)
  "Insert JSON-OBJECT into current buffer.

If SUMMARIZE is non-nil, insert a short representation of
JSON-OBJECT instead of a full one."
  (insert "{")
  (if summarize
      (unless (seq-empty-p json-object)
        (json-navigator--insert-ellipsis))
    (json-navigator--insert-sequence json-object #'json-navigator--insert-pair))
  (insert "}"))

(defun json-navigator--insert-pair (json-pair)
  "Insert JSON-PAIR into current buffer.

The value of JSON-PAIR is summarized."
  (insert (format "\"%s\": " (car json-pair)))
  (json-navigator--insert (cdr json-pair) t))

(defun json-navigator--insert-array (json-array &optional summarize)
  "Insert JSON-ARRAY into current buffer.

If SUMMARIZE is non-nil, insert a short representation of JSON-ARRAY
instead of a full one."
  (if summarize
      (insert (format "Array[%s]" (seq-length json-array)))
    (insert "[")
    (json-navigator--insert-sequence
     json-array
     (lambda (item) (json-navigator--insert item t)))
    (insert "]")))

(defun json-navigator--insert (json &optional summarize)
  "Insert into current buffer a representation of JSON.

If SUMMARIZE is non-nil, insert a short representation of JSON
instead of a full one."
  (cond
   ((json-navigator-object-p json) (json-navigator--insert-object json summarize))
   ((json-navigator-array-p json) (json-navigator--insert-array json summarize))
   ((json-navigator-pair-p json) (insert (format "%s" (car json))))
   (t (insert (format "%s" json)))))

(defun json-navigator-display-tree (json)
  "Display hierarchy of JSON in a tree widget."
  (switch-to-buffer
   (hierarchy-tree-display
    (json-navigator-create-hierarchy json)
    (lambda (item _) (json-navigator--insert (json-navigator--unwrap item)))))
  (json-navigator-mode))

;;;###autoload
(defun json-navigator-navigate-after-point ()
  "Navigate JSON after point."
  (interactive)
  (json-navigator-display-tree (json-navigator--read-after-point)))

;;;###autoload
(defun json-navigator-navigate-region (&optional start end)
  "Navigate JSON inside region between START and END.
If START (respectively END) is nil, use `point-min' (respectively
`point-max') instead.

Interactively, if no region is active, use the whole buffer instead."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list)))
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (json-navigator-display-tree (json-navigator--read-region start end))))

(provide 'json-navigator)

;;; json-navigator.el ends here

;;  LocalWords:  json

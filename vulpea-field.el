;;; vulpea-field --- Easily add fields to vulpea database -*- lexical-binding: t -*-

;; Copyright (C) 2024 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 29 June 2024
;; Version: 1.0
;; Keywords: org capture task todo context
;; X-URL: https://github.com/jwiegley/vulpea-field

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'vulpea)
(require 'vulpea-db)

;;;###autoload
(defsubst vulpea-field-query (table-name note)
  "Query the field value for NOTE from TABLE-NAME.

TABLE-NAME is a symbol naming the database table to query.
NOTE is a vulpea-note structure.

Returns a list of query results containing the field values."
  (eval
   `(org-roam-db-query
     [:select [field] :from ,table-name
              :where (= note-id $s1)]
     ,(vulpea-note-id note))))

;;;###autoload
(defun vulpea-field-setup (table-name predicate accessor processor)
  "Set up a custom field table in the vulpea database.

TABLE-NAME is a symbol naming the database table to create.
PREDICATE is a function taking a note and returning non-nil if the
  note should have a field entry in this table.
ACCESSOR is a function taking a note and returning the raw field
  datum, or nil if no field should be stored.
PROCESSOR is a function taking a note and the datum returned by
  ACCESSOR, returning the final value to store in the database.

This function creates the table schema with a note-id primary key
and a field column, sets up appropriate indices, and registers an
insert hook to populate the table when notes are synced."
  (vulpea-db-define-table
   ;; name
   table-name
   ;; version
   1
   ;; schema
   '([(note-id :unique :primary-key)
      field]
     ;; useful to automatically cleanup your table whenever a note/node/file is removed
     (:foreign-key [note-id] :references nodes [id] :on-delete :cascade))
   ;; index
   '((field-id-index [note-id])))

  (add-hook 'vulpea-db-insert-note-functions
            (apply-partially #'vulpea-field--insert
                             table-name predicate accessor processor)))

(defun vulpea-field--insert (table-name predicate accessor processor note)
  "Insert or update a field entry for NOTE in TABLE-NAME.

TABLE-NAME is a symbol naming the database table.
PREDICATE is a function that determines if NOTE should be processed.
ACCESSOR is a function that extracts the raw field datum from NOTE.
PROCESSOR is a function that transforms the datum for storage.
NOTE is the vulpea-note being inserted.

This function first deletes any existing entry for NOTE, then inserts
a new entry if ACCESSOR returns non-nil data. Errors during insertion
are logged as warnings rather than signaled."
  (when (funcall predicate note)
    (eval
     `(org-roam-db-query
       [:delete :from ,table-name
                :where (= note-id $s1)]
       ,(vulpea-note-id note)))
    (vulpea-utils-with-note note
      (when-let ((datum (funcall accessor note)))
        (eval
         `(org-roam-db-query!
           (lambda (err)
             (lwarn 'org-roam :warning "%s for field '%s' in %s (%s) %s"
                    (error-message-string err)
                    ,datum
                    ,(vulpea-note-title note)
                    ,(vulpea-note-id note)
                    ,(vulpea-note-path note)))
           [:insert :into ,table-name :values $v1]
           (vector ,(vulpea-note-id note)
                   ,(funcall processor note datum))))))))

(provide 'vulpea-field)
;;; vulpea-field.el ends here

;;; json-navigator-test.el --- Tests for json-navigator  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'json-navigator)

(require 'assess)
(require 'buttercup)

(require 'json)

(defun json-navigator--my-insert (_)
  "Dummy function.")

(describe "json-navigator"

  (describe "object-p"
    (describe "returns non-nil"
      (it "for an empty object"
        (expect (json-navigator-object-p (json-navigator--read-string "{}"))
                :to-be-truthy))

      (it "for a non-empty object"
        (expect (json-navigator-object-p (json-navigator--read-string "{\"a\": true}"))
                :to-be-truthy)))

    (describe "returns nil"
      (it "for a pair"
        (expect (json-navigator-object-p (car (json-navigator--read-string "{\"a\": true}")))
                :not :to-be-truthy))

      (it "for an array"
        (expect (json-navigator-object-p (json-navigator--read-string "[1,2,3]"))
                :not :to-be-truthy))))

  (describe "pair-p"
    (describe "returns non-nil"
      (it "for a pair"
        (expect (json-navigator-pair-p (car (json-navigator--read-string "{\"a\": true}")))
                :to-be-truthy)))

    (describe "returns nil"
      (it "for an empty object"
        (expect (json-navigator-pair-p (json-navigator--read-string "{}"))
                :not :to-be-truthy))

      (it "for a non-empty object"
        (expect (json-navigator-pair-p (json-navigator--read-string "{\"a\": true}"))
                :not :to-be-truthy))

      (it "for an array"
        (expect (json-navigator-pair-p (json-navigator--read-string "[1,2,3]"))
                :not :to-be-truthy))))

  (describe "array-p"
    (describe "returns non-nil"
      (it "for an array"
        (expect (json-navigator-array-p (json-navigator--read-string "[1,2,3]"))
                :to-be-truthy)))

    (describe "returns nil"
      (it "for an empty object"
        (expect (json-navigator-array-p (json-navigator--read-string "{}"))
                :not :to-be-truthy))

      (it "for a non-empty object"
        (expect (json-navigator-array-p (json-navigator--read-string "{\"a\": true}"))
                :not :to-be-truthy))

      (it "for a pair"
        (expect (json-navigator-array-p (car (json-navigator--read-string "{\"a\": true}")))
                :not :to-be-truthy))))

  (describe "children"
    (it "returns null for an empty object"
      (expect (json-navigator--children (json-navigator--read-string "{}"))
              :to-be nil))

    (it "returns an object's pairs for a non-empty object"
      (expect (json-navigator--children (json-navigator--read-string "{\"a\": true}"))
              :to-equal '((a . t))))

    (it "returns a pair's value for a pair"
      (expect (json-navigator--children (car (json-navigator--read-string "{\"a\": \"foobar\"}")))
              :to-equal '("foobar")))

    (it "returns an array's values for an array"
      (expect (json-navigator--children (json-navigator--read-string "[1, 2, 3]"))
              :to-equal '(1 2 3)))

    (it "returns an empty list for a string"
      (expect (json-navigator--children (json-navigator--read-string "\"foobar\""))
              :to-be nil)))

  (describe "create-hierarchy"
    (it "calls hierarchy-from-list"
      (spy-on #'hierarchy-from-list)
      (json-navigator-create-hierarchy "json")
      (expect #'hierarchy-from-list
              :to-have-been-called-with "json" t #'json-navigator--children)))

  (describe "read-after-point"
    (it "transforms null into :json-null"
      (with-temp-buffer
        (insert "[null]")
        (goto-char (point-min))
        (let ((result (json-navigator--read-after-point)))
          (expect result :to-equal (vector :json-null))))))

  (describe "read-string"
    (it "returns the result of read-after-point"
      (spy-on #'json-navigator--read-after-point :and-return-value "read-after-point")
      (expect (json-navigator--read-string "foobar") :to-equal "read-after-point"))

    (it "calls read-after-point with point looking at the parameter"
      (spy-on #'json-navigator--read-after-point
              :and-call-fake (lambda ()
                               (expect (looking-at "foobar") :to-be-truthy)))
      (json-navigator--read-string "foobar")))

  (describe "read-region"
    (it "returns the result of read-string"
      (spy-on #'json-navigator--read-string :and-return-value "read-string")
      (with-temp-buffer
        (insert "abc")
        (expect (json-navigator--read-region 2 3) :to-equal "read-string")))

    (it "passes region as parameter to read-string"
      (spy-on #'json-navigator--read-string)
      (with-temp-buffer
        (insert "abc")
        (json-navigator--read-region 2 3)
        (expect #'json-navigator--read-string :to-have-been-called-with "b"))))

  (describe "unwrap"
    (it "returns the value of a wrapped object"
      (expect (json-navigator--unwrap (cons 1 2)) :to-be 2)))

  (describe "insert-ellipsis"
    (it "inserts an ellipsis at point"
      (with-temp-buffer
        (insert "a")
        (json-navigator--insert-ellipsis)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "a…"))))

  (describe "insert-sequence"
    (it "calls insertfn for each element"
      (spy-on #'json-navigator--my-insert)
      (with-temp-buffer (json-navigator--insert-sequence '(a b c) #'json-navigator--my-insert))
      (expect #'json-navigator--my-insert :to-have-been-called-with 'a)
      (expect #'json-navigator--my-insert :to-have-been-called-with 'b)
      (expect #'json-navigator--my-insert :to-have-been-called-with 'c))

    (it "inserts commas between elements"
      (spy-on #'json-navigator--my-insert
              :and-call-fake (lambda (item)
                               (cond
                                ((eq item 'a) (expect (point) :to-be (point-min)))
                                (t (backward-char 2)
                                   (expect (looking-at-p ", ") :to-be-truthy)))))
      (with-temp-buffer (json-navigator--insert-sequence '(a b c) #'json-navigator--my-insert))
      (expect #'json-navigator--my-insert :to-have-been-called-with 'a)
      (expect #'json-navigator--my-insert :to-have-been-called-with 'b)
      (expect #'json-navigator--my-insert :to-have-been-called-with 'c))

    (it "does not call insertfn beyond display-length limit"
      (let ((json-navigator-display-length 2))
        (spy-on #'json-navigator--my-insert)
        (with-temp-buffer (json-navigator--insert-sequence '(a b c) #'json-navigator--my-insert))
        (expect #'json-navigator--my-insert :to-have-been-called-with 'a)
        (expect #'json-navigator--my-insert :to-have-been-called-with 'b)
        (expect #'json-navigator--my-insert :not :to-have-been-called-with 'c)))

    (it "inserts ellipsis beyond display-length limit"
      (let ((json-navigator-display-length 2))
        (spy-on #'json-navigator--my-insert)
        (spy-on #'json-navigator--insert-ellipsis)
        (with-temp-buffer (json-navigator--insert-sequence '(a b c) #'json-navigator--my-insert))
        (expect #'json-navigator--insert-ellipsis :to-have-been-called)))

    (it "does not insert ellipsis inside display-length limit"
      (let ((json-navigator-display-length 4))
        (spy-on #'json-navigator--my-insert)
        (spy-on #'json-navigator--insert-ellipsis)
        (with-temp-buffer (json-navigator--insert-sequence '(a b c) #'json-navigator--my-insert))
        (expect #'json-navigator--insert-ellipsis :not :to-have-been-called))))

  (describe "insert-object"
    (describe "with nil summarize"
      (before-each
        (spy-on #'json-navigator--insert))

      (it "inserts {} for an empty object"
        (with-temp-buffer
          (json-navigator--insert-object (json-navigator--read-string "{}"))
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "{}")))

      (it "inserts each key")))

  (describe "navigate-after-point"
    (before-each
      (spy-on #'json-navigator-display-tree :and-return-value "display-tree")
      (spy-on #'json-navigator--read-after-point :and-return-value "read-after-point"))

    (it "passes the result of read-after-point to display-tree"
      (json-navigator-navigate-after-point)
      (expect #'json-navigator-display-tree :to-have-been-called-with "read-after-point")))

  (describe "navigate-region"

    (before-each
      (spy-on #'json-navigator-display-tree :and-return-value "display-tree")
      (spy-on #'json-navigator--read-region :and-return-value "read-region"))

    (it "calls read-region with start and end parameters"
      (json-navigator-navigate-region 3 5)
      (expect #'json-navigator--read-region :to-have-been-called-with 3 5))

    (it "calls read-region with point-min if start is nil"
      (spy-on #'point-min :and-return-value "point-min")
      (json-navigator-navigate-region nil 5)
      (expect #'json-navigator--read-region :to-have-been-called-with "point-min" 5)
      (expect #'point-min :to-have-been-called))

    (it "calls read-region with point-max if end is nil"
      (spy-on #'point-max :and-return-value "point-max")
      (json-navigator-navigate-region 3 nil)
      (expect #'json-navigator--read-region :to-have-been-called-with 3 "point-max")
      (expect #'point-max :to-have-been-called))

    (it "calls display-tree with the result of read-region"
      (json-navigator-navigate-region 3 5)
      (expect #'json-navigator-display-tree :to-have-been-called-with "read-region"))))

(provide 'json-navigator-test)
;;; json-navigator-test.el ends here

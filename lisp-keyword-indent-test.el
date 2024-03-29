;;; lisp-keyword-indent-test.el --- Test lisp-keyword-indent -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lisp-keyword-indent)

(when noninteractive
  (transient-mark-mode))

(add-hook 'emacs-lisp-mode-hook 'lisp-keyword-indent-mode)

(defun lisp-keyword-indent-test--keyword-at-point (_ _ rules)
  (lisp-keyword-indent--keyword-at-point rules))

(cl-defun lisp-keyword-indent-tes--execute-fn-at-point(&key expect input point-at exe-func rules)
  (should (equal expect
                 (with-temp-buffer
                   (insert input)
                   (emacs-lisp-mode)
                   (goto-char (point-max))
                   (re-search-backward point-at)
                   (funcall exe-func (point) (syntax-ppss) rules)))))

(cl-defun lisp-keyword-indent-test--indent-region (&key expect input)
  (should (equal expect
                 (with-temp-buffer
                   (insert input)
                   (emacs-lisp-mode)
                   (let ((indent-tabs-mode nil)
                         (indent-region-function nil))
                     (indent-region (point-min) (point-max)))
                   (buffer-substring-no-properties (point-min) (point-max))))))

;;; test point

(ert-deftest lsip-keyword-indent-test-keyword-at-point ()
  (mapc
   (lambda (it)
     (lisp-keyword-indent-tes--execute-fn-at-point
      :input    "'(func :foo 1 :bar 2 :qux 3)"
      :exe-func 'lisp-keyword-indent-test--keyword-at-point
      :rules    (alist-get t lisp-keyword-indent-rules)
      :point-at (car it)
      :expect   (cdr it)))
   '(("func" . nil)
     (":foo" . ":foo")
     ("1"    . nil)
     (":bar" . ":bar")
     ("2"    . nil)
     (":qux" . ":qux")
     ("3"    . nil))))

(ert-deftest lsip-keyword-indent-test-last-keyword ()
  (mapc
   (lambda (it)
     (lisp-keyword-indent-tes--execute-fn-at-point
      :input    "'(func :foo 1 :bar 2 :qux 3)"
      :exe-func 'lisp-keyword-indent--last-keyword
      :rules    (alist-get t lisp-keyword-indent-rules)
      :point-at (car it)
      :expect   (cdr it)))
   '(("func" . nil)
     (":foo" . nil)
     ("1"    . (:sexp ":foo" :point 8 :indent 7  :distance 1))
     (":bar" . (:sexp ":foo" :point 8 :indent 7  :distance 2))
     ("2"    . (:sexp ":bar" :point 15 :indent 14 :distance 1))
     (":qux" . (:sexp ":bar" :point 15 :indent 14 :distance 2))
     ("3"    . (:sexp ":qux" :point 22 :indent 21 :distance 1)))))

(ert-deftest lsip-keyword-indent-test-first-keyword ()
  (mapc
   (lambda (it)
     (lisp-keyword-indent-tes--execute-fn-at-point
      :input    "'(func :foo 1 :bar 2 :qux 3)"
      :exe-func 'lisp-keyword-indent--first-keyword
      :point-at (car it)
      :expect   (cdr it)))
   '(("func" . nil)
     (":foo" . nil)
     ("1"    . (:sexp ":foo" :point 8 :indent 7  :distance 1))
     (":bar" . (:sexp ":foo" :point 8 :indent 7  :distance 2))
     ("2"    . (:sexp ":foo" :point 8 :indent 7  :distance 3))
     (":qux" . (:sexp ":foo" :point 8 :indent 7  :distance 4))
     ("3"    . (:sexp ":foo" :point 8 :indent 7  :distance 5)))))

;;; test region

(ert-deftest lisp-keyword-indent-test-1 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(:a 1
  :b 2
  :c 3)"
   :input "\
'(:a 1
:b 2
:c 3)"))

;; test unclosed list
(ert-deftest lisp-keyword-indent-test-1-1 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(:a 1
  :b 2
  :c 3"
   :input "\
'(:a 1
  :b 2
  :c 3"))

(ert-deftest lisp-keyword-indent-test-2 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(:a
  1
  :b
  2
  :c
  3)"
   :input "\
'(:a
1
:b
2
:c
3)"))

(ert-deftest lisp-keyword-indent-test-2-2 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(:a
  1
  2
  3
  :b
  4
  5
  :c
  6)"
   :input "\
'(:a
1
2
3
:b
4
5
:c
6)"))

(ert-deftest lisp-keyword-indent-test-3 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-defun (&a
             1
           &b
             2
           &c
             3))"
   :input "\
(cl-defun (&a
1
&b
2
&c
3))"))

(ert-deftest lisp-keyword-indent-test-3-2 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(&a
    1
    2
    3
  &b
    4
    5
  &c
    6)"
   :input "\
'(&a
1
2
3
&b
4
5
&c
6)"))

(ert-deftest lisp-keyword-indent-test-4 ()
  (let ((lisp-keyword-indent-value-offset 2))
    (lisp-keyword-indent-test--indent-region
     :expect "\
'(func :a 1
       :b 2
       :c 3)"
     :input "\
'(func :a 1
:b 2
:c 3)")))

(ert-deftest lisp-keyword-indent-test-5 ()
  (let ((lisp-keyword-indent-value-offset 2))
    (lisp-keyword-indent-test--indent-region
     :expect "\
'(func arg
       :a 1
       :b 2
       :c 3)"
     :input "\
'(func arg
:a 1
:b 2
:c 3)")))

(ert-deftest lisp-keyword-indent-test-6 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(:a 1
  :b (:c 3
      :d 4))"
   :input  "\
'(:a 1
:b (:c 3
:d 4))"))

(ert-deftest lisp-keyword-indent-test-7 ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(:a
  1
  :b
  (:c
   3
   :d
   4))"
   :input  "\
'(:a
1
:b
(:c
3
:d
4))"))

(ert-deftest lisp-keyword-indent-normal-list ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(a
  b
  c)"
   :input "\
'(a
b
c)")
(lisp-keyword-indent-test--indent-region
   :expect "\
'(a b
    c)"
   :input "\
'(a b
c)")
(lisp-keyword-indent-test--indent-region
   :expect "\
'(\"a\" b
  c)"
   :input "\
'(\"a\" b
c)")
(lisp-keyword-indent-test--indent-region
   :expect "\
'((a) b
  c)"
   :input "\
'((a) b
c)"))

(ert-deftest lisp-keyword-indent-normal-list-contains-function ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
'(defun b ()
   c)"
   :input "\
'(defun b ()
c)"))

(ert-deftest lisp-keyword-indent-non-keyvalue ()
  (let ((lisp-keyword-indent-rules
         '((t . ((":" nil 2 1))))))
    (lisp-keyword-indent-test--indent-region
     :expect "\
'(:keyword
    keyvalue
  non-keyvalue)"
     :input "\
'(:keyword
keyvalue
non-keyvalue)")))

(ert-deftest lisp-keyword-indent-test-cl-loop ()
  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-loop for x
           in '(1 2 3)
         when (eq x 1)
           do (message \"This is 1\")
           and collect 1
                 into result
         else
           do (message \"This is other\")
         finally return result)"
   :input "\
(cl-loop for x
         in '(1 2 3)
         when (eq x 1)
         do (message \"This is 1\")
         and collect 1
         into result
         else
         do (message \"This is other\")
         finally return result)")

  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-loop for k
           being the hash-keys of h
           using (hash-values v)
         do (message \"%s %s\" k v))"
   :input "\
(cl-loop for k
         being the hash-keys of h
         using (hash-values v)
         do (message \"%s %s\" k v))")

  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-loop for i in nums
         when (oddp i) do
           (message \"%s\" i)
         and
           collect i)"
   :input "\
(cl-loop for i in nums
         when (oddp i) do
         (message \"%s\" i)
         and
         collect i)")

  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-loop for n
           from 1
           below 5
           by 1
         when (oddp n)
           collect n into odd-nums
         collect n into all-nums
         finally return (list :odd odd-nums :all all-nums))"
   :input "\
(cl-loop for n
         from 1
         below 5
         by 1
         when (oddp n)
         collect n into odd-nums
         collect n into all-nums
         finally return (list :odd odd-nums :all all-nums))")

  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-loop for ov being
           the overlays
           of (current-buffer)
           from (point-min)
           to (point-max)
         collect ov)"
   :input "\
(cl-loop for ov being
         the overlays
         of (current-buffer)
         from (point-min)
         to (point-max)
         collect ov)"))

(ert-deftest lisp-keyword-indent-test-cl-defmethod ()
  (skip-unless (version< "26" emacs-version))
  (lisp-keyword-indent-test--indent-region
   :expect "\
(cl-defmethod foo :before ()
  nil)"
   :input "\
(cl-defmethod foo :before ()
nil)"))

(provide 'lisp-keyword-indent-test)

;;; lisp-keyword-indent-test.el ends here

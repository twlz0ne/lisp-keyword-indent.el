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

(cl-defun lisp-keyword-indent-tes--execute-fn-at-point(&key expect input point-at exe-func)
  (should (equal expect
                 (with-temp-buffer
                   (insert input)
                   (emacs-lisp-mode)
                   (goto-char (point-max))
                   (re-search-backward point-at)
                   (funcall exe-func (ppss-innermost-start (syntax-ppss)))))))

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
      :exe-func 'lisp-keyword-indent--keyword-at-point
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
      :point-at (car it)
      :expect   (cdr it)))
   '(("func" . nil)
     (":foo" . (:sexp ":foo" :indent 7  :distance 1))
     ("1"    . (:sexp ":foo" :indent 7  :distance 2))
     (":bar" . (:sexp ":bar" :indent 14 :distance 1))
     ("2"    . (:sexp ":bar" :indent 14 :distance 2))
     (":qux" . (:sexp ":qux" :indent 21 :distance 1))
     ("3"    . (:sexp ":qux" :indent 21 :distance 2)))))

(ert-deftest lsip-keyword-indent-test-first-keyword ()
  (mapc
   (lambda (it)
     (lisp-keyword-indent-tes--execute-fn-at-point
      :input    "'(func :foo 1 :bar 2 :qux 3)"
      :exe-func 'lisp-keyword-indent--first-keyword
      :point-at (car it)
      :expect   (cdr it)))
   '(("func" . nil)
     (":foo" . (:sexp ":foo" :indent 7  :distance 1))
     ("1"    . (:sexp ":foo" :indent 7  :distance 2))
     (":bar" . (:sexp ":foo" :indent 7  :distance 3))
     ("2"    . (:sexp ":foo" :indent 7  :distance 4))
     (":qux" . (:sexp ":foo" :indent 7  :distance 5))
     ("3"    . (:sexp ":foo" :indent 7  :distance 6)))))

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
'(defun
  b
  c)"
   :input "\
'(defun
b
c)"))

(ert-deftest lisp-keyword-indent-non-keyvalue ()
  (let ((lisp-keyword-indent-rules
         '((":" . (:value-nums 1 :value-offset 2)))))
    (lisp-keyword-indent-test--indent-region
     :expect "\
'(:keyword
    keyvalue
  non-keyvalue)"
     :input "\
'(:keyword
keyvalue
non-keyvalue)")))

(provide 'lisp-keyword-indent-test)

;;; lisp-keyword-indent-test.el ends here

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

(setq lisp-indent-function 'lisp-keyword-indent)

(cl-defun lisp-keyword-indent-test-fn (&key expect-string input-string indent-words)
  (should (equal expect-string
                 (with-temp-buffer
                   (insert input-string)
                   (emacs-lisp-mode)
                   (goto-char (point-min))
                   (mapc (lambda (word)
                           (re-search-forward word nil t)
                           (backward-char)
                           (call-interactively 'indent-for-tab-command))
                         indent-words)
                   (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest lisp-keyword-indent-test-1 ()
  (lisp-keyword-indent-test-fn
   :expect-string "\
'(:a 1
  :b 2
  :c 3)"
   :input-string "\
'(:a 1
:b 2
:c 3)"
   :indent-words '(":b" ":c")))

;; test unclosed list
(ert-deftest lisp-keyword-indent-test-1-1 ()
  (lisp-keyword-indent-test-fn
   :expect-string "\
'(:a 1
     :b 2
     :c 3"
   :input-string "\
'(:a 1
:b 2
:c 3"
   :indent-words '(":b" ":c")))

(ert-deftest lisp-keyword-indent-test-2 ()
  (lisp-keyword-indent-test-fn
   :expect-string "\
'(:a
  1
  :b
  2
  :c
  3)"
   :input-string "\
'(:a
1
:b
2
:c
3)"
   :indent-words '("1" ":b" "2" ":c" "3")))

(ert-deftest lisp-keyword-indent-test-2-2 ()
  (lisp-keyword-indent-test-fn
   :expect-string "\
'(:a
  1
  2
  3
  :b
  4
  5
  :c
  6)"
   :input-string "\
'(:a
1
2
3
:b
4
5
:c
6)"
     :indent-words '("1" "2" "3" ":b" "4" "5" ":c" "6")))

(ert-deftest lisp-keyword-indent-test-3 ()
  (let ((lisp-keyword-indent-value-offset 2))
    (lisp-keyword-indent-test-fn
     :expect-string "\
'(:a
    1
  :b
    2
  :c
    3)"
     :input-string "\
'(:a
1
:b
2
:c
3)"
     :indent-words '("1" ":b" "2" ":c" "3"))))

(ert-deftest lisp-keyword-indent-test-3-2 ()
  (let ((lisp-keyword-indent-value-offset 2))
    (lisp-keyword-indent-test-fn
     :expect-string "\
'(:a
    1
    2
    3
  :b
    4
    5
  :c
    6)"
     :input-string "\
'(:a
1
2
3
:b
4
5
:c
6)"
     :indent-words '("1" "2" "3" ":b" "4" "5" ":c" "6"))))

(ert-deftest lisp-keyword-indent-test-4 ()
  (let ((lisp-keyword-indent-value-offset 2))
    (lisp-keyword-indent-test-fn
     :expect-string "\
'(func :a 1
       :b 2
       :c 3)"
     :input-string "\
'(func :a 1
:b 2
:c 3)"
     :indent-words '(":b" ":c"))))

(ert-deftest lisp-keyword-indent-test-5 ()
  (let ((lisp-keyword-indent-value-offset 2))
    (lisp-keyword-indent-test-fn
     :expect-string "\
'(func arg
       :a 1
       :b 2
       :c 3)"
     :input-string "\
'(func arg
:a 1
:b 2
:c 3)"
     :indent-words '(":a" ":b" ":c"))))

(ert-deftest lisp-keyword-indent-test-6 ()
  (lisp-keyword-indent-test-fn
   :expect-string "\
'(:a 1
  :b (:c 3
      :d 4))"
   :input-string  "\
'(:a 1
:b (:c 3
:d 4))"
   :indent-words '(":b" ":d")))

(ert-deftest lisp-keyword-indent-test-7 ()
  (lisp-keyword-indent-test-fn
   :expect-string "\
'(:a
  1
  :b
  (:c
   3
   :d
   4))"
   :input-string  "\
'(:a
1
:b
(:c
3
:d
4))"
   :indent-words '("a" "1" ":b" ":c" "3" ":d" "4")))

(provide 'lisp-keyword-indent-test)

;;; lisp-keyword-indent-test.el ends here

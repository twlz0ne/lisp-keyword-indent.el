;;; lisp-keyword-indent.el --- Keyword indent for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/02
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/twlz0ne/lisp-keyword-indent.el
;; Keywords: 

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

;; Rectify the indent of keyword for Emacs Lisp.
;;
;; Before:
;;
;;   '(:a 1
;;        :b 2
;;        :c 3)
;;
;; After:
;;
;;   '(:a 1
;;     :b 2
;;     :c 3)
;;
;; Or when the `lisp-keyword-indent-value-offset` is set to non-zero:
;;
;;   '(:a
;;        1
;;        2
;;        3
;;     :b
;;        4
;;        5
;;     :c
;;        6)
;;
;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/07/02  Initial version.

;;; Code:

(defcustom lisp-keyword-indent-keyword-starter '(":" "&")
  "Starter of keyword."
  :type 'list
  :group 'lisp-keyword-indent)

(defcustom lisp-keyword-indent-value-offset 0
  "Indent offset of the value of keyword."
  :type 'integer
  :group 'lisp-keyword-indent)

(defun lisp-keyword-indent--keyword-at-point ()
  "Return the keyword at point.

When the optional argument NO-PROPERTIES is non-nil,
strip text properties from the return value. "
  (let ((sexp (thing-at-point 'sexp)))
    (when (string-match-p
           (concat
            "\\`\\("
            (mapconcat 'identity lisp-keyword-indent-keyword-starter "\\|")
            "\\)")
           (or sexp ""))
      sexp)))

(defun lisp-keyword-indent--indent-of-first (&optional point)
  "Return the indent of first keyword before POINT.

If POINT is nil, use `(point)' as default."
    (save-excursion
      (when point
        (goto-char point))
      (let ((list-bound (bounds-of-thing-at-point 'list))
            (region-end (or (cdr (bounds-of-thing-at-point 'sexp))
                            (point))))
      (save-restriction
        (narrow-to-region (car list-bound) region-end)
        (goto-char (+ (point-min)
                      (if (eq (char-after (car list-bound)) ?\') 2 1)))
        (when (or (lisp-keyword-indent--keyword-at-point)
                  (catch 'found
                    (while (let ((last-point (point)))
                             (ignore-errors
                               (forward-sexp))
                             (not (eq last-point (point))))
                      (when (lisp-keyword-indent--keyword-at-point)
                        (throw 'found t)))))
          (ignore-errors
            (backward-sexp))
          (widen)
          (current-column))))))

(defun lisp-keyword-indent (indent-point state)
  "Reset keyword indent after `lisp-indent-function'."
  (let ((indent (funcall 'lisp-indent-function indent-point state)))
    (condition-case err
        (let* ((start-of-last (nth 2 state))
               (lisp-keyword-indent (lisp-keyword-indent--indent-of-first start-of-last)))
          (if lisp-keyword-indent
              (if (and (> lisp-keyword-indent-value-offset 0)
                       (save-excursion
                         (goto-char indent-point)
                         (forward-sexp 1)
                         (not (lisp-keyword-indent--keyword-at-point))))
                  (+ lisp-keyword-indent lisp-keyword-indent-value-offset)
                lisp-keyword-indent)
            indent))
      (error
       (print err)
       indent))))

(provide 'lisp-keyword-indent)

;;; lisp-keyword-indent.el ends here

;;; lisp-keyword-indent.el --- Keyword indent for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/02
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
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
;; Also can indent `cl-defun' like following:
;;
;;   (cl-defun func (&a
;;                     1
;;                     2
;;                     3
;;                   &b
;;                     4
;;                     5
;;                   &c
;;                     6))
;;
;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/07/02  Initial version.

;;; Code:

(require 'thingatpt)

(defcustom lisp-keyword-indent-rules
  '((":" . (:multiple-value nil :value-offset 0))
    ("&" . (:multiple-value t   :value-offset 2)))
  "Rules of keyword indent."
  :type 'list
  :group 'lisp-keyword-indent)

(defun lisp-keyword-indent--keyword-at-point (&optional no-properties)
  "Return the keyword at point.

When the optional argument NO-PROPERTIES is non-nil,
strip text properties from the return value. "
  (let ((sexp (thing-at-point 'sexp no-properties))
        (prefixes (mapcar 'car lisp-keyword-indent-rules)))
    (when (and sexp
               prefixes
               (string-match-p
                (concat
                 "\\`\\(?:"
                 (mapconcat 'regexp-quote prefixes "\\|")
                 "\\)")
                sexp))
      sexp)))

(defun lisp-keyword-indent--first-keyword (&optional point)
  (save-excursion
    (when point
      (goto-char point))
    (let ((sexp-bound (bounds-of-thing-at-point 'sexp)))
      (when sexp-bound
        (goto-char (cdr sexp-bound))))
    (let ((list-bound (bounds-of-thing-at-point 'list))
          (distance 0)
          (temp-distance 0)
          indent
          sexp)
      (when list-bound
        (save-restriction
          (narrow-to-region
           (+ (car list-bound)
              (if (eq (char-after (car list-bound)) ?\') 1 0))
           (point))
          (while (let ((first-point (point))
                       (curr-sexp (lisp-keyword-indent--keyword-at-point t)))
                   (when curr-sexp
                     (setq sexp curr-sexp)
                     (save-restriction
                       (widen)
                       (setq indent (current-column))
                       (setq distance temp-distance)))
                   (ignore-errors
                     (backward-sexp))
                   (setq temp-distance (1+ temp-distance))
                   (not (eq first-point (point)))))
          (when sexp
            (list :sexp sexp :indent indent :distance distance)))))))

(defun lisp-keyword-indent--last-keyword (&optional point)
  (save-excursion
    (when point
      (goto-char point))
    (let ((sexp-bound (bounds-of-thing-at-point 'sexp)))
      (when sexp-bound
        (goto-char (cdr sexp-bound))))
    (let ((list-bound (bounds-of-thing-at-point 'list))
          (distance 0)
          (sexp))
      (when list-bound
        (save-restriction
          (narrow-to-region
           (+ (car list-bound)
              (if (eq (char-after (car list-bound)) ?\') 1 0))
           (point))
          (when (catch 'found
                  (while (let ((last-point (point)))
                           (ignore-errors
                             (backward-sexp))
                           (setq distance (1+ distance))
                           (not (eq last-point (point))))
                    (setq sexp (lisp-keyword-indent--keyword-at-point t))
                    (when sexp (throw 'found sexp))))
            (widen)
            (list :sexp sexp :indent (current-column) :distance distance)))))))

(defun lisp-keyword-indent--origin-function ()
  (let ((advice (advice--symbol-function 'lisp-indent-function)))
    (if (advice--p advice)
        (advice--cdr advice)
      'lisp-indent-function)))

(defun lisp-keyword-indent-1 (indent-point state)
  (condition-case err
      (let* ((start-of-last (nth 2 state))
             (indent-sexp (save-excursion
                            (goto-char indent-point)
                            (ignore-errors
                              (forward-sexp 1))
                            (beginning-of-sexp)
                            (thing-at-point 'sexp t)))
             (indent-prefix (and indent-sexp
                                 (assoc (substring indent-sexp 0 1)
                                        lisp-keyword-indent-rules)
                                 t)))
        ;; indent keyword
        (if indent-prefix
            (let ((first-keyword-state (lisp-keyword-indent--first-keyword start-of-last)))
              (when first-keyword-state
                (plist-get first-keyword-state :indent)))
          ;; indent keyvalue
          (let* ((last-keyword-state (lisp-keyword-indent--last-keyword start-of-last))
                 (rule (and last-keyword-state
                        (assoc-default
                        (substring (plist-get last-keyword-state :sexp) 0 1)
                        lisp-keyword-indent-rules))))
            (when (and rule last-keyword-state
                       (or (plist-get rule :multiple-value)
                           (and (not (plist-get rule :multiple-value))
                                (< (plist-get last-keyword-state :distance) 2))))
              (+ (plist-get last-keyword-state :indent) (plist-get rule :value-offset))))))
    (error
     (print err)
     nil)))

(defun lisp-keyword-indent (indent-point state)
  "Reset keyword indent after `lisp-indent-function'."
  (let ((indent (funcall (lisp-keyword-indent--origin-function) indent-point state))
        (keyword-indent (lisp-keyword-indent-1 indent-point state)))
    (or keyword-indent indent)))

;;;###autoload
(define-minor-mode lisp-keyword-indent-mode
  "Minor mode for keyword indent of Emacs Lisp."
  :init-value nil
  :lighter ""
  :keymap nil
  (if lisp-keyword-indent-mode
      (advice-add 'lisp-indent-function :override #'lisp-keyword-indent)
    (advice-remove 'lisp-indent-function #'lisp-keyword-indent)))

(provide 'lisp-keyword-indent)

;;; lisp-keyword-indent.el ends here

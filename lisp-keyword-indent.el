;;; lisp-keyword-indent.el --- Keyword indent for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/02
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/lisp-keyword-indent.el
;; Keywords: tools

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

;;; Code:

(require 'thingatpt)

(defcustom lisp-keyword-indent-rules
  '((":" . (:value-nums 1 :value-offset 0))
    ("&" . (:value-nums t :value-offset 2)))
  "Rules of keyword indent.

Eache element of it is in the form of:

  (\"prefix\" PROPERTIES)

Following are supported properties:

:value-nums             Should be one of the 0 (no value) or 1 (1 value) or (
                        multiple values), default 0.

:value-offset           Offset of keyvalue, default 0.

:extra-check            Extrac check for keyword.  It should be nil or a
                        function takes two arguments (RULE and STATE), and
                        return t if success, e.g.:

                        (lambda (rule state)
                           (let ((point (car (last (ppss-open-parens state)))))
                             (when point
                               (save-excursion
                                 (goto-char (1+ point))
                                 (not (eq 'if (symbol-at-point)))))))"
  :type 'list
  :group 'lisp-keyword-indent)

(defcustom list-keyword-indent-ignore-forms
  '(defmethod cl-defmethod advice-add)
  "List of forms that don't apply keywork indent."
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

(defun lisp-keyword-indent--first-keyword (start &optional point)
  (save-excursion
    (when point
      (goto-char point))
    (let ((sexp-bound (bounds-of-thing-at-point 'sexp)))
      (when sexp-bound
        (goto-char (cdr sexp-bound))))
    (let ((distance 0)
          (temp-distance 0)
          indent
          sexp)
      (save-restriction
        (narrow-to-region
         (+ start (if (eq (char-after start) ?\') 1 0))
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
          (list :sexp sexp :indent indent :distance distance))))))

(defun lisp-keyword-indent--last-keyword (start &optional point)
  "Return last keyword before POINT.

Return value is in the form of:

  (:sexp     SEXP       ;; keyword sexp
   :indent   INDENT     ;; indent of keyword
   :distance DISTANCE)  ;; distance from point (in sexps)"
  (save-excursion
    (when point
      (goto-char point))
    (let ((sexp-bound (bounds-of-thing-at-point 'sexp)))
      (when sexp-bound
        (goto-char (cdr sexp-bound))))
    (let ((distance 0)
          (sexp))
      (when (save-excursion
              (goto-char start)
              (down-list)
              (not (memq (sexp-at-point) list-keyword-indent-ignore-forms)))
        (save-restriction
          (narrow-to-region
           (+ start
              (if (eq (char-after start) ?\') 1 0))
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
  (let ((advice (advice--symbol-function lisp-indent-function)))
    (if (advice--p advice)
        (advice--cdr advice)
      lisp-indent-function)))

(defalias 'lisp-keyword-indent--beginning-of-sexp
  (cond ((fboundp 'thing-at-point--beginning-of-sexp) 'thing-at-point--beginning-of-sexp)
        ((fboundp 'beginning-of-sexp) 'beginning-of-sexp)))

(defun lisp-keyword-indent-1 (indent-point state)
  (let* ((start-of-last (nth 2 state))
         (start-of-innermost (ppss-innermost-start state))
         (indent-sexp (save-excursion
                        (goto-char indent-point)
                        (when (ignore-errors (forward-sexp 1) t)
                          (lisp-keyword-indent--beginning-of-sexp)
                          (thing-at-point 'sexp t))))
         (indent-rule (and indent-sexp
                           (cdr (assoc (substring indent-sexp 0 1)
                                       lisp-keyword-indent-rules)))))
    (or
     ;; indent keyword
     (if indent-rule
         (let* ((checker (plist-get indent-rule :extra-check))
                (first-keyword-state
                 (when (if checker (funcall checker indent-rule state) t)
                   (lisp-keyword-indent--first-keyword start-of-innermost
                                                       start-of-last))))
           (when first-keyword-state
             (plist-get first-keyword-state :indent)))
       ;; indent keyvalue
       (let* ((last-sate (lisp-keyword-indent--last-keyword start-of-innermost
                                                            start-of-last))
              (last-rule (when last-sate
                           (assoc-default
                            (substring (plist-get last-sate :sexp) 0 1)
                            lisp-keyword-indent-rules)))
              (value-nums (when last-rule
                            (or (plist-get last-rule :value-nums) 0)))
              (distance (plist-get last-sate :distance)))
         (when value-nums
           (if (or (not (numberp value-nums))
                   (and (numberp value-nums) (<= distance value-nums)))
               (+ (plist-get last-sate :indent)
                  (or (plist-get last-rule :value-offset) 0))
             ;; not keyvalue, align prev keyword
             (+ (plist-get last-sate :indent))))))
     ;; no rule
     (let ((outer-start (car (reverse (nth 9 state)))))
       ;; in quote list
       (when (and (eq (char-before outer-start) ?\')
                  (eq (char-after outer-start) ?\()
                  (not (or (eq (char-after (1+ outer-start)) ?\")
                           (eq (char-after (1+ outer-start)) ?\())))
         ;; align prev sexp
         (save-excursion
           (goto-char indent-point)
           (backward-sexp)
           (current-column)))))))

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
      (advice-add lisp-indent-function :override #'lisp-keyword-indent)
    (advice-remove lisp-indent-function #'lisp-keyword-indent)))

;;;###autoload
(define-globalized-minor-mode global-lisp-keyword-indent-mode
  lisp-keyword-indent-mode (lambda () (lisp-keyword-indent-mode 1)))

(provide 'lisp-keyword-indent)

;;; lisp-keyword-indent.el ends here

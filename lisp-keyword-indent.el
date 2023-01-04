;;; lisp-keyword-indent.el --- Keyword indent for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/02
;; Version: 0.3.5
;; Last-Updated: 2023-01-04 21:05:49 +0800
;;           by: Gong Qijian
;; Package-Requires: ((emacs "25.1"))
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
(require 'rx)

(defvar lisp-keyword-indent--cl-loop-initialization-keywords
  '("for" "with" "repeat" "named")
  "Keywords which introduce initialization clauses.")

(defvar lisp-keyword-indent--cl-loop-iteration-keywords
  '("while" "until" "always" "never" "thereis")
  "Keywords which introduce iteration clauses.")

(defvar lisp-keyword-indent--cl-loop-condition-keywords
  '("if" "when" "unless" "else" "end")
  "keywords introduce condition clauses.")

(defvar lisp-keyword-indent--cl-loop-accumulation-keywords
  '("collect" "append" "nconc" "concat" "vconcat" "count" "sum"
    "maximize" "minimize")
  "Keywords which introduce accumulation clauses.")

(defvar lisp-keyword-indent-cl-loop-regexp
  (rx-to-string
   `(seq (or ,@lisp-keyword-indent--cl-loop-initialization-keywords
             ,@lisp-keyword-indent--cl-loop-iteration-keywords
             ,@lisp-keyword-indent--cl-loop-condition-keywords
             "initially" "finally" "return")
         eow))
  "Regexp matching the cl-loop keywords.")

(defun lisp-keyword-indent--check-cl-loop-do-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let ((kw (lisp-keyword-indent--last-keyword indent-point state rules)))
        (when (or (member (plist-get kw :sexp) '("and"))
                  (member (plist-get kw :sexp)
                          lisp-keyword-indent--cl-loop-condition-keywords))
          (+ 2 (plist-get kw :indent)))))))

(defun lisp-keyword-indent--back-indentation (point)
  "Return back indentation and point at POINT."
  (save-excursion
    (goto-char point)
    (back-to-indentation)
    (cons (current-column) (point))))

(defun lisp-keyword-indent--cl-loop-do-and-body-indent (_ state _)
  "Return indent of do/and body."
  (pcase-let* ((keyword-point (plist-get state :point))
               (`(,back-indent . ,back-point)
                (lisp-keyword-indent--back-indentation keyword-point)))
    (if (equal back-point keyword-point)
        (+ (plist-get state :indent) 2)
      (+ back-indent 2))))

(defun lisp-keyword-indent--check-cl-loop-and-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let ((kw (lisp-keyword-indent--last-keyword indent-point state rules)))
        (when (member (plist-get kw :sexp) '("do"))
          (pcase-let* ((keyword-point (plist-get kw :point))
                       (`(,back-indent . ,back-point)
                        (lisp-keyword-indent--back-indentation keyword-point)))
            (if (equal back-point keyword-point)
                (plist-get kw :indent)
              back-indent)))))))

(defun lisp-keyword-indent--check-cl-loop-accumulation-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let* ((kw (lisp-keyword-indent--last-keyword indent-point state rules))
             (sexp (plist-get kw :sexp)))
        (when (or (member sexp '("do" "and"))
                  (member sexp lisp-keyword-indent--cl-loop-condition-keywords))
          (pcase-let* ((keyword-point (plist-get kw :point))
                       (`(,back-indent . ,back-point)
                        (lisp-keyword-indent--back-indentation keyword-point)))
            (if (equal back-point keyword-point)
                (+ (plist-get kw :indent) 2)
              (+ back-indent 2))))))))

(defcustom lisp-keyword-indent-rules
  (list
   '(t
     (":\\(?:\\sw+\\)" nil 0 1)
     ("&\\(?:\\sw+\\)" nil 2 t))
   '(cl-defmethod . nil)
   (list
    'cl-loop
    ;; for/with/repeat/named
    ;; while/until/always/never/thereis
    ;; if/when/unless/else/end
    ;; initially/finally/return
    (list lisp-keyword-indent-cl-loop-regexp nil 2 t)
    ;; do clause
    (list
     (rx-to-string '(seq (or "do") eow))
     #'lisp-keyword-indent--check-cl-loop-do-clause
     #'lisp-keyword-indent--cl-loop-do-and-body-indent
     t)
    ;; and clause
    (list
     (rx-to-string '(seq (or "and") eow))
     #'lisp-keyword-indent--check-cl-loop-and-clause
     #'lisp-keyword-indent--cl-loop-do-and-body-indent
     t)
    ;; collect/append/nconc/concat/vconcat/count/sum/maximize/minimize
    (list
     (rx-to-string `(seq (or ,@lisp-keyword-indent--cl-loop-accumulation-keywords) eow))
     #'lisp-keyword-indent--check-cl-loop-accumulation-clause
     2 t))
   '(loop . cl-loop))
  "A list of indent rules.

Eache element of it can be in one of the following forms:

  (t    . (INDENT-RULE ...))    ;; for all forms
  (FORM . (INDENT-RULE ...))    ;; for specific form
  (FORM . ANOTHER-FORM)         ;; same as another form
  (FORM . nil)                  ;; no rule for form

Each INDENT-RULE should be in the following form:

  (ANCHOR-MATCHER       ;; regex to match the indent anchor.
   ANCHOR-INDENTER      ;; Indentation of the anchor it self.
   ITEM-INDENTER        ;; Indentation of the anchor\\='s item.
   ITEM-NUMS)           ;; How many items the indentation apply on, t means all.

An INDENTER can be nil (use default indent), a number (offset of default indent)
or a function accepts 3 arguments (INDENT-POINT STATE FORM-RULES) and return
an abs indent if success, for example:

  (lambda (indent-point state rules)
    (let ((point (car (last (ppss-open-parens state)))))
      (when point
        (save-excursion
          (goto-char (1+ point))
          (not (eq \\='if (symbol-at-point)))))))"
  :type 'list
  :group 'lisp-keyword-indent)

(defun lisp-keyword-indent--rule-at-point (&optional rules)
  "Return the rule at point by RULES."
  (let ((sexp (thing-at-point 'sexp t)))
    (when sexp
      (catch 'break
        (dolist (rule (or rules (alist-get t lisp-keyword-indent-rules)))
          (when (string-match (concat "\\`\\(?:" (car rule) "\\)") sexp)
            (throw 'break (cons sexp rule))))))))

(defun lisp-keyword-indent--keyword-at-point (&optional rules)
  "Return the keyword at point by RULES."
  (let ((sexp (thing-at-point 'sexp t)))
    (when sexp
      (catch 'break
        (dolist (rule (or rules (alist-get t lisp-keyword-indent-rules)))
          (when (string-match-p (concat "\\`\\(?:" (car rule) "\\)") sexp)
            (throw 'break sexp)))))))

(defun lisp-keyword-indent--first-keyword (indent-point _state rules)
  "Return first keyword before POINT.

Return value is in the form of:

  (:sexp     SEXP       ;; keyword sexp
   :point    POINT      ;; point of keyword
   :indent   INDENT     ;; indent of keyword
   :distance DISTANCE)  ;; numbers of sexp between INDENT-POINT and keyword"
  (let ((temp-distance 0)
        distance
        column
        point
        sexp)
    (save-excursion
      (goto-char indent-point)
      (while (unless (bobp)
               (condition-case nil (progn (backward-sexp) t) (scan-error nil)))
        (let ((curr-sexp (lisp-keyword-indent--keyword-at-point rules)))
            (setq temp-distance (1+ temp-distance))
            (when curr-sexp
              (setq sexp curr-sexp)
              (setq point (point))
              (setq column (current-column))
              (setq distance temp-distance))))
      (when sexp
        (list :sexp sexp :point point :indent column :distance distance)))))

(defun lisp-keyword-indent--last-keyword (indent-point _state rules)
  "Return last keyword before POINT.

Return value is in the form of:

  (:sexp     SEXP       ;; keyword sexp
   :point    POINT      ;; point of keyword
   :indent   INDENT     ;; indent of keyword
   :distance DISTANCE)  ;; numbers of sexp between INDENT-POINT and keyword"
  (let ((temp-distance 0)
        distance
        column
        point
        sexp)
    (save-excursion
      (goto-char indent-point)
      (while (unless (or sexp (bobp))
               (condition-case nil (progn (backward-sexp) t) (scan-error nil)))
          (let ((curr-sexp (lisp-keyword-indent--keyword-at-point rules)))
            (setq temp-distance (1+ temp-distance))
            (when curr-sexp
              (setq sexp curr-sexp)
              (setq point (point))
              (setq column (current-column))
              (setq distance temp-distance))))
      (when sexp
        (list :sexp sexp :point point :indent column :distance distance)))))

(defun lisp-keyword-indent--origin-function ()
  (let ((advice (advice--symbol-function lisp-indent-function)))
    (if (advice--p advice)
        (advice--cdr advice)
      lisp-indent-function)))

(defalias 'lisp-keyword-indent--beginning-of-sexp
  (cond ((fboundp 'thing-at-point--beginning-of-sexp) 'thing-at-point--beginning-of-sexp)
        ((fboundp 'beginning-of-sexp) 'beginning-of-sexp)))

(defun lisp-keyword-indent--match-rule (string rules)
  (catch 'break
    (dolist (rule (or rules (alist-get t lisp-keyword-indent-rules)))
      (when (string-match-p (concat "\\`\\(?:" (car rule) "\\)") string)
       (throw 'break rule)))))

(defun lisp-keyword-indent--innermost-form (state)
  "Return the innermost form by STATE."
  (save-excursion
    (goto-char (1+ (nth 1 state)))
    (symbol-at-point)))

(defun lisp-keyword-indent--form-rules (form)
  "Return rules for FORM."
  (let ((elt (assq form lisp-keyword-indent-rules)))
    (if (not elt)
        (alist-get t lisp-keyword-indent-rules)
      (if (symbolp (cdr elt))
          (alist-get (cdr elt) lisp-keyword-indent-rules)
        (cdr elt)))))

(defun lisp-keyword-indent-1 (indent-point state)
  (let* ((innermost-form (lisp-keyword-indent--innermost-form state))
         (rules (lisp-keyword-indent--form-rules innermost-form))
         (sexp-rule (when rules
                      (save-excursion
                        (goto-char indent-point)
                        (when (ignore-errors (forward-sexp 1) t)
                          (lisp-keyword-indent--beginning-of-sexp)
                          (lisp-keyword-indent--rule-at-point rules)))))
         (indent-rule (when sexp-rule (cdr sexp-rule))))
    (when rules
      ;; indent keyword
      (if indent-rule
          (pcase-let*
              ((`(,_ ,anchor-indenter . ,_) indent-rule)
               (indenter-return nil)
               (first-keyword-state
                (when (if (functionp anchor-indenter)
                          (setq indenter-return
                                (funcall anchor-indenter
                                         indent-point
                                         state
                                         rules))
                        t)
                  (lisp-keyword-indent--first-keyword indent-point
                                                      state rules))))
            (when first-keyword-state
              (or indenter-return
                  (+ (plist-get first-keyword-state :indent)
                     (or anchor-indenter 0)))))
        ;; indent keyvalue
        (pcase-let*
            ((last-state (lisp-keyword-indent--last-keyword indent-point
                                                            state rules))
             (last-rule (when last-state
                          (lisp-keyword-indent--match-rule
                           (plist-get last-state :sexp) rules)))
             (`(,_ ,_ ,item-offset ,item-nums) last-rule)
             (sexp-distance (plist-get last-state :distance)))
          (when item-nums
            (if (or (not (numberp item-nums))
                    (and (numberp item-nums) (<= sexp-distance item-nums)))
                (if (functionp item-offset)
                    (funcall item-offset indent-point last-state last-rule)
                  (+ (or (plist-get last-state :indent) 0)
                     (or item-offset 0)))
              ;; not keyvalue, align prev keyword
              (+ (plist-get last-state :indent)))))))))

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
  :global t
  (if lisp-keyword-indent-mode
      (advice-add 'lisp-indent-function :override #'lisp-keyword-indent)
    (advice-remove 'lisp-indent-function #'lisp-keyword-indent)))

(provide 'lisp-keyword-indent)

;;; lisp-keyword-indent.el ends here

;;; lisp-keyword-indent.el --- Keyword indent for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/02
;; Version: 0.3.5
;; Last-Updated: 2022-12-16 11:13:29 +0800
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

(defun lisp-keyword-indent--check-cl-loop-and-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let ((kw (lisp-keyword-indent--last-keyword indent-point state rules)))
        (when (member (plist-get kw :sexp) '("do"))
          (plist-get kw :indent))))))

(defun lisp-keyword-indent--check-cl-loop-accumulation-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let* ((kw (lisp-keyword-indent--last-keyword indent-point state rules))
             (sexp (plist-get kw :sexp)))
        (when (or (member sexp '("do" "and"))
                  (member sexp lisp-keyword-indent--cl-loop-condition-keywords))
          (+ 2 (plist-get kw :indent)))))))

(defcustom lisp-keyword-indent-rules
  (list
   '(t
     (":\\(?:\\sw+\\)" :value-nums 1 :value-offset 0)
     ("&\\(?:\\sw+\\)" :value-nums t :value-offset 2))
   '(cl-defmethod . nil)
   (list
    'cl-loop
    ;; for/with/repeat/named
    ;; while/until/always/never/thereis
    ;; if/when/unless/else/end
    ;; initially/finally/return
    (list
     lisp-keyword-indent-cl-loop-regexp
     :value-nums t :value-offset 2)
    ;; do clause
    (list
     (rx-to-string '(seq (or "do") eow))
     :value-nums t :value-offset 2
     :extra-check #'lisp-keyword-indent--check-cl-loop-do-clause)
    ;; and clause
    (list
     (rx-to-string '(seq (or "and") eow))
     :value-nums t :value-offset 2
     :extra-check #'lisp-keyword-indent--check-cl-loop-and-clause)
    ;; collect/append/nconc/concat/vconcat/count/sum/maximize/minimize
    (list
     (rx-to-string `(seq (or ,@lisp-keyword-indent--cl-loop-accumulation-keywords) eow))
     :value-nums t :value-offset 2
     :extra-check #'lisp-keyword-indent--check-cl-loop-accumulation-clause))
   '(loop . cl-loop))
  "Rules of keyword indent.

Eache element of it can be in one of the following forms:

  (t    . ((REGEXP PROPERTIES) ...))            ;; for all forms
  (FORM . ((REGEXP PROPERTIES) ...))            ;; for specific form
  (FORM . ANOTHER-FORM)                         ;; same as another form
  (FORM . nil)                                  ;; no rule for form

Following are supported properties:

:value-nums             Should be one of the 0 (no value) or 1 (1 value) or (
                        multiple values), default 0.

:value-offset           Offset of keyvalue, default 0.

:extra-check            Extrac check for keyword.  It should be nil or a
                        function takes three arguments (INDENT-POINT, STATE
                        and RULE), and return t if success, e.g.:

                        (lambda (indent-point state rule)
                           (let ((point (car (last (ppss-open-parens state)))))
                             (when point
                               (save-excursion
                                 (goto-char (1+ point))
                                 (not (eq 'if (symbol-at-point)))))))"
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
   :indent   INDENT     ;; indent of keyword
   :distance DISTANCE)  ;; numbers of sexp between INDENT-POINT and keyword"
  (let ((temp-distance 0)
        distance
        column
        sexp)
    (save-excursion
      (goto-char indent-point)
      (while (unless (bobp)
               (condition-case nil (progn (backward-sexp) t) (scan-error nil)))
        (let ((curr-sexp (lisp-keyword-indent--keyword-at-point rules)))
            (setq temp-distance (1+ temp-distance))
            (when curr-sexp
              (setq sexp curr-sexp)
              (setq column (current-column))
              (setq distance temp-distance))))
      (when sexp
        (list :sexp sexp :indent column :distance distance)))))

(defun lisp-keyword-indent--last-keyword (indent-point _state rules)
  "Return last keyword before POINT.

Return value is in the form of:

  (:sexp     SEXP       ;; keyword sexp
   :indent   INDENT     ;; indent of keyword
   :distance DISTANCE)  ;; numbers of sexp between INDENT-POINT and keyword"
  (let ((temp-distance 0)
        distance
        column
        sexp)
    (save-excursion
      (goto-char indent-point)
      (while (unless (or sexp (bobp))
               (condition-case nil (progn (backward-sexp) t) (scan-error nil)))
          (let ((curr-sexp (lisp-keyword-indent--keyword-at-point rules)))
            (setq temp-distance (1+ temp-distance))
            (when curr-sexp
              (setq sexp curr-sexp)
              (setq column (current-column))
              (setq distance temp-distance))))
      (when sexp
        (list :sexp sexp :indent column :distance distance)))))

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
    (or
     (and rules
          ;; indent keyword
          (if indent-rule
              (let* ((checker (plist-get (cdr indent-rule) :extra-check))
                     (checker-return nil)
                     (first-keyword-state
                      (when (if checker
                                (setq checker-return
                                      (funcall checker
                                               indent-point
                                               state
                                               rules))
                              t)
                        (lisp-keyword-indent--first-keyword indent-point
                                                            state rules))))
                (when first-keyword-state
                  (+ (or checker-return (plist-get first-keyword-state :indent))
                     (or (plist-get (cdr indent-rule) :offset) 0))))
            ;; indent keyvalue
            (let* ((last-state (lisp-keyword-indent--last-keyword indent-point
                                                                  state rules))
                   (last-rule (when last-state
                                (lisp-keyword-indent--match-rule
                                 (plist-get last-state :sexp) rules)))
                   (value-nums (when last-rule
                                 (or (plist-get (cdr last-rule) :value-nums) 0)))
                   (sexp-distance (plist-get last-state :distance)))
              (when value-nums
                (if (or (not (numberp value-nums))
                        (and (numberp value-nums) (<= sexp-distance value-nums)))
                    (+ (plist-get last-state :indent)
                       (or (plist-get (cdr last-rule) :value-offset) 0))
                  ;; not keyvalue, align prev keyword
                  (+ (plist-get last-state :indent)))))))
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
  :global t
  (if lisp-keyword-indent-mode
      (advice-add 'lisp-indent-function :override #'lisp-keyword-indent)
    (advice-remove 'lisp-indent-function #'lisp-keyword-indent)))

(provide 'lisp-keyword-indent)

;;; lisp-keyword-indent.el ends here

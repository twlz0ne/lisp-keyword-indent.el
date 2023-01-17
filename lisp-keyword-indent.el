;;; lisp-keyword-indent.el --- Keyword indent for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/07/02
;; Version: 0.3.5
;; Last-Updated: 2023-06-07 11:58:21 +0800
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
  '("initially" "finally" "return")
  "Keywords which introduce initialization clauses.")

(defvar lisp-keyword-indent--cl-loop-iteration-keywords
  '("for" "with" "repeat" "named")
  "Keywords which introduce iteration clauses.")

(defvar lisp-keyword-indent--cl-loop-termination-keywords
  '("while" "until" "always" "never" "thereis")
  "Keywords which introduce termination clauses.")

(defvar lisp-keyword-indent--cl-loop-condition-keywords
  '("if" "when" "unless")
  "keywords introduce condition clauses.")

(defvar lisp-keyword-indent--cl-loop-condition-branch-keywords
  '("else")
  "keywords introduce condition branch clauses.")

(defvar lisp-keyword-indent--cl-loop-accumulation-keywords
  '("collect" "append" "nconc" "concat" "vconcat" "count" "sum"
    "maximize" "minimize")
  "Keywords which introduce accumulation clauses.")

(defvar lisp-keyword-indent-cl-loop-regexp
  (rx-to-string
   `(seq (or ,@lisp-keyword-indent--cl-loop-initialization-keywords
             ,@lisp-keyword-indent--cl-loop-iteration-keywords
             ,@lisp-keyword-indent--cl-loop-termination-keywords
             ,@lisp-keyword-indent--cl-loop-condition-keywords
             ,@lisp-keyword-indent--cl-loop-condition-branch-keywords)
         eow))
  "Regexp matching the cl-loop keywords.")

(defun lisp-keyword-indent--check-cl-loop-do-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let* ((kw (lisp-keyword-indent--last-keyword indent-point state rules))
             (last-sexp (plist-get kw :sexp))
             (distance (plist-get kw :distance)))
        (if (or (and (<= distance 2)
                     (member last-sexp
                             lisp-keyword-indent--cl-loop-condition-keywords))
                (and (<= distance 1)
                     (or (member last-sexp '("do" "and"))
                         (member last-sexp
                                 lisp-keyword-indent--cl-loop-condition-branch-keywords))))
            (+ 2 (plist-get kw :indent))
          (lisp-keyword-indent--innermost-indent 2 indent-point))))))

(defun lisp-keyword-indent--cl-loop-do-and-body-indent (_ anchor _)
  "Return indent of do/and body."
  (let ((anchor-distance (plist-get anchor :distance))
        (base-indent (car (lisp-keyword-indent--back-indentation
                           (plist-get anchor :point)))))
    (if (<= anchor-distance 1)
        (+ base-indent 2)
      base-indent)))

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

(defun lisp-keyword-indent--cl-loop-accumulation-item-indent (indent-point anchor rules)
  (let ((anchor-distance (plist-get anchor :distance)))
    (+ (plist-get anchor :indent)
       (if (or (= 1 anchor-distance)
               (= 3 anchor-distance)
               (and (= 2 anchor-distance)
                    (eq 'into (car (lisp-keyword-indent--thing-and-indent-at indent-point)))))
           2
         0))))

(defun lisp-keyword-indent--check-cl-loop-accumulation-clause (indent-point state rules)
  (let ((point (nth 1 state)))
    (when point
      (let* ((kw (lisp-keyword-indent--last-keyword indent-point state rules))
             (sexp (plist-get kw :sexp))
             (distance (plist-get kw :distance)))
        (cond
         ((or (and (<= distance 2)
               (member sexp lisp-keyword-indent--cl-loop-condition-keywords)) 
           (and (<= distance 1)
               (or (member sexp '("do" "and"))
                   (member sexp lisp-keyword-indent--cl-loop-condition-branch-keywords))))
          (pcase-let* ((keyword-point (plist-get kw :point))
                       (`(,back-indent . ,back-point)
                        (lisp-keyword-indent--back-indentation keyword-point)))
            (if (equal back-point keyword-point)
                (+ (plist-get kw :indent) 2)
              (+ back-indent 2))))
         (t (lisp-keyword-indent--innermost-indent 2 indent-point)))))))

(defun lisp-keyowrd-indent--read-backward (n max)
  "Read after move backward sexp."
  (when (<= (abs n) max)
    (if (< 0 n)
        (backward-sexp n)
      (backward-sexp (+ n max)))
    (read (current-buffer))))

(defun lisp-keyword-indent--read-forward (arg)
  (let (sexps)
    (dotimes (n arg)
      (push (read (current-buffer)) sexps))
    (reverse sexps)))

(defun lisp-keyword-indent--thing-and-indent-at (point)
  "Return indent and thing at POINT."
  (save-excursion
    (goto-char point)
    (save-restriction
      ;; (narrow-to-region (point) (point-at-eol)) ;; Avoid cross line
      (let ((indent (current-column)))
        (ignore-errors 'invalid-read-syntax
                       (cons (read (current-buffer)) indent))))))

(defun lisp-keyword-indent--back-indentation (&optional point)
  "Return back indentation and point at POINT."
  (save-excursion
    (when point
      (goto-char point))
    (back-to-indentation)
    (cons (current-column) (point))))

(defun lisp-keyword-indent--innermost-indent (n &optional start-point)
  "Return indent of the Nth element of innermost form."
  (save-excursion
    (when start-point
      (goto-char start-point))
    (up-list -1)
    (down-list)
    (forward-sexp n)
    (backward-sexp 1)
    (current-column)))

(defun lisp-keyword-indent--cl-loop-anchor-indent (indent-point state rules)
  (pcase-let ((`(,last-form . ,last-indent)
               (lisp-keyword-indent--thing-and-indent-at (nth 2 state))))
    (if (memq last-form '(initially finally))
        (+ last-indent 2)
      ;; FIXME: Temporary fix the indent of `return'.  The indent of other forms
      ;; maybe should redisign 2023-06-07 11.41.06
      (let ((indent-of-return
             (save-excursion
               (goto-char indent-point)
               ;; If a form is read, it means there are only blanks before the form.
               (if (eq 'return (read (current-buffer)))
                   (progn
                     (backward-sexp 2)
                     (if (member (thing-at-point 'sexp t) '("initially" "finally"))
                         (+ (current-column) 2)
                       (backward-sexp 1)
                       (if (member (thing-at-point 'sexp t) '("if" "when"))
                           (+ (current-column) 2))))))))
        (or indent-of-return
            (lisp-keyword-indent--innermost-indent 2))))))

(defun lisp-keyword-indent--cl-loop-item-indent (indent-point anchor rules)
  (let* ((anchor-sexp (plist-get anchor :sexp))
         (anchor-point (plist-get anchor :point))
         (anchor-distance (plist-get anchor :distance))
         (back-indent
          (save-excursion
            (goto-char anchor-point)
            (if (looking-back "(\\(cl-\\)?loop[\s\t]*")
                (plist-get anchor :indent)
              (car (lisp-keyword-indent--back-indentation))))))
    (+ back-indent
       (or (save-excursion
             (cond 
              ;; For clauses:
              ((string= anchor-sexp "for")
               (let ((sexps
                      (progn
                        (goto-char anchor-point)
                        (lisp-keyword-indent--read-forward anchor-distance))))
                 (cond
                  ;;   for VAR                            \ 0 1
                  ;;     from/upfrom/downfrom EXPR1       \ 2 3
                  ;;     to/upto/downto/above/below EXPR2 \ 4 5
                  ;;     [by EXPR3]                         6 7
                  ((and (memq (nth 2 sexps) '(from upfrom downfrom))
                        (memq (nth 4 sexps) '(to upto downto above below))
                        (nthcdr 5 sexps))
                   (if (nth 7 sexps)
                       0 ;; Balanced
                     (if (eq (nth 6 sexps) 'by)
                         2 ;; Unbalanced
                       (if (eq (ignore-errors (read (current-buffer))) 'by)
                           2 ;; Balanced but followed by a `by'
                         0))))
                  ;;   0   1   2 3     4    5
                  ;;   for VAR = EXPR1 then EXPR2
                  ((and (eq (nth 2 sexps) '=) (eq (nth 4 sexps) 'then) (nth 5 sexps))
                   0)
                  ;;   0   1   2            3     4  5
                  ;;   for VAR in/on/in-ref LIST [by FUNC]
                  ((and (memq (nth 2 sexps) '(in on in-ref)) (nthcdr 3 sexps))
                   (if (nth 5 sexps)
                       0
                     (if (eq (nth 4 sexps) 'by)
                         2
                       (if (eq (ignore-errors (read (current-buffer))) 'by)
                           2
                         0))))
                  ;;   0   1   2                 3
                  ;;   for VAR across/across-ref ARRAY
                  ((and (memq (nth 2 sexps) '(across across-ref)) (nth 3 sexps))
                   0)
                  ;;   for VAR being:
                  ((and (eq (nth 2 sexps) 'being) (eq (nth 3 sexps) 'the))
                   (cond 
                    ;;     3   4        5         6        [7      8]
                    ;;     the elements of/of-ref SEQUENCE [using (index VAR2)]
                    ;;     the hash-keys/hash-values of HASH-TABLE [using (hash-values/hash-keys V2)]
                    ;;     the key-codes/key-bindings/key-seqs of KEYMAP [using (key-bindings VAR2)]
                    ((and (memq (nth 4 sexps) '(elements
                                                hash-keys hash-values
                                                key-codes key-bindings key-seqs))
                          (memq (nth 5 sexps) '(of of-ref))
                          (nthcdr 6 sexps))
                     (if (nth 8 sexps)
                         0
                       (if (eq (nth 7 sexps) 'using)
                           2
                         (if (eq (ignore-errors (read (current-buffer))) 'using)
                             2
                           0))))
                    ;;     3   4        5  6
                    ;;     the symbols [of OBARRAY]
                    ;;     the windows [of FRAME]
                    ((memq (nth 4 sexps) '(symbols windows))
                     (if (nth 6 sexps)
                         0
                       (if (eq (nth 5 sexps) 'of)
                           2
                         (if (eq (ignore-errors (read (current-buffer))) 'of)
                             2
                           0))))
                    ;;     the frames/buffers
                    ((memq (nth 3 sexps) '(frames buffers)) 0)
                    ;;     3   4                   5  6        7    8      9  10
                    ;;     the overlays/intervals [of BUFFER] [from POS1] [to POS2]
                    ((memq (nth 4 sexps) '(overlays intervals))
                     (let ((current-sexp
                            (car (lisp-keyword-indent--thing-and-indent-at
                                  indent-point))))
                       (if (or (and (eq (nth 9 sexps) 'to) (not (nth 10 sexps)))
                               (and (eq (nth 7 sexps) 'from) (or (not (nth 8 sexps)) (eq current-sexp 'to)))
                               (and (eq (nth 5 sexps) 'of) (or (not (nth 6 sexps)) (eq current-sexp 'from)))
                               (and (not (nth 5 sexps)) (eq current-sexp 'of)))
                           2
                         0)))
                    (t 2)))
                  (t 2))))
              ;; initially/finally [do] EXPRS...
              ;; [finally] return EXPR
              ((member anchor-sexp lisp-keyword-indent--cl-loop-initialization-keywords)
               (if (<= anchor-distance 1)
                   2
                 0))
              ;; while/until/always/never/thereis CONDITION
              ((member anchor-sexp lisp-keyword-indent--cl-loop-condition-keywords)
               (if (<= anchor-distance 2)
                   2
                 0))))
           0))))

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
    (list lisp-keyword-indent-cl-loop-regexp
          #'lisp-keyword-indent--cl-loop-anchor-indent
          #'lisp-keyword-indent--cl-loop-item-indent
          t)
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
     #'lisp-keyword-indent--cl-loop-accumulation-item-indent
     t))
   '(loop . (:as cl-loop)))
  "A list of indent rules.

Eache element of it can be in one of the following forms:

  (t    . (INDENT-RULE ...))    ;; for all forms
  (FORM . (INDENT-RULE ...))    ;; for specific form
  (FORM . (:as ANOTHER-FORM))   ;; same as another form
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
      (let ((val (cdr elt)))
        (if (eq (car val) :as)
            (alist-get (cadr val) lisp-keyword-indent-rules)
          val)))))

(defun lisp-keyword-indent-1 (indent-point state)
  (let* ((innermost-form (lisp-keyword-indent--innermost-form state))
         (rules (lisp-keyword-indent--form-rules innermost-form))
         (sexp-rule (when rules
                      (save-excursion
                        (goto-char indent-point)
                        (save-restriction
                          (narrow-to-region (point-at-bol) (point-at-eol))
                          (ignore-errors
                            (forward-sexp 1)
                            (lisp-keyword-indent--beginning-of-sexp)
                            (lisp-keyword-indent--rule-at-point rules))))))
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

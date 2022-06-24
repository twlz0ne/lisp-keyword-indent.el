[![CI](https://github.com/twlz0ne/lisp-keyword-indent.el/workflows/CI/badge.svg)](https://github.com/twlz0ne/lisp-keyword-indent.el/actions?query=workflow%3ACI)

# lisp-keyword-indent.el

Rectify the indent of keyword for Emacs Lisp.

## Installation

Clone this repository to `~/.emacs.d/site-lisp/lisp-keyword-indent`. Add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lisp-keyword-indent"))
```

## Usage

```elisp
(setq lisp-indent-function 'lisp-indent-function)
(global-lisp-keyword-indent-mode 1)
```

Before:

```elisp
'(:a 1
     :b 2
     :c 3)
```


After:

```elisp
'(:a 1
  :b 2
  :c 3)
```

Also can indent `cl-defun` like following:

```elisp
(cl-defun func (&whole
                  whole-args
                  normal args
                &optional
                  optionA
                &key
                  keyA
                  keyB
                &allow-other-keys
                &rest
                  rest-part))
```

And `cl-loop` like following:

```elisp
(cl-loop for x
           in '(1 2 3)
         when (eq x 1)
           do (message "This is 1")
           and collect 1
                 into result
         else
           do (message "This is other")
         finally return result)
```


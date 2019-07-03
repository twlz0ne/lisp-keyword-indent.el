[![Build Status](https://travis-ci.com/twlz0ne/lisp-keyword-indent.el.svg?branch=master)](https://travis-ci.com/twlz0ne/lisp-keyword-indent.el)

# lisp-keyword-indent.el

Rectify the indent of keyword for Emacs Lisp.


## Installation

Clone this repository to `~/.emacs.d/site-lisp/lisp-keyword-indent`. Add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lisp-keyword-indent"))
```

## Usage

```elisp
(setq listp-indent-function 'lisp-keyword-indent)
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

Or when the `lisp-keyword-indent-value-offset` is set to non-zero:

```elisp
'(:a
     1
     2
     3
  :b
     4
     5
  :c
     6)

```

Also support `&' keywords:

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

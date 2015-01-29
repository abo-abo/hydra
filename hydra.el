;;; hydra.el --- Make bindings that stick around

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/hydra
;; Version: 0.3.1
;; Keywords: bindings
;; Package-Requires: ((cl-lib "0.5"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package can be used to tie related commands into a family of
;; short bindings with a common prefix - a Hydra.
;;
;; Once you summon the Hydra (through the prefixed binding), all the
;; heads can be called in succession with only a short extension.
;; The Hydra is vanquished once Hercules, any binding that isn't the
;; Hydra's head, arrives.  Note that Hercules, besides vanquishing the
;; Hydra, will still serve his orignal purpose, calling his proper
;; command.  This makes the Hydra very seamless, it's like a minor
;; mode that disables itself automagically.
;;
;; Here's how to use the examples bundled with Hydra:
;;
;;    (require 'hydra-examples)
;;    (hydra-create "C-M-y" hydra-example-move-window-splitter)
;;    (hydra-create "M-g" hydra-example-goto-error)
;;
;; You can expand the examples in-place, it still looks elegant:
;;
;;     (hydra-create "<f2>"
;;       '(("g" text-scale-increase "zoom in")
;;         ("l" text-scale-decrease "zoom out")))
;;
;; The third element of each list is the optional doc string that will
;; be displayed in the echo area when `hydra-is-helpful' is t.

;;; Code:
(require 'cl-lib)

(defgroup hydra nil
  "Make bindings that stick around."
  :group 'bindings
  :prefix "hydra-")

(defcustom hydra-is-helpful t
  "When t, display a hint with possible bindings in the echo area."
  :type 'boolean
  :group 'hydra)

(defalias 'hydra-set-transient-map
  (if (fboundp 'set-transient-map)
      'set-transient-map
    'set-temporary-overlay-map))

(defvar hydra-last nil
  "The result of the last `hydra-set-transient-map' call.")

;;;###autoload
(defmacro hydra-create (body heads &optional method)
  "Create a hydra with a BODY prefix and HEADS with METHOD.
This will result in `global-set-key' statements with the keys
being the concatenation of BODY and each head in HEADS.  HEADS is
an list of (KEY FUNCTION &optional HINT).

After one of the HEADS is called via BODY+KEY, it and the other
HEADS can be called with only KEY (no need for BODY).  This state
is broken once any key binding that is not in HEADS is called.

METHOD is a lambda takes two arguments: a KEY and a COMMAND.
It defaults to `global-set-key'.
When `(keymapp METHOD)`, it becomes:

    (lambda (key command) (define-key METHOD key command))"
  (declare (indent 1))
  `(defhydra ,(intern
               (concat
                "hydra-" (replace-regexp-in-string " " "_" body)))
       ,(cond ((hydra--callablep method)
              method)
             ((null method)
              `(global-map ,body))
             (t
              (list method body)))
     "hydra"
     ,@(eval heads)))

(defun hydra--callablep (x)
  "Test if X looks like it's callable."
  (or (functionp x)
      (and (consp x)
           (memq (car x) '(function quote)))))

(defmacro defhydra (name body &optional docstring &rest heads)
  "Create a hydra named NAME with a prefix BODY.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY should be either:

    (BODY-MAP &optional BODY-KEY)
or:

    (lambda (KEY CMD) ...)

BODY-MAP should be a keymap; `global-map' is acceptable here.
BODY-KEY should be a string processable by `kbd'.

DOCSTRING will be displayed in the echo area to identify the
hydra.

HEADS is a list of (KEY CMD &optional HINT)."
  (unless (stringp docstring)
    (setq heads (cons docstring heads))
    (setq docstring "hydra"))
  (let* ((keymap (make-sparse-keymap))
         (names (mapcar
                 (lambda (x)
                   (define-key keymap (kbd (car x))
                     (intern (format "%S/%s" name (cadr x)))))
                 heads))
         (body-name (intern (format "%S/body" name)))
         (body-key (unless (hydra--callablep body)
                     (cadr body)))
         (method (if (hydra--callablep body)
                     body
                   (car body)))
         (hint (format "%s: %s."
                       docstring
                       (mapconcat
                        (lambda (h)
                          (format
                           (if (cl-caddr h)
                               (concat "[%s]: " (cl-caddr h))
                             "%s")
                           (propertize (car h) 'face 'font-lock-keyword-face)))
                        heads ", ")))
         (doc (format
               "Create a hydra with %s body and the heads:\n\n%s\n\n%s"
               (if body-key
                   (format "a \"%s\"" body-key)
                 "no")
               (mapconcat
                (lambda (x)
                  (format "\"%s\":    `%S'" (car x) (cadr x)))
                heads ",\n")
               (format "The body can be accessed via `%S'." body-name))))
    `(progn
       ,@(cl-mapcar
          (lambda (head name)
            `(defun ,name ()
               ,(format "%s\n\nCall the head: `%S'." doc (cadr head))
               (interactive)
               ,@(if (null (cadr head))
                     '((when hydra-last (funcall hydra-last)))
                     `((call-interactively #',(cadr head))
                       (when hydra-is-helpful
                         (message ,hint))
                       (setq hydra-last
                             (hydra-set-transient-map ',keymap t))))))
          heads names)
       ,@(unless (or (null body-key)
                     (null method)
                     (hydra--callablep method))
                 `((unless (keymapp (lookup-key ,method (kbd ,body-key)))
                     (define-key ,method (kbd ,body-key) nil))))
       ,@(delq nil
               (cl-mapcar
                (lambda (head name)
                  (unless (or (null body-key) (null method))
                    (list
                     (if (hydra--callablep method)
                         'funcall
                       'define-key)
                     method
                     (vconcat (kbd body-key) (kbd (car head)))
                     (list 'function name))))
                heads names))
       (defun ,body-name ()
         ,doc
         (interactive)
         (when hydra-is-helpful
           (message ,hint))
         (setq hydra-last
               (hydra-set-transient-map ',keymap t))))))

(provide 'hydra)
;;; hydra.el ends here

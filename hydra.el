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
  (let* ((keymap (make-sparse-keymap))
         (heads (eval heads))
         (names (mapcar
                 (lambda (x)
                   (define-key keymap (kbd (car x))
                     (intern (format "hydra-%s-%S" body (cadr x)))))
                 heads))
         (hint (format "hydra: %s."
                       (mapconcat
                        (lambda (h)
                          (format
                           (if (cl-caddr h)
                               (concat "[%s]: " (cl-caddr h))
                             "%s")
                           (propertize (car h) 'face 'font-lock-keyword-face)))
                        heads ", ")))
         (doc (format
               "Create a hydra with a \"%s\" body and the heads:\n\n%s."
               body
               (mapconcat
                (lambda (x)
                  (format "\"%s\":    `%S'" (car x) (cadr x)))
                heads ",\n"))))
    `(progn
       ,@(cl-mapcar
          (lambda (head name)
            `(defun ,name ()
               ,(format "%s\n\nCall the head: `%S'." doc (cadr head))
               (interactive)
               ,@(if (null (cadr head))
                     '((funcall hydra-last))
                     `((call-interactively #',(cadr head))
                       (when hydra-is-helpful
                         (message ,hint))
                       (setq hydra-last
                             (hydra-set-transient-map ',keymap t))))))
          heads names)
       (defun ,(intern (format "hydra-%s-body" body)) ()
         ,doc
         (interactive)
         (when hydra-is-helpful
           (message ,hint))
         (setq hydra-last (hydra-set-transient-map ',keymap t)))
       ,@(cond ((null method)
                `((unless (keymapp (global-key-binding (kbd ,body)))
                    (global-set-key (kbd ,body) nil))))
               ((or (functionp method)
                    (and (consp method)
                         (memq (car method) '(function quote))))
                nil)
               (t
                `((unless (keymapp (lookup-key ,method (kbd ,body)))
                    (define-key ,method (kbd ,body) nil)))))
       ,@(cl-mapcar
          (lambda (head name)
            `(,@(cond ((null method)
                      (list 'global-set-key))
                      ((or (functionp method)
                           (and (consp method)
                                (memq (car method) '(function quote))))
                      (list 'funcall method))
                     (t
                      (list 'define-key method)))
                ,(vconcat (kbd body) (kbd (car head))) #',name))
          heads names))))

(provide 'hydra)
;;; hydra.el ends here

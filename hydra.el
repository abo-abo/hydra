;;; hydra.el --- make bindings that stick around.

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/hydra
;; Version: 0.2.0
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: bindings

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package can be used to tie related commands into a family of
;; short bindings with a common prefix - a Hydra.
;;
;; Once you summon the Hydra (through the prefixed binding), all the
;; heads can be called in succession with only a short extension.  The
;; Hydra is vanquished once Hercules, any binding that isn't the
;; Hydra's head, arrives.  Note that Hercules, besides vanquishing the
;; Hydra, will still serve his orignal purpose, calling his proper
;; command.  This makes the Hydra very seamless, it's like a minor
;; mode that disables itself automagically.
;;
;; Here's how I use the examples bundled with Hydra:
;;
;;    (require 'hydra-examples)
;;    (hydra-create "C-M-w" hydra-example-move-window-splitter)
;;
;; You can expand the examples in-place, it still looks elegant:
;;
;;     (hydra-create "<f2>"
;;       '(("g" text-scale-increase)
;;         ("l" text-scale-decrease)))

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
                   (define-key keymap (car x)
                     (intern (format "hydra-%s-%S" body (cadr x)))))
                 heads))
         (method (cond ((null method)
                        'global-set-key)

                       ((keymapp (eval method))
                        `(lambda (key command) (define-key ,method key command)))

                       (t
                        method)))
         (hint (concat "hydra: "
                       (mapconcat
                        (lambda (h) (if (caddr h)
                                        (format "[%s]: %s"
                                                (propertize (car h)
                                                            'face 'font-lock-keyword-face)
                                                (caddr h))
                                      (propertize (car h) 'face 'font-lock-keyword-face)))
                        heads ", ")
                       ".")))
    `(progn
       (when (eq ',method 'global-set-key)
         (global-set-key ,(kbd body) nil))
       ,@(cl-mapcar
          (lambda (head name)
            `(defun ,name ()
               ,(format
                 "Create a hydra with a \"%s\" body and the heads:

%s.

Call the head: `%S'."
                 body
                 (mapconcat
                  (lambda (x)
                    (format "\"%s\":    `%S'" (car x) (cadr x)))
                  heads ",\n")
                 (cadr head))
               (interactive)
               (call-interactively #',(cadr head))
               (when hydra-is-helpful
                 (message ,hint))
               (set-transient-map ',keymap t)))
          heads names)
       ,@(cl-mapcar
          (lambda (head name)
            `(,method ,(kbd (concat body " " (car head))) #',name))
          heads names))))

(provide 'hydra)

;;; hydra.el ends here

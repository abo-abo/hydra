;;; hydra.el --- Make bindings that stick around

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/hydra
;; Version: 0.8.1
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
;;
;; It's better to take the examples simply as templates and use
;; `defhydra' instead of `hydra-create', since it's more flexible.
;;
;;     (defhydra hydra-zoom (global-map "<f2>")
;;       "zoom"
;;       ("g" text-scale-increase "in")
;;       ("l" text-scale-decrease "out"))

;;; Code:
;;* Requires
(require 'cl-lib)

(defalias 'hydra-set-transient-map
  (if (fboundp 'set-transient-map)
      'set-transient-map
    'set-temporary-overlay-map))

;;* Customize
(defgroup hydra nil
  "Make bindings that stick around."
  :group 'bindings
  :prefix "hydra-")

(defcustom hydra-is-helpful t
  "When t, display a hint with possible bindings in the echo area."
  :type 'boolean
  :group 'hydra)

(defcustom hydra-keyboard-quit ""
  "This binding will quit an amaranth Hydra.
It's the only other way to quit it besides though a blue head.
It's possible to set this to nil.")

(defface hydra-face-red
    '((t (:foreground "#7F0055" :bold t)))
  "Red Hydra heads will persist indefinitely."
  :group 'hydra)

(defface hydra-face-blue
    '((t (:foreground "#758BC6" :bold t)))
  "Blue Hydra heads will vanquish the Hydra.")

(defface hydra-face-amaranth
    '((t (:foreground "#E52B50" :bold t)))
  "Amaranth Hydra can exit only through a blue head.")

;;* Universal Argument
(defvar hydra-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-u] 'hydra--universal-argument)
    (define-key map [?-] 'hydra--negative-argument)
    (define-key map [?0] 'hydra--digit-argument)
    (define-key map [?1] 'hydra--digit-argument)
    (define-key map [?2] 'hydra--digit-argument)
    (define-key map [?3] 'hydra--digit-argument)
    (define-key map [?4] 'hydra--digit-argument)
    (define-key map [?5] 'hydra--digit-argument)
    (define-key map [?6] 'hydra--digit-argument)
    (define-key map [?7] 'hydra--digit-argument)
    (define-key map [?8] 'hydra--digit-argument)
    (define-key map [?9] 'hydra--digit-argument)
    (define-key map [kp-0] 'hydra--digit-argument)
    (define-key map [kp-1] 'hydra--digit-argument)
    (define-key map [kp-2] 'hydra--digit-argument)
    (define-key map [kp-3] 'hydra--digit-argument)
    (define-key map [kp-4] 'hydra--digit-argument)
    (define-key map [kp-5] 'hydra--digit-argument)
    (define-key map [kp-6] 'hydra--digit-argument)
    (define-key map [kp-7] 'hydra--digit-argument)
    (define-key map [kp-8] 'hydra--digit-argument)
    (define-key map [kp-9] 'hydra--digit-argument)
    (define-key map [kp-subtract] 'hydra--negative-argument)
    map)
  "Keymap that all Hydras inherit.  See `universal-argument-map'.")

(defvar hydra-curr-map
  (make-sparse-keymap)
  "Keymap of the current Hydra called.")

(defun hydra--universal-argument (arg)
  "Forward to (`universal-argument' ARG)."
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4))))
  (hydra-set-transient-map hydra-curr-map t))

(defun hydra--digit-argument (arg)
  "Forward to (`digit-argument' ARG)."
  (interactive "P")
  (let ((universal-argument-map hydra-curr-map))
    (digit-argument arg)))

(defun hydra--negative-argument (arg)
  "Forward to (`negative-argument' ARG)."
  (interactive "P")
  (let ((universal-argument-map hydra-curr-map))
    (negative-argument arg)))

;;* Misc internals
(defvar hydra-last nil
  "The result of the last `hydra-set-transient-map' call.")

(defun hydra--callablep (x)
  "Test if X is callable."
  (or (functionp x)
      (and (consp x)
           (memq (car x) '(function quote)))))

(defun hydra--head-property (h prop &optional default)
  "Return the value of property PROP for Hydra head H.
Return DEFAULT if PROP is not in H."
  (let ((plist (if (stringp (cl-caddr h))
                   (cl-cdddr h)
                 (cddr h))))
    (if (memq prop h)
        (plist-get plist prop)
      default)))

(defun hydra--color (h body-color)
  "Return the color of a Hydra head H with BODY-COLOR."
  (if (null (cadr h))
      'blue
    (or (hydra--head-property h :color) body-color)))

(defun hydra--face (h body-color)
  "Return the face for a Hydra head H with BODY-COLOR."
  (cl-case (hydra--color h body-color)
    (blue 'hydra-face-blue)
    (red 'hydra-face-red)
    (amaranth 'hydra-face-amaranth)
    (t (error "Unknown color for %S" h))))

(defun hydra--hint (docstring heads body-color)
  "Generate a hint from DOCSTRING and HEADS and BODY-COLOR.
It's intended for the echo area, when a Hydra is active."
  (format "%s: %s."
          docstring
          (mapconcat
           (lambda (h)
             (format
              (if (stringp (cl-caddr h))
                  (concat "[%s]: " (cl-caddr h))
                "%s")
              (propertize
               (car h) 'face
               (hydra--face h body-color))))
           heads ", ")))

(defun hydra-disable ()
  "Disable the current Hydra."
  (cond
    ;; Emacs 25
    ((functionp hydra-last)
     (funcall hydra-last))

    ;; Emacs 24.4.1
    ((boundp 'overriding-terminal-local-map)
     (setq overriding-terminal-local-map nil))

    ;; older
    (t
     (while (and (consp (car emulation-mode-map-alists))
                 (consp (caar emulation-mode-map-alists))
                 (equal (cl-cdaar emulation-mode-map-alists) ',keymap))
       (setq emulation-mode-map-alists
             (cdr emulation-mode-map-alists))))))

(defun hydra--doc (body-key body-name heads)
  "Generate a part of Hydra docstring.
BODY-KEY is the body key binding.
BODY-NAME is the symbol that identifies the Hydra.
HEADS is a list of heads."
  (format
   "Create a hydra with %s body and the heads:\n\n%s\n\n%s"
   (if body-key
       (format "a \"%s\"" body-key)
     "no")
   (mapconcat
    (lambda (x)
      (format "\"%s\":    `%S'" (car x) (cadr x)))
    heads ",\n")
   (format "The body can be accessed via `%S'." body-name)))

(defun hydra--make-defun (name cmd color
                          doc hint keymap
                          body-color body-pre body-post &optional other-post)
  "Make a defun wrapper, using NAME, CMD, COLOR, DOC, HINT, and KEYMAP.
BODY-COLOR, BODY-PRE, and BODY-POST are used as well."
  `(defun ,name ()
     ,doc
     (interactive)
     ,@(when body-pre (list body-pre))
     (hydra-disable)
     (catch 'hydra-disable
       ,@(delq nil
               (if (eq color 'blue)
                   `(,(when cmd `(call-interactively #',cmd))
                      ,body-post)
                 `(,(when cmd
                          `(condition-case err
                               (prog1 t
                                 (call-interactively #',cmd))
                             ((debug error)
                              (message "%S" err)
                              (sit-for 0.8)
                              nil)))
                    (when hydra-is-helpful
                      (message ,hint))
                    (setq hydra-last
                          (hydra-set-transient-map
                           (setq hydra-curr-map ',keymap)
                           t
                           ,@(if (and (not (eq body-color 'amaranth)) body-post)
                                 `((lambda () ,body-post)))))
                    ,other-post))))))

;;* Macros
;;** hydra-create
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
  (declare (indent 1)
           (obsolete defhydra "0.8.0"))
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

;;** defhydra
;;;###autoload
(defmacro defhydra (name body &optional docstring &rest heads)
  "Create a hydra named NAME with a prefix BODY.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY should be either:

    (BODY-MAP &optional BODY-KEY &rest PLIST)
or:

    (lambda (KEY CMD) ...)

BODY-MAP should be a keymap; `global-map' is acceptable here.
BODY-KEY should be a string processable by `kbd'.

DOCSTRING will be displayed in the echo area to identify the
hydra.

HEADS is a list of (KEY CMD &optional HINT &rest PLIST).

PLIST in both cases recognizes only the :color key so far, which
in turn can be either red or blue."
  (declare (indent 2))
  (mapc (lambda (x)
          (let ((l (cadr x)))
            (when (and (not (null l))
                       (listp l))
              (setcar (cdr x) (eval l)))))
        heads)
  (unless (stringp docstring)
    (setq heads (cons docstring heads))
    (setq docstring "hydra"))
  (when (keywordp (car body))
    (setq body (cons nil (cons nil body))))
  (let* ((keymap (copy-keymap hydra-base-map))
         (names (mapcar
                 (lambda (x)
                   (define-key keymap (kbd (car x))
                     (intern (format "%S/%s" name
                                     (if (symbolp (cadr x))
                                         (cadr x)
                                       (concat "lambda-" (car x)))))))
                 heads))
         (body-name (intern (format "%S/body" name)))
         (body-key (unless (hydra--callablep body)
                     (cadr body)))
         (body-color (if (hydra--callablep body)
                         'red
                       (or (plist-get (cddr body) :color)
                           'red)))
         (body-pre (plist-get (cddr body) :pre))
         (body-post (plist-get (cddr body) :post))
         (method (if (hydra--callablep body)
                     body
                   (car body)))
         (hint (hydra--hint docstring heads body-color))
         (doc (hydra--doc body-key body-name heads)))
    (when (and (or body-pre body-post)
               (version< emacs-version "24.4"))
      (error "At least Emacs 24.4 is needed for :pre and :post"))
    (when (and body-pre (symbolp body-pre))
      (setq body-pre `(funcall #',body-pre)))
    (when (and body-post (symbolp body-post))
      (setq body-post `(funcall #',body-post)))
    (when (eq body-color 'amaranth)
      (if (cl-some `(lambda (h)
                      (eq (hydra--color h ',body-color) 'blue))
                   heads)
          (define-key keymap [t]
            `(lambda ()
               (interactive)
               (message "An amaranth Hydra can only exit through a blue head")
               (hydra-set-transient-map hydra-curr-map t)
               (when hydra-is-helpful
                 (sit-for 0.8)
                 (message ,hint))))
        (error "An amaranth Hydra must have at least one blue head in order to exit"))
      (when hydra-keyboard-quit
        (define-key keymap hydra-keyboard-quit
          `(lambda ()
             (interactive)
             (hydra-disable)
             ,body-post))))
    `(progn
       ,@(cl-mapcar
          (lambda (head name)
            (hydra--make-defun
             name (cadr head) (hydra--color head body-color)
             (format "%s\n\nCall the head: `%S'." doc (cadr head))
             hint keymap
             body-color body-pre body-post))
          heads names)
       ,@(unless (or (null body-key)
                     (null method)
                     (hydra--callablep method))
           `((unless (keymapp (lookup-key ,method (kbd ,body-key)))
               (define-key ,method (kbd ,body-key) nil))))
       ,@(delq nil
               (cl-mapcar
                (lambda (head name)
                  (unless (or (null body-key)
                              (null method))
                    (let ((bind (hydra--head-property head :bind 'default)))
                      (cond ((null bind) nil)

                            ((eq bind 'default)
                             (list
                              (if (hydra--callablep method)
                                  'funcall
                                'define-key)
                              method
                              (vconcat (kbd body-key) (kbd (car head)))
                              (list 'function name)))

                            (t
                             (error "Invalid :bind property %S" head))))))
                heads names))
       ,(hydra--make-defun body-name nil nil doc hint keymap
                           body-color body-pre body-post
                           '(setq prefix-arg current-prefix-arg)))))

(provide 'hydra)

;;; Local Variables:
;;; outline-regexp: ";;\\*+"
;;; End:

;;; hydra.el ends here

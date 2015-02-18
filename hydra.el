;;; hydra.el --- Make bindings that stick around

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/hydra
;; Version: 0.10.0
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
;; Here's an example Hydra, bound in the global map (you can use any
;; keymap in place of `global-map'):
;;
;;     (defhydra hydra-zoom (global-map "<f2>")
;;       "zoom"
;;       ("g" text-scale-increase "in")
;;       ("l" text-scale-decrease "out"))
;;
;; It allows to start a command chain either like this:
;; "<f2> gg4ll5g", or "<f2> lgllg".
;;
;; Here's another approach, when you just want a "callable keymap":
;;
;;     (defhydra hydra-toggle (:color blue)
;;       "toggle"
;;       ("a" abbrev-mode "abbrev")
;;       ("d" toggle-debug-on-error "debug")
;;       ("f" auto-fill-mode "fill")
;;       ("t" toggle-truncate-lines "truncate")
;;       ("w" whitespace-mode "whitespace")
;;       ("q" nil "cancel"))
;;
;; This binds nothing so far, but if you follow up with:
;;
;;     (global-set-key (kbd "C-c C-v") 'hydra-toggle/body)
;;
;; you will have bound "C-c C-v a", "C-c C-v d" etc.
;;
;; Knowing that `defhydra' defines e.g. `hydra-toggle/body' command,
;; you can nest Hydras if you wish, with `hydra-toggle/body' possibly
;; becoming a blue head of another Hydra.
;;
;; Initially, Hydra shipped with a simplified `hydra-create' macro, to
;; which you could hook up the examples from hydra-examples.el.  It's
;; better to take the examples simply as templates and use `defhydra'
;; instead of `hydra-create', since it's more flexible.

;;; Code:
;;* Requires
(require 'cl-lib)
(require 'lv)

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

(defcustom hydra-lv t
  "When non-nil, `lv-message' will be used to display hints instead of `message'."
  :type 'boolean)

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
;;* Fontification
(defun hydra-add-font-lock ()
  "Fontify `defhydra' statements."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defhydra\\)\\_> +\\(.*?\\)\\_>"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face)))))

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

(defun hydra--make-callable (x)
  "Generate a callable symbol from X.
If X is a function symbol or a lambda, return it.  Otherwise, it
should be a single statement.  Wrap it in an interactive lambda."
  (if (or (symbolp x) (functionp x))
      x
    `(lambda ()
       (interactive)
       ,x)))

(defun hydra--head-property (h prop &optional default)
  "Return for Hydra head H the value of property PROP.
Return DEFAULT if PROP is not in H."
  (let ((plist (if (or (stringp (cl-caddr h))
                       (null (cl-caddr h)))
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
  (let (alist)
    (dolist (h heads)
      (let ((val (assoc (cadr h) alist))
            (pstr (propertize (car h) 'face
                              (hydra--face h body-color))))
        (unless (and (> (length h) 2)
                     (null (cl-caddr h)))
          (if val
              (setf (cadr val)
                    (concat (cadr val) " " pstr))
            (push
             (cons (cadr h)
                   (cons pstr
                         (and (stringp (cl-caddr h)) (cl-caddr h))))
             alist)))))

    (format "%s: %s."
            docstring
            (mapconcat
             (lambda (x)
               (format
                (if (cdr x)
                    (concat "[%s]: " (cdr x))
                  "%s")
                (car x)))
             (nreverse (mapcar #'cdr alist))
             ", "))))

(defun hydra-cleanup ()
  "Clean up after a Hydra."
  (when (window-live-p lv-wnd)
    (let ((buf (window-buffer lv-wnd)))
      (delete-window lv-wnd)
      (kill-buffer buf))))

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

(defun hydra--message (format-str &rest args)
  "Forward to (`message' FORMAT-STR ARGS).
Or to `lv-message' if `hydra-lv' is non-nil."
  (if hydra-lv
      (apply #'lv-message format-str args)
    (apply #'message format-str args)))

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
BODY-COLOR, BODY-PRE, BODY-POST, and OTHER-POST are used as well."
  `(defun ,name ()
     ,doc
     (interactive)
     ,@(when body-pre (list body-pre))
     (hydra-disable)
     ,@(when (eq color 'blue) '((hydra-cleanup)))
     (catch 'hydra-disable
       ,@(delq nil
               (if (eq color 'blue)
                   `(,(when cmd `(call-interactively #',cmd))
                      ,body-post)
                 `(,(when cmd
                          `(condition-case err
                               (prog1 t
                                 (call-interactively #',cmd))
                             (error
                              (message "%S" err)
                              (unless hydra-lv
                                (sit-for 0.8))
                              nil)))
                    (when hydra-is-helpful
                      (,hint))
                    (setq hydra-last
                          (hydra-set-transient-map
                           (setq hydra-curr-map ',keymap)
                           t
                           ,(if (and (not (eq body-color 'amaranth)) body-post)
                                `(lambda () (hydra-cleanup) ,body-post)
                                `(lambda () (hydra-cleanup)))))
                    ,other-post))))))

;;* Macros
;;** defhydra
;;;###autoload
(defmacro defhydra (name body &optional docstring &rest heads)
  "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head. It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil. If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from the body and are allowed to
override each key.  The keys recognized are :color and :bind.
:color can be:

- red (default): this head will continue the Hydra state.
- blue: this head will stop the Hydra state.
- amaranth (applies to body only): similar to red, but no binding
except a blue head can stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything. In that case, typically you will bind the
generated NAME/body command. This command is also the return
result of `defhydra'."
  (declare (indent defun))
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
         (hint-name (intern (format "%S/hint" name)))
         (body-key (unless (hydra--callablep body)
                     (cadr body)))
         (body-color (if (hydra--callablep body)
                         'red
                       (or (plist-get (cddr body) :color)
                           'red)))
         (body-pre (plist-get (cddr body) :pre))
         (body-post (plist-get (cddr body) :post))
         (method (or (plist-get body :bind)
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
          (progn
            (when (cl-some `(lambda (h)
                              (eq (hydra--color h ',body-color) 'red))
                           heads)
              (warn "Amaranth body color: upgrading all red heads to amaranth"))
            (define-key keymap [t]
              `(lambda ()
                 (interactive)
                 (message "An amaranth Hydra can only exit through a blue head")
                 (hydra-set-transient-map hydra-curr-map t)
                 (when hydra-is-helpful
                   (unless hydra-lv
                     (sit-for 0.8))
                   (,hint-name)))))
        (error "An amaranth Hydra must have at least one blue head in order to exit"))
      (when hydra-keyboard-quit
        (define-key keymap hydra-keyboard-quit
          `(lambda ()
             (interactive)
             (hydra-disable)
             (hydra-cleanup)
             ,body-post))))
    `(progn
       ,@(cl-mapcar
          (lambda (head name)
            (hydra--make-defun
             name (hydra--make-callable (cadr head)) (hydra--color head body-color)
             (format "%s\n\nCall the head: `%S'." doc (cadr head))
             hint-name keymap
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
                  (when (or body-key method)
                    (let ((bind (hydra--head-property head :bind 'default))
                          (final-key (if body-key
                                         (vconcat (kbd body-key) (kbd (car head)))
                                       (kbd (car head)))))
                      (cond ((null bind) nil)

                            ((eq bind 'default)
                             (list
                              (if (hydra--callablep method)
                                  'funcall
                                'define-key)
                              method
                              final-key
                              (list 'function name)))

                            ((hydra--callablep bind)
                             `(funcall (function ,bind)
                                       ,final-key
                                       (function ,name)))

                            (t
                             (error "Invalid :bind property %S" head))))))
                heads names))
       (defun ,hint-name ()
         (hydra--message ,hint))
       ,(hydra--make-defun body-name nil nil doc hint-name keymap
                           body-color body-pre body-post
                           '(setq prefix-arg current-prefix-arg)))))

(defmacro defhydradio (name body &rest heads)
  "Create toggles with prefix NAME.
BODY specifies the options; there are none currently.
HEADS have the format:

    (TOGGLE-NAME &optional VALUE DOC)

TOGGLE-NAME will be used along with NAME to generate a variable
name and a function that cycles it with the same name.  VALUE
should be an array. The first element of VALUE will be used to
inialize the variable.
VALUE defaults to [nil t].
DOC defaults to TOGGLE-NAME split and capitalized."
  (declare (indent defun))
  (cons 'progn
        (apply #'append
               (mapcar (lambda (h)
                         (hydra--radio name h))
                       heads))))

(defun hydra--radio (parent head)
  "Generate a hydradio from HEAD."
  (let* ((name (car head))
         (full-name (intern (format "%S/%S" parent name)))
         (val (or (cadr head) [nil t]))
         (doc (or (cl-caddr head)
                  (mapconcat #'capitalize
                             (split-string (symbol-name name) "-")
                             " "))))
    `((defvar ,full-name ,(hydra--quote-maybe (aref val 0)) ,doc)
      (put ',full-name 'range ,val)
      (defun ,full-name ()
        (hydra--cycle-radio ',full-name)))))

(defun hydra--quote-maybe (x)
  "Quote X if it's a symbol."
  (if (symbolp x)
      (list 'quote x)
    x))

(defun hydra--cycle-radio (sym)
  "Set SYM to the next value in its range."
  (let* ((val (symbol-value sym))
         (range (get sym 'range))
         (i 0)
         (l (length range)))
    (setq i (catch 'done
              (while (< i l)
                (if (equal (aref range i) val)
                    (throw 'done (1+ i))
                  (incf i)))
              (error "Val not in range for %S" sym)))
    (set sym
         (aref range
               (if (>= i l)
                   0
                 i)))))

(provide 'hydra)

;;; Local Variables:
;;; outline-regexp: ";;\\*+"
;;; End:

;;; hydra.el ends here

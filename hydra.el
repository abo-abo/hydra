;;; hydra.el --- Make bindings that stick around. -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/hydra
;; Version: 0.12.1
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

(defun hydra-set-transient-map (map _keep-pred &optional on-exit)
  (if (fboundp 'set-transient-map)
      (set-transient-map map (hydra--pred on-exit))
    (with-no-warnings
      (set-temporary-overlay-map map (hydra--pred on-exit)))))

(defun hydra--pred (on-exit)
  "Generate a predicate on whether to continue the Hydra state.
Call ON-EXIT for clean-up.
This is a compatibility code for Emacs older than 24.4."
  `(lambda ()
     (if (lookup-key hydra-curr-map (this-command-keys-vector))
         t
       (hydra-keyboard-quit)
       ,(when on-exit
              `(funcall ,(hydra--make-callable on-exit)))
       nil)))

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
  "When non-nil, `lv-message' (not `message') will be used to display hints."
  :type 'boolean)

(defcustom hydra-verbose nil
  "When non-nil, hydra will issue some non essential style warnings."
  :type 'boolean)

(defcustom hydra-key-format-spec "%s"
  "Default `format'-style specifier for _a_  syntax in docstrings.
When nil, you can specify your own at each location like this: _ 5a_.")

(defface hydra-face-red
    '((t (:foreground "#FF0000" :bold t)))
  "Red Hydra heads will persist indefinitely."
  :group 'hydra)

(defface hydra-face-blue
    '((t (:foreground "#0000FF" :bold t)))
  "Blue Hydra heads will vanquish the Hydra.")

(defface hydra-face-amaranth
    '((t (:foreground "#E52B50" :bold t)))
  "Amaranth body has red heads and warns on intercepting non-heads.
Vanquishable only through a blue head.")

(defface hydra-face-pink
    '((t (:foreground "#FF6EB4" :bold t)))
  "Pink body has red heads and on intercepting non-heads calls them without quitting.
Vanquishable only through a blue head.")

(defface hydra-face-teal
    '((t (:foreground "#367588" :bold t)))
  "Teal body has blue heads an warns on intercepting non-heads.
Vanquishable only through a blue head.")

;;* Fontification
(defun hydra-add-font-lock ()
  "Fontify `defhydra' statements."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defhydra\\)\\_> +\\(.*?\\)\\_>"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     ("(\\(defhydradio\\)\\_> +\\(.*?\\)\\_>"
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
    (define-key map [switch-frame] 'hydra--handle-switch-frame)
    map)
  "Keymap that all Hydras inherit.  See `universal-argument-map'.")

(defvar hydra-curr-map
  (make-sparse-keymap)
  "Keymap of the current Hydra called.")

(defun hydra--handle-switch-frame (evt)
  "Quit hydra and call old switch-frame event handler for EVT."
  (interactive "e")
  (hydra-keyboard-quit)
  (funcall (lookup-key (current-global-map) [switch-frame]) evt))

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
  (let ((universal-argument-map
         (if (fboundp 'universal-argument--mode)
             hydra-curr-map
           universal-argument-map)))
    (digit-argument arg)))

(defun hydra--negative-argument (arg)
  "Forward to (`negative-argument' ARG)."
  (interactive "P")
  (let ((universal-argument-map hydra-curr-map))
    (negative-argument arg)))
;;* Repeat
(defvar hydra-repeat--prefix-arg nil
  "Prefix arg to use with `hydra-repeat'.")

(defvar hydra-repeat--command nil
  "Command to use with `hydra-repeat'.")

(defun hydra-repeat (&optional arg)
  "Repeat last command with last prefix arg.
When ARG is non-nil, use that instead."
  (interactive "p")
  (if (eq arg 1)
      (unless (string-match "hydra-repeat$" (symbol-name last-command))
        (setq hydra-repeat--command last-command)
        (setq hydra-repeat--prefix-arg last-prefix-arg))
    (setq hydra-repeat--prefix-arg arg))
  (setq current-prefix-arg hydra-repeat--prefix-arg)
  (funcall hydra-repeat--command))

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

(defun hydra-plist-get-default (plist prop default)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).

Return the value corresponding to PROP, or DEFAULT if PROP is not
one of the properties on the list."
  (if (memq prop plist)
      (plist-get plist prop)
    default))

(defun hydra--head-property (h prop &optional default)
  "Return for Hydra head H the value of property PROP.
Return DEFAULT if PROP is not in H."
  (hydra-plist-get-default (cl-cdddr h) prop default))

(defun hydra--aggregate-color (head-color body-color)
  "Return the resulting head color for HEAD-COLOR and BODY-COLOR."
  (cond ((eq head-color 'red)
         (cl-case body-color
           (red 'red)
           (blue 'red)
           (amaranth 'amaranth)
           (pink 'pink)
           (cyan 'amaranth)))
        ((eq head-color 'blue)
         (cl-case body-color
           (red 'blue)
           (blue 'blue)
           (amaranth 'teal)
           (pink 'blue)
           (cyan 'teal)))
        (t
         (error "Can't aggregate head %S to body %S"
                head-color body-color))))

(defun hydra--head-color (h body)
  "Return the color of a Hydra head H with BODY."
  (let* ((exit (hydra--head-property h :exit 'default))
         (color (hydra--head-property h :color))
         (foreign-keys (hydra--body-foreign-keys body))
         (head-color
          (cond ((eq exit 'default)
                 (cl-case color
                   (blue 'blue)
                   (red 'red)
                   (t
                    (unless (null color)
                      (error "Use only :blue or :red for heads: %S" h)))))
                ((null exit)
                 (if color
                     (error "Don't mix :color and :exit - they are aliases: %S" h)
                   (cl-case foreign-keys
                     (run 'pink)
                     (warn 'amaranth)
                     (t 'red))))
                ((eq exit t)
                 (if color
                     (error "Don't mix :color and :exit - they are aliases: %S" h)
                   'blue))
                (t
                 (error "Unknown :exit %S" exit)))))
    (cond ((null (cadr h))
           (when head-color
             (hydra--complain
              "Doubly specified blue head - nil cmd is already blue: %S" h))
           'blue)
          ((null head-color)
           (hydra--body-color body))
          ((null foreign-keys)
           head-color)
          ((eq foreign-keys 'run)
           (if (eq head-color 'red)
               'pink
             'blue))
          ((eq foreign-keys 'warn)
           (if (memq head-color '(red amaranth))
               'amaranth
             'teal))
          (t
           (error "Unexpected %S %S" h body)))))

(defun hydra--body-foreign-keys (body)
  "Return what BODY does with a non-head binding."
  (or
   (plist-get (cddr body) :foreign-keys)
   (let ((color (plist-get (cddr body) :color)))
     (cl-case color
       ((amaranth teal) 'warn)
       (pink 'run)))))

(defun hydra--body-color (body)
  "Return the color of BODY.
BODY is the second argument to `defhydra'"
  (let ((color (plist-get (cddr body) :color))
        (exit (plist-get (cddr body) :exit))
        (foreign-keys (plist-get (cddr body) :foreign-keys)))
    (cond ((eq foreign-keys 'warn)
           (if exit 'teal 'amaranth))
          ((eq foreign-keys 'run) 'pink)
          (exit 'blue)
          (color color)
          (t 'red))))

(defun hydra--face (h body)
  "Return the face for a Hydra head H with BODY."
  (cl-case (hydra--head-color h body)
    (blue 'hydra-face-blue)
    (red 'hydra-face-red)
    (amaranth 'hydra-face-amaranth)
    (pink 'hydra-face-pink)
    (teal 'hydra-face-teal)
    (t (error "Unknown color for %S" h))))

(defvar hydra--input-method-function nil
  "Store overridden `input-method-function' here.")

(defun hydra-default-pre ()
  "Default setup that happens in each head before :pre."
  (when (eq input-method-function 'key-chord-input-method)
    (unless hydra--input-method-function
      (setq hydra--input-method-function input-method-function)
      (setq input-method-function nil))))

(defun hydra-keyboard-quit ()
  "Quitting function similar to `keyboard-quit'."
  (interactive)
  (hydra-disable)
  (cancel-timer hydra-timer)
  (when hydra--input-method-function
    (setq input-method-function hydra--input-method-function)
    (setq hydra--input-method-function nil))
  (if hydra-lv
      (when (window-live-p lv-wnd)
        (let ((buf (window-buffer lv-wnd)))
          (delete-window lv-wnd)
          (kill-buffer buf)))
    (message ""))
  nil)

(defun hydra-disable ()
  "Disable the current Hydra."
  (cond
    ;; Emacs 25
    ((functionp hydra-last)
     (funcall hydra-last))

    ;; Emacs 24.3 or older
    ((< emacs-minor-version 4)
     (setq emulation-mode-map-alists
           (cl-remove-if
            (lambda (x)
              (and (consp x)
                   (consp (car x))
                   (equal (cdar x) hydra-curr-map)))
            emulation-mode-map-alists)))

    ;; Emacs 24.4.1
    (t
     (setq overriding-terminal-local-map nil))))

(defun hydra--unalias-var (str prefix)
  "Return the symbol named STR if it's bound as a variable.
Otherwise, add PREFIX to the symbol name."
  (let ((sym (intern-soft str)))
    (if (boundp sym)
        sym
      (intern (concat prefix "/" str)))))

(defun hydra--hint (body heads)
  "Generate a hint for the echo area.
BODY, and HEADS are parameters to `defhydra'."
  (let (alist)
    (dolist (h heads)
      (let ((val (assoc (cadr h) alist))
            (pstr (hydra-fontify-head h body)))
        (unless (null (cl-caddr h))
          (if val
              (setf (cadr val)
                    (concat (cadr val) " " pstr))
            (push
             (cons (cadr h)
                   (cons pstr (cl-caddr h)))
             alist)))))
    (mapconcat
     (lambda (x)
       (format
        (if (> (length (cdr x)) 0)
            (concat "[%s]: " (cdr x))
          "%s")
        (car x)))
     (nreverse (mapcar #'cdr alist))
     ", ")))

(defvar hydra-fontify-head-function nil
  "Possible replacement for `hydra-fontify-head-default'.")

(defun hydra-fontify-head-default (head body)
  "Produce a pretty string from HEAD and BODY.
HEAD's binding is returned as a string with a colored face."
  (propertize (car head) 'face (hydra--face head body)))

(defun hydra-fontify-head-greyscale (head body)
  "Produce a pretty string from HEAD and BODY.
HEAD's binding is returned as a string wrapped with [] or {}."
  (let ((color (hydra--head-color head body)))
    (format
     (if (eq color 'blue)
         "[%s]"
       "{%s}") (car head))))

(defun hydra-fontify-head (head body)
  "Produce a pretty string from HEAD and BODY."
  (funcall (or hydra-fontify-head-function 'hydra-fontify-head-default)
           head body))

(defun hydra--format (_name body docstring heads)
  "Generate a `format' statement from STR.
\"%`...\" expressions are extracted into \"%S\".
_NAME, BODY, DOCSTRING and HEADS are parameters of `defhydra'.
The expressions can be auto-expanded according to NAME."
  (setq docstring (replace-regexp-in-string "\\^" "" docstring))
  (let ((rest (hydra--hint body heads))
        (start 0)
        varlist
        offset)
    (while (setq start
                 (string-match
                  "\\(?:%\\( ?-?[0-9]*s?\\)\\(`[a-z-A-Z/0-9]+\\|(\\)\\)\\|\\(?:_\\( ?-?[0-9]*\\)\\([^_]+\\)_\\)"
                  docstring start))
      (cond ((eq ?_ (aref (match-string 0 docstring) 0))
             (let* ((key (match-string 4 docstring))
                    (head (assoc key heads)))
               (if head
                   (progn
                     (push (hydra-fontify-head head body) varlist)
                     (setq docstring
                           (replace-match
                            (or
                             hydra-key-format-spec
                             (concat "%" (match-string 3 docstring) "s"))
                            t nil docstring)))
                 (error "Unrecognized key: _%s_" key))))

            (t
             (let* ((varp (if (eq ?` (aref (match-string 2 docstring) 0)) 1 0))
                    (spec (match-string 1 docstring))
                    (lspec (length spec)))
               (setq offset
                     (with-temp-buffer
                       (insert (substring docstring (+ 1 start varp
                                                       (length spec))))
                       (goto-char (point-min))
                       (push (read (current-buffer)) varlist)
                       (- (point) (point-min))))
               (when (or (zerop lspec)
                         (/= (aref spec (1- (length spec))) ?s))
                 (setq spec (concat spec "S")))
               (setq docstring
                     (concat
                      (substring docstring 0 start)
                      "%" spec
                      (substring docstring (+ start offset 1 lspec varp))))))))
    (if (eq ?\n (aref docstring 0))
        `(concat (format ,(substring docstring 1) ,@(nreverse varlist))
                 ,rest)
      `(format ,(concat docstring ": " rest ".")))))

(defun hydra--message (name body docstring heads)
  "Generate code to display the hint in the preferred echo area.
Set `hydra-lv' to choose the echo area.
NAME, BODY, DOCSTRING, and HEADS are parameters of `defhydra'."
  (let ((format-expr (hydra--format name body docstring heads)))
    `(if hydra-lv
         (lv-message ,format-expr)
       (message ,format-expr))))

(defun hydra--complain (format-string &rest args)
  "Forward to (`message' FORMAT-STRING ARGS) unless `hydra-verbose' is nil."
  (when hydra-verbose
    (apply #'warn format-string args)))

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

(defun hydra--make-defun (name body doc head
                          keymap body-pre body-post &optional other-post)
  "Make a defun wrapper, using NAME, BODY, DOC, HEAD, and KEYMAP.
NAME and BODY are the arguments to `defhydra'.
DOC was generated with `hydra--doc'.
HEAD is one of the HEADS passed to `defhydra'.
BODY-PRE and BODY-POST are pre-processed in `defhydra'.
OTHER-POST is an optional extension to the :post key of BODY."
  (let ((name (hydra--head-name head name body))
        (cmd (when (car head)
               (hydra--make-callable
                (cadr head))))
        (color (when (car head)
                 (hydra--head-color head body)))
        (doc (if (car head)
                 (format "%s\n\nCall the head: `%S'." doc (cadr head))
               doc))
        (hint (intern (format "%S/hint" name)))
        (body-color (hydra--body-color body))
        (body-timeout (plist-get body :timeout)))
    `(defun ,name ()
       ,doc
       (interactive)
       (hydra-default-pre)
       ,@(when body-pre (list body-pre))
       (hydra-disable)
       ,@(when (memq color '(blue teal)) '((hydra-keyboard-quit)))
       (catch 'hydra-disable
         ,@(delq nil
                 (if (memq color '(blue teal))
                     `(,(when cmd `(call-interactively #',cmd))
                        ,body-post)
                   `(,(when cmd
                            `(condition-case err
                                 (call-interactively #',cmd)
                               ((quit error)
                                (message "%S" err)
                                (unless hydra-lv
                                  (sit-for 0.8)))))
                      (when hydra-is-helpful
                        (,hint))
                      (setq hydra-last
                            (hydra-set-transient-map
                             (setq hydra-curr-map ,keymap)
                             t
                             ,(if (and
                                   (not (memq body-color
                                              '(amaranth pink teal)))
                                   body-post)
                                  `(lambda () (hydra-keyboard-quit) ,body-post)
                                  `(lambda () (hydra-keyboard-quit)))))
                      ,(or other-post
                           (when body-timeout
                             (list 'hydra-timeout
                                   body-timeout
                                   (when body-post
                                     (hydra--make-callable body-post))))))))))))

(defun hydra-pink-fallback ()
  "On intercepting a non-head, try to run it."
  (let ((keys (this-single-command-keys))
        kb)
    (when (equal keys [backspace])
      (setq keys ""))
    (setq kb (key-binding keys))
    (if kb
        (if (commandp kb)
            (condition-case err
                (call-interactively kb)
              ((quit error)
               (message "%S" err)
               (unless hydra-lv
                 (sit-for 0.8))))
          (message "Pink Hydra can't currently handle prefixes, continuing"))
      (message "Pink Hydra could not resolve: %S" keys))))

(defun hydra--modify-keymap (keymap def)
  "In KEYMAP, add DEF to each sub-keymap."
  (cl-labels
      ((recur (map)
         (if (atom map)
             map
           (if (eq (car map) 'keymap)
               (cons 'keymap
                     (cons
                      def
                      (recur (cdr map))))
             (cons
              (recur (car map))
              (recur (cdr map)))))))
    (recur keymap)))

(defmacro hydra--make-funcall (sym)
  "Transform SYM into a `funcall' that calls it."
  `(when (and ,sym (symbolp ,sym))
     (setq ,sym `(funcall #',,sym))))

(defun hydra--handle-nonhead (keymap name body heads)
  "Setup KEYMAP for intercepting non-head bindings.
NAME, BODY and HEADS are parameters to `defhydra'."
  (let ((body-color (hydra--body-color body))
        (body-post (plist-get (cddr body) :post)))
    (if body-post
        (hydra--make-funcall body-post)
      (when hydra-keyboard-quit
        (define-key keymap hydra-keyboard-quit #'hydra-keyboard-quit)))
    (when (memq body-color '(amaranth pink teal))
      (if (cl-some (lambda (h)
                     (memq (hydra--head-color h body) '(blue teal)))
                   heads)
          (progn
            (setcdr
             keymap
             (cdr
              (hydra--modify-keymap
               keymap
               (cons t
                     `(lambda ()
                        (interactive)
                        ,(cond
                          ((memq body-color '(amaranth teal))
                           '(message "An amaranth Hydra can only exit through a blue head"))
                          (t
                           '(hydra-pink-fallback)))
                        (hydra-set-transient-map hydra-curr-map t)
                        (when hydra-is-helpful
                          (unless hydra-lv
                            (sit-for 0.8))
                          (,(intern (format "%S/hint" name))))))))))
        (unless (eq body-color 'teal)
          (error
           "An %S Hydra must have at least one blue head in order to exit"
           body-color))))))

(defun hydra--head-name (h name body)
  "Return the symbol for head H of hydra with NAME and BODY."
  (let ((str (format "%S/%s" name
                     (if (symbolp (cadr h))
                         (cadr h)
                       (concat "lambda-" (car h))))))
    (when (and (memq (hydra--head-color h body) '(blue teal))
               (not (memq (cadr h) '(body nil))))
      (setq str (concat str "-and-exit")))
    (intern str)))

(defun hydra--delete-duplicates (heads)
  "Return HEADS without entries that have the same CMD part.
In duplicate HEADS, :cmd-name is modified to whatever they duplicate."
  (let ((ali '(((hydra-repeat . red) . hydra-repeat)))
        res entry)
    (dolist (h heads)
      (if (setq entry (assoc (cons (cadr h)
                                   (hydra--head-color h '(nil nil)))
                             ali))
          (setf (cl-cdddr h) (plist-put (cl-cdddr h) :cmd-name (cdr entry)))
        (push (cons (cons (cadr h)
                          (hydra--head-color h '(nil nil)))
                    (plist-get (cl-cdddr h) :cmd-name))
              ali)
        (push h res)))
    (nreverse res)))

(defun hydra--pad (lst n)
  "Pad LST with nil until length N."
  (let ((len (length lst)))
    (if (= len n)
        lst
      (append lst (make-list (- n len) nil)))))

(defun hydra--matrix (lst rows cols)
  "Create a matrix from elements of LST.
The matrix size is ROWS times COLS."
  (let ((ls (copy-sequence lst))
        res)
    (dotimes (_c cols)
      (push (hydra--pad (hydra-multipop ls rows) rows) res))
    (nreverse res)))

(defun hydra--cell (fstr names)
  "Format a rectangular cell based on FSTR and NAMES.
FSTR is a format-style string with two string inputs: one for the
doc and one for the symbol name.
NAMES is a list of variables."
  (let ((len (cl-reduce
              (lambda (acc it) (max (length (symbol-name it)) acc))
              names
              :initial-value 0)))
    (mapconcat
     (lambda (sym)
       (if sym
           (format fstr
                   (documentation-property sym 'variable-documentation)
                   (let ((name (symbol-name sym)))
                     (concat name (make-string (- len (length name)) ?^)))
                   sym)
         ""))
     names
     "\n")))

(defun hydra--vconcat (strs &optional joiner)
  "Glue STRS vertically.  They must be the same height.
JOINER is a function similar to `concat'."
  (setq joiner (or joiner #'concat))
  (mapconcat
   (lambda (s)
     (if (string-match " +$" s)
         (replace-match "" nil nil s)
       s))
   (apply #'cl-mapcar joiner
          (mapcar
           (lambda (s) (split-string s "\n"))
           strs))
   "\n"))

(defcustom hydra-cell-format "% -20s %% -8`%s"
  "The default format for docstring cells."
  :type 'string)

(defun hydra--table (names rows cols &optional cell-formats)
  "Format a `format'-style table from variables in NAMES.
The size of the table is ROWS times COLS.
CELL-FORMATS are `format' strings for each column.
If CELL-FORMATS is a string, it's used for all columns.
If CELL-FORMATS is nil, `hydra-cell-format' is used for all columns."
  (setq cell-formats
        (cond ((null cell-formats)
               (make-list cols hydra-cell-format))
              ((stringp cell-formats)
               (make-list cols cell-formats))
              (t
               cell-formats)))
  (hydra--vconcat
   (cl-mapcar
    #'hydra--cell
    cell-formats
    (hydra--matrix names rows cols))
   (lambda (&rest x)
     (mapconcat #'identity x "    "))))

(defun hydra-reset-radios (names)
  "Set varibles NAMES to their defaults.
NAMES should be defined by `defhydradio' or similar."
  (dolist (n names)
    (set n (aref (get n 'range) 0))))

(defvar hydra-timer (timer-create)
  "Timer for `hydra-timeout'.")

(defun hydra-timeout (secs &optional function)
  "In SECS seconds call FUNCTION, then function `hydra-keyboard-quit'.
Cancel the previous `hydra-timeout'."
  (cancel-timer hydra-timer)
  (setq hydra-timer (timer-create))
  (timer-set-time hydra-timer
                  (timer-relative-time (current-time) secs))
  (timer-set-function
   hydra-timer
   `(lambda ()
      ,(when function
             `(funcall ,function))
      (hydra-keyboard-quit)))
  (timer-activate hydra-timer))

;;* Macros
;;;###autoload
(defmacro defhydra (name body &optional docstring &rest heads)
  "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only. BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit and :bind.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'."
  (declare (indent defun))
  (cond ((stringp docstring))
        ((and (consp docstring)
              (memq (car docstring) '(hydra--table concat format)))
         (setq docstring (concat "\n" (eval docstring))))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "hydra")))
  (when (keywordp (car body))
    (setq body (cons nil (cons nil body))))
  (let* ((keymap (copy-keymap hydra-base-map))
         (keymap-name (intern (format "%S/keymap" name)))
         (body-name (intern (format "%S/body" name)))
         (body-key (cadr body))
         (body-plist (cddr body))
         (body-map (or (car body)
                       (plist-get body-plist :bind)))
         (body-pre (plist-get body-plist :pre))
         (body-body-pre (plist-get body-plist :body-pre))
         (body-post (plist-get body-plist :post)))
    (hydra--make-funcall body-post)
    (when body-post
      (setq heads (cons (list hydra-keyboard-quit #'hydra-keyboard-quit nil :exit t)
                        heads)))
    (dolist (h heads)
      (let ((len (length h)))
        (cond ((< len 2)
               (error "Each head should have at least two items: %S" h))
              ((= len 2)
               (setcdr (cdr h)
                       (list
                        (hydra-plist-get-default body-plist :hint "")))
               (setcdr (nthcdr 2 h)
                       (list :cmd-name (hydra--head-name h name body))))
              (t
               (let ((hint (cl-caddr h)))
                 (unless (or (null hint)
                             (stringp hint))
                   (setcdr (cdr h) (cons
                                    (hydra-plist-get-default body-plist :hint "")
                                    (cddr h))))
                 (let ((hint-and-plist (cddr h)))
                   (if (null (cdr hint-and-plist))
                       (setcdr hint-and-plist
                               (list :cmd-name
                                     (hydra--head-name h name body)))
                     (plist-put (cdr hint-and-plist)
                                :cmd-name
                                (hydra--head-name h name body)))))))))
    (let ((doc (hydra--doc body-key body-name heads))
          (heads-nodup (hydra--delete-duplicates heads)))
      (mapc
       (lambda (x)
         (define-key keymap (kbd (car x))
           (plist-get (cl-cdddr x) :cmd-name)))
       heads)
      (hydra--make-funcall body-pre)
      (hydra--make-funcall body-body-pre)
      (hydra--handle-nonhead keymap name body heads)
      `(progn
         ;; create keymap
         (set (defvar ,keymap-name
                nil
                ,(format "Keymap for %S." name))
              ',keymap)
         ;; create defuns
         ,@(mapcar
            (lambda (head)
              (hydra--make-defun name body doc head keymap-name
                                 body-pre body-post))
            heads-nodup)
         ;; free up keymap prefix
         ,@(unless (or (null body-key)
                       (null body-map)
                       (hydra--callablep body-map))
                   `((unless (keymapp (lookup-key ,body-map (kbd ,body-key)))
                       (define-key ,body-map (kbd ,body-key) nil))))
         ;; bind keys
         ,@(delq nil
                 (mapcar
                  (lambda (head)
                    (let ((name (hydra--head-property head :cmd-name)))
                      (when (and (cadr head)
                                 (not (eq (cadr head) 'hydra-keyboard-quit))
                                 (or body-key body-map))
                        (let ((bind (hydra--head-property head :bind body-map))
                              (final-key
                               (if body-key
                                   (vconcat (kbd body-key) (kbd (car head)))
                                 (kbd (car head)))))
                          (cond ((null bind) nil)
                                ((hydra--callablep bind)
                                 `(funcall ,bind ,final-key (function ,name)))
                                ((and (symbolp bind)
                                      (if (boundp bind)
                                          (keymapp (symbol-value bind))
                                        t))
                                 `(define-key ,bind ,final-key (function ,name)))
                                (t
                                 (error "Invalid :bind property `%S' for head %S" bind  head)))))))
                  heads))
         (defun ,(intern (format "%S/hint" name)) ()
           ,(hydra--message name body docstring heads))
         ,(hydra--make-defun
           name body doc '(nil body)
           keymap-name
           (or body-body-pre body-pre) body-post
           '(setq prefix-arg current-prefix-arg))))))

(defmacro defhydradio (name _body &rest heads)
  "Create radios with prefix NAME.
_BODY specifies the options; there are none currently.
HEADS have the format:

    (TOGGLE-NAME &optional VALUE DOC)

TOGGLE-NAME will be used along with NAME to generate a variable
name and a function that cycles it with the same name.  VALUE
should be an array.  The first element of VALUE will be used to
inialize the variable.
VALUE defaults to [nil t].
DOC defaults to TOGGLE-NAME split and capitalized."
  (declare (indent defun))
  `(progn
     ,@(apply #'append
              (mapcar (lambda (h)
                        (hydra--radio name h))
                      heads))
     (defvar ,(intern (format "%S/names" name))
       ',(mapcar (lambda (h) (intern (format "%S/%S" name (car h))))
                 heads))))

(defmacro hydra-multipop (lst n)
  "Return LST's first N elements while removing them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

(defun hydra--radio (parent head)
  "Generate a hydradio with PARENT from HEAD."
  (let* ((name (car head))
         (full-name (intern (format "%S/%S" parent name)))
         (doc (cadr head))
         (val (or (cl-caddr head) [nil t])))
    `((defvar ,full-name ,(hydra--quote-maybe (aref val 0)) ,doc)
      (put ',full-name 'range ,val)
      (defun ,full-name ()
        (hydra--cycle-radio ',full-name)))))

(defun hydra--quote-maybe (x)
  "Quote X if it's a symbol."
  (cond ((null x)
         nil)
        ((symbolp x)
         (list 'quote x))
        (t
         x)))

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
                  (cl-incf i)))
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

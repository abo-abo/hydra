;;; hydra-examples.el --- Some applications for Hydra

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

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
;; These are the sample Hydras.
;;
;; If you want to use them plainly, set `hydra-examples-verbatim' to t
;; before requiring this file. But it's probably better to only look
;; at them and use them as templates for building your own.

;;; Code:

(require 'hydra)

;;* Examples
;;** Example 1: text scale
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

;; This example generates three commands:
;;
;;     `hydra-zoom/text-scale-increase'
;;     `hydra-zoom/text-scale-decrease'
;;     `hydra-zoom/body'
;;
;; In addition, two of them are bound like this:
;;
;;     (global-set-key (kbd "<f2> g") 'hydra-zoom/text-scale-increase)
;;     (global-set-key (kbd "<f2> l") 'hydra-zoom/text-scale-decrease)
;;
;; Note that you can substitute `global-map' with e.g. `emacs-lisp-mode-map' if you need.
;; The functions generated will be the same, except the binding code will change to:
;;
;;     (define-key emacs-lisp-mode-map [f2 103]
;;       (function hydra-zoom/text-scale-increase))
;;     (define-key emacs-lisp-mode-map [f2 108]
;;       (function hydra-zoom/text-scale-decrease))

;;** Example 2: move window splitter
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-splitter (global-map "C-M-s")
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right)))

;;** Example 3: jump to error
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-error (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

;; This example introduces only one new thing: since the command
;; passed to the "q" head is nil, it will quit the Hydra without doing
;; anything. Heads that quit the Hydra instead of continuing are
;; referred to as having blue :color. All the other heads have red
;; :color, unless other is specified.

;;** Example 4: toggle rarely used modes
(when (bound-and-true-p hydra-examples-verbatim)
  (global-set-key
   (kbd "C-c C-v")
   (defhydra hydra-toggle (:color blue)
     "toggle"
     ("a" abbrev-mode "abbrev")
     ("d" toggle-debug-on-error "debug")
     ("f" auto-fill-mode "fill")
     ("t" toggle-truncate-lines "truncate")
     ("w" whitespace-mode "whitespace")
     ("q" nil "cancel"))))

;; Note that in this case, `defhydra' returns the `hydra-toggle/body'
;; symbol, which is then passed to `global-set-key'.
;;
;; Another new thing is that both the keymap and the body prefix are
;; skipped.  This means that `defhydra' will bind nothing - that's why
;; `global-set-key' is necessary.
;;
;; One more new thing is that you can assign a :color to the body. All
;; heads will inherit this color. The code above is very much equivalent to:
;;
;;     (global-set-key (kbd "C-c C-v a") 'abbrev-mode)
;;     (global-set-key (kbd "C-c C-v d") 'toggle-debug-on-error)
;;
;; The differences are:
;;
;; * You get a hint immediately after "C-c C-v"
;; * You can cancel and call a command immediately, e.g. "C-c C-v C-n"
;;   is equivalent to "C-n" with Hydra approach, while it will error
;;   that "C-c C-v C-n" isn't bound with the usual approach.

;;** Example 5: mini-vi
(defun hydra-vi/pre ()
  (set-cursor-color "#e52b50"))

(defun hydra-vi/post ()
  (set-cursor-color "#ffffff"))

(when (bound-and-true-p hydra-examples-verbatim)
  (global-set-key
   (kbd "C-z")
   (defhydra hydra-vi (:pre hydra-vi/pre :post hydra-vi/post :color amaranth)
     "vi"
     ("l" forward-char)
     ("h" backward-char)
     ("j" next-line)
     ("k" previous-line)
     ("m" set-mark-command "mark")
     ("a" move-beginning-of-line "beg")
     ("e" move-end-of-line "end")
     ("d" delete-region "del" :color blue)
     ("y" kill-ring-save "yank" :color blue)
     ("q" nil "quit"))))

;; This example introduces :color amaranth. It's similar to red,
;; except while you can quit red with any binding which isn't a Hydra
;; head, you can quit amaranth only with a blue head. So you can quit
;; this mode only with "d", "y", "q" or "C-g".
;;
;; Another novelty are the :pre and :post handlers. :pre will be
;; called before each command, while :post will be called when the
;; Hydra quits. In this case, they're used to override the cursor
;; color while Hydra is active.

;;** Example 6: selective global bind
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-next-error (global-map "C-x")
    "next-error"
    ("`" next-error "next")
    ("j" next-error "next" :bind nil)
    ("k" previous-error "previous" :bind nil)))

;; This example will bind "C-x `" in `global-map', but it will not
;; bind "C-x j" and "C-x k".
;; You can still "C-x `jjk" though.

;;* Windmove helpers
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;;* Obsoletes
(defvar hydra-example-text-scale
  '(("g" text-scale-increase "zoom in")
    ("l" text-scale-decrease "zoom out"))
  "A two-headed hydra for text scale manipulation.")
(make-obsolete-variable
 'hydra-example-text-scale
 "Don't use `hydra-example-text-scale', just write your own
`defhydra' using hydra-examples.el as a template"
 "0.9.0")

(defvar hydra-example-move-window-splitter
  '(("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right))
  "A four-headed hydra for the window splitter manipulation.
Works best if you have not more than 4 windows.")
(make-obsolete-variable
 'hydra-example-move-window-splitter
 "Don't use `hydra-example-move-window-splitter', just write your own
`defhydra' using hydra-examples.el as a template"
 "0.9.0")

(defvar hydra-example-goto-error
  '(("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev"))
  "A three-headed hydra for jumping between \"errors\".
Useful for e.g. `occur', `rgrep' and the like.")
(make-obsolete-variable
 'hydra-example-goto-error
 "Don't use `hydra-example-goto-error', just write your own
`defhydra' using hydra-examples.el as a template"
 "0.9.0")

(defvar hydra-example-windmove
  '(("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right))
  "A four-headed hydra for `windmove'.")
(make-obsolete-variable
 'hydra-example-windmove
 "Don't use `hydra-example-windmove', just write your own
`defhydra' using hydra-examples.el as a template"
 "0.9.0")

(provide 'hydra-examples)
;;; hydra-examples.el ends here

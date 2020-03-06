;;; hydra-test.el --- Tests for Hydra

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

;;; Code:

(require 'ert)
(require 'hydra)
(setq text-quoting-style 'grave)
(message "Emacs version: %s" emacs-version)

(ert-deftest hydra-red-error ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-error (global-map "M-g")
       "error"
       ("h" first-error "first")
       ("j" next-error "next")
       ("k" previous-error "prev")
       ("SPC" hydra-repeat "rep" :bind nil)))
    '(progn
      (set
       (defvar hydra-error/params nil
         "Params of hydra-error.")
       (quote (global-map "M-g")))
      (set
       (defvar hydra-error/docstring nil
         "Docstring of hydra-error.")
       "error")
      (set
       (defvar hydra-error/heads nil
         "Heads for hydra-error.")
       (quote
        (("h"
          first-error
          "first"
          :exit nil)
         ("j"
          next-error
          "next"
          :exit nil)
         ("k"
          previous-error
          "prev"
          :exit nil)
         ("SPC"
          hydra-repeat
          "rep"
          :bind nil
          :exit nil))))
      (set
       (defvar hydra-error/keymap nil
         "Keymap for hydra-error.")
       (quote
        (keymap
         (32 . hydra-repeat)
         (107 . hydra-error/previous-error)
         (106 . hydra-error/next-error)
         (104 . hydra-error/first-error)
         (kp-subtract . hydra--negative-argument)
         (kp-9 . hydra--digit-argument)
         (kp-8 . hydra--digit-argument)
         (kp-7 . hydra--digit-argument)
         (kp-6 . hydra--digit-argument)
         (kp-5 . hydra--digit-argument)
         (kp-4 . hydra--digit-argument)
         (kp-3 . hydra--digit-argument)
         (kp-2 . hydra--digit-argument)
         (kp-1 . hydra--digit-argument)
         (kp-0 . hydra--digit-argument)
         (57 . hydra--digit-argument)
         (56 . hydra--digit-argument)
         (55 . hydra--digit-argument)
         (54 . hydra--digit-argument)
         (53 . hydra--digit-argument)
         (52 . hydra--digit-argument)
         (51 . hydra--digit-argument)
         (50 . hydra--digit-argument)
         (49 . hydra--digit-argument)
         (48 . hydra--digit-argument)
         (45 . hydra--negative-argument)
         (21 . hydra--universal-argument))))
      (set
       (defvar hydra-error/hint nil
         "Dynamic hint for hydra-error.")
       (quote
        (format
         #("error: [h]: first, [j]: next, [k]: prev, [SPC]: rep."
           8 9 (face hydra-face-red)
           20 21 (face hydra-face-red)
           31 32 (face hydra-face-red)
           42 45 (face hydra-face-red)))))
      (defun hydra-error/first-error nil
        "Call the head `first-error' in the \"hydra-error\" hydra.

The heads for the associated hydra are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `hydra-repeat'

The body can be accessed via `hydra-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-error/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote first-error))
              (hydra--call-interactively-remap-maybe
               (function first-error)))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-error/hint
         (quote hydra-error))
        (hydra-set-transient-map
         hydra-error/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil))
      (defun hydra-error/next-error nil
        "Call the head `next-error' in the \"hydra-error\" hydra.

The heads for the associated hydra are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `hydra-repeat'

The body can be accessed via `hydra-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-error/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote next-error))
              (hydra--call-interactively-remap-maybe
               (function next-error)))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-error/hint
         (quote hydra-error))
        (hydra-set-transient-map
         hydra-error/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil))
      (defun hydra-error/previous-error nil
        "Call the head `previous-error' in the \"hydra-error\" hydra.

The heads for the associated hydra are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `hydra-repeat'

The body can be accessed via `hydra-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-error/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote previous-error))
              (hydra--call-interactively-remap-maybe
               (function previous-error)))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-error/hint
         (quote hydra-error))
        (hydra-set-transient-map
         hydra-error/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil))
      (unless (keymapp
               (lookup-key
                global-map
                (kbd "M-g")))
        (define-key global-map (kbd "M-g")
          nil))
      (define-key global-map [134217831 104]
       (quote hydra-error/first-error))
      (define-key global-map [134217831 106]
       (quote hydra-error/next-error))
      (define-key global-map [134217831 107]
       (quote
        hydra-error/previous-error))
      (defun hydra-error/body nil
        "Call the body in the \"hydra-error\" hydra.

The heads for the associated hydra are:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error',
\"SPC\":    `hydra-repeat'

The body can be accessed via `hydra-error/body', which is bound to \"M-g\"."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore nil))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-error/body)))
        (hydra-show-hint
         hydra-error/hint
         (quote hydra-error))
        (hydra-set-transient-map
         hydra-error/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest hydra-blue-toggle ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-toggle (:color blue)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel")))
    '(progn
      (set
       (defvar hydra-toggle/params nil
         "Params of hydra-toggle.")
       (quote
        (nil
         nil
         :exit t
         :foreign-keys nil)))
      (set
       (defvar hydra-toggle/docstring nil
         "Docstring of hydra-toggle.")
       "toggle")
      (set
       (defvar hydra-toggle/heads nil
         "Heads for hydra-toggle.")
       (quote
        (("t"
          toggle-truncate-lines
          "truncate"
          :exit t)
         ("f"
          auto-fill-mode
          "fill"
          :exit t)
         ("a"
          abbrev-mode
          "abbrev"
          :exit t)
         ("q" nil "cancel" :exit t))))
      (set
       (defvar hydra-toggle/keymap nil
         "Keymap for hydra-toggle.")
       (quote
        (keymap
         (113 . hydra-toggle/nil)
         (97 . hydra-toggle/abbrev-mode-and-exit)
         (102 . hydra-toggle/auto-fill-mode-and-exit)
         (116 . hydra-toggle/toggle-truncate-lines-and-exit)
         (kp-subtract . hydra--negative-argument)
         (kp-9 . hydra--digit-argument)
         (kp-8 . hydra--digit-argument)
         (kp-7 . hydra--digit-argument)
         (kp-6 . hydra--digit-argument)
         (kp-5 . hydra--digit-argument)
         (kp-4 . hydra--digit-argument)
         (kp-3 . hydra--digit-argument)
         (kp-2 . hydra--digit-argument)
         (kp-1 . hydra--digit-argument)
         (kp-0 . hydra--digit-argument)
         (57 . hydra--digit-argument)
         (56 . hydra--digit-argument)
         (55 . hydra--digit-argument)
         (54 . hydra--digit-argument)
         (53 . hydra--digit-argument)
         (52 . hydra--digit-argument)
         (51 . hydra--digit-argument)
         (50 . hydra--digit-argument)
         (49 . hydra--digit-argument)
         (48 . hydra--digit-argument)
         (45 . hydra--negative-argument)
         (21 . hydra--universal-argument))))
      (set
       (defvar hydra-toggle/hint nil
         "Dynamic hint for hydra-toggle.")
       (quote
        (format
         #("toggle: [t]: truncate, [f]: fill, [a]: abbrev, [q]: cancel."
           9 10 (face hydra-face-blue)
           24 25 (face hydra-face-blue)
           35 36 (face hydra-face-blue)
           48 49 (face hydra-face-blue)))))
      (defun hydra-toggle/toggle-truncate-lines-and-exit nil
        "Call the head `toggle-truncate-lines' in the \"hydra-toggle\" hydra.

The heads for the associated hydra are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `hydra-toggle/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-toggle/body))
        (progn
          (setq this-command
                (quote toggle-truncate-lines))
          (hydra--call-interactively-remap-maybe
           (function
            toggle-truncate-lines))))
      (defun hydra-toggle/auto-fill-mode-and-exit nil
        "Call the head `auto-fill-mode' in the \"hydra-toggle\" hydra.

The heads for the associated hydra are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `hydra-toggle/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-toggle/body))
        (progn
          (setq this-command
                (quote auto-fill-mode))
          (hydra--call-interactively-remap-maybe
           (function auto-fill-mode))))
      (defun hydra-toggle/abbrev-mode-and-exit nil
        "Call the head `abbrev-mode' in the \"hydra-toggle\" hydra.

The heads for the associated hydra are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `hydra-toggle/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-toggle/body))
        (progn
          (setq this-command
                (quote abbrev-mode))
          (hydra--call-interactively-remap-maybe
           (function abbrev-mode))))
      (defun hydra-toggle/nil nil
        "Call the head `nil' in the \"hydra-toggle\" hydra.

The heads for the associated hydra are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `hydra-toggle/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-toggle/body)))
      (defun hydra-toggle/body nil
        "Call the body in the \"hydra-toggle\" hydra.

The heads for the associated hydra are:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `hydra-toggle/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore nil))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-toggle/body)))
        (hydra-show-hint
         hydra-toggle/hint
         (quote hydra-toggle))
        (hydra-set-transient-map
         hydra-toggle/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest hydra-amaranth-vi ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-vi
       (:pre
        (set-cursor-color "#e52b50")
        :post
        (set-cursor-color "#ffffff")
        :color amaranth)
       "vi"
       ("j" next-line)
       ("k" previous-line)
       ("q" nil "quit")))
    '(progn
      (set
       (defvar hydra-vi/params nil
         "Params of hydra-vi.")
       (quote
        (nil
         nil
         :exit nil
         :foreign-keys warn
         :post (set-cursor-color "#ffffff")
         :pre (set-cursor-color "#e52b50"))))
      (set
       (defvar hydra-vi/docstring nil
         "Docstring of hydra-vi.")
       "vi")
      (set
       (defvar hydra-vi/heads nil
         "Heads for hydra-vi.")
       (quote
        (("j" next-line "" :exit nil)
         ("k"
          previous-line
          ""
          :exit nil)
         ("q" nil "quit" :exit t))))
      (set
       (defvar hydra-vi/keymap nil
         "Keymap for hydra-vi.")
       (quote
        (keymap
         (113 . hydra-vi/nil)
         (107 . hydra-vi/previous-line)
         (106 . hydra-vi/next-line)
         (kp-subtract . hydra--negative-argument)
         (kp-9 . hydra--digit-argument)
         (kp-8 . hydra--digit-argument)
         (kp-7 . hydra--digit-argument)
         (kp-6 . hydra--digit-argument)
         (kp-5 . hydra--digit-argument)
         (kp-4 . hydra--digit-argument)
         (kp-3 . hydra--digit-argument)
         (kp-2 . hydra--digit-argument)
         (kp-1 . hydra--digit-argument)
         (kp-0 . hydra--digit-argument)
         (57 . hydra--digit-argument)
         (56 . hydra--digit-argument)
         (55 . hydra--digit-argument)
         (54 . hydra--digit-argument)
         (53 . hydra--digit-argument)
         (52 . hydra--digit-argument)
         (51 . hydra--digit-argument)
         (50 . hydra--digit-argument)
         (49 . hydra--digit-argument)
         (48 . hydra--digit-argument)
         (45 . hydra--negative-argument)
         (21 . hydra--universal-argument))))
      (set
       (defvar hydra-vi/hint nil
         "Dynamic hint for hydra-vi.")
       (quote
        (format
         #("vi: j, k, [q]: quit."
           4 5 (face hydra-face-amaranth)
           7 8 (face hydra-face-amaranth)
           11 12 (face hydra-face-teal)))))
      (defun hydra-vi/next-line nil
        "Call the head `next-line' in the \"hydra-vi\" hydra.

The heads for the associated hydra are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (set-cursor-color "#e52b50")
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-vi/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote next-line))
              (hydra--call-interactively-remap-maybe
               (function next-line)))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-vi/hint
         (quote hydra-vi))
        (hydra-set-transient-map
         hydra-vi/keymap
         (lambda nil
           (hydra-keyboard-quit)
           (set-cursor-color "#ffffff"))
         (quote warn)))
      (defun hydra-vi/previous-line nil
        "Call the head `previous-line' in the \"hydra-vi\" hydra.

The heads for the associated hydra are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (set-cursor-color "#e52b50")
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-vi/body)))
        (condition-case err
            (progn
              (setq this-command
                    (quote previous-line))
              (hydra--call-interactively-remap-maybe
               (function previous-line)))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-vi/hint
         (quote hydra-vi))
        (hydra-set-transient-map
         hydra-vi/keymap
         (lambda nil
           (hydra-keyboard-quit)
           (set-cursor-color "#ffffff"))
         (quote warn)))
      (defun hydra-vi/nil nil
        "Call the head `nil' in the \"hydra-vi\" hydra.

The heads for the associated hydra are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (set-cursor-color "#e52b50")
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-vi/body)))
      (defun hydra-vi/body nil
        "Call the body in the \"hydra-vi\" hydra.

The heads for the associated hydra are:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (set-cursor-color "#e52b50")
        (let ((hydra--ignore nil))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-vi/body)))
        (hydra-show-hint
         hydra-vi/hint
         (quote hydra-vi))
        (hydra-set-transient-map
         hydra-vi/keymap
         (lambda nil
           (hydra-keyboard-quit)
           (set-cursor-color "#ffffff"))
         (quote warn))
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest hydra-zoom-duplicate-1 ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-zoom ()
       "zoom"
       ("r" (text-scale-set 0) "reset")
       ("0" (text-scale-set 0) :bind nil :exit t)
       ("1" (text-scale-set 0) nil :bind nil :exit t)))
    '(progn
      (set
       (defvar hydra-zoom/params nil
         "Params of hydra-zoom.")
       (quote (nil nil)))
      (set
       (defvar hydra-zoom/docstring nil
         "Docstring of hydra-zoom.")
       "zoom")
      (set
       (defvar hydra-zoom/heads nil
         "Heads for hydra-zoom.")
       (quote
        (("r"
          (text-scale-set 0)
          "reset"
          :exit nil)
         ("0"
          (text-scale-set 0)
          ""
          :bind nil
          :exit t)
         ("1"
          (text-scale-set 0)
          nil
          :bind nil
          :exit t))))
      (set
       (defvar hydra-zoom/keymap nil
         "Keymap for hydra-zoom.")
       (quote
        (keymap
         (114 . hydra-zoom/lambda-r)
         (kp-subtract . hydra--negative-argument)
         (kp-9 . hydra--digit-argument)
         (kp-8 . hydra--digit-argument)
         (kp-7 . hydra--digit-argument)
         (kp-6 . hydra--digit-argument)
         (kp-5 . hydra--digit-argument)
         (kp-4 . hydra--digit-argument)
         (kp-3 . hydra--digit-argument)
         (kp-2 . hydra--digit-argument)
         (kp-1 . hydra--digit-argument)
         (kp-0 . hydra--digit-argument)
         (57 . hydra--digit-argument)
         (56 . hydra--digit-argument)
         (55 . hydra--digit-argument)
         (54 . hydra--digit-argument)
         (53 . hydra--digit-argument)
         (52 . hydra--digit-argument)
         (51 . hydra--digit-argument)
         (50 . hydra--digit-argument)
         (49 . hydra-zoom/lambda-0-and-exit)
         (48 . hydra-zoom/lambda-0-and-exit)
         (45 . hydra--negative-argument)
         (21 . hydra--universal-argument))))
      (set
       (defvar hydra-zoom/hint nil
         "Dynamic hint for hydra-zoom.")
       (quote
        (format
         #("zoom: [r 0]: reset."
           7 8 (face hydra-face-red)
           9 10 (face hydra-face-blue)))))
      (defun hydra-zoom/lambda-r nil
        "Call the head `(text-scale-set 0)' in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `hydra-zoom/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-zoom/body)))
        (condition-case err
            (hydra--call-interactively-remap-maybe
             (function
              (lambda nil
               (interactive)
               (text-scale-set 0))))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-zoom/hint
         (quote hydra-zoom))
        (hydra-set-transient-map
         hydra-zoom/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil))
      (defun hydra-zoom/lambda-0-and-exit nil
        "Call the head `(text-scale-set 0)' in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `hydra-zoom/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-zoom/body))
        (hydra--call-interactively-remap-maybe
         (function
          (lambda nil
           (interactive)
           (text-scale-set 0)))))
      (defun hydra-zoom/body nil
        "Call the body in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `hydra-zoom/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore nil))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-zoom/body)))
        (hydra-show-hint
         hydra-zoom/hint
         (quote hydra-zoom))
        (hydra-set-transient-map
         hydra-zoom/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest hydra-zoom-duplicate-2 ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-zoom ()
       "zoom"
       ("r" (text-scale-set 0) "reset")
       ("0" (text-scale-set 0) :bind nil :exit t)
       ("1" (text-scale-set 0) nil :bind nil)))
    '(progn
      (set
       (defvar hydra-zoom/params nil
         "Params of hydra-zoom.")
       (quote (nil nil)))
      (set
       (defvar hydra-zoom/docstring nil
         "Docstring of hydra-zoom.")
       "zoom")
      (set
       (defvar hydra-zoom/heads nil
         "Heads for hydra-zoom.")
       (quote
        (("r"
          (text-scale-set 0)
          "reset"
          :exit nil)
         ("0"
          (text-scale-set 0)
          ""
          :bind nil
          :exit t)
         ("1"
          (text-scale-set 0)
          nil
          :bind nil
          :exit nil))))
      (set
       (defvar hydra-zoom/keymap nil
         "Keymap for hydra-zoom.")
       (quote
        (keymap
         (114 . hydra-zoom/lambda-r)
         (kp-subtract . hydra--negative-argument)
         (kp-9 . hydra--digit-argument)
         (kp-8 . hydra--digit-argument)
         (kp-7 . hydra--digit-argument)
         (kp-6 . hydra--digit-argument)
         (kp-5 . hydra--digit-argument)
         (kp-4 . hydra--digit-argument)
         (kp-3 . hydra--digit-argument)
         (kp-2 . hydra--digit-argument)
         (kp-1 . hydra--digit-argument)
         (kp-0 . hydra--digit-argument)
         (57 . hydra--digit-argument)
         (56 . hydra--digit-argument)
         (55 . hydra--digit-argument)
         (54 . hydra--digit-argument)
         (53 . hydra--digit-argument)
         (52 . hydra--digit-argument)
         (51 . hydra--digit-argument)
         (50 . hydra--digit-argument)
         (49 . hydra-zoom/lambda-r)
         (48 . hydra-zoom/lambda-0-and-exit)
         (45 . hydra--negative-argument)
         (21 . hydra--universal-argument))))
      (set
       (defvar hydra-zoom/hint nil
         "Dynamic hint for hydra-zoom.")
       (quote
        (format
         #("zoom: [r 0]: reset."
           7 8 (face hydra-face-red)
           9 10 (face hydra-face-blue)))))
      (defun hydra-zoom/lambda-r nil
        "Call the head `(text-scale-set 0)' in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `hydra-zoom/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore t))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-zoom/body)))
        (condition-case err
            (hydra--call-interactively-remap-maybe
             (function
              (lambda nil
               (interactive)
               (text-scale-set 0))))
          ((quit error)
           (message
            (error-message-string err))))
        (hydra-show-hint
         hydra-zoom/hint
         (quote hydra-zoom))
        (hydra-set-transient-map
         hydra-zoom/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil))
      (defun hydra-zoom/lambda-0-and-exit nil
        "Call the head `(text-scale-set 0)' in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `hydra-zoom/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (hydra-keyboard-quit)
        (setq hydra-curr-body-fn
              (quote hydra-zoom/body))
        (hydra--call-interactively-remap-maybe
         (function
          (lambda nil
           (interactive)
           (text-scale-set 0)))))
      (defun hydra-zoom/body nil
        "Call the body in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"r\":    `(text-scale-set 0)',
\"0\":    `(text-scale-set 0)',
\"1\":    `(text-scale-set 0)'

The body can be accessed via `hydra-zoom/body'."
        (interactive)
        (require (quote hydra))
        (hydra-default-pre)
        (let ((hydra--ignore nil))
          (hydra-keyboard-quit)
          (setq hydra-curr-body-fn
                (quote hydra-zoom/body)))
        (hydra-show-hint
         hydra-zoom/hint
         (quote hydra-zoom))
        (hydra-set-transient-map
         hydra-zoom/keymap
         (lambda nil
           (hydra-keyboard-quit)
           nil)
         nil)
        (setq prefix-arg
              current-prefix-arg))))))

(ert-deftest defhydradio ()
  (should (equal
           (macroexpand
            '(defhydradio hydra-test ()
              (num "Num" [0 1 2 3 4 5 6 7 8 9 10])
              (str "Str" ["foo" "bar" "baz"])))
           '(progn
             (defvar hydra-test/num 0
               "Num")
             (put 'hydra-test/num 'range [0 1 2 3 4 5 6 7 8 9 10])
             (defun hydra-test/num ()
               (hydra--cycle-radio 'hydra-test/num))
             (defvar hydra-test/str "foo"
               "Str")
             (put 'hydra-test/str 'range ["foo" "bar" "baz"])
             (defun hydra-test/str ()
               (hydra--cycle-radio 'hydra-test/str))
             (defvar hydra-test/names '(hydra-test/num hydra-test/str))))))

(ert-deftest hydra-blue-compat ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-toggle (:color blue)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel")))
    (macroexpand
     '(defhydra hydra-toggle (:exit t)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel"))))))

(ert-deftest hydra-amaranth-compat ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-vi
       (:pre
        (set-cursor-color "#e52b50")
        :post
        (set-cursor-color "#ffffff")
        :color amaranth)
       "vi"
       ("j" next-line)
       ("k" previous-line)
       ("q" nil "quit")))
    (macroexpand
     '(defhydra hydra-vi
       (:pre
        (set-cursor-color "#e52b50")
        :post
        (set-cursor-color "#ffffff")
        :foreign-keys warn)
       "vi"
       ("j" next-line)
       ("k" previous-line)
       ("q" nil "quit"))))))

(ert-deftest hydra-pink-compat ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-zoom (global-map "<f2>"
                            :color pink)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit")))
    (macroexpand
     '(defhydra hydra-zoom (global-map "<f2>"
                            :foreign-keys run)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit"))))))

(ert-deftest hydra-teal-compat ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-zoom (global-map "<f2>"
                            :color teal)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit")))
    (macroexpand
     '(defhydra hydra-zoom (global-map "<f2>"
                            :foreign-keys warn
                            :exit t)
       "zoom"
       ("g" text-scale-increase "in")
       ("l" text-scale-decrease "out")
       ("q" nil "quit"))))))

(ert-deftest hydra-format-1 ()
  (should (equal
           (let ((hydra-fontify-head-function
                  'hydra-fontify-head-greyscale))
             (hydra--format
              'hydra-toggle
              nil
              "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
" '(("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("g" golden-ratio-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil "quit"))))
           '(format
             "%s abbrev-mode:       %S
%s debug-on-error:    %S
%s auto-fill-mode:    %S
[{q}]: quit."
             "{a}" abbrev-mode
             "{d}" debug-on-error
             "{f}" auto-fill-function))))

(ert-deftest hydra-format-2 ()
  (should (equal
           (let ((hydra-fontify-head-function
                  'hydra-fontify-head-greyscale))
             (hydra--format
              'bar
              nil
              "\n  bar %s`foo\n"
              '(("a" (quote t) "" :cmd-name bar/lambda-a :exit nil)
                ("q" nil "" :cmd-name bar/nil :exit t))))
           '(format "  bar %s\n{a}, [q]." foo))))

(ert-deftest hydra-format-3 ()
  (should (equal
           (let ((hydra-fontify-head-function
                  'hydra-fontify-head-greyscale))
             (hydra--format
              'bar
              nil
              "\n_<SPC>_   ^^ace jump\n"
              '(("<SPC>" ace-jump-char-mode nil :cmd-name bar/ace-jump-char-mode))))
           '(format "%s   ace jump\n" "{<SPC>}"))))

(ert-deftest hydra-format-4 ()
  (should
   (equal (hydra--format
           nil
           '(nil nil :hint nil)
           "\n_j_,_k_"
           '(("j" nil nil :exit t) ("k" nil nil :exit t)))
          '(format "%s,%s"
            #("j" 0 1 (face hydra-face-blue))
            #("k" 0 1 (face hydra-face-blue))))))

(ert-deftest hydra-format-5 ()
  (should
   (equal (hydra--format
           nil nil "\n_-_: mark          _u_: unmark\n"
           '(("-" Buffer-menu-mark nil)
             ("u" Buffer-menu-unmark nil)))
          '(format
            "%s: mark          %s: unmark\n"
            #("-" 0 1 (face hydra-face-red))
            #("u" 0 1 (face hydra-face-red))))))

(ert-deftest hydra-format-6 ()
  (should
   (equal (hydra--format
           nil nil "\n[_]_] forward [_[_] backward\n"
           '(("]" forward-char nil)
             ("[" backward-char nil)))
          '(format
            "[%s] forward [%s] backward\n"
            #("]"
              0 1 (face
                   hydra-face-red))
            #("["
              0 1 (face
                   hydra-face-red))))))

(ert-deftest hydra-format-7 ()
  (should
   (equal
    (hydra--format nil nil "test"
                   '(("%" forward-char "" :exit nil)
                     ("b" backward-char "" :exit nil)))
    '(format
      #("test: %%%%, b."
        6 7 (face hydra-face-red)
        7 8 (face hydra-face-red)
        8 9 (face hydra-face-red)
        9 10 (face hydra-face-red)
        12 13 (face hydra-face-red)))))
  (should
   (equal
    (hydra--format nil nil "\n_%_ forward\n"
                   '(("%" forward-char nil :exit nil)))
    '(format
      "%s forward\n"
      #("%%"
        0 2 (face hydra-face-red))))))

(ert-deftest hydra-format-8 ()
  (should
   (equal
    (hydra--format nil '(nil nil :hint nil) "test"
                   '(("f" forward-char nil :exit nil)
                     ("b" backward-char "back" :exit nil)))
    '(format
      #("test: [b]: back."
        7 8 (face hydra-face-red))))))

(ert-deftest hydra-format-9 ()
  (should
   (equal
    (hydra--format nil '(nil nil :hint nil) "\n_f_(foo)"
                   '(("f" forward-char nil :exit nil)))
    '(format
      "%s(foo)"
      #("f" 0 1 (face hydra-face-red))))))

(ert-deftest hydra-format-10 ()
  (should
   (equal
    (hydra--format nil '(nil nil) "Test:"
                   '(("j" next-line (format-time-string "%H:%M:%S" (current-time))
                      :exit nil)))
    '(concat
      (format "Test:\n")
      (mapconcat
       (function
        hydra--eval-and-format)
       (quote
        ((#("j" 0 1 (face hydra-face-red))
           format-time-string
           "%H:%M:%S"
           (current-time))))
       ", ")
      "."))))

(ert-deftest hydra-format-11 ()
  (should
   (equal
    (hydra--format nil '(nil nil :hint nil) "\n_f_ #+begin__src/#+end__src"
                   '(("f" forward-char nil :exit nil)))
    '(format
      "%s #+begin_src/#+end_src"
      #("f" 0 1 (face hydra-face-red))))))

(ert-deftest hydra-format-with-sexp-1 ()
  (should (equal
           (let ((hydra-fontify-head-function
                  'hydra-fontify-head-greyscale))
             (hydra--format
              'hydra-toggle nil
              "\n_n_ narrow-or-widen-dwim %(progn (message \"checking\")(buffer-narrowed-p))asdf\n"
              '(("n" narrow-to-region nil) ("q" nil "cancel" :exit t))))
           '(format
             "%s narrow-or-widen-dwim %Sasdf\n[[q]]: cancel."
             "{n}"
             (progn
               (message "checking")
               (buffer-narrowed-p))))))

(ert-deftest hydra-format-with-sexp-2 ()
  (should (equal
           (let ((hydra-fontify-head-function
                  'hydra-fontify-head-greyscale))
             (hydra--format
              'hydra-toggle nil
              "\n_n_ narrow-or-widen-dwim %s(progn (message \"checking\")(buffer-narrowed-p))asdf\n"
              '(("n" narrow-to-region nil) ("q" nil "cancel" :exit t))))
           '(format
             "%s narrow-or-widen-dwim %sasdf\n[[q]]: cancel."
             "{n}"
             (progn
               (message "checking")
               (buffer-narrowed-p))))))

(ert-deftest hydra-compat-colors-2 ()
  (should
   (equal
    (cddr (macroexpand
           '(defhydra hydra-test (:color amaranth)
             ("a" fun-a)
             ("b" fun-b :color blue)
             ("c" fun-c :color blue)
             ("d" fun-d :color blue)
             ("e" fun-e :color blue)
             ("f" fun-f :color blue))))
    (cddr (macroexpand
           '(defhydra hydra-test (:color teal)
             ("a" fun-a :color red)
             ("b" fun-b)
             ("c" fun-c)
             ("d" fun-d)
             ("e" fun-e)
             ("f" fun-f)))))))

(ert-deftest hydra-compat-colors-3 ()
  (should
   (equal
    (cddr (macroexpand
           '(defhydra hydra-test ()
             ("a" fun-a)
             ("b" fun-b :color blue)
             ("c" fun-c :color blue)
             ("d" fun-d :color blue)
             ("e" fun-e :color blue)
             ("f" fun-f :color blue))))
    (cddr (macroexpand
           '(defhydra hydra-test (:color blue)
             ("a" fun-a :color red)
             ("b" fun-b)
             ("c" fun-c)
             ("d" fun-d)
             ("e" fun-e)
             ("f" fun-f)))))))

(ert-deftest hydra-compat-colors-4 ()
  (should
   (equal
    (cddr (macroexpand
           '(defhydra hydra-test ()
             ("a" fun-a)
             ("b" fun-b :exit t)
             ("c" fun-c :exit t)
             ("d" fun-d :exit t)
             ("e" fun-e :exit t)
             ("f" fun-f :exit t))))
    (cddr (macroexpand
           '(defhydra hydra-test (:exit t)
             ("a" fun-a :exit nil)
             ("b" fun-b)
             ("c" fun-c)
             ("d" fun-d)
             ("e" fun-e)
             ("f" fun-f)))))))

(ert-deftest hydra--pad ()
  (should (equal (hydra--pad '(a b c) 3)
                 '(a b c)))
  (should (equal (hydra--pad '(a) 3)
                 '(a nil nil))))

(ert-deftest hydra--matrix ()
  (should (equal (hydra--matrix '(a b c) 2 2)
                 '((a b) (c nil))))
  (should (equal (hydra--matrix '(a b c d e f g h i) 4 3)
                 '((a b c d) (e f g h) (i nil nil nil)))))

(ert-deftest hydra--cell ()
  (should (equal (hydra--cell "% -75s %%`%s" '(hydra-hint-display-type hydra-verbose))
                 "The utility to show hydra hint                                              %`hydra-hint-display-type
When non-nil, hydra will issue some non essential style warnings.           %`hydra-verbose^^^^^^^^^^")))

(ert-deftest hydra--vconcat ()
  (should (equal (hydra--vconcat '("abc\ndef" "012\n34" "def\nabc"))
                 "abc012def\ndef34abc")))

(defhydradio hydra-tng ()
  (picard "_p_ Captain Jean Luc Picard:")
  (riker "_r_ Commander William Riker:")
  (data "_d_ Lieutenant Commander Data:")
  (worf "_w_ Worf:")
  (la-forge "_f_ Geordi La Forge:")
  (troi "_t_ Deanna Troi:")
  (dr-crusher "_c_ Doctor Beverly Crusher:")
  (phaser "_h_ Set phasers to " [stun kill]))

(ert-deftest hydra--table ()
  (let ((hydra-cell-format "% -30s %% -8`%s"))
    (should (equal (hydra--table hydra-tng/names 5 2)
                   (substring "
_p_ Captain Jean Luc Picard:   % -8`hydra-tng/picard^^    _t_ Deanna Troi:               % -8`hydra-tng/troi^^^^^^
_r_ Commander William Riker:   % -8`hydra-tng/riker^^^    _c_ Doctor Beverly Crusher:    % -8`hydra-tng/dr-crusher
_d_ Lieutenant Commander Data: % -8`hydra-tng/data^^^^    _h_ Set phasers to             % -8`hydra-tng/phaser^^^^
_w_ Worf:                      % -8`hydra-tng/worf^^^^
_f_ Geordi La Forge:           % -8`hydra-tng/la-forge" 1)))
    (should (equal (hydra--table hydra-tng/names 4 3)
                   (substring "
_p_ Captain Jean Luc Picard:   % -8`hydra-tng/picard    _f_ Geordi La Forge:           % -8`hydra-tng/la-forge^^
_r_ Commander William Riker:   % -8`hydra-tng/riker^    _t_ Deanna Troi:               % -8`hydra-tng/troi^^^^^^
_d_ Lieutenant Commander Data: % -8`hydra-tng/data^^    _c_ Doctor Beverly Crusher:    % -8`hydra-tng/dr-crusher
_w_ Worf:                      % -8`hydra-tng/worf^^    _h_ Set phasers to             % -8`hydra-tng/phaser^^^^" 1)))))

(ert-deftest hydra--make-funcall ()
  (should (equal (let ((body-pre 'foo))
                   (hydra--make-funcall body-pre)
                   body-pre)
                 '(funcall (function foo)))))

(defhydra hydra-simple-1 (global-map "C-c")
  ("a" (insert "j"))
  ("b" (insert "k"))
  ("q" nil))

(defhydra hydra-simple-2 (global-map "C-c" :color amaranth)
  ("c" self-insert-command)
  ("d" self-insert-command)
  ("q" nil))

(defhydra hydra-simple-3 (global-map "C-c")
  ("g" goto-line)
  ("1" find-file)
  ("q" nil))

(defun remapable-print ()
  (interactive)
  (insert "remapable print was called"))
(defun remaped-print ()
  (interactive)
  (insert "*remaped* print was called"))
(define-key global-map (kbd "C-=") 'remapable-print)
(define-key global-map [remap remapable-print] 'remaped-print)

(defhydra hydra-simple-with-remap (global-map "C-c")
  ("r" remapable-print)
  ("q" nil))

(defmacro hydra-with (in &rest body)
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
            (progn
              (switch-to-buffer temp-buffer)
              (transient-mark-mode 1)
              (insert ,in)
              (goto-char (point-min))
              (when (search-forward "~" nil t)
                (backward-delete-char 1)
                (set-mark (point)))
              (goto-char (point-max))
              (search-backward "|")
              (delete-char 1)
              (setq current-prefix-arg nil)
              ,@body
              (insert "|")
              (when (region-active-p)
                (exchange-point-and-mark)
                (insert "~"))
              (buffer-substring-no-properties
               (point-min)
               (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(ert-deftest hydra-integration-1 ()
  (should (string= (hydra-with "|"
                               (execute-kbd-macro
                                (kbd "C-c aabbaaqaabbaa")))
                   "jjkkjjaabbaa|"))
  (should (string= (hydra-with "|"
                               (condition-case nil
                                   (execute-kbd-macro
                                    (kbd "C-c aabb C-g"))
                                 (quit nil))
                               (execute-kbd-macro "aaqaabbaa"))
                   "jjkkaaqaabbaa|")))

(ert-deftest hydra-integration-2 ()
  (should (string= (hydra-with "|"
                               (execute-kbd-macro
                                (kbd "C-c c 1 c 2 d 4 c q")))
                   "ccddcccc|"))
  (should (string= (hydra-with "|"
                               (execute-kbd-macro
                                (kbd "C-c c 1 c C-u d C-u 10 c q")))
                   "ccddddcccccccccc|")))

(ert-deftest hydra-integration-3 ()
  (should (string= (hydra-with "foo\nbar|"
                               (execute-kbd-macro
                                (kbd "C-c g 1 RET q")))
                   "|foo\nbar")))

(ert-deftest hydra-remap-lookup-1 ()
  "try calling a remapped command while option is disabled "
  (setq hydra-look-for-remap nil)
  (should (string= (hydra-with "|"
                               (execute-kbd-macro
                                (kbd "C-c rq")))
                   "remapable print was called|")))
(ert-deftest hydra-remap-lookup-2 ()
  "try calling a remapped command while option is enabled"
  (setq hydra-look-for-remap t)
  (should (string= (hydra-with "|"
                               (execute-kbd-macro
                                (kbd "C-c rq")))
                   "*remaped* print was called|")))

(ert-deftest hydra-columns-1 ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defhydra hydra-info (:color blue
                                                     :columns 3)
                                "Info-mode"
                                ("?" Info-summary "summary")
                                ("]" Info-forward-node "forward")
                                ("[" Info-backward-node "backward")
                                ("<" Info-top-node "top node")
                                (">" Info-final-node "final node")
                                ("h" Info-help "help")
                                ("d" Info-directory "info dir")
                                ("f" Info-follow-reference "follow ref")
                                ("g" Info-goto-node "goto node")
                                ("l" Info-history-back "hist back")
                                ("r" Info-history-forward "hist forward")
                                ("i" Info-index "index")
                                ("I" Info-virtual-index "virtual index")
                                ("L" Info-history "hist")
                                ("n" Info-next "next")
                                ("p" Info-prev "previous")
                                ("s" Info-search "search")
                                ("S" Info-search-case-sensitively "case-search")
                                ("T" Info-toc "TOC")
                                ("u" Info-up "up")
                                ("m" Info-menu "menu")
                                ("t" hydra-info-to/body "info-to")))))))
                 #("Info-mode:
?: summary       ]: forward       [: backward
<: top node      >: final node    h: help
d: info dir      f: follow ref    g: goto node
l: hist back     r: hist forward  i: index
I: virtual index L: hist          n: next
p: previous      s: search        S: case-search
T: TOC           u: up            m: menu
t: info-to"
                   11 12 (face hydra-face-blue)
                   28 29 (face hydra-face-blue)
                   45 46 (face hydra-face-blue)
                   57 58 (face hydra-face-blue)
                   74 75 (face hydra-face-blue)
                   91 92 (face hydra-face-blue)
                   99 100 (face hydra-face-blue)
                   116 117 (face hydra-face-blue)
                   133 134 (face hydra-face-blue)
                   146 147 (face hydra-face-blue)
                   163 164 (face hydra-face-blue)
                   180 181 (face hydra-face-blue)
                   189 190 (face hydra-face-blue)
                   206 207 (face hydra-face-blue)
                   223 224 (face hydra-face-blue)
                   231 232 (face hydra-face-blue)
                   248 249 (face hydra-face-blue)
                   265 266 (face hydra-face-blue)
                   280 281 (face hydra-face-blue)
                   297 298 (face hydra-face-blue)
                   314 315 (face hydra-face-blue)
                   322 323 (face hydra-face-blue)))))

(ert-deftest hydra-columns-2 ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defhydra hydra-foo (:color blue)
                                "Silly hydra"
                                ("x" forward-char "forward" :column "sideways")
                                ("y" backward-char "back")
                                ("a" next-line "down" :column "vertical")
                                ("b" previous-line "up")))))))
                 #("Silly hydra:
sideways    | vertical
----------- | -----------
x: forward  | a: down
y: back     | b: up
"
                   62 63 (face hydra-face-blue)
                   76 77 (face hydra-face-blue)
                   84 85 (face hydra-face-blue)
                   98 99 (face hydra-face-blue)))))

;; checked:
;; basic rendering
;; column compatibility with ruby style and no column specified
;; column declared several time
;; nil column
(ert-deftest hydra-column-basic ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                                          :color pink
                                                          :post (deactivate-mark))
                                "
  ^_k_^         ()()
_h_   _l_       (O)(o)
  ^_j_^         (  O )
^^^^            (’’)(’’)
^^^^
"
                                ("h" backward-char nil)
                                ("l" forward-char nil)
                                ("k" previous-line nil)
                                ("j" next-line nil)
                                ("Of" 5x5 "outside of table 1")
                                ("e" exchange-point-and-mark "exchange" :column "firstcol")
                                ("n" copy-rectangle-as-kill "new-copy")
                                ("d" delete-rectangle "delete")
                                ("r" (if (region-active-p)
                                         (deactivate-mark)
                                       (rectangle-mark-mode 1)) "reset" :column "secondcol")
                                ("y" yank-rectangle "yank")
                                ("u" undo "undo")
                                ("s" string-rectangle "string")
                                ("p" kill-rectangle "paste")
                                ("o" nil "ok" :column "firstcol")
                                ("Os" 5x5-bol "outside of table 2" :column nil)
                                ("Ot" 5x5-eol "outside of table 3")))))))
                 #("  k         ()()
h   l       (O)(o)
  j         (  O )
            (’’)(’’)

firstcol    | secondcol
----------- | ------------
e: exchange | r: reset
n: new-copy | y: yank
d: delete   | u: undo
o: ok       | s: string
            | p: paste
[Of]: outside of table 1, [Os]: outside of table 2, [Ot]: outside of table 3."
                   2 3 (face hydra-face-pink)
                   17 18 (face hydra-face-pink)
                   21 22 (face hydra-face-pink)
                   38 39 (face hydra-face-pink)
                   128 129 (face hydra-face-pink)
                   142 143 (face hydra-face-pink)
                   151 152 (face hydra-face-pink)
                   165 166 (face hydra-face-pink)
                   173 174 (face hydra-face-pink)
                   187 188 (face hydra-face-pink)
                   195 196 (face hydra-face-blue)
                   209 210 (face hydra-face-pink)
                   233 234 (face hydra-face-pink)
                   243 245 (face hydra-face-pink)
                   269 271 (face hydra-face-pink)
                   295 297 (face hydra-face-pink)))))

;; check column order is the same as they appear in defhydra
(ert-deftest hydra-column-order ()
  (should (equal (eval
                  (cadr
                   (nth 2
                        (nth 5
                             (macroexpand
                              '(defhydra hydra-window-order
                                (:color red :timeout 4)
                                ("z" ace-window "ace" :color blue :column "Switch")
                                ("h" windmove-left "← window")
                                ("j" windmove-down "↓ window")
                                ("l" windmove-right "→ window")
                                ("s" split-window-below "split window" :color blue :column "Split Management")
                                ("v" split-window-right "split window vertically" :color blue)
                                ("d" delete-window "delete current window")
                                ("f" follow-mode "toggle follow mode")
                                ("u" winner-undo "undo window conf" :column "Undo/Redo")
                                ("r" winner-redo "redo window conf")
                                ("b" balance-windows "balance window height" :column "1-Sizing")
                                ("m" maximize-window "maximize current window")
                                ("k" windmove-up "↑ window" :column "Switch")
                                ("M" minimize-window "minimize current window" :column "1-Sizing")
                                ("q" nil "quit menu" :color blue :column nil)))))))
                 #("Switch      | Split Management           | Undo/Redo           | 1-Sizing
----------- | -------------------------- | ------------------- | --------------------------
z: ace      | s: split window            | u: undo window conf | b: balance window height
h: ← window | v: split window vertically | r: redo window conf | m: maximize current window
j: ↓ window | d: delete current window   |                     | M: minimize current window
l: → window | f: toggle follow mode      |                     |
k: ↑ window |                            |                     |
[q]: quit menu."
                   173 174 (face hydra-face-blue)
                   187 188 (face hydra-face-blue)
                   216 217 (face hydra-face-red)
                   238 239 (face hydra-face-red)
                   263 264 (face hydra-face-red)
                   277 278 (face hydra-face-blue)
                   306 307 (face hydra-face-red)
                   328 329 (face hydra-face-red)
                   355 356 (face hydra-face-red)
                   369 370 (face hydra-face-red)
                   420 421 (face hydra-face-red)
                   447 448 (face hydra-face-red)
                   461 462 (face hydra-face-red)
                   512 513 (face hydra-face-red)
                   578 579 (face hydra-face-blue)))))

(ert-deftest hydra-column-sexp ()
  (should (equal
           (eval (nth 5
                      (macroexpand
                       '(defhydra hydra-toggle-stuff ()
                         "Toggle"
                         ("d" toggle-debug-on-error "debug-on-error" :column "Misc")
                         ("a" abbrev-mode
                          (format "abbrev: %s"
                           (if (bound-and-true-p abbrev-mode)
                               "[x]"
                             "[ ]")))))))
           '(concat
             (format "Toggle:\n")
             "Misc"
             "\n"
             "-----------------"
             "\n"
             #("d: debug-on-error"
               0 1 (face hydra-face-red))
             "\n"
             (format
              "%1s: %-15s"
              #("a" 0 1 (face hydra-face-red))
              (format
               "abbrev: %s"
               (if (bound-and-true-p abbrev-mode)
                   "[x]"
                 "[ ]")))
             "\n"))))

(defhydra hydra-extendable ()
  "extendable"
  ("j" next-line "down"))

(ert-deftest hydra-extend ()
  (should (equal (macroexpand
                  '(defhydra+ hydra-extendable ()
                    ("k" previous-line "up")))
                 (macroexpand
                  '(defhydra hydra-extendable ()
                    "extendable"
                    ("j" next-line "down")
                    ("k" previous-line "up")))))
  (should (equal (macroexpand
                  '(defhydra+ hydra-extendable ()
                    ("k" previous-line "up" :exit t)))
                 (macroexpand
                  '(defhydra hydra-extendable ()
                    "extendable"
                    ("j" next-line "down")
                    ("k" previous-line "up" :exit t))))))

(provide 'hydra-test)

;;; hydra-test.el ends here

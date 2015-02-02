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

(ert-deftest defhydra-red-error ()
  (should
   (equal
    (macroexpand
     '(defhydra hydra-error (global-map "M-g")
       "error"
       ("h" first-error "first")
       ("j" next-error "next")
       ("k" previous-error "prev")))
    '(progn
      (defun hydra-error/first-error ()
        "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `first-error'."
        (interactive)
        (call-interactively #'first-error)
        (when hydra-is-helpful
          (message #("error: [h]: first, [j]: next, [k]: prev."
                     8 9 (face font-lock-keyword-face)
                     20 21 (face font-lock-keyword-face)
                     31 32 (face font-lock-keyword-face))))
        (setq hydra-last
              (hydra-set-transient-map
               '(keymap
                 (107 . hydra-error/previous-error)
                 (106 . hydra-error/next-error)
                 (104 . hydra-error/first-error)) t)))

      (defun hydra-error/next-error ()
        "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `next-error'."
        (interactive)
        (call-interactively #'next-error)
        (when hydra-is-helpful
          (message #("error: [h]: first, [j]: next, [k]: prev."
                     8 9 (face font-lock-keyword-face)
                     20 21 (face font-lock-keyword-face)
                     31 32 (face font-lock-keyword-face))))
        (setq hydra-last
              (hydra-set-transient-map
               '(keymap
                 (107 . hydra-error/previous-error)
                 (106 . hydra-error/next-error)
                 (104 . hydra-error/first-error)) t)))

      (defun hydra-error/previous-error ()
        "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `previous-error'."
        (interactive)
        (call-interactively #'previous-error)
        (when hydra-is-helpful
          (message #("error: [h]: first, [j]: next, [k]: prev."
                     8 9 (face font-lock-keyword-face)
                     20 21 (face font-lock-keyword-face)
                     31 32 (face font-lock-keyword-face))))
        (setq hydra-last
              (hydra-set-transient-map
               '(keymap
                 (107 . hydra-error/previous-error)
                 (106 . hydra-error/next-error)
                 (104 . hydra-error/first-error)) t)))

      (unless (keymapp (lookup-key global-map (kbd "M-g")))
        (define-key global-map (kbd "M-g") nil))
      (define-key global-map [134217831 104] #'hydra-error/first-error)
      (define-key global-map [134217831 106] #'hydra-error/next-error)
      (define-key global-map [134217831 107] #'hydra-error/previous-error)

      (defun hydra-error/body ()
        "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'."
        (interactive)
        (when hydra-is-helpful
          (message #("error: [h]: first, [j]: next, [k]: prev."
                     8 9 (face font-lock-keyword-face)
                     20 21 (face font-lock-keyword-face)
                     31 32 (face font-lock-keyword-face))))
        (setq hydra-last
              (hydra-set-transient-map
               '(keymap
                 (107 . hydra-error/previous-error)
                 (106 . hydra-error/next-error)
                 (104 . hydra-error/first-error)) t)))))))

(ert-deftest hydra-blue-toggle ()
  (should
   (equal
    (macroexpand
     '(defhydra toggle (:color blue)
       "toggle"
       ("t" toggle-truncate-lines "truncate")
       ("f" auto-fill-mode "fill")
       ("a" abbrev-mode "abbrev")
       ("q" nil "cancel")))
    '(progn
      (defun toggle/toggle-truncate-lines ()
        "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `toggle-truncate-lines'."
        (interactive)
        (hydra-disable)
        (call-interactively #'toggle-truncate-lines))
      (defun toggle/auto-fill-mode ()
        "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `auto-fill-mode'."
        (interactive)
        (hydra-disable)
        (call-interactively #'auto-fill-mode))
      (defun toggle/abbrev-mode ()
        "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `abbrev-mode'."
        (interactive)
        (hydra-disable)
        (call-interactively #'abbrev-mode))
      (defun toggle/nil ()
        "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `nil'."
        (interactive)
        (hydra-disable))
      (defun toggle/body ()
        "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'."
        (interactive)
        (when hydra-is-helpful
          (message #("toggle: [t]: truncate, [f]: fill, [a]: abbrev, [q]: cancel."
                     9 10 (face hydra-face-blue)
                     24 25 (face hydra-face-blue)
                     35 36 (face hydra-face-blue)
                     48 49 (face hydra-face-blue))))
        (setq hydra-last
              (hydra-set-transient-map
               '(keymap (113 . toggle/nil)
                 (97 . toggle/abbrev-mode)
                 (102 . toggle/auto-fill-mode)
                 (116 . toggle/toggle-truncate-lines))
               t)))))))

(provide 'hydra-test)

;;; hydra-test.el ends here

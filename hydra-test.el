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
      (defun hydra-error/first-error nil "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `first-error'."
             (interactive)
             (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                               20 21 (face hydra-face-red)
                                               31 32 (face hydra-face-red))))
             (setq hydra-last
                   (hydra-set-transient-map
                    (setq hydra-curr-map '(keymap (107 . hydra-error/previous-error)
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
                                           (21 . hydra--universal-argument)))
                    t))
             (call-interactively (function first-error)))
      (defun hydra-error/next-error nil "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `next-error'."
             (interactive)
             (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                               20 21 (face hydra-face-red)
                                               31 32 (face hydra-face-red))))
             (setq hydra-last
                   (hydra-set-transient-map
                    (setq hydra-curr-map '(keymap (107 . hydra-error/previous-error)
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
                                           (21 . hydra--universal-argument)))
                    t))
             (call-interactively (function next-error)))
      (defun hydra-error/previous-error nil "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `previous-error'."
             (interactive)
             (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                               20 21 (face hydra-face-red)
                                               31 32 (face hydra-face-red))))
             (setq hydra-last
                   (hydra-set-transient-map
                    (setq hydra-curr-map '(keymap (107 . hydra-error/previous-error)
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
                                           (21 . hydra--universal-argument)))
                    t))
             (call-interactively (function previous-error)))
      (unless (keymapp (lookup-key global-map (kbd "M-g")))
        (define-key global-map (kbd "M-g")
          nil))
      (define-key global-map [134217831 104]
       (function hydra-error/first-error))
      (define-key global-map [134217831 106]
       (function hydra-error/next-error))
      (define-key global-map [134217831 107]
       (function hydra-error/previous-error))
      (defun hydra-error/body nil "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'."
             (interactive)
             (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                               20 21 (face hydra-face-red)
                                               31 32 (face hydra-face-red))))
             (setq hydra-last
                   (hydra-set-transient-map
                    '(keymap (107 . hydra-error/previous-error)
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
                      (21 . hydra--universal-argument))
                    t)))))))

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
      (defun toggle/toggle-truncate-lines nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `toggle-truncate-lines'."
             (interactive)
             (hydra-disable)
             (call-interactively (function toggle-truncate-lines)))
      (defun toggle/auto-fill-mode nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `auto-fill-mode'."
             (interactive)
             (hydra-disable)
             (call-interactively (function auto-fill-mode)))
      (defun toggle/abbrev-mode nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `abbrev-mode'."
             (interactive)
             (hydra-disable)
             (call-interactively (function abbrev-mode)))
      (defun toggle/nil nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `nil'."
             (interactive)
             (hydra-disable))
      (defun toggle/body nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'."
             (interactive)
             (when hydra-is-helpful (message #("toggle: [t]: truncate, [f]: fill, [a]: abbrev, [q]: cancel." 9 10 (face hydra-face-blue)
                                               24 25 (face hydra-face-blue)
                                               35 36 (face hydra-face-blue)
                                               48 49 (face hydra-face-blue))))
             (setq hydra-last
                   (hydra-set-transient-map
                    '(keymap (113 . toggle/nil)
                      (97 . toggle/abbrev-mode)
                      (102 . toggle/auto-fill-mode)
                      (116 . toggle/toggle-truncate-lines)
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
                      (21 . hydra--universal-argument))
                    t)))))))

(provide 'hydra-test)

;;; hydra-test.el ends here

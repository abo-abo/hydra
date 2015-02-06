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

(ert-deftest hydra-red-error ()
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
             (hydra-disable)
             (catch (quote hydra-disable)
               (condition-case err (prog1 t (call-interactively (function first-error)))
                 ((debug error)
                  (message "%S" err)
                  (sit-for 0.8)
                  nil))
               (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                                 20 21 (face hydra-face-red)
                                                 31 32 (face hydra-face-red))))
               (setq hydra-last
                     (hydra-set-transient-map
                      (setq hydra-curr-map
                            (quote (keymap (107 . hydra-error/previous-error)
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
                      t))))
      (defun hydra-error/next-error nil "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `next-error'."
             (interactive)
             (hydra-disable)
             (catch (quote hydra-disable)
               (condition-case err (prog1 t (call-interactively (function next-error)))
                 ((debug error)
                  (message "%S" err)
                  (sit-for 0.8)
                  nil))
               (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                                 20 21 (face hydra-face-red)
                                                 31 32 (face hydra-face-red))))
               (setq hydra-last
                     (hydra-set-transient-map
                      (setq hydra-curr-map
                            (quote (keymap (107 . hydra-error/previous-error)
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
                      t))))
      (defun hydra-error/previous-error nil "Create a hydra with a \"M-g\" body and the heads:

\"h\":    `first-error',
\"j\":    `next-error',
\"k\":    `previous-error'

The body can be accessed via `hydra-error/body'.

Call the head: `previous-error'."
             (interactive)
             (hydra-disable)
             (catch (quote hydra-disable)
               (condition-case err (prog1 t (call-interactively (function previous-error)))
                 ((debug error)
                  (message "%S" err)
                  (sit-for 0.8)
                  nil))
               (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                                 20 21 (face hydra-face-red)
                                                 31 32 (face hydra-face-red))))
               (setq hydra-last
                     (hydra-set-transient-map
                      (setq hydra-curr-map
                            (quote (keymap (107 . hydra-error/previous-error)
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
                      t))))
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
             (hydra-disable)
             (catch (quote hydra-disable)
               (when hydra-is-helpful (message #("error: [h]: first, [j]: next, [k]: prev." 8 9 (face hydra-face-red)
                                                 20 21 (face hydra-face-red)
                                                 31 32 (face hydra-face-red))))
               (setq hydra-last
                     (hydra-set-transient-map
                      (setq hydra-curr-map
                            (quote (keymap (107 . hydra-error/previous-error)
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
                      t))
               (setq prefix-arg current-prefix-arg)))))))

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
             (catch (quote hydra-disable)
               (call-interactively (function toggle-truncate-lines))))
      (defun toggle/auto-fill-mode nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `auto-fill-mode'."
             (interactive)
             (hydra-disable)
             (catch (quote hydra-disable)
               (call-interactively (function auto-fill-mode))))
      (defun toggle/abbrev-mode nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `abbrev-mode'."
             (interactive)
             (hydra-disable)
             (catch (quote hydra-disable)
               (call-interactively (function abbrev-mode))))
      (defun toggle/nil nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'.

Call the head: `nil'."
             (interactive)
             (hydra-disable)
             (catch (quote hydra-disable)))
      (defun toggle/body nil "Create a hydra with no body and the heads:

\"t\":    `toggle-truncate-lines',
\"f\":    `auto-fill-mode',
\"a\":    `abbrev-mode',
\"q\":    `nil'

The body can be accessed via `toggle/body'."
             (interactive)
             (hydra-disable)
             (catch (quote hydra-disable)
               (when hydra-is-helpful (message #("toggle: [t]: truncate, [f]: fill, [a]: abbrev, [q]: cancel." 9 10 (face hydra-face-blue)
                                                 24 25 (face hydra-face-blue)
                                                 35 36 (face hydra-face-blue)
                                                 48 49 (face hydra-face-blue))))
               (setq hydra-last
                     (hydra-set-transient-map
                      (setq hydra-curr-map
                            (quote (keymap (113 . toggle/nil)
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
                                           (21 . hydra--universal-argument))))
                      t))
               (setq prefix-arg current-prefix-arg)))))))

(ert-deftest hydra-amaranth-vi ()
  (unless (version< emacs-version "24.4")
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
        (defun hydra-vi/next-line nil "Create a hydra with no body and the heads:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'.

Call the head: `next-line'."
               (interactive)
               (set-cursor-color "#e52b50")
               (hydra-disable)
               (catch (quote hydra-disable)
                 (condition-case err (prog1 t (call-interactively (function next-line)))
                   ((debug error)
                    (message "%S" err)
                    (sit-for 0.8)
                    nil))
                 (when hydra-is-helpful (message #("vi: j, k, [q]: quit." 4 5 (face hydra-face-amaranth)
                                                   7 8 (face hydra-face-amaranth)
                                                   11 12 (face hydra-face-blue))))
                 (setq hydra-last
                       (hydra-set-transient-map
                        (setq hydra-curr-map
                              (quote (keymap (7 lambda nil (interactive)
                                                (hydra-disable)
                                                (set-cursor-color "#ffffff"))
                                             (t lambda nil (interactive)
                                                (message "An amaranth Hydra can only exit through a blue head")
                                                (hydra-set-transient-map hydra-curr-map t)
                                                (when hydra-is-helpful (sit-for 0.8)
                                                      (message #("vi: j, k, [q]: quit." 4 5 (face hydra-face-amaranth)
                                                                 7 8 (face hydra-face-amaranth)
                                                                 11 12 (face hydra-face-blue)))))
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
                        t))))
        (defun hydra-vi/previous-line nil "Create a hydra with no body and the heads:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'.

Call the head: `previous-line'."
               (interactive)
               (set-cursor-color "#e52b50")
               (hydra-disable)
               (catch (quote hydra-disable)
                 (condition-case err (prog1 t (call-interactively (function previous-line)))
                   ((debug error)
                    (message "%S" err)
                    (sit-for 0.8)
                    nil))
                 (when hydra-is-helpful (message #("vi: j, k, [q]: quit." 4 5 (face hydra-face-amaranth)
                                                   7 8 (face hydra-face-amaranth)
                                                   11 12 (face hydra-face-blue))))
                 (setq hydra-last
                       (hydra-set-transient-map
                        (setq hydra-curr-map
                              (quote (keymap (7 lambda nil (interactive)
                                                (hydra-disable)
                                                (set-cursor-color "#ffffff"))
                                             (t lambda nil (interactive)
                                                (message "An amaranth Hydra can only exit through a blue head")
                                                (hydra-set-transient-map hydra-curr-map t)
                                                (when hydra-is-helpful (sit-for 0.8)
                                                      (message #("vi: j, k, [q]: quit." 4 5 (face hydra-face-amaranth)
                                                                 7 8 (face hydra-face-amaranth)
                                                                 11 12 (face hydra-face-blue)))))
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
                        t))))
        (defun hydra-vi/nil nil "Create a hydra with no body and the heads:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'.

Call the head: `nil'."
               (interactive)
               (set-cursor-color "#e52b50")
               (hydra-disable)
               (catch (quote hydra-disable)
                 (set-cursor-color "#ffffff")))
        (defun hydra-vi/body nil "Create a hydra with no body and the heads:

\"j\":    `next-line',
\"k\":    `previous-line',
\"q\":    `nil'

The body can be accessed via `hydra-vi/body'."
               (interactive)
               (set-cursor-color "#e52b50")
               (hydra-disable)
               (catch (quote hydra-disable)
                 (when hydra-is-helpful (message #("vi: j, k, [q]: quit." 4 5 (face hydra-face-amaranth)
                                                   7 8 (face hydra-face-amaranth)
                                                   11 12 (face hydra-face-blue))))
                 (setq hydra-last
                       (hydra-set-transient-map
                        (setq hydra-curr-map
                              (quote (keymap (7 lambda nil (interactive)
                                                (hydra-disable)
                                                (set-cursor-color "#ffffff"))
                                             (t lambda nil (interactive)
                                                (message "An amaranth Hydra can only exit through a blue head")
                                                (hydra-set-transient-map hydra-curr-map t)
                                                (when hydra-is-helpful (sit-for 0.8)
                                                      (message #("vi: j, k, [q]: quit." 4 5 (face hydra-face-amaranth)
                                                                 7 8 (face hydra-face-amaranth)
                                                                 11 12 (face hydra-face-blue)))))
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
                        t))
                 (setq prefix-arg current-prefix-arg))))))))

(provide 'hydra-test)

;;; hydra-test.el ends here

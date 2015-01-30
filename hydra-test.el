(require 'ert)

(ert-deftest defhydra ()
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

(provide 'hydra-test)

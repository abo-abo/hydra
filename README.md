This is a package for GNU Emacs that can be used to tie related
commands into a family of short bindings with a common prefix - a
Hydra.

Once you summon the Hydra through the prefixed binding (the body + any
one head), all heads can be called in succession with only a short
extension.

The Hydra is vanquished once Hercules, any binding that isn't the
Hydra's head, arrives.  Note that Hercules, besides vanquishing the
Hydra, will still serve his orignal purpose, calling his proper
command.  This makes the Hydra very seamless, it's like a minor mode
that disables itself auto-magically.

Here's how I use the examples bundled with Hydra:

    (require 'hydra-examples)
    (hydra-create "C-M-y" hydra-example-move-window-splitter)
    (hydra-create "M-g" hydra-example-goto-error)

You can expand the examples in-place, it still looks elegant:

    (hydra-create "<f2>"
      '(("g" text-scale-increase)
        ("l" text-scale-decrease)))

See the [introductory blog post](http://oremacs.com/2015/01/20/introducing-hydra/) for more information.

![hydra](http://oremacs.com/download/Hydra.png)

## Using Hydra to define bindings other than global ones

You can use the third optional argument of `hydra-create` for this (it defaults to `global-set-key`).
Here's an example:

```cl
(hydra-create "C-z"
  '(("l" forward-char)
    ("h" backward-char)
    ("j" next-line)
    ("k" previous-line))
  (lambda (key command)
    (define-key lispy-mode-map key command)))
```

For this simple case, there's even a shortcut: if you give a keymap as the third argument,
the lambda will be generated for you:

```cl
(hydra-create "C-z"
  '(("l" forward-char)
    ("h" backward-char)
    ("j" next-line)
    ("k" previous-line))
    lispy-mode-map)
```

## Can Hydras can be helpful?

They can, if

```cl
(setq hydra-is-helpful t)
```

In that case, you'll get a hint in the echo area consisting of current Hydra's heads.
You can even add comments to the heads like this:

```
(defvar hydra-example-text-scale
  '(("g" text-scale-increase "zoom in")
    ("l" text-scale-decrease "zoom out"))
  "A two-headed hydra for text scale manipulation.")
```

With this, you'll see `hydra: [g]: zoom in, [l]: zoom out.` in your
echo area, once the zoom Hydra becomes active.

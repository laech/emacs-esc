# esc

Provides a global minor mode `esc-mode` to make the escape key behave
like a normal escape key, in both GUI and terminal Emacs, without
interfering with commands that rely on terminal escape key sequences,
such as <kbd>M-f</kbd>, `xterm-mouse-mode`, etc.

## Installation

Clone the stable branch:

```shell
git clone -b stable https://gitlab.com/lae/emacs-esc.git
```

Add to your Emacs init file:

```elisp
(add-to-list 'load-path "<path-to-cloned-directory>")
(require 'esc)
(esc-mode) ;; Add this line to the end of your init file, see below
```

`esc-mode` will bind a special function to <kbd>ESC</kbd> in
`input-decode-map`, if you try to bind any key prefixed with
<kbd>ESC</kbd> in there after `esc-mode` is enabled, you'll see an
error `Key sequence <your-keys> starts with non-prefix key ESC`,
therefore you need to add such key bindings before enabling
`esc-mode`.

## Customization

Customizations can be done via: <kbd>M-x</kbd> `customize-group`
<kbd>return</kbd> `esc` <kbd>return</kbd>.

 * `esc-quit-function`: Name of function to call when escape is
   pressed on its own. Default is to delegate to whatever action that
   is under <kbd>C-g</kbd> at the time of invocation.

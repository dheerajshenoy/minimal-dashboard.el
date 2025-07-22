# minimal-dashboard.el

A very minimal dashboard plugin for Emacs — displays a centered image and message when Emacs starts, with optional window resize responsiveness and layout control.

![screenshot](screenshot.png)

## Features

- Minimal dashboard
- Automatic re-centering on window resize. (OPTIONAL)

## Installation

Clone this repository somewhere inside your Emacs `load-path`:

```sh
git clone https://github.com/dheerajshenoy/minimal-dashboard.el
```

```elisp
(use-package minimal-dashboard
    :load-path "<path-to-the-directory>"
    :ensure nil
    :custom
    (minimal-dashboard-image-path "~/.config/emacs/logo.svg") ;; path to image
    (minimal-dashboard-text "Welcome to Emacs") ;; plain text

    ;; You can have function returning a string as well
    ;; (minimal-dashboard-text (lambda () (format "started in %s" (emacs-init-time))))
    (minimal-dashboard-enable-resize-handling t) ;; to refresh when buffer is resized
    (minimal-dashboard-modeline-shown nil) ;; visibility of the modeline
    (initial-buffer-choice #'minimal-dashboard)) ;; shown during startup
```

## Customization

Following customizable variables are present:

- `minimal-dashboard-buffer-name` - name of the dashboard buffer.
- `minimal-dashboard-image-path` - path to the image to display (SVG, PNG, etc).
- `minimal-dashboard-text` - text displayed below the image.
- `minimal-dashboard-enable-resize-handling` -  re-center layout on window resize.
- `minimal-dashboard-modeline-shown` - visibility of the modeline

Use `M-x customize-group RET minimal-dashboard RET` to configure interactively.

## Why ?

Wanted to learn elisp and wanted a clean and minimal dashboard during startup :)

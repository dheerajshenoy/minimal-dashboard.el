# minimal-dashboard.el

A very minimal dashboard plugin for Emacs — displays a centered image and message when Emacs starts,
with optional window resize responsiveness and layout control.

![screenshot](screenshot.png)

Respects window resizing

![window-resizing](resizing.gif)

## Features

- Minimal dashboard
- Automatic re-centering on window resize. (OPTIONAL)

## Installation

1. Clone this repository somewhere inside your Emacs `load-path`:

```sh
git clone https://github.com/dheerajshenoy/minimal-dashboard.el
```

2. Use VC or download and require manually

```elisp
(use-package minimal-dashboard
    :vc (minimal-dashboard :url "https://github.com/dheerajshenoy/minimal-dashboard.el")
    ;; :load-path "<path-to-cloned-directory>" ;; uncomment this line if you have downloaded and don't want to use VC
    :init
    (setq initial-buffer-choice #'minimal-dashboard) ;; set initial buffer as dashboard
    :custom
    (minimal-dashboard-image-path "~/.config/emacs/logo.svg") ;; path to image
    (minimal-dashboard-text "Welcome to Emacs") ;; plain text

    ;; You can have function returning a string as well
    ;; (minimal-dashboard-text (lambda () (format "started in %s" (emacs-init-time))))

    ;; Multi-line text (with center alignment) is also supported
    ;; (minimal-dashboard-text "My multiline\nstring is here")

    (minimal-dashboard-enable-resize-handling t) ;; to refresh when buffer is resized
    (minimal-dashboard-modeline-shown nil)) ;; visibility of the modeline
```

> [!NOTE]
> By default, the image used is the splash screen image and the default text is "Welcome to Emacs"

## Customization

Following customizable variables are present:

- `minimal-dashboard-buffer-name` - name of the dashboard buffer.
- `minimal-dashboard-image-path` - path to the image to display (SVG, PNG, etc).
- `minimal-dashboard-text` - text displayed below the image.
- `minimal-dashboard-enable-resize-handling` -  re-center layout on window resize.
- `minimal-dashboard-modeline-shown` - visibility of the modeline

Use `M-x customize-group RET minimal-dashboard RET` to configure interactively.

## Keybindings

There's only one keybinding and that is `q` to quit the dashboard buffer.

## Why ?

Wanted to learn elisp and wanted a clean and minimal dashboard during startup :)

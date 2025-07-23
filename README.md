# minimal-dashboard.el

A very minimal dashboard plugin for Emacs — displays a centered image and message when Emacs starts,
with optional window resize responsiveness and layout control.

Version: 0.1.2

![screenshot](screenshot.png)

Respects window resizing

![window-resizing](resizing.gif)

## Features

- Minimal dashboard
- Automatic re-centering on window resize (OPTIONAL)
- Mouse click custom callback image and text (OPTIONAL)

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

    (minimal-dashboard-buffer-name "Dashboard")
    ;; (minimal-dashboard-buffer-name #'some-func-that-returns-a-string)

    (minimal-dashboard-image-path "~/.config/emacs/logo.svg") ;; path to image
    ;; (minimal-dashboard-image-path #'some-func-that-returns-a-valid-image-path)

    (minimal-dashboard-text "Welcome to Emacs") ;; plain text

    ;; You can have function returning a string as well
    ;; (minimal-dashboard-text (lambda () (format "started in %s" (emacs-init-time))))

    ;; Multi-line text (with center alignment) is also supported
    ;; (minimal-dashboard-text "My multiline\nstring is here")

    ;; Click support for image
    ;; (minimal-dashboard-image-click-handler
    ;;   (lambda (event)
    ;;     (pcase (event-basic-type event)
    ;;       ('mouse-1 (message "Left click on image"))
    ;;       ('mouse-2 (message "Middle click on image"))
    ;;       ('mouse-3 (message "Right click on image")))))

    ;; Click support for text
    ;; (minimal-dashboard-text-click-handler
    ;;   (lambda (event)
    ;;     (pcase (event-basic-type event)
    ;;       ('mouse-1 (message "Left click on text"))
    ;;       ('mouse-2 (message "Middle click on text"))
    ;;       ('mouse-3 (message "Right click on text")))))

    (minimal-dashboard-enable-resize-handling t) ;; to refresh when buffer is resized
    (minimal-dashboard-modeline-shown nil)) ;; visibility of the modeline
```

> [!NOTE]
> By default, the image used is the splash screen image and the default text is "Welcome to Emacs"

## Note

> [!NOTE]
> `minimal-dashboard-buffer-name`, `minimal-dashboard-image-path`, `minimal-dashboard-text`
> takes a string or a function that returns a string

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
## CHANGELOG

Check [CHANGELOG](./CHANGELOG.md)

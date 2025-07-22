# minimal-dashboard.el

A very minimal dashboard plugin for Emacs — displays a centered image and message when Emacs starts, with optional window resize responsiveness and layout control.

![screenshot](screenshot.png) <!-- Optional: Add screenshot.png to your repo -->

## Features

- Centered image and text display.
- Optional automatic re-centering on window resize.
- Customizable buffer name, image path, message text, and resize behavior.
- Automatically removes window-size-change hook when the buffer is killed.

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

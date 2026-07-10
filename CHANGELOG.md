# CHANGELOG

## 0.1.4

- Update the resizing gif in README
- Add note about package installation in README from MELPA
- Fix `minimal-dashboard--resize-handler`: correctly add buffer-local `window-size-change-functions` hook and clean it up via `kill-buffer-hook`; removing resize handling when disabled now works
- Fix `minimal-dashboard--refresh-buffer-name`: resolved name is now stored in an internal variable instead of overwriting the user-facing `minimal-dashboard-buffer-name`, preserving function values across calls
- Fix `minimal-dashboard-mode-hook`: now runs after the buffer is fully set up (read-only, keymaps, resize hook) rather than before
- Cache mouse keymaps for image and text click handlers; maps are built once on `defcustom` change instead of on every render/resize
- Cache image pixel size alongside the image object to avoid redundant `image-size` calls on resize
- Unify duplicated text-line insertion loop into a single `dolist`
- Remove redundant bare `defvar` declarations shadowed by `defcustom` definitions
- Remove commented-out dead code

## 0.1.3

- User option to customize the scale of the dashboard image

## 0.1.2

- Allow function/string for image-path
- Mouse button click callback on text and image

## 0.1.1

- Add customization options for changing image and text
- Propertize the text with face
- Allow function/string for text

## 0.1

- Display image and text with center alignment

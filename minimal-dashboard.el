;; minimal-dashboard.el --- A very minimal dashboard plugin for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29"))
;; Keywords: dashboard, minimal
;; URL: https://github.com/dheerajshenoy/minimal-dashboard.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a minimal Emacs dashboard implementation that displays
;; minimal amount of information like image and text, which are of
;; course customizable. The image is centered both vertically and
;; horizontally, and the layout is automatically re-centered when the
;; window is resized (this behaviour can be changed as well).

;; Features:
;; - Displays an image centered in the buffer.
;; - Optional automatic re-centering on window resize via `window-size-change-functions`.
;; - Automatically removes resize hook when the buffer is killed.
;; - Disables text scaling and Ctrl+MouseWheel zooming in the dashboard buffer to
;;   prevent layout disruption.
;; - The buffer name and resize behavior can be customized via user-defined variables
;;   or adjusted directly in the code.

;; Customization options:
;;
;; - `minimal-dashboard-buffer-name` - Customize the buffer name.
;; - `minimal-dashboard-image-path` - Path to the image (default uses a built-in GNUS SVG).
;; - `minimal-dashboard-text` - Text to be displayed beneath the image.
;; - `minimal-dashboard-enable-resize-handling` - Enable or disable resize responsiveness.
;;

;; To use:
;; (load-file "/path/to/minimal-dashboard.el")
;; (minimal-dashboard)


;;;; Group

(defgroup minimal-dashboard nil
  "Group for minimal-dashboard"
  :prefix "minimal-dashboard-"
  :group 'applications
  :version "0.1")

;;;; Keymaps

(defvar minimal-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" #'kill-current-buffer)
    map)
  "Keymap for `minimal-dashboard' buffer.")

;;;; Variables

(defcustom minimal-dashboard-buffer-name "*My Dashboard*"
  "Name of the minimal-dashboard buffer."
  :type 'string
  :group 'minimal-dashboard)

(defcustom minimal-dashboard-enable-resize-handling t
  "If non-nil, re-center information in dashboard buffer when window is resized."
  :type 'boolean
  :group 'minimal-dashboard)

(defcustom minimal-dashboard-image-path (expand-file-name "images/gnus/gnus.svg" data-directory)
  "Path to the image that is displayed in the dashboard.

Default image is the image provided by GNUS utility."
  :type 'string
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq minimal-dashboard--cached-image (create-image value))))

(defcustom minimal-dashboard-modeline-shown nil
  "Visibility of the mode-line in the dashboard buffer."
  :type 'boolean
  :group 'minimal-dashboard)

(defcustom minimal-dashboard-text "Welcome to Emacs"
  "Text displayed in the dashboard.

If it's a string, it will be inserted directly.
If it's a function, it should return a string."
  :type '(choice
          (string :tag "Static text")
          (function :tag "Function returning text"))
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq minimal-dashboard--cached-text
               (if (functionp minimal-dashboard-text)
                   (funcall minimal-dashboard-text)
              minimal-dashboard-text))))

;;;; Variables

(defvar minimal-dashboard--cached-image nil
  "Cached minimal-dashboard image.")

(defvar minimal-dashboard--cached-text nil
  "Cached minimal-dashboard text.")

;;;; Helper functions

(defun minimal-dashboard--get-cached-image ()
  "Return the cached image, or create and cache it if it doesn't exist."
  (or minimal-dashboard--cached-image
      (setq minimal-dashboard--cached-image
            (create-image minimal-dashboard-image-path))))

(defun minimal-dashboard--get-cached-text ()
  "Return the cached text, or create and cache it if it doesn't exist."
  (or minimal-dashboard--cached-text
      (setq minimal-dashboard--cached-text
            (if (functionp minimal-dashboard-text)
                (funcall minimal-dashboard-text)
              minimal-dashboard-text))))


(defun minimal-dashboard--on-resize (frame)
  "Function that is called when buffer is resized."
  (dolist (window (window-list frame))
    (when (string= (buffer-name (window-buffer window)) minimal-dashboard-buffer-name)
      (with-selected-window window
        (let ((inhibit-read-only t))
          (erase-buffer)
          (minimal-dashboard--insert-centered-info))))))

(defun minimal-dashboard--insert-centered-info ()
  "Insert a centered image and text in the dashboard buffer."
  (let* ((image (minimal-dashboard--get-cached-image))
         (image-size (when image (image-size image)))
         (img-height (or (cdr image-size) 0))
         (img-width (or (car image-size) 0))
         (text (minimal-dashboard--get-cached-text))
         (text-width (length text))
         (win-h (window-height))
         (win-w (window-width))
         ;; Vertical padding: if image exists, reserve 2 lines for text, else 0
         (vpad (max 0 (round (- win-h img-height (if text 2 0)) 2)))
         (hpad-img (max 0 (round (- win-w img-width) 2)))
         (hpad-text (max 0 (round (- win-w text-width) 2))))
    ;; Vertical padding before image/text
    (insert-char ?\n vpad)
    ;; Insert image if available
    (when image
      (insert-char ?\s hpad-img)
      (insert-image image)
      (insert "\n\n"))
    ;; Insert text if available
    (when text
      (insert-char ?\s hpad-text)
      (insert text))
    (goto-char (point-min))))

;;;; Main point of entry

;;;###autoload
(defun minimal-dashboard ()
  "Show dashboard with image, set up hooks."
  (interactive)
  (let ((buf (get-buffer-create minimal-dashboard-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (view-read-only nil)) ;; prevent view-mode activation
        (erase-buffer)
        (minimal-dashboard--insert-centered-info)
        (unless minimal-dashboard-modeline-shown
          (setq-local mode-line-format nil))
        (setq-local cursor-type nil)
        (read-only-mode 1)
        (use-local-map minimal-dashboard-mode-map)

        (when minimal-dashboard-enable-resize-handling
          ;; Add hook once
          (unless (member #'minimal-dashboard--on-resize window-size-change-functions)
            (add-hook 'window-size-change-functions #'minimal-dashboard--on-resize))
          ;; Clean up hook on buffer kill
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (remove-hook 'window-size-change-functions #'minimal-dashboard--on-resize))
                    nil t)))
      )
    (switch-to-buffer buf)
    buf))

(provide 'minimal-dashboard)

;;; minimal-dashboard.el --- A very minimal dashboard plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dheeraj Vittal Shenoy

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; URL: https://github.com/dheerajshenoy/minimal-dashboard.el
;; Version: 0.1.4
;; Package-Requires: ((emacs "27.1"))
;; Keywords: startup, screen, tools, dashboard


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

;; A very minimal dashboard plugin for Emacs — displays a centered
;; image and message when Emacs starts, with optional window resize
;; responsiveness and layout control.

;;; Code:

;;;###autoload
(defgroup minimal-dashboard nil
  "Group for `minimal-dashboard'."
  :prefix "minimal-dashboard-"
  :group 'applications
  :version "0.1.2")

;;; Faces

(defface minimal-dashboard-text-face
  '((t :inherit shadow))
  "Face used for dashboard text."
  :group 'minimal-dashboard)

;;; Keymaps

(defvar minimal-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" #'kill-current-buffer)
    map)
  "Keymap for `minimal-dashboard' buffer.")

;;; Internal state

(defvar minimal-dashboard--cached-image nil
  "Cached `minimal-dashboard' image.")

(defvar minimal-dashboard--cached-image-path nil
  "File path of the currently cached dashboard image.")

(defvar minimal-dashboard--cached-image-size nil
  "Pixel size of the cached image as (WIDTH . HEIGHT).")

(defvar minimal-dashboard--cached-text nil
  "Cached `minimal-dashboard' text.")

(defvar minimal-dashboard--resolved-buffer-name nil
  "Resolved name of the dashboard buffer.")

(defvar minimal-dashboard--image-mouse-map nil
  "Cached keymap for dashboard image mouse clicks.")

(defvar minimal-dashboard--text-mouse-map nil
  "Cached keymap for dashboard text mouse clicks.")

(defvar minimal-dashboard-mode-hook nil
  "Hook run after the dashboard buffer is fully set up.")

;;; Mouse map builder

(defun minimal-dashboard--build-mouse-map (handler)
  "Return a keymap bound to all three mouse buttons calling HANDLER, or nil."
  (when handler
    (let ((map (make-sparse-keymap)))
      (dolist (btn '(mouse-1 mouse-2 mouse-3))
        (define-key map (vector btn)
                    (lambda (event)
                      (interactive "e")
                      (funcall handler event))))
      map)))

;;; Setter helpers
;; These are called on dashboard creation and by defcustom :set functions.

(defun minimal-dashboard--refresh-cached-image ()
  "Invalidate the cached image so it is rebuilt on the next render."
  (setq minimal-dashboard--cached-image nil
        minimal-dashboard--cached-image-path nil
        minimal-dashboard--cached-image-size nil))

(defun minimal-dashboard--refresh-cached-text ()
  "Rebuild the cached dashboard text."
  (setq minimal-dashboard--cached-text
        (if (functionp minimal-dashboard-text)
            (funcall minimal-dashboard-text)
          minimal-dashboard-text)))

(defun minimal-dashboard--refresh-buffer-name ()
  "Resolve and return the dashboard buffer name, caching it in an internal var."
  (setq minimal-dashboard--resolved-buffer-name
        (if (functionp minimal-dashboard-buffer-name)
            (funcall minimal-dashboard-buffer-name)
          minimal-dashboard-buffer-name)))

(defun minimal-dashboard--resize-handler ()
  "Set up or tear down resize handling for the current buffer."
  (if minimal-dashboard-enable-resize-handling
      (progn
        (add-hook 'window-size-change-functions #'minimal-dashboard--on-resize nil t)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (remove-hook 'window-size-change-functions
                                 #'minimal-dashboard--on-resize t))
                  nil t))
    (remove-hook 'window-size-change-functions #'minimal-dashboard--on-resize t)))

;;; Custom variables

(defcustom minimal-dashboard-image-scale 1.0
  "Scale of the dashboard image."
  :type 'float
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (minimal-dashboard--refresh-cached-image)))

(defcustom minimal-dashboard-buffer-name "*My Dashboard*"
  "Name of the `minimal-dashboard' buffer.

Can be a string or a function returning a string."
  :type '(choice
          (string :tag "Buffer name")
          (function :tag "Function returning a buffer name"))
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (minimal-dashboard--refresh-buffer-name)))

(defcustom minimal-dashboard-enable-resize-handling t
  "If non-nil, re-center information in dashboard buffer when window is resized."
  :type 'boolean
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (when-let* ((name minimal-dashboard--resolved-buffer-name)
                     (buf (get-buffer name)))
           (with-current-buffer buf
             (minimal-dashboard--resize-handler)))))

(defcustom minimal-dashboard-image-path
  (expand-file-name "images/splash.svg" data-directory)
  "Path to the image displayed in the dashboard.

Can be a string (path) or a function returning a path.
If nil, no image is shown."
  :type '(choice
          (string :tag "Path to image")
          (function :tag "Function returning a path"))
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (minimal-dashboard--refresh-cached-image)))

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
         (minimal-dashboard--refresh-cached-text)))

(defcustom minimal-dashboard-image-click-handler nil
  "Function to call when the dashboard image is clicked.

Supports `mouse-1' (left), `mouse-2' (middle), and `mouse-3' (right).
The function receives the mouse event as its argument.

Example:

\(setq `minimal-dashboard-image-click-handler'
      (lambda (event)
        (pcase (event-basic-type event)
          (`mouse-1 (function1))
          (`mouse-2 (function2))
          (`mouse-3 (function3)))))"
  :type '(choice (const nil) function)
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq minimal-dashboard--image-mouse-map
               (minimal-dashboard--build-mouse-map value))))

(defcustom minimal-dashboard-text-click-handler nil
  "Function to call when the dashboard text is clicked.

Supports `mouse-1' (left), `mouse-2' (middle), and `mouse-3' (right).
The function receives the mouse event as its argument.

Example:

\(setq `minimal-dashboard-text-click-handler'
      (lambda (event)
        (pcase (event-basic-type event)
          (`mouse-1 (function1))
          (`mouse-2 (function2))
          (`mouse-3 (function3)))))"
  :type '(choice (const nil) function)
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq minimal-dashboard--text-mouse-map
               (minimal-dashboard--build-mouse-map value))))

;;; Core functions

(defun minimal-dashboard--get-cached-image ()
  "Return the cached dashboard image, rebuilding it if the path changed."
  (let ((path (if (functionp minimal-dashboard-image-path)
                  (funcall minimal-dashboard-image-path)
                minimal-dashboard-image-path)))
    (when (and path (file-exists-p path))
      (unless (and minimal-dashboard--cached-image
                   (equal path minimal-dashboard--cached-image-path))
        (let ((img (create-image path nil nil :scale minimal-dashboard-image-scale)))
          (setq minimal-dashboard--cached-image img
                minimal-dashboard--cached-image-path path
                minimal-dashboard--cached-image-size (image-size img)))))
    minimal-dashboard--cached-image))

(defun minimal-dashboard--get-cached-text ()
  "Return the cached text, computing and caching it if needed."
  (or minimal-dashboard--cached-text
      (setq minimal-dashboard--cached-text
            (if (functionp minimal-dashboard-text)
                (funcall minimal-dashboard-text)
              minimal-dashboard-text))))

(defun minimal-dashboard--on-resize (&optional _frame)
  "Redraw the dashboard when its window is resized."
  (when-let* ((name minimal-dashboard--resolved-buffer-name)
              (buf (get-buffer name))
              (win (get-buffer-window buf)))
    (when (window-live-p win)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (minimal-dashboard--insert-centered-info))))))

(defun minimal-dashboard--insert-centered-info ()
  "Insert a centered image and text in the dashboard buffer."
  (let* ((image (when (display-images-p)
                  (minimal-dashboard--get-cached-image)))
         (image-size (or minimal-dashboard--cached-image-size '(0 . 0)))
         (img-width (car image-size))
         (img-height (cdr image-size))
         (text (minimal-dashboard--get-cached-text))
         (text-lines (when (stringp text) (split-string text "\n")))
         (win-h (window-height))
         (win-w (window-width))
         (vpad (max 0 (round (- win-h img-height (if text-lines 2 0)) 2)))
         (hpad-img (max 0 (round (- win-w img-width) 2))))

    (insert-char ?\n vpad)

    (when image
      (insert-char ?\s hpad-img)
      (if minimal-dashboard--image-mouse-map
          (insert (propertize " " 'display image
                              'mouse-face 'highlight
                              'keymap minimal-dashboard--image-mouse-map))
        (insert-image image))
      (insert "\n\n"))

    (when text-lines
      (dolist (line text-lines)
        (insert-char ?\s (max 0 (round (- win-w (string-width line)) 2)))
        (let ((props (list 'face 'minimal-dashboard-text-face)))
          (when minimal-dashboard--text-mouse-map
            (setq props (append props (list 'mouse-face 'highlight
                                            'keymap minimal-dashboard--text-mouse-map))))
          (insert (apply #'propertize line props)))
        (insert "\n"))))

  (goto-char (point-min)))

;;; Entry point

;;;###autoload
(defun minimal-dashboard ()
  "Show the minimal dashboard buffer."
  (interactive)
  (let ((buf (get-buffer-create (minimal-dashboard--refresh-buffer-name))))
    (delete-other-windows)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (view-read-only nil))
        (erase-buffer)
        (minimal-dashboard--insert-centered-info)
        (unless minimal-dashboard-modeline-shown
          (setq-local mode-line-format nil))
        (setq-local cursor-type nil)
        (setq buffer-read-only t)
        (use-local-map minimal-dashboard-mode-map)
        (minimal-dashboard--resize-handler))
      (run-hooks 'minimal-dashboard-mode-hook)
      (switch-to-buffer buf)
      buf)))

(provide 'minimal-dashboard)

;;; minimal-dashboard.el ends here

;;; minimal-dashboard.el --- A very minimal dashboard plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dheeraj Vittal Shenoy

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; URL: https://github.com/dheerajshenoy/minimal-dashboard.el
;; Version: 0.1.3
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

;;; Group



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

(defvar minimal-dashboard-image-scale 1.0)
(defvar minimal-dashboard-image-path (expand-file-name "images/splash.svg" data-directory))
(defvar minimal-dashboard-dashboard-text)
(defvar minimal-dashboard--cached-image)
(defvar minimal-dashboard--cached-text)
(defvar minimal-dashboard--cached-image-path nil
  "Stores the file path of the currently cached dashboard image.")
(defvar minimal-dashboard-enable-resize-handling)
(defvar minimal-dashboard-buffer-name)
(defvar minimal-dashboard-text)
(defvar minimal-dashboard-mode-hook nil
  "Hook run after the dashboard is created.")

;;; Variable setters

; These functions serve dual purpose and avoid redundancy. These are
; called when creating the dashboard and are also used to update and
; refresh the behavior when setting a config option.

(defun minimal-dashboard--refresh-cached-image ()
  "Setter for use with `defcustom' for updating the cached image."
  (setq minimal-dashboard--cached-image
        (if (functionp minimal-dashboard-image-path)
            (when-let* ((path (funcall minimal-dashboard-image-path)))
              (create-image path nil nil :scale minimal-dashboard-image-scale))
          (create-image minimal-dashboard-image-path nil nil :scale minimal-dashboard-image-scale))))

(defun minimal-dashboard--refresh-cached-text ()
  "Setter for use with `defcustom' for updating the cached text."
  (setq minimal-dashboard--cached-text
        (if (functionp minimal-dashboard-text)
            (funcall minimal-dashboard-text)
          minimal-dashboard-text)))

(defun minimal-dashboard--resize-handler ()
  "Resizing updating handler for defcustom setter.

This is called when the custom variable
`minimal-dashboard-enable-resize-handling' changes."
  (when minimal-dashboard-enable-resize-handling
    ;; Make the hook buffer-local to this buffer only
    (make-local-variable 'window-size-change-functions)
    (add-hook 'window-size-change-functions
              #'minimal-dashboard--on-resize)
    ;; Clean up when buffer is killed
    (remove-hook 'kill-buffer-hook #'minimal-dashboard--on-resize t)))

(defun minimal-dashboard--refresh-buffer-name ()
  "Set the dashboard name."
  (setq minimal-dashboard-buffer-name
        (if (functionp minimal-dashboard-buffer-name)
            (funcall minimal-dashboard-buffer-name)
          minimal-dashboard-buffer-name)))

(defun minimal-dashboard--build-mouse-map (handler)
  "Return a keymap for mouse buttons if HANDLER is non-nil."
  (when handler
    (let ((map (make-sparse-keymap)))
      (dolist (btn '(mouse-1 mouse-2 mouse-3))
        (define-key map (vector btn)
                    `(lambda (event)
                       (interactive "e")
                       (funcall ,handler event))))
      map)))

;;; Variables

;; (defcustom minimal-dashboard-image-scale 1.0
;;   "Scale of the dashboard image."
;;   :type 'float
;;   :group 'minimal-dashboard
;;   :set (lambda (symbol value)
;;          (set-default symbol value)
;;          (minimal-dashboard--refresh-cached-image)))

(defcustom minimal-dashboard-image-scale 1.0
  "Scale of the dashboard image."
  :type 'float
  :group 'minimal-dashboard
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq minimal-dashboard--cached-image nil))) ;; invalidate cache

(defcustom minimal-dashboard-buffer-name "*My Dashboard*"
  "Name of the `minimal-dashboard' buffer."
  :type '(choice
          (string :tag "Path to image")
          (function :tag "Function returning a path to an image"))
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
         (minimal-dashboard--resize-handler)))

;; (defcustom minimal-dashboard-image-path (expand-file-name "images/splash.svg" data-directory)
;;   "Path to the image that is displayed in the dashboard.
;;
;; By default we use the splash Emacs image. If nil, no image is displayed."
;;   :type '(choice
;;           (string :tag "Path to image")
;;           (function :tag "Function returning a path to an image"))
;;   :group 'minimal-dashboard
;;   :set (lambda (symbol value)
;;          (set-default symbol value)
;;          (minimal-dashboard--refresh-cached-image)))

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
         (setq minimal-dashboard--cached-image nil))) ;; invalidate cache


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

Supports `mouse-1` (left button), `mouse-2` (middle button) and
`mouse-3` (right button). The function is called with the event
argument.

Example usage:

\(setq `minimal-dashboard-image-click-handler'
      (lambda (event)
        (pcase (event-basic-type event)
          (`mouse-1 function1)
          (`mouse-2 function2)
          (`mouse-3 function3))))"
  :type 'function
  :group 'minimal-dashboard)

(defcustom minimal-dashboard-text-click-handler nil
  "Function to call when the dashboard image is clicked.

Supports `mouse-1` (left button), `mouse-2` (middle button) and
`mouse-3` (right button). The function is called with the event
argument.

Example usage:

\(setq `minimal-dashboard-text-click-handler'
      (lambda (event)
        (pcase (event-basic-type event)
          (`mouse-1 function1)
          (`mouse-2 function2)
          (`mouse-3 function3))))"
  :type 'function
  :group 'minimal-dashboard)

;;; Variables

(defvar minimal-dashboard--cached-image nil
  "Cached `minimal-dashboard' image.")

(defvar minimal-dashboard--cached-text nil
  "Cached `minimal-dashboard' text.")

;;; Helper functions

;; (defun minimal-dashboard--get-cached-image ()
;;   "Return the cached image, or create and cache it if it doesn't exist."
;;   (or minimal-dashboard--cached-image
;;       (when (and minimal-dashboard-image-path
;;                  (stringp minimal-dashboard-image-path))
;;         (setq minimal-dashboard--cached-image
;;               (create-image minimal-dashboard-image-path nil nil :scale minimal-dashboard-image-scale)))))

(defun minimal-dashboard--get-cached-image ()
  "Return the cached dashboard image, refreshing it if needed."
  (let ((path (if (functionp minimal-dashboard-image-path)
                  (funcall minimal-dashboard-image-path)
                minimal-dashboard-image-path)))
    (when (and path (file-exists-p path))
      (unless (and minimal-dashboard--cached-image
                   (equal path minimal-dashboard--cached-image-path))
        (setq minimal-dashboard--cached-image
              (create-image path nil nil :scale minimal-dashboard-image-scale)
              minimal-dashboard--cached-image-path path)))
    minimal-dashboard--cached-image))

(defun minimal-dashboard--get-cached-text ()
  "Return the cached text, or create and cache it if it doesn't exist."
  (or minimal-dashboard--cached-text
      (setq minimal-dashboard--cached-text
            (if (functionp minimal-dashboard-text)
                (funcall minimal-dashboard-text)
              minimal-dashboard-text))))

(defun minimal-dashboard--on-resize (&optional _frame)
  "Called when window showing dashboard buffer is resized.
FRAME is optional and provided by `window-size-change-functions'."
  (when-let* ((buf (get-buffer minimal-dashboard-buffer-name))
              (win (get-buffer-window buf)))
    (when (window-live-p win)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (minimal-dashboard--insert-centered-info))))))

(defun minimal-dashboard--insert-centered-info ()
  "Insert a centered image and text in the dashboard buffer efficiently."
  (let* ((image (when (display-images-p)
                  (minimal-dashboard--get-cached-image)))
         (image-size (if image (image-size image) '(0 . 0)))
         (img-width (car image-size))
         (img-height (cdr image-size))
         (text (minimal-dashboard--get-cached-text))
         (text-lines (when (stringp text) (split-string text "\n")))
         (win-h (window-height))
         (win-w (window-width)))

    (let* ((vpad (max 0 (round (- win-h img-height (if text-lines 2 0)) 2)))
           (hpad-img (max 0 (round (- win-w img-width) 2))))

      ;; Vertical padding before image/text
      (insert-char ?\n vpad)

      ;; Insert image if available
      (when image
        (insert-char ?\s hpad-img)
        (if minimal-dashboard-image-click-handler
            (insert (propertize " " 'display image
                                'mouse-face 'highlight
                                'keymap
                                (minimal-dashboard--build-mouse-map
                                 minimal-dashboard-image-click-handler)))
          (insert-image image))
        (insert "\n\n"))

      ;; Insert text lines if available
      (when text-lines
        (if minimal-dashboard-text-click-handler
            (dolist (line text-lines)
              (insert-char ?\s (max 0 (round (- win-w (string-width line)) 2)))
              (insert (propertize " " 'display line
                                  'face 'minimal-dashboard-text
                                  'mouse-face 'highlight
                                  'keymap
                                  (minimal-dashboard--build-mouse-map
                                   minimal-dashboard-text-click-handler)))
              (insert "\n"))
          (dolist (line text-lines)
            (insert-char ?\s (max 0 (round (- win-w (string-width line)) 2)))
            (insert (propertize line 'face 'minimal-dashboard-text-face))
            (insert "\n"))))))
  (goto-char (point-min)))

;;; Main point of entry

;;;###autoload
(defun minimal-dashboard ()
  "Show dashboard with image, set up hooks."
  (interactive)
  (let ((buf (get-buffer-create (minimal-dashboard--refresh-buffer-name))))
    (delete-other-windows)
    (with-current-buffer buf
      (run-hooks 'minimal-dashboard-mode-hook)
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
    (switch-to-buffer buf)
    buf)))

(provide 'minimal-dashboard)

;;; minimal-dashboard.el ends here

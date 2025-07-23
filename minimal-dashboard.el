;;; minimal-dashboard.el --- A very minimal dashboard plugin for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Maintainer: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Version: 0.1.2
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


;;;; Group

;;;###autoload
(defgroup minimal-dashboard nil
  "Group for minimal-dashboard"
  :prefix "minimal-dashboard-"
  :group 'applications
  :version "0.1")

;;;; Faces

(defface minimal-dashboard-text-face
  '((t :inherit shadow))
  "Face used for dashboard text."
  :group 'minimal-dashboard)

;;;; Keymaps

(defvar minimal-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "q" #'kill-current-buffer)
    map)
  "Keymap for `minimal-dashboard' buffer.")

;;;; Variable setters

;;; These functions serve dual purpose and avoid redundancy. These are
;;; called when creating the dashboard and are also used to update and
;;; refresh the behavior when setting a config option.

(defun minimal-dashboard--refresh-cached-image ()
  "Setter for use with `defcustom' for updating the cached image."
  (setq minimal-dashboard--cached-image
        (if (functionp minimal-dashboard-image-path)
            (let ((path (funcall minimal-dashboard-image-path)))
              (when path
                (create-image path)))
          (create-image minimal-dashboard-image-path))))

(defun minimal-dashboard--refresh-cached-text ()
  "Setter for use with `defcustom' for updating the cached text."
  (setq minimal-dashboard--cached-text
        (if (functionp minimal-dashboard-text)
            (funcall minimal-dashboard-text)
          minimal-dashboard-text)))

(defun minimal-dashboard--resize-handler ()
  "Resizing updating handler."
  (if minimal-dashboard-enable-resize-handling

      ;; Enable
      (unless (member #'minimal-dashboard--on-resize window-size-change-functions)
        (add-hook 'window-size-change-functions #'minimal-dashboard--on-resize)

        ;; Disable ok kill
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (remove-hook 'window-size-change-functions #'minimal-dashboard--on-resize))
                  nil t))

    ;; Disable
    (remove-hook 'window-size-change-functions #'minimal-dashboard--on-resize)))

(defun minimal-dashboard--refresh-buffer-name ()
  "Set the dashboard name."
  (setq minimal-dashboard-buffer-name
        (if (functionp minimal-dashboard-buffer-name)
            (funcall minimal-dashboard-buffer-name)
          minimal-dashboard-buffer-name)))

(defun minimal-dashboard--build-mouse-map (handler)
  "Return a keymap for mouse-1/2/3 if HANDLER is non-nil."
  (when handler
    (let ((map (make-sparse-keymap)))
      (dolist (btn '(mouse-1 mouse-2 mouse-3))
        (define-key map (vector btn)
                    `(lambda (event)
                       (interactive "e")
                       (funcall ,handler event))))
      map)))

;;;; Variables

(defcustom minimal-dashboard-buffer-name "*My Dashboard*"
  "Name of the minimal-dashboard buffer."
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

(defcustom minimal-dashboard-image-path (expand-file-name "images/splash.svg" data-directory)
  "Path to the image that is displayed in the dashboard.

By default we use the splash emacs image. If nil, no image is displayed."
  :type '(choice
          (string :tag "Path to image")
          (function :tag "Function returning a path to an image"))
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

Supports mouse button 1 (left button), 2 (right button) and 3 (middle
button). The function is called with the event argument.

Example:
(setq minimal-dashboard-image-click-handler
      (lambda (event)
        (pcase (event-basic-type event)
          ('mouse-1 function1)
          ('mouse-2 function2)
          ('mouse-3 function3))))"
  :type 'function
  :group 'minimal-dashboard)

(defcustom minimal-dashboard-text-click-handler nil
  "Function to call when the dashboard image is clicked.

Supports mouse button 1 (left button), 2 (right button) and 3 (middle
button). The function is called with the event argument.

Example:
(setq minimal-dashboard-text-click-handler
      (lambda (event)
        (pcase (event-basic-type event)
          ('mouse-1 (message \"Left click on text\"))
          ('mouse-2 (message \"Middle click on text\"))
          ('mouse-3 (message \"Right click on text\")))))."
  :type 'function
  :group 'minimal-dashboard)

;;;; Variables

(defvar minimal-dashboard--cached-image nil
  "Cached minimal-dashboard image.")

(defvar minimal-dashboard--cached-text nil
  "Cached minimal-dashboard text.")

;;;; Helper functions

(defun minimal-dashboard--get-cached-image ()
  "Return the cached image, or create and cache it if it doesn't exist."
  (or minimal-dashboard--cached-image
      (when (and minimal-dashboard-image-path
                 (stringp minimal-dashboard-image-path))
        (setq minimal-dashboard--cached-image
              (create-image minimal-dashboard-image-path)))))

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
    (let ((buf (window-buffer window)))
      (when (buffer-local-value 'minimal-dashboard--buffer-p buf)
        (with-selected-window window
          (let ((inhibit-read-only t))
            (erase-buffer)
            (minimal-dashboard--insert-centered-info)))))))

(defun minimal-dashboard--insert-centered-info ()
  "Insert a centered image and text in the dashboard buffer efficiently."
  (let* ((image (minimal-dashboard--get-cached-image))
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

;;;; Main point of entry

;;;###autoload
(defun minimal-dashboard ()
  "Show dashboard with image, set up hooks."
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
        (setq-local cursor-type nil
                    minimal-dashboard--buffer-p t)
        (setq buffer-read-only t)
        (use-local-map minimal-dashboard-mode-map)
        (minimal-dashboard--resize-handler)
      )
    (switch-to-buffer buf)
    buf)))

(provide 'minimal-dashboard)

;; minimal-dashboard.el ends here

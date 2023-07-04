;;; bufferbin.el --- Quick mouse access to buffers    -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ryan Walsh

;; Author: Ryan Walsh <blueridge-data@github>
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/blueridge-data/bufferbin
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A small tool for quick mouse access to buffers.  Bufferbin is unique
;; from other tools in two respects: 1) it utilizes a sidebar window for
;; deterministic behavior; and 2) it allows the user to specify the
;; window to utilize when opening the buffer.

;;; Code:

(defconst bufferbin-default-ignore '("^ "))
(defconst bufferbin-default-filter '())
(defconst bufferbin-default-fonts '())

;; ====================
;; User Customization
;; ====================

(defgroup bufferbin nil
  "Bufferbin group configuration options."
  :group 'bufferbin
  :prefix "bufferbin-")

(defcustom bufferbin-window-width 30
  "The width of the bufferbin window."
  :type 'integer
  :group 'bufferbin)

(defcustom bufferbin-direction 'left
  "The side of the screen to display pop-up window."
  :type '(choice (const left)
         (const right))
  :group 'bufferbin)

(defcustom bufferbin-buffer-name "*Buffer-Bin*"
  "Name the buffer."
  :type 'string
  :group 'bufferbin)

(defcustom bufferbin-header t
  "Include a header in the window."
  :type 'boolean
  :group 'bufferbin)

(defcustom bufferbin-header-label "BUFFER BIN"
  "Window header label."
  :type 'string
  :group 'bufferbin)

(defcustom bufferbin-modeline-label "Buffer Bin"
  "Include a header in the window."
  :type 'string
  :group 'bufferbin)

(defcustom bufferbin-ignore-regex bufferbin-default-ignore
  "List of regex's to ignore in buffer list."
  :type '(repeat regexp)
  :group 'bufferbin)

(defcustom bufferbin-filter-regex bufferbin-default-filter
  "List of regex's to filter and display."
  :type '(repeat regexp)
  :group 'bufferbin)

(defcustom bufferbin-mode-fonts bufferbin-default-fonts
  "Association list to map major modes to fonts."
  :type '(repeat alist)
  :group 'bufferbin)

;; ====================
;; Major Mode
;; ====================

(defvar bufferbin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "k") 'bufferbin-close)
    (define-key map (kbd "f") 'bufferbin-add-filter)
    (define-key map (kbd "r") 'bufferbin-reset-filter)
    (define-key map (kbd "RET") 'bufferbin-refresh-list)
    (define-key map [down-mouse-1] 'bufferbin-downmouse-action)
    (define-key map [mouse-1] 'bufferbin-leftclick-action)
    (define-key map [drag-mouse-1] 'bufferbin-dragleftclick-action)
    map))

;;;###autoload
(define-derived-mode bufferbin-mode special-mode "Bufferbin"
  "Establish constraints for bufferbin major mode."
  (setq buffer-read-only         t
        truncate-lines           t
        indent-tabs-mode         nil
        desktop-save-buffer      nil
        window-size-fixed        nil))

;; ====================
;; Buffer Actions
;; ====================

(defun bufferbin-get-buffer ()
  "Parse name of buffer located at 'thing-at-point'."
  (interactive)
  (replace-regexp-in-string "\n" "" (thing-at-point 'line)))

(defun bufferbin-get-window ()
  "Get the window in which to display buffer."
  (interactive)
  (let ((new-event (read-event "Click window to open buffer")))
    (while (not (eq 'mouse-1 (elt new-event 0)))
      (setq new-event (read-event "Click window to open buffer")))
    (car (car (cdr new-event)))))

(defun bufferbin-visit-buffer (selected-buf selected-win)
  "Switch to SELECTED-WIN and and display SELECTED-BUF."
  (when (window-live-p selected-win)
    (select-window selected-win)
    (switch-to-buffer selected-buf)))

(defun bufferbin-downmouse-action (event)
  "Lock down 'point' when EVENT is 'clicked' to remain static during a mouse drag."
  (interactive "e")
  (when (eq 'down-mouse-1 (elt event 0))
    (select-window (car (cadr event)))
    (goto-char (posn-point (cadr event)))))

(defun bufferbin-leftclick-action (event)
  "Get buffer at point when EVENT is 'left-click' and display in a selected window."
  (interactive "e")
  (when (eq 'mouse-1 (elt event 0))
	(let ((selected-position (elt (car (cdr event)) 1)))
	  (if (< selected-position (point-max))
		  (let ((selected-buf (bufferbin-get-buffer))
				(selected-win (bufferbin-get-window)))
			(bufferbin-visit-buffer selected-buf selected-win))))))

(defun bufferbin-dragleftclick-action (event)
  "Display buffer at point in a selected window from EVENT."
  (interactive "e")
  (when (eq 'drag-mouse-1 (elt event 0))
	(let* ((info1 (elt (cdr event) 0))
		   (info2 (elt (cdr event) 1))
		   ;; (source-window (elt info1 0))
		   (target-window (elt info2 0))
		   (source-pos (elt info1 1)))
		   ;; (target-pos (elt info2 1)))
      (if (< source-pos (point-max))
          (bufferbin-visit-buffer (bufferbin-get-buffer) target-window)))))

;; ====================
;; Filtering (regex)
;; ====================

(defun bufferbin-search-regex (regex buf)
  "Return t if REGEX matches BUF, otherwise nil."
  (cond ((eq regex '()) nil)
        ((string-match (car regex) buf) t)
        (t (bufferbin-search-regex (cdr regex) buf))))

(defun bufferbin-ignored-p (buf)
  "Determine if BUF has been ignored."
  (bufferbin-search-regex bufferbin-ignore-regex buf))

(defun bufferbin-filtered-p (buf)
  "Return t if BUF is within filtered regex."
  (if bufferbin-filter-regex
      (bufferbin-search-regex bufferbin-filter-regex buf)
    t))

(defun bufferbin-add-filter (regex)
  "Add REGEX to filter list."
  (interactive "sFilter by regex: \n")
  (add-to-list 'bufferbin-filter-regex regex)
  (bufferbin-refresh-list))

(defun bufferbin-reset-filter ()
  "Reset regex filter to default list."
  (interactive)
  (setq bufferbin-filter-regex bufferbin-default-filter)
  (bufferbin-refresh-list))

;; ====================
;; Display Buffer List
;; ====================

(defun bufferbin-mode-font-p (buf)
  "Check if BUF is a key in the 'bufferbin-mode-fonts' alist."
  (assoc (buffer-local-value 'major-mode buf) bufferbin-mode-fonts))

(defun bufferbin-get-mode-font (buf)
  "Get value associated with key BUF from the 'bufferbin-mode-fonts' alist."
  (cdr (assoc (buffer-local-value 'major-mode buf) bufferbin-mode-fonts)))

(defun bufferbin-writeln (text &optional format)
  "Print TEXT using optional FORMAT, otherwise use 'default'."
  (let ((custom (or format 'default)))
    (insert (propertize text 'face custom) "\n")))

(defun bufferbin-get-fontface (buf)
  "Identify and return appropriate fontface for BUF:
1. GREY out if BUF is not associated with a file (begin with '*')
2. Apply RED if BUF is modified
3. Apply user defined fontface if BUF major mode matches the alist
4. Otherwise, the default fontface is applied."
  (cond ((string-match "^*" buf) 'font-lock-comment-face)
        ((buffer-modified-p (get-buffer buf)) 'font-lock-warning-face)
        ((bufferbin-mode-font-p (get-buffer buf))
         (bufferbin-get-mode-font (get-buffer buf)))
        (t 'default)))

(defun bufferbin-print-list()
  "Print list of filtered, non-ignored buffers, sorted by name."
 (with-current-buffer bufferbin-buffer-name
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (item (sort (mapcar #'buffer-name (buffer-list)) 'string<))
      (if (and (not (bufferbin-ignored-p item))
               (bufferbin-filtered-p item))
          (bufferbin-writeln item (bufferbin-get-fontface item))))
    (setq buffer-read-only t)))

(defun bufferbin-refresh-list ()
  "Refresh list of current buffers."
  (interactive)
  (if (not (eq 'none (bufferbin-current-visibility)))
        (bufferbin-print-list)
    (message "Enable Bufferbin before refreshing list")))

(defun bufferbin-delayed-refresh ()
  "Allow 'buffer-modified-p' to update after 'first-change-hook' is called."
  (interactive)
  (run-at-time "1 sec" nil #'bufferbin-refresh-list))
       
;; ====================
;; Window Management
;; ====================

(defun bufferbin-current-visibility ()
  "Return state of bufferbin visibility."
  (cond ((get-buffer-window bufferbin-buffer-name) 'visible)
        ((get-buffer bufferbin-buffer-name) 'exists)
        (t 'none)))

(defun bufferbin-show ()
  "Show bufferbin in current frame."
  (display-buffer bufferbin-buffer-name
                  `(display-buffer-in-side-window
                    . ((side . ,bufferbin-direction)
                       (window-width . ,bufferbin-window-width)
                       (dedicated . t))))
  (bufferbin-refresh-list))

(defun bufferbin-hide ()
  "Hide bufferbin from current frame."
  (delete-window (get-buffer-window bufferbin-buffer-name)))

;; ====================
;; Core
;; ====================

(declare-function bufferbin-mode "bufferbin-mode")

(defun bufferbin-add-hooks ()
  "Add hooks to update buffer list appropriately."
  (add-hook 'buffer-list-update-hook #'bufferbin-refresh-list)
  (add-hook 'after-save-hook #'bufferbin-refresh-list)
  (add-hook 'first-change-hook #'bufferbin-delayed-refresh))

(defun bufferbin-remove-hooks ()
  "Remove hooks when bufferbin is closed."
  (remove-hook 'buffer-list-update-hook #'bufferbin-refresh-list)
  (remove-hook 'after-save-hook #'bufferbin-refresh-list)
  (remove-hook 'first-change-hook #'bufferbin-delayed-refresh))

(defun bufferbin-init ()
  "Initialize buffer bin upon first start."
  (with-current-buffer (get-buffer-create bufferbin-buffer-name)
    (bufferbin-mode)
    (when bufferbin-header
      (setq header-line-format bufferbin-header-label))
    (setq mode-line-format (format bufferbin-modeline-label)))
  (bufferbin-show)
  (bufferbin-add-hooks))

(defun bufferbin-close ()
  "Kill buffer and remove hooks."
  (interactive)
  (bufferbin-remove-hooks)
  (kill-buffer bufferbin-buffer-name))

;;;###autoload
(defun bufferbin ()
  "Main program."
  (interactive)
  (pcase (bufferbin-current-visibility)
    ('visible (bufferbin-hide))
    ('exists  (bufferbin-show))
    ('none    (bufferbin-init))))

(provide 'bufferbin)

;;; bufferbin.el ends here

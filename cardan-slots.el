;;; cardan-slots.el --- Define cardan commands   -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Atraveller

;; Author: Atraveller <atraveller@protonmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Commentary:

;; Define slots view.

;;; Code:

(require 'cardan-utils)
(require 'cardan-slot)

(defvar cardan-slots-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'cardan-do-load-more-slots)
    (define-key map (kbd "q") 'kill-current-buffer)
    map)
  "Keymap for `cardan-slots-mode'.")

(defvar cardan-slot-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'cardan-view-slot-from-point)
    map)
  "Keymap for `cardan-slot-row'.")

(defun cardan-do-load-more-slots ()
  "Click on load more button programmatically."
  (interactive)
  (push-button (- (point-max) 1)))

(define-derived-mode cardan-slots-mode fundamental-mode "cardan-slots"
  "Major mode for slots.")

(defun cardan-view-slot-from-point ()
  "View slot by reading text-properties at point."
  (interactive)
  (let* ((pos (point))
         (block-hash (get-text-property pos 'block-hash)))
    (cardan-view-slot block-hash)))

(defun cardan-format-slot (slot-data)
  "Format hash-table SLOT-DATA."
  (let* ((epoch-num (cardan-get-epoch-num slot-data))
         (slot-num (cardan-get-slot-num slot-data))
         (time-issued (gethash "cbeTimeIssued" slot-data))
         (tx-num (gethash "cbeTxNum" slot-data))
         (total-sent (string-to-number (cardan-get-in-hash '("cbeTotalSent" "getCoin") slot-data)))
         (slot-leader (substring (gethash "cbeBlockLead" slot-data) 0 8))
         (bytes (gethash "cbeSize" slot-data))
         (column-gap "  "))
    (concat
      (propertize
        (format "[%d, %d]" epoch-num slot-num)
        'face 'cardan-epoch-slot-face)
      column-gap
      (propertize
        (format "<%s>" (cardan-format-unix-timestamp time-issued))
        'face 'cardan-time-issued-face)
      column-gap
      (propertize
        (format "%5sTx" tx-num)
        'face 'cardan-tx-num-face)
      column-gap
      (propertize
        (format "%20s" (cardan-format-ada (cardan-lovelace-to-ada total-sent)))
        'face 'cardan-total-sent-face)
      column-gap
      (propertize
        slot-leader
        'face 'cardan-slot-leader-face)
      column-gap
      (propertize
        (format "%8sB" bytes)
        'face 'cardan-bytes-face))))

(defun cardan-load-more-slots (button page-num)
  "Load slots in the given PAGE-NUM when load previous BUTTON is clicked."
  (cardan-list-block-pages
    page-num
    (lambda (response)
      (let* ((slots (car (cdr (gethash "Right" response))))
             (buffer-name "*Slots*"))
        (with-current-buffer buffer-name
          (read-only-mode 0)
          (goto-char (button-start button))
          (dolist (slot-data slots)
            (insert (cardan-slot-row slot-data))
            (insert "\n"))
          (button-put button
            'action (lambda (_button)
                      (funcall 'cardan-load-more-slots _button (- page-num 1))))
          (read-only-mode 1))))))

(defun cardan-slot-row (slot-data)
  "Generate slot row from SLOT-DATA."
  (propertize (cardan-format-slot slot-data)
    'keymap cardan-slot-row-map
    'epoch-num (cardan-get-epoch-num slot-data)
    'slot-num (cardan-get-slot-num slot-data)
    'block-hash (cardan-get-block-hash slot-data)))

(defun cardan-view-slots ()
  "View slots."
  (cardan-list-block-pages
    nil
    (lambda (response)
      (let* ((current-page-num (car (gethash "Right" response)))
             (slots (car (cdr (gethash "Right" response))))
             (buffer-name "*Slots*")
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (erase-buffer)
          (cardan-slots-mode)
          (dolist (slot-data slots)
            (insert (cardan-slot-row slot-data))
            (insert "\n"))
          (insert-text-button
            (substitute-command-keys
              (format "Type \\<%s>\\[%s] to previous slots"
                'cardan-slots-mode-map
                'cardan-do-load-more-slots))
            'action (lambda (_button)
                      (funcall 'cardan-load-more-slots _button (- current-page-num 1)))
            'follow-link t)
          (read-only-mode 1))))))

;;; _
(provide 'cardan-slots)
;; End:
;;; cardan-slots.el ends here

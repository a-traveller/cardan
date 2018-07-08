;;; cardan-utils.el --- Define cardan utility functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Atraveller

;; Author: Atraveller <atraveller@protonmail.com>
;; Homepage: https://gitlab.com/a-traveller/cardan

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

;; Define cardan utility functions.

;;; Code:

(require 'json)

(defvar CARDAN-EXPLORER-BASE-URL "https://cardanoexplorer.com")
(defvar CARDAN-SENSITIVE-CONFIG-FILE-PATH "~/Downloads/dd.json.gpg")

(defgroup cardan nil
  "Your cardan."
  :group 'tools)

(defgroup cardan-faces nil
  "Faces used by Magit."
  :group 'cardan
  :group 'faces)

(defface cardan-epoch-slot-face
  '((((class color) (background light)) :foreground "#ce851f")
    (((class color) (background  dark)) :foreground "#ce851f"))
  "Face for epoch and slot pair."
  :group 'cardan-faces)

(defface cardan-time-issued-face
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for time issued."
  :group 'cardan-faces)

(defface cardan-tx-num-face
  nil
  "Face for transaction number."
  :group 'cardan-faces)

(defface cardan-total-sent-face
  nil
  "Face for total sent."
  :group 'cardan-faces)

(defface cardan-fee-face
  nil
  "Face for fee."
  :group 'cardan-faces)

(defface cardan-hash-face
  nil
  "Face for block hash."
  :group 'cardan-faces)

(defface cardan-address-hash-face
  '((((class color) (background light)) :foreground "#ab85a3" :height 0.8)
    (((class color) (background  dark)) :foreground "#ab85a3" :height 0.8))
  "Face for address hash."
  :group 'cardan-faces)

(defface cardan-address-hash-highlight-face
  '((((class color)) :box "#eee"))
  "Face for highlighted address hash."
  :group 'cardan-faces)

(defface cardan-slot-leader-face
  '((((class color) (background light)) :foreground "#ab85a3")
    (((class color) (background  dark)) :foreground "#ab85a3"))
  "Face for slot leader hash."
  :group 'cardan-faces)

(defface cardan-bytes-face
  nil
  "Face for payload size in bytes."
  :group 'cardan-faces)

(defun format-unix-timestamp (timestamp)
  "Format unix timestamp to friendly display from TIMESTAMP."
  (concat
    (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time timestamp))
    " "
    (car (cdr (current-time-zone)))))

(defun read-sensitive-config ()
  "Read sensitive config."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file CARDAN-SENSITIVE-CONFIG-FILE-PATH)))


(defun parse-response-to-hash-table (json-string)
  "Parse JSON-STRING to hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string json-string)))

(defun get-json-response (url callback)
  "Get response as json from URL and CALLBACK."
  (let ((json-string nil))
    (url-retrieve
      url
      (lambda (_status)
        (with-current-buffer (current-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (setq json-string (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer (current-buffer)))
        (funcall callback (parse-response-to-hash-table json-string))))))

(defun get-in-hash (path-list hash-table)
  "Get value in HASH-TABLE with PATH-LIST."
  (if path-list
    (get-in-hash
      (cdr path-list)
      (gethash (car path-list) hash-table))
    hash-table))

(defun list-block-pages (page-num callback)
  "List blocks using optional PAGE-NUM and CALLBACK."
  (let
    ((url (concat
            CARDAN-EXPLORER-BASE-URL
            "/api/blocks/pages"
            (if (eq page-num nil) "" (concat "?page=" (number-to-string page-num))))))
    (get-json-response url callback)))

(defun read-block-summary (block-hash callback)
  "Read block summary from BLOCK-HASH and CALLBACK."
  (let
    ((url (concat
            CARDAN-EXPLORER-BASE-URL
            "/api/blocks/summary/"
            block-hash)))
    (get-json-response url callback)))

(defun list-block-transactions (block-hash callback)
  "List block transactions from BLOCK-HASH and CALLBACK."
  (let
    ((url (concat
            CARDAN-EXPLORER-BASE-URL
            "/api/blocks/txs/"
            block-hash
            "?limit=5000")))
    (get-json-response url callback)))

(defun read-address (address-hash callback)
  "Read address summary from ADDRESS-HASH and CALLBACK."
  (let
    ((url (concat
            CARDAN-EXPLORER-BASE-URL
            "/api/addresses/summary/"
            address-hash)))
    (get-json-response url callback)))

(defun group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
         (char (or char ","))
         (str (if (stringp num) num (number-to-string num)))
         ;; omitting any trailing non-digit chars
         ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
         (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
                  char
                  (substring str (- pt size)))
        pt (- pt size)))
    str))

(defun mask-decimal (string length character)
  "Mask a number STRING with CHARACTER to LENGTH."
  (let* ((decimal-length (length (car (cdr (split-string string "\\.")))))
          (mask-length (if (> length decimal-length) (- length decimal-length) 0)))
    (concat string (make-string mask-length character))))

(defun format-ada (amount)
  "Format ada AMOUNT."
  (mask-decimal (group-number amount) 6 ?0))

(defun lovelace-to-ada (lovelace)
  "Convert LOVELACE to ADA."
  (/ (float lovelace) 1000000))

(lovelace-to-ada 30000000000)

(defun get-epoch-num (slot-data)
  "Get epoch number from SLOT-DATA."
  (gethash "cbeEpoch" slot-data))

(defun get-slot-num (slot-data)
  "Get slot number from SLOT-DATA."
  (gethash "cbeSlot" slot-data))

(defun get-block-hash (slot-data)
  "Get block hash from SLOT-DATA."
  (gethash "cbeBlkHash" slot-data))

(defun get-total-sent (slot-data)
  "Get total sent coins from SLOT-DATA."
  (string-to-number (get-in-hash '("cbeTotalSent" "getCoin") slot-data)))

(defun get-fee (slot-data)
  "Get fee from SLOT-DATA."
  (string-to-number (get-in-hash '("cbeFees" "getCoin") slot-data)))

(defun format-transaction (transaction-data)
  "Format a transaction data instance from blocks/txs api TRANSACTION-DATA."
  (let ((transaction-id (gethash "ctbId" transaction-data))
        (time-issued (gethash "ctbTimeIssued" transaction-data))
        (inputs (gethash "ctbInputs" transaction-data))
        (outputs (gethash "ctbOutputs" transaction-data))
        (output-sum (string-to-number (get-in-hash '("ctbOutputSum" "getCoin") transaction-data))))
    (concat
      (propertize
        (format "<%s>" (format-unix-timestamp time-issued))
        'face 'cardan-time-issued-face)
      " "
      transaction-id
      "\n"
      (propertize
        (format "%s" (car (car inputs)))
        'face 'cardan-address-hash-face)
      "\n"
      (mapconcat 'format-output outputs "\n")
      "\n"
      (make-string 20 ?-)
      "\n"
      (format "%20s" (format-ada (lovelace-to-ada output-sum)))
      "\n")))

(defun format-transactions (transactions)
  "Format a list of TRANSACTIONS."
  (concat (mapconcat 'format-transaction transactions "\n")))

(provide 'cardan-utils)
;;; cardan-utils.el ends here

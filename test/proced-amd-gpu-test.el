;;; proced-amd-gpu-test.el --- Tests for proced-amd-gpu.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for proced-amd-gpu.el

;;; Code:

(require 'buttercup)
(require 'json)
(require 'proced-amd-gpu)

(defconst proced-amd-gpu--mock-pid "1337")
(defconst proced-amd-gpu--versions '("v0.2.0" "v0.2.1"))

(cl-defmacro proced--within-buffer (format filter &body body)
  "Execute BODY within a proced buffer using format FORMAT and filter FILTER."
  `(let ((proced-format ,format)
         (proced-filter ,filter)
         (proced-auto-update-flag nil)
         (inhibit-message t))
     (proced)
     (unwind-protect
         (with-current-buffer "*Proced*"
           ,@body)
       (kill-buffer "*Proced*"))))

(defun proced--move-to-column (attribute)
  "Move to the column under ATTRIBUTE in the current proced buffer."
  (move-to-column (string-match attribute proced-header-line)))

(defun proced-amd-gpu--test-file-content (version)
  (with-temp-buffer
    (insert-file-contents 
     (expand-file-name (format "sample-output-%s.json" version)))
    (while (search-forward proced-amd-gpu--mock-pid nil t)
      (replace-match (number-to-string (emacs-pid)) nil t))
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "test new attributes"
  (it "Appear in buffer"
    (add-to-list
     'proced-format-alist
     '(custom user pid ppid sess tree pcpu pgpu pmem rss vram start time state (args comm)))
    (mapc (lambda (version)
            (let* ((text (proced-amd-gpu--test-file-content version))
                   (proced-amd-gpu-command (list "watch" "-n" "1" "echo" text)))
              (proced--within-buffer
               'custom
               'user
               (search-forward (number-to-string (emacs-pid)) nil t)
               (proced--move-to-column "%GPU"))))
          proced-amd-gpu--versions)))

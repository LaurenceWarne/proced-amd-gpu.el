;;; proced-amd-gpu.el --- Proced integration for AMD GPU statistics -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/proced-amd-gpu.el
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Proced integration for AMD GPU statistics.
;; See also https://github.com/Umio-Yasuno/amdgpu_top

;;; Code:

(require 'cl-lib)
(require 'proced)
(require 'json)

(defgroup proced-amd-gpu nil
  "Proced integration for AMD GPU statistics."
  :group 'tools)

(defcustom proced-amd-gpu-executable "amdgpu_top"
  "Executable used to obtain GPU statistics."
  :group 'proced-amd-gpu
  :type 'string)

(defcustom proced-amd-gpu-unit-translations
  ;; Convert MiB to kilobytes
  (list (cons "MiB" (lambda (n) (* 1024 n))))
  "Alist used to map amdgpu_top attribute values, given the unit."
  :group 'proced-amd-gpu
  :type 'list)

(defcustom proced-amd-gpu-grammar-alist
  '((pgpu "%GPU" proced-amd-gpu-format-pgpu right proced-< nil (usage pid) (nil t t))
    (vram "VRAM" proced-amd-gpu-format-vram right proced-< nil (vram pid) (nil t t))
    (gtt "GTT" proced-amd-gpu-format-gtt right proced-< nil (gtt pid) (nil t t))
    (gfx "GFX" proced-amd-gpu-format-gfx right proced-< nil (gfx pid) (nil t t))
    (encode "Encode" proced-amd-gpu-format-encode right proced-< nil (encode pid) (nil t t))
    (decode "Decode" proced-amd-gpu-format-decode right proced-< nil (decode pid) (nil t t))
    (dma "DMA" proced-amd-gpu-format-dma right proced-< nil (dma pid) (nil t t)))
  "Proced AMD GPU process alist."
  :group 'proced-amd-gpu
  :type 'list)

(defvar proced-amd-gpu--attribute-state (make-hash-table)
  "Global value of the GPU attribute state.

It's a hash table mapping (pid . attribute) to the attribute value.")

(defun proc-amd-gpu--extract-pid (proc-identifier)
  "Extract pid (as a string) from PROC-IDENTIFIER."
  (if (string-match-p (rx string-start (+ digit) string-end) proc-identifier)
      proc-identifier
    (substring (seq-drop-while (lambda (c) (not (= c ?\())) proc-identifier) 1 -1)))

(defun proced-amd-gpu--json-to-hash-table (string)
  "Decode STRING into an a hashtable mapping pids and attribute symbols to values."
  (when-let* ((decoded (ignore-errors (json-parse-string (string-trim string))))
              (proc-infos (if-let ((v021-result (ignore-errors (elt (gethash "devices" decoded) 0))))
                              (gethash "fdinfo" v021-result)
                            (gethash "fdinfo" decoded)))
              (new-hash (make-hash-table :test #'equal)))
    (cl-loop for proc-identifier being the hash-keys of proc-infos
             using (hash-values proc-table)
             for pid = (proc-amd-gpu--extract-pid proc-identifier)
             do
             (cl-loop
              for attr being the hash-keys of (gethash "usage" proc-table)
              using (hash-values attr-value)
              do
              (let* ((attr-symbol (intern (downcase attr)))
                     (translation-fn (alist-get
                                      (gethash "unit" attr-value)
                                      proced-amd-gpu-unit-translations
                                      #'identity))
                     (translated-value (funcall translation-fn
                                                (gethash "value" attr-value))))
                (puthash (cons (string-to-number pid) attr-symbol)
                         translated-value
                         new-hash))))
    new-hash))

(defun proced-amd-gpu--process-filter (proc string)
  "Load STRING into `proced-amd-gpu--attribute-state'.

PROC should be an \"amdgpu_top\" process."
  (when-let* ((process-live-p proc)
              (new-hash (proced-amd-gpu--json-to-hash-table string)))
    (setq proced-amd-gpu--attribute-state new-hash)))

(defun proced-amd-gpu--initialise ()
  "Start the amdgpu_top process."
  (unless (get-process proced-amd-gpu-executable)
    (make-process :name proced-amd-gpu-executable
                  :command (list "amdgpu_top" "-J")
                  :filter #'proced-amd-gpu--process-filter)))

(defun proced-amd-gpu-attribute (attribute-sym process-attrs default &optional attribute-key)
  "Return ATTRIBUTE-SYM for the process with PROCESS-ATTRS.

Return DEFAULT if ATTRIBUTE-SYM is not output by \"amdgpu_top\" against
the process.

If ATTRIBUTE-KEY is specified, use it to obtain the attribute from the amdgpu_top data."
  (cons attribute-sym
        (if-let* ((pid (alist-get 'pid process-attrs))
                  (attribute (gethash (cons pid (or attribute-key attribute-sym))
                                      proced-amd-gpu--attribute-state)))
            attribute
          default)))

(defun proced-amd-gpu-pgpu (process-attrs)
  "Return USAGE integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'pgpu process-attrs 0 'cpu))

(defun proced-amd-gpu-vram (process-attrs)
  "Return VRAM integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'vram process-attrs 0))

(defun proced-amd-gpu-gtt (process-attrs)
  "Return GTT integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'gtt process-attrs 0))

(defun proced-amd-gpu-gfx (process-attrs)
  "Return GFX integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'gfx process-attrs 0))

(defun proced-amd-gpu-encode (process-attrs)
  "Return ENCODE integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'encode process-attrs 0))

(defun proced-amd-gpu-decode (process-attrs)
  "Return DECODE integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'decode process-attrs 0))

(defun proced-amd-gpu-dma (process-attrs)
  "Return DMA integral value for the process with PROCESS-ATTRS."
  (proced-amd-gpu-attribute 'dma process-attrs 0))

;; Format functions

(defun proced-amd-gpu-format-pgpu (usage)
  "Format the integer USAGE, which should be a percentage."
  (proced-format-cpu usage))

(defun proced-amd-gpu-format-vram (vram)
  "Format the integer VRAM, which should be in kilobytes."
  (proced-format-rss vram))

(defun proced-amd-gpu-format-gtt (gtt)
  "Format the integer GTT, which should be in kilobytes."
  (proced-format-rss gtt))

(defun proced-amd-gpu-format-gfx (gfx)
  "Format the integer GFX, which should be a percentage."
  (proced-format-cpu gfx))

(defun proced-amd-gpu-format-encode (encode)
  "Format the integer ENCODE, which should be a percentage."
  (proced-format-cpu encode))

(defun proced-amd-gpu-format-decode (decode)
  "Format the integer DECODE, which should be a percentage."
  (proced-format-cpu decode))

(defun proced-amd-gpu-format-dma (dma)
  "Format the integer DMA, which should be a percentage."
  (proced-format-cpu dma))

(add-to-list 'proced-custom-attributes 'proced-amd-gpu-pgpu)
(add-to-list 'proced-custom-attributes 'proced-amd-gpu-vram)
(add-to-list 'proced-custom-attributes 'proced-amd-gpu-gtt)
(add-to-list 'proced-custom-attributes 'proced-amd-gpu-gfx)
(add-to-list 'proced-custom-attributes 'proced-amd-gpu-encode)
(add-to-list 'proced-custom-attributes 'proced-amd-gpu-decode)
(add-to-list 'proced-custom-attributes 'proced-amd-gpu-dma)

(mapc (lambda (grammar)
        (add-to-list 'proced-grammar-alist grammar))
      proced-amd-gpu-grammar-alist)

(proced-amd-gpu--initialise)

(provide 'proced-amd-gpu)

;;; proced-amd-gpu.el ends here

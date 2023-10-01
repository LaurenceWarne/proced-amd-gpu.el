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

(require 'proced)

;; Example output:

;; {
;;   "DeviceName": "AMD Radeon Graphics",
;;   "GRBM": {
;;     "Color Block": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Depth Block": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Geometry Engine": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Graphics Pipe": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Primitive Assembly": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Shader Export": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Shader Processor Interpolator": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Texture Pipe": {
;;       "unit": "%",
;;       "value": 0
;;     }
;;   },
;;   "GRBM2": {
;;     "Command Processor -  Compute": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Command Processor -  Fetcher": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Command Processor - Graphics": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Efficiency Arbiter": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Render Backend Memory Interface": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "RunList Controller": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "SDMA": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Unified Translation Cache Level-2": {
;;       "unit": "%",
;;       "value": 0
;;     }
;;   },
;;   "Sensors": {
;;     "Fan": {
;;       "unit": "RPM",
;;       "value": 0
;;     },
;;     "GFX Power": {
;;       "unit": "W",
;;       "value": 12
;;     },
;;     "GFX_MCLK": {
;;       "unit": "MHz",
;;       "value": 96
;;     },
;;     "GFX_SCLK": {
;;       "unit": "MHz",
;;       "value": 26
;;     },
;;     "VDDGFX": {
;;       "unit": "mV",
;;       "value": 705
;;     }
;;   },
;;   "VRAM": {
;;     "Total GTT": {
;;       "unit": "MiB",
;;       "value": 15999
;;     },
;;     "Total GTT Usage": {
;;       "unit": "MiB",
;;       "value": 115
;;     },
;;     "Total VRAM": {
;;       "unit": "MiB",
;;       "value": 16368
;;     },
;;     "Total VRAM Usage": {
;;       "unit": "MiB",
;;       "value": 900
;;     }
;;   },
;;   "fdinfo": {
;;     "amdgpu_top (220645)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 0
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 0
;;         }
;;       }
;;     },
;;     "amdgpu_top (7964)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 0
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 0
;;         }
;;       }
;;     },
;;     "exe (6328)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 3
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 45
;;         }
;;       }
;;     },
;;     "firefox-esr (2495)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 18
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 226
;;         }
;;       }
;;     },
;;     "ibus-extension- (2412)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 2
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 6
;;         }
;;       }
;;     },
;;     "ibus-ui-gtk3 (2411)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 2
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 6
;;         }
;;       }
;;     },
;;     "ibus-x11 (2416)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 2
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 6
;;         }
;;       }
;;     },
;;     "steam (52357)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 4
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 12
;;         }
;;       }
;;     },
;;     "steamwebhelper (52413)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 52
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 53
;;         }
;;       }
;;     },
;;     "steamwebhelper (52483)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 10
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 183
;;         }
;;       }
;;     },
;;     "xdg-desktop-por (2517)": {
;;       "usage": {
;;         "Compute": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "DMA": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Decode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "Encode": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GFX": {
;;           "unit": "%",
;;           "value": 0
;;         },
;;         "GTT": {
;;           "unit": "MiB",
;;           "value": 2
;;         },
;;         "VRAM": {
;;           "unit": "MiB",
;;           "value": 6
;;         }
;;       }
;;     }
;;   },
;;   "gpu_activity": {
;;     "GFX": {
;;       "unit": "%",
;;       "value": 1
;;     },
;;     "MediaEngine": {
;;       "unit": "%",
;;       "value": 0
;;     },
;;     "Memory": {
;;       "unit": "%",
;;       "value": 1
;;     }
;;   },
;;   "gpu_metrics": {
;;     "Throttle Status": []
;;   },
;;   "period": {
;;     "duration": 6110,
;;     "unit": "ms"
;;   }
;; }

(defvar proced-amd-gpu--attribute-state nil
  "Global value of the GPU attribute state.

It's a hash table mapping (pid . attribute) to the attribute value.")

(defun proced-amd-gpu--vram (process-attrs)
  "Return VRAM string for the process with PROCESS-ATTRS."
  (if-let* ((pid (assq 'pid process-attrs))
            (vram (gethash (cons pid 'vram) proced-amd-gpu--attribute-state)))
      vram
    ""))

(add-to-list 'proced-custom-attributes 'proced-amd-gpu--vram)

(add-to-list 'proced-grammar-alist
             '(compute "Compute" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             '(dma "DMA" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             '(decode "Decode" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             '(encode "Encode" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             '(gfx "GFX" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             '(gt "GTT" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             '(vram "VRAM" "%s" left proced-string-lessp nil (node pid) (nil t nil))
             )

(provide 'proced-amd-gpu)

;;; proced-amd-gpu.el ends here

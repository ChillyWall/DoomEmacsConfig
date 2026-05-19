;;; $DOOMDIR/modes/goldendict.el -*- lexical-binding: t; -*-

(defgroup goldendict nil
  "GoldenDict lookup settings."
  :group 'convenience)

(defcustom goldendict-scanpop t
  "Whether to use GoldenDict's scanpop feature for lookups."
  :type 'boolean
  :group 'goldendict)

(defun goldendict-lookup-selection ()
  (interactive)
  (when (evil-visual-state-p)
    (let* ((range (evil-visual-range))
           (beg (car range))
           (end (cadr range))
           (selected-text (buffer-substring-no-properties beg end)))

      (evil-normal-state)

      (call-process "goldendict" nil 0 nil
                    (if goldendict-scanpop "-s" "")
                    selected-text))))

;;;###autoload
(define-minor-mode goldendict-mode "GoldenDict Mode"
  :global t
  :init-value nil
  :lighter " GoldenDict"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c d") 'goldendict-lookup-selection)
            map))

(provide 'goldendict-mode)

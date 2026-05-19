;;; $DOOMDIR/modes/latex.el -*- lexical-binding: t; -*-

(setq! +latex-viewers '(pdf-tools zathura))

(after! tex
  (setq! TeX-engine 'luatex)
  (setq-default TeX-master nil)
  (setq! font-latex-fontify-script nil))

(set-formatter! 'latexindent
  '("latexindent" "-m" "--logfile=/dev/null")
  :modes '(latex-mode))

;;; $DOOMDIR/modes/latex.el -*- lexical-binding: t; -*-

(setq! +latex-viewers '(pdf-tools zathura))

(after! tex
  (setq! TeX-engine 'luatex)
  (setq-default TeX-master nil))

;;; $DOOMDIR/modes/copilot.el -*- lexical-binding: t; -*-

;; accept completion from copilot and fallback to company
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("M-n" . 'copilot-next-completion)
              ("M-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

;; OPTIONAL configuration
(after! gptel
  (setq!
   gptel-model 'TheAzazel/gemma3-12b-abliterated:latest
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "ollama.aide.npu5v5.cn"
                   :protocol "https"
                   :stream t
                   :models '("TheAzazel/gemma3-12b-abliterated:latest"))))

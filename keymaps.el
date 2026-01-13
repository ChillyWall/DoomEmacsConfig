;;; $DOOMDIR/core/keymaps.el -*- lexical-binding: t; -*-

(setq! doom-leader-key ";"
       doom-localleader-key "M-\\")

(after! evil-escape
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.2))

(map!
 :desc "Next buffer"     :n  "]b"   #'centaur-tabs-forward-tab
 :desc "Previous buffer" :n  "[b"   #'centaur-tabs-backward-tab
 :leader
 :desc "Project sidebar" :n  "e" #'+treemacs/toggle
 (:prefix-map ("b" . "buffer")
  :desc "Previous buffer" :n  "["  #'centaur-tabs-backward-tab
  :desc "Next buffer"     :n  "]"  #'centaur-tabs-forward-tab))

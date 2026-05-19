;;; $DOOMDIR/keymaps.el -*- lexical-binding: t; -*-

(setq! doom-leader-key ";"
       doom-localleader-key "M-\\")

(after! evil-escape
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.2))

(map!
 :map centaur-tabs-mode-map
 (:when (modulep! :ui tabs)
   :desc "Next buffer"     :n  "]b"   #'centaur-tabs-forward-tab
   :desc "Previous buffer" :n  "[b"   #'centaur-tabs-backward-tab
   :leader :prefix "b"
   (:desc "Previous buffer" :n  "["  #'centaur-tabs-backward-tab
    :desc "Next buffer"     :n  "]"  #'centaur-tabs-forward-tab)))

(map!
 :leader
 (:when (modulep! :ui treemacs)
   :desc "Project sidebar" :n  "e" #'+treemacs/toggle)
 (:when (modulep! :app rss)
   :prefix "o" :desc "elfeed" :n "e" #'elfeed)
 (:when (modulep! :app emms)
   :prefix "o" :desc "emms" :n "m" #'emms))

(map! :map goldendict-mode-map
      :leader :prefix "s"
      :desc "look up in goldendict" :v "g" #'goldendict-lookup-selection)

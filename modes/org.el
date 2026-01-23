;;; $DOOMDIR/modes/org.el -*- lexical-binding: t; -*-
(setq! org-directory "~/org/")
(setq! org-agenda-files
       (directory-files-recursively "~/org/roam/daily" ".org$"))
(setq! org-roam-directory "~/org/roam")
(setq! org-roam-dailies-directory "~/org/roam/daily")
(setq! org-roam-dailies-capture-templates
       '(("d" "default" entry
          "* %?"
          :target (file+head "%<%Y/%m/%Y-%m-%d>.org" ;; 这里定义了 文件夹/文件名 格式
                             "#+title: %<%Y-%m-%d>\n"))))

(use-package! org-roam-ql
  :after org-roam

  :config
  (map! :map org-roam-mode-map
        "C-c v" #'org-roam-ql-buffer-dispatch
        :map minibuffer-mode-map
        "C-c i" #'org-roam-ql-insert-node-title))

(use-package! org-mem
  :defer
  :config
  (setq org-mem-do-sync-with-org-id t) ;; Optional
  (setq org-mem-watch-dirs
        (list "~/org/roam/")) ;; Your org-roam-directory here
  (org-mem-updater-mode))

(use-package! org-node
  :init
  ;; Optional key bindings
  (map! :leader
        :desc "org-node" "n R" org-node-global-prefix-map)

  (after! org
    (map! :map org-mode-map
          :localleader
          :desc "org-node" "M" org-node-org-prefix-map))

  :config
  (org-node-cache-mode)
  (org-node-roam-accelerator-mode)
  (setq org-node-creation-fn #'org-node-new-via-roam-capture)
  (setq org-node-file-slug-fn #'org-node-slugify-like-roam-default)
  (setq org-node-file-timestamp-format "%Y%m%d%H%M%S-"))

(use-package! org-ref
  :config
  (map! :localleader
        :map org-mode-map
        :desc "org-ref" "l r" #'org-ref-insert-link-menu)
  (setq org-ref-enable-multi-file-references t))

(after! ox-pandoc
  ;; 添加导出到latex和markdown的选项
  (let ((new-items
         '((?k "to markdown." org-pandoc-export-to-markdown)
           (?K "to markdown and open." org-pandoc-export-to-markdown-and-open)
           (?t "to latex and." org-pandoc-export-to-latex)
           (?T "to latex and open" org-pandoc-export-to-latex-and-open))))
    (dolist (item new-items)
      (add-to-list 'org-pandoc-menu-entry item)))
  ;; 重新设置org-export-dispatcher界面中ox-pandoc的选项
  (org-export-define-derived-backend 'pandoc 'org
    :translate-alist '((entity    . org-pandoc-entity)
		       (export-block . org-pandoc-export-block)
		       (export-snippet . org-pandoc-export-snippet)
		       (latex-environment . org-pandoc-latex-environ)
                       (link      . org-pandoc-link)
                       (paragraph . org-pandoc-paragraph)
                       (src-block . org-pandoc-src-block)
                       (table     . org-pandoc-table)
                       (template  . org-pandoc-template))
    ;; :export-block "PANDOC"
    :menu-entry
    `(?p "export via pandoc"
      ,org-pandoc-menu-entry)
    :options-alist
    '((:pandoc-options "PANDOC_OPTIONS" nil nil space)
      (:pandoc-extensions "PANDOC_EXTENSIONS" nil nil space)
      (:pandoc-metadata "PANDOC_METADATA" nil nil space)
      (:pandoc-variables "PANDOC_VARIABLES" nil nil space)
      (:epub-chapter-level "EPUB_CHAPTER_LEVEL" nil nil t)
      (:epub-cover-image "EPUB_COVER" nil nil t)
      (:epub-stylesheet "EPUB_STYLESHEET" nil nil t)
      (:epub-embed-font "EPUB_EMBED_FONT" nil nil newline)
      (:epub-meta "EPUB_META" nil nil newline)
      (:epub-css "EPUB_CSS" nil nil newline)
      (:epub-rights "EPUB_RIGHTS" nil nil newline)
      (:bibliography "BIBLIOGRAPHY")
      (:with-cite-processors nil nil org-pandoc-with-cite-processors))))

(add-to-list 'org-roam-capture-templates
             '("s" "snippets" plain "%?" :target
               (file+head "snippets/%<%Y>/%<%m>/%<%d>-${slug}.org" "#+title: ${title}\n") :unnarrowed t))

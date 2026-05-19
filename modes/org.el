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
                             "#+filetags: :daily:\n#+title: %<%Y-%m-%d>\n"))))

(after! org
  (setq! org-format-latex-options
         '(:foreground default :background default :scale 1.2 :html-foreground "Black"
           :html-background "Transparent" :html-scale 1.0 :matchers
           ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq! org-pretty-entities nil)
  (setq! org-appear-autolinks t)
  (setq! org-appear-delay 0.5))

(setq! org-preview-latex-default-process 'dvisvgm)

;; 显示零宽空格
(after! whitespace
  (setq! whitespace-space-regexp "\\( +\\|\u200b\\)")
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?▾])))

;; 快速插入零宽空格
(map! :map org-mode-map
      :ni "M-SPC M-SPC"
      (lambda () (interactive) (insert "\u200b")))

;; 导出时（导出为 org 时除外），去除零宽空格
(defun +org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200b" "" text)))

(after! ox
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zero-width-space t))

;; org-roam 相关配置
(use-package! org-roam-ql
  :after org-roam

  :config
  (map! :map org-roam-mode-map
        "C-c v" #'org-roam-ql-buffer-dispatch
        :map minibuffer-mode-map
        "C-c i" #'org-roam-ql-insert-node-title))

;; 这里是 org-mem 的配置，org-mem 是一个基于 org-id 的笔记同步工具，可以将
;; org-roam 中的笔记同步到 org-mem 中，方便在其他设备上查看和编辑。
(use-package! org-mem
  :defer
  :config
  (setq org-mem-do-sync-with-org-id t) ;; Optional
  (setq org-mem-watch-dirs
        (list "~/org/roam/")) ;; Your org-roam-directory here
  (org-mem-updater-mode))

;; 这里是 org-node 的配置，org-node 是一个基于 org-roam 的笔记管理工具，可以将
;; org-roam 中的笔记以节点的形式展示出来，方便查看和编辑。
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

;; 这里是 org-ref 的配置，org-ref 是一个基于 org-mode 的文献管理工具，可以将文献
;; 以链接的形式插入到 org-mode 中，方便查看和编辑。
(use-package! org-ref
  :config
  (map! :localleader
        :map org-mode-map
        :desc "org-ref" "l r" #'org-ref-insert-link-menu)
  (setq org-ref-enable-multi-file-references t))

;; 这里是 ox-pandoc 的配置，添加了导出到latex和markdown的选项
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

;; org-roam-capture的模板
(add-to-list 'org-roam-capture-templates
             '("s" "snippets" plain "%?" :target
               (file+head "snippets/%<%Y>/%<%m>/%<%d>-${slug}.org" "#+title: ${title}\n") :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("j" "Project journal" plain "%?" :target
               (file+head "Projects/obstacle_fusion/journal/%<%m>/%<%d>-${slug}.org" "#+title: ${title}\n#+filetags: :obstacle_fusion:journal: ") :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("e" "Project experiment" plain "%?" :target
               (file+head "Projects/obstacle_fusion/experiments/%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+filetags: :obstacle_fusion:experiment: ") :unnarrowed t))

;; 添加导出到github markdown的选项
(after! ox-gfm
  (add-to-list 'org-export-backends 'gfm))

;; org-noter 的配置，添加了快捷键
(after! org-noter
  (map! :map org-noter-doc-mode-map
        "C-c i" #'org-noter-insert-note
        "C-c q" #'org-noter-kill-session))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(add-hook! 'org-mode-hook #'org-special-block-extras-mode)

(setq org-highlight-latex-and-related '(native latex script entities))

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

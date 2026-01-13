;;; $DOOMDIR/modes/markdown.el -*- lexical-binding: t; -*-

(after! markdown-mode
  (setq markdown-command "pandoc --filter=pandoc-crossref -L diagram.lua -L pandoc-sidenote.lua --number-sections --embed-resources"))

(use-package! markdown-preview-mode
  :after markdown-mode
  :init
  (map! :after markdown-mode  ; 确保是在 markdown-mode 加载后绑定快捷键
        :map markdown-mode-map
        :localleader
        :desc "Markdown Preview"              "p" #'markdown-preview-mode
        :desc "Markdown Preview Cleanup"      "c" #'markdown-preview-cleanup
        :desc "Markdown Preview Open Browser" "b" #'markdown-preview-open-browser)

  :config
  ;; 设置资源存放路径
  (setq! markdown-preview-assets-dir (expand-file-name "assets" doom-user-dir))

  (setq! markdown-preview-stylesheets '("theme.css" "skylighting-solarized-theme.css" "tufte.css"))

  (setq! markdown-preview-script-onupdate "MathJax.typesetPromise()")

  (setq! markdown-preview-delay-time 300)

  ;; 覆盖使用的模板文件为我自己的模板
  (setq! markdown-preview--preview-template
         (expand-file-name "assets/preview.html" doom-user-dir))

  ;; 覆盖启动http服务器的函数，增加从指定目录读取文件的功能
  (defun markdown-preview--start-http-server (port)
    "启动 HTTP 服务器，支持从自定义资产目录读取资源并返回正确 MIME 类型。"
    (unless markdown-preview--http-server
      (let ((docroot default-directory))
        (advice-add 'make-network-process :filter-args #'markdown-preview--fix-network-process-wait)
        (setq markdown-preview--http-server
              (ws-start
               (lambda (request)
                 (with-slots (process headers) request
                   (let* ((path (substring (cdr (assoc :GET headers)) 1))
                          ;; 核心修改：优先去资源文件夹找，找不到再去当前文档目录找
                          (assets-file (expand-file-name path markdown-preview-assets-dir))
                          (local-file (expand-file-name path docroot))
                          (target-file (if (file-exists-p assets-file) assets-file local-file)))
                     (cond
                      ;; 处理默认预览 HTML
                      ((string= path "")
                       (ws-send-file process
                                     (expand-file-name markdown-preview-file-name
                                                       (with-current-buffer (gethash (markdown-preview--parse-uuid headers)
                                                                                     markdown-preview--preview-buffers)
                                                         default-directory))))
                      ;; 处理图标
                      ((string= path "favicon.ico")
                       (ws-send-file process (expand-file-name path markdown-preview--home-dir)))
                      ;; 处理本地文件读取与 MIME 类型检测
                      ((and (not (file-directory-p target-file)) (file-exists-p target-file))
                       (let ((mime (cond ((string-match-p "\\.css$" target-file) "text/css")
                                         ((string-match-p "\\.js$" target-file) "application/javascript")
                                         (t "text/html"))))
                         (ws-send-file process target-file mime)))
                      ;; 404
                      (t (ws-send-404 process))))))
               markdown-preview-http-port nil :host markdown-preview-http-host))
        (advice-remove 'make-network-process #'markdown-preview--fix-network-process-wait)))))

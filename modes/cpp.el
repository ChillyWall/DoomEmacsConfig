;;; $DOOMDIR/modes/cpp.el -*- lexical-binding: t; -*-

;; 配置clangd参数
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("--background-index"
          "--clang-tidy"
          "--header-insertion=iwyu"
          "--completion-style=detailed"
          "--function-arg-placeholders=true"
          "--fallback-style=llvm"
          "--experimental-modules-support"))
  (set-lsp-priority! 'clangd 2))

;; 配置cmake-integration，在CMakeLists.txt和C/C++文件中通过 <localleader>m 启用
(use-package! cmake-integration
  :defer t
  :init
  (map! :after cc-mode
        :map (c++-mode-map c-mode-map c++-ts-mode-map c-ts-mode-map)
        :localleader
        :desc "CMake Menu" :n "m" #'cmake-integration-transient)
  (map! :after cmake-mode
        :map cmake-mode-map
        :localleader
        :desc "CMake Menu" :n "m" #'cmake-integration-transient))

;; 添加doxygen的treesitter parser
(after! treesit
  (add-to-list 'treesit-language-source-alist '(doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")))

;; 为C++的注释通过treesit添加Doxygen高亮支持
(add-hook!
 'c++-ts-mode-hook
 ;; 设置doxygen的解析范围，仅对使用 /* */ 的注释块启用
 (setq! treesit-range-settings
        (treesit-range-rules
         :embed 'doxygen
         :host 'cpp
         :local t  ;; 每各注释块启用一个parser，防止互相干扰
         '(((comment) @cap
            (:match "^/\\*" @cap)))))
 ;; 增加指示节点语言的函数，在inspect中正确显示节点
 (setq-local treesit-language-at-point-function
             (lambda (pos)
               (let* ((node (treesit-node-at pos 'cpp)))
                 (if (and node
                          (string-match-p "comment" (treesit-node-type node)))
                     'doxygen
                   'cpp))))
 ;; 设置doxygen中的颜色高亮
 (setq-local treesit-font-lock-settings
             (append treesit-font-lock-settings
                     (treesit-font-lock-rules
                      :language 'doxygen
                      :feature 'doxygen-core
                      :override t
                      '((tag_name) @font-lock-doc-markup-face
                        (type) @font-lock-type-face
                        (identifier) @font-lock-variable-name-face))))
 ;; 启用之前定义的feature
 (setq-local treesit-font-lock-feature-list
             (mapcar (lambda (level)
                       (if (member 'comment level)
                           (append level '(doxygen-core))
                         level))
                     treesit-font-lock-feature-list)))

;;; 日本語環境
;; 言語を日本語に設定
(set-language-environment       "Japanese")
;; utf-8を使用
(prefer-coding-system           'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)
;; Localeに合わせた環境の設定
(set-locale-environment nil)

;;; キーバインド
;; C-hでバックスペース
(keyboard-translate ?\C-h ?\C-?)
;; 基本
(define-key global-map (kbd "M-?") 'help-for-help)  ; ヘルプ
(define-key global-map (kbd "C-z") 'undo)  ; undo
(define-key global-map (kbd "C-c C-v") 'scroll-down)  ; 1画面分上へ
(define-key global-map (kbd "C-c C-i") 'hippie-expand)  ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim)  ; コメントアウト
(define-key global-map (kbd "C-c C-l") 'goto-line)  ; 指定行へ移動
(define-key global-map (kbd "C-c C-f") 'occur)  ; 検索
;; ウィンドウ移動
;; 次のウィンドウへ移動
(define-key global-map (kbd "C-M-n") 'next-multiframe-window)
;; 前のウィンドウへ移動
(define-key global-map (kbd "C-M-p") 'previous-multiframe-window)
;; 定義へ移動
;; C-x F -> 関数定義へ移動
;; C-x K -> キーにバインドされている関数定義へ移動
;; C-x V -> 変数定義へ移動
(find-function-setup-keys)

;;; 画像
;; 画像ファイルを表示
(auto-image-file-mode t)

;;; 各種表示部分
;; メニューバーを消す
(menu-bar-mode -1)
;; ツールバー非表示
;; (tool-bar-mode -1)
;; スクロールバー非表示
;; (set-scroll-bar-mode nil)
;; 起動時のメッセージを非表示
(setq inhibit-startup-screen t)
;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;;; カーソル
;; カーソルの点滅を止める
(blink-cursor-mode 0)
;; リージョンに色を付ける
(transient-mark-mode 1)

;; 画面スクロール
(setq scroll-conservatively 1)
(setq scroll-step 1)
(setq scroll-margin 4) ; default=0

;;; eval
;; evalした結果を全部表示
(setq eval-expression-print-length nil)

;;; 括弧
;; 対応する括弧を光らせる。
(show-paren-mode 1)
;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)

;;; 位置
;; 現在行を目立たせる
(global-hl-line-mode)
;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; バックアップ
;; バックアップファイルを作らない
(setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; バッファの自動再読み込み
;; 他のプログラムで変更されたファイルを自動で再読み込み
(global-auto-revert-mode 1)

;;; 補完
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; 部分一致の補完機能を使う
;; p-bでprint-bufferとか
(partial-completion-mode t)
;; 補完可能なものを随時表示
(icomplete-mode 1)

;;; C-x bでミニバッファにバッファ候補を表示
(iswitchb-mode t)
(iswitchb-default-keybindings)

;;; 履歴
;; 履歴数を大きくする
(setq history-length 10000)
;; ミニバッファの履歴を保存する
(savehist-mode 1)
;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)

;;; リージョンの大文字小文字変換を有効にする。
;; C-x C-u -> upcase
;; C-x C-l -> downcase
;;(put 'upcase-region 'disabled nil)
;;(put 'downcase-region 'disabled nil)

;;; 現在の関数名をウィンドウ上部に表示する。
(which-function-mode 1)

;; @ octave-mode
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

;;------------------------------
;; 以下, auto-install.elが絡んだ設定
;;------------------------------
;;; パスを通す
(setq load-path (cons "~/dotfiles/emacs.d/elisp" load-path))

;;; @ auto-install.el
;; 以下のurlから、wgetなりcurlなりでダウンロードして./elispの直下に配置
;; http://www.emacswiki.org/emacs/download/auto-install.el
;; (require 'auto-install)
;; (setq auto-install-directory "~/dotfiles/emacs.d/elisp/")
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup) 

;;; @ auto-complete
;; 以下のコマンドでインストールすること
;; M-x auto-install-batch <RET>  
;; Extension name: auto-complete development version <RET>
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
;; 補完情報源の指定
(setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-filename ac-source-abbrev))
;; 補完可能になるまでの遅延時間(sec)
(setq ac-delay 0.1)
;; 補完メニュー表示開始までの時間(sec)：nilで表示なし
(setq ac-auto-show-menu 0.1)

;;; @ js2-mode.el
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; @ uniquify
;; 同名ファイルのバッファ名の識別文字列を変更する
;; uniquifyはemacs標準機能
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; @ dired-x.el
;; 最近のemacsにはdiredの拡張版dired-xも同梱されているので標準で使用可能
(require 'dired-x)
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(define-key global-map (kbd "C-x C-j") 'dired-jump-other-window)  ; diredを別ウィンドウで起動

;;; @ popwin.el
;; (auto-install-from-url "https://raw.github.com/m2ym/popwin-el/master/popwin.el")
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)
(push '(dired-mode :position top :height 12) popwin:special-display-config)

;;; @ tabbar.el
;; (M-x auto-install-from-emacswiki tabbar.el)
(require 'tabbar)
(tabbar-mode 1)
 
;; タブ上でマウスホイール操作無効
(tabbar-mwheel-mode -1)
 
;; グループ化しない
(setq tabbar-buffer-groups-function nil)
 
;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
 
;; ウインドウからはみ出たタブを省略して表示
(setq tabbar-auto-scroll-flag nil)
 
;; タブとタブの間の長さ
(setq tabbar-separator '(1.5))
 
;; 外観変更
(set-face-attribute
 'tabbar-default nil
 :background "black"
 :foreground "gray72"
 :height 1.0)
(set-face-attribute
 'tabbar-unselected nil
 :background "black"
 :foreground "grey72"
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background "black"
 :foreground "#c82829"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box nil)
(set-face-attribute
 'tabbar-separator nil
 :height 1.2)

;; タブに表示させるバッファの設定
(defvar my-tabbar-displayed-buffers
 '("scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
  "*Regexps matches buffer names always included tabs.")
(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
 
;; タブ移動キーバインド
(global-set-key (kbd "M-n") 'tabbar-forward-tab)
(global-set-key (kbd "M-p") 'tabbar-backward-tab)
 
;; タブモードのオン/オフをトグル
(global-set-key (kbd "M-4") 'tabbar-mode)

;;; @ recentf-ext
;; (M-x auto-install-from-emacswiki recentf-ext.el)
;; 最近開いたファイル/ディレクトリの一覧を使用可能に
(setq recentf-max-saved-items 100)
(require 'recentf-ext)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

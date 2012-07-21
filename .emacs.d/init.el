
;;; 日本語環境
;; 言語を日本語に設定
(set-language-environment 'Japanese)
;; utf-8を使用
(prefer-coding-system 'utf-8)
;; Localeに合わせた環境の設定
(set-locale-environment nil)

;;; パスを通す
(setq load-path (cons "~/config_file/.emacs.d/elisp" load-path))

;;; auto-install.el
(require 'auto-install)
(setq auto-install-directory "~/config_file/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup) 

;;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)


;;; キーバインド
;; C-hでバックスペース
(keyboard-translate ?\C-h ?\C-?)
;; 基本
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
(define-key global-map (kbd "C-z") 'undo)                 ; undo
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim)       ; コメントアウト
(define-key global-map (kbd "C-c C-l") 'goto-line)      ; 指定行へ移動
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


;;; バー
;; メニューバーを消す
(menu-bar-mode -1)
;; ツールバーを消す
(tool-bar-mode -1)


;;; カーソル
;; カーソルの点滅を止める
(blink-cursor-mode 0)


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


;;; 補完
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; 部分一致の補完機能を使う
;; p-bでprint-bufferとか
(partial-completion-mode t)
;; 補完可能なものを随時表示
(icomplete-mode 1)


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
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;; 現在の関数名をウィンドウ上部に表示する。
(which-function-mode 1)

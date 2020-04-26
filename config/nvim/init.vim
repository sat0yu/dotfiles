" ----- common settings -----
" エンコード ( + UTF-8文字化け対応 )
set termencoding=utf-8
set encoding=utf-8
set fenc=utf-8
set fileencoding=utf-8
set fileencodings=ucs-boms,utf-8,euc-jp,cp932
set fileformats=unix,dos,mac
" Enable True Color
set termguicolors
" バックアップファイルを作らない
set nobackup
" スワップファイルを作らない
set noswapfile
" 編集中のファイルが変更されたら自動で読み直す
set autoread
" 入力中のコマンドをステータスに表示する
set showcmd
" シンタックスハイライト
set t_Co=256
syntax enable
syntax on
" ステータスバーを常に表示
set laststatus=2
" スクロール時の余白確保
set scrolloff=10
" Macターミナル上での動作が重くなる現象を軽減
set lazyredraw
" yank した文字列をクリップボードにコピー
set clipboard=unnamed
" 行番号を表示
set number
" 現在の行を強調表示
set cursorline
" カーソルの左右移動で行末から次の行の行頭へ移動
set whichwrap=b,s,h,l,<,>,[,],~
" 変更中のファイルでも、保存しないで他のファイルを表示
set hidden
" 新しく開く代わりにすでに開いてあるバッファを開く
set switchbuf=useopen
" カレントディレクトリを自動的に変更する
set autochdir
" 小文字の検索でも大文字も見つかるようにする
set ignorecase
" ただし大文字も含めた検索の場合はその通りに検索する
set smartcase
" インクリメンタルサーチを行う
set incsearch
" スペルチェックを行う
set spell
" 検索結果をハイライト
set hlsearch
" マウスモード有効
if !has('nvim') && has('mouse')
    set mouse=a
    if has('mouse_sgr')
        set ttymouse=sgr
    elseif v:version > 703 || v:version is 703 && has('patch632')
        set ttymouse=sgr
    else
        set ttymouse=xterm2
    endif
endif
" クリップボードからコピーした際に不要なインデントを防ぐ
if &term =~ "xterm"
    let &t_SI .= "\e[?2004h"
    let &t_EI .= "\e[?2004l"
    let &pastetoggle = "\e[201~"

    function XTermPasteBegin(ret)
        set paste
        return a:ret
    endfunction

    inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif
" 不可視文字を表示
set list
set listchars=trail:-,extends:>,precedes:<,nbsp:%,eol:↲,tab:▸\
" タブを半角スペースに変換, タブ幅を指定
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
augroup fileTypeIndent
    autocmd!
    autocmd BufNewFile,BufRead *.js    setlocal tabstop=2 softtabstop=2 shiftwidth=2
    autocmd BufNewFile,BufRead *.ts    setlocal tabstop=2 softtabstop=2 shiftwidth=2
    autocmd BufNewFile,BufRead *.rb    setlocal tabstop=2 softtabstop=2 shiftwidth=2
    autocmd BufNewFile,BufRead *.rake  setlocal tabstop=2 softtabstop=2 shiftwidth=2
    autocmd BufNewFile,BufRead *.yml   setlocal tabstop=2 softtabstop=2 shiftwidth=2
augroup END
" 改行時のインデント挙動
set autoindent
set smartindent
" ビープ音を可視化
set visualbell
" 対応括弧をハイライト表示する
set showmatch
" 対応括弧の表示秒数を0.1秒にする
set matchtime=1
" インサートモード時にバックスペースを使う
set backspace=indent,eol,start
" 長い行でも@で省略せずに表示
set display=lastline
" 補完ポップアップの高さ
set pumheight=10
" コマンドモードでも補完
set wildmode=longest:full
set wildmenu
set history=5000
" ファイルを開いたときに前回の編集箇所に移動
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
" ESCを2回入力で検索時のハイライトを解除
nnoremap <ESC><ESC> :nohlsearch<CR>
" 選択した範囲のインデントサイズを連続変更
vnoremap < <gv
vnoremap > >gv
" 検索語が画面中央にくるようにする
nmap n nzz
nmap N Nzz
" Yの動作をDと揃える(行末までヤンク)
nnoremap Y y$
" insertモードでカーソルの形を変える
if !has('gui_running')
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
" 保存時に行末の空白を除去する
autocmd BufWritePre * :%s/\s\+$//ge
" □とか○の文字があってもカーソル位置がずれないようにする
" iTerm2の Preferences → Profiles → Textタブで
" 「Treat ambiguous-width characters as double width」をチェック
if exists('&ambiwidth')
    set ambiwidth=double
endif
" 表示行単位で移動する
nnoremap j gj
nnoremap k gk
nnoremap <Down> gj
nnoremap <Up>   gk
" バッファ操作
nnoremap <C-b><C-n> :bn<CR>
nnoremap <C-b><C-p> :bp<CR>
nnoremap <C-b><C-k> :Kwbd<CR>

" ----- key-binds in insert mode -----
inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-h> <bs>
inoremap <c-d> <delete>
inoremap <c-f> <right>
inoremap <c-b> <left>
" jjでエスケープ
inoremap <silent> jj <ESC>

" ----- window operation settings -----
" prefix key
nnoremap s <Nop>
" ウィンドウ分割
nnoremap ss :<C-u>sp<CR>
nnoremap sv :<C-u>vs<CR>
" ウィンドウを閉じる
nnoremap sq :<C-u>q<CR>
nnoremap sQ :<C-u>bd<CR>
" ウィンドウ間の移動
nnoremap sh <C-w>h
nnoremap sj <C-w>j
nnoremap sk <C-w>k
nnoremap sl <C-w>l
nnoremap sw <C-w>w
" ウィンドウそのもののの移動
nnoremap sH <C-w>H
nnoremap sJ <C-w>J
nnoremap sK <C-w>K
nnoremap sL <C-w>L
" カレントウィンドウのサイズ変更
nnoremap so <C-w>_<C-w>|
nnoremap sO <C-w>=
nnoremap s= <C-w>=
" ウィンドウサイズの変更はvim-submodeへ移行
"nnoremap s> <C-w>>
"nnoremap s< <C-w><
"nnoremap s+ <C-w>+
"nnoremap s- <C-w>-
" タブページ関連
nnoremap st :<C-u>tabnew<CR>
" タブの移動はvim-submodeへ移行
"nnoremap sn gt
"nnoremap sp gT

" ----- load plugins
" Plugins will be downloaded under the specified directory.
call plug#begin('~/.local/share/nvim/plugged')
" Declare the list of plugins.
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'preservim/nerdtree'
Plug 'mhinz/vim-startify'
Plug 'tyru/caw.vim'
Plug 'itchyny/lightline.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" List ends here. Plugins become visible to Vim after this call.
call plug#end()

" ----- load plugin settings
runtime! plugins/*.vim

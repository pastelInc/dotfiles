set nocompatible
" Note: Skip initialization for vim-tiny or vim-small.
 if 0 | endif

if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!
" Plugins
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'h1mesuke/unite-outline'
NeoBundle 'leafgarland/typescript-vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle "thinca/vim-template"
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'elzr/vim-json'
NeoBundle 'pangloss/vim-javascript'
if has('lua')
    NeoBundle 'Shougo/neocomplete
endif 

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

if has('lua')
  " neocomplete setting
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  let g:neocomplete#enable_underbar_completion = 1
  let g:neocomplete#enable_auto_select = 1
  let g:neocomplete#sources#syntax#min_syntax_length = 3
  let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

  if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
  endif
  let g:neocomplete#keyword_patterns['default'] = '\h\w*'

  imap <expr><CR> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? neocomplete#close_popup() : "\<CR>"
  smap <expr><CR> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<CR>"

  inoremap <expr><S-CR> neocomplete#smart_close_popup()."\<CR>"
  inoremap <expr><C-l> neocomplete#smart_close_popup()
  inoremap <expr><TAB> pumvisible() ? neocomplete#complete_common_string() : "\<TAB>"
  inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
  inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
endif


" neosnippet settings
set completeopt-=preview
let g:neosnippet#snippets_directory = '~/.vim/snippets'
let g:neosnippet#disable_runtime_snippets = {'javascript': 1}
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)


filetype plugin indent on
syntax on
set list
set listchars=eol:$,tab:>\
set number
set ruler
set title
set expandtab
set autoindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set backspace=start,indent
set scrolloff=20

"colorscheme freya

if exists('&ambiwidth')
    set ambiwidth=double
endif

" encoding settings
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,cp932

"## specify input encoding
map -E :e ++enc=euc-jp
map -J :e ++enc=iso-2022-jp
map -S :e ++enc=sjis
map -U :e ++enc=utf-8

"## specify output encoding
map =E :set fileencoding=euc-jp
map =J :set fileencoding=iso-2022-jp
map =S :set fileencoding=sjis
map =U :set fileencoding=utf-8

" statusline settings
set laststatus=2
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" case
set smartcase

" nobackup
set nowritebackup
set nobackup
set noswapfile

" hlsearch
set hlsearch
nmap <Esc><Esc> :nohlsearch<CR><Esc>

" unite.vim setting
let g:unite_enable_start_insert=0
"let g:unite_source_grep_default_opts = '-Hn'
"prefix key
nmap <Space> [unite]

"カレントディレクトリを表示
nnoremap <silent> [unite]a :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
"バッファと最近開いたファイル一覧を表示
nnoremap <silent> [unite]f :<C-u>Unite<Space>buffer file_mru<CR>
"最近開いたディレクトリを表示
nnoremap <silent> [unite]d :<C-u>Unite<Space>directory_mru<CR>
"バッファを表示
nnoremap <silent> [unite]b :<C-u>Unite<Space>buffer<CR>
"レジストリを表示
nnoremap <silent> [unite]r :<C-u>Unite<Space>register<CR>
"タブを表示
nnoremap <silent> [unite]t :<C-u>Unite<Space>tab<CR>
"ヒストリ/ヤンクを表示
nnoremap <silent> [unite]h :<C-u>Unite<Space>history/yank<CR>
"outline
nnoremap <silent> [unite]o :<C-u>Unite<Space>outline<CR>
"file_rec:!
nnoremap <silent> [unite]<CR> :<C-u>Unite<Space>file_rec:!<CR>
"unite.vimを開いている間のキーマッピング
autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()"{{{
    " ESCでuniteを終了
    nmap <buffer> <ESC> <Plug>(unite_exit)
endfunction"}}}
"nnoremap <silent> ,ug :<C-u>Unite grep<CR>
"nnoremap <silent> ,qg :<C-u>Unite -no-quit grep<CR>
"nnoremap <silent> ,ut :<C-u>Unite -immediately tab:no-current<CR>
"nnoremap <silent> ,uw :<C-u>Unite -immediately window:no-current<CR>

au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

nnoremap <silent> ,h :<C-u>VimShell<CR>
nnoremap <silent> H :<C-u>vne<CR>:<C-u>VimShell<CR>

" VimFiler settings
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_directory_display_top = 0
let g:vimfiler_ignore_pattern = '^\%(.git\|.DS_Store\)$'
nnoremap <silent> ,f :<C-u>VimFilerBufferDir -create -quit <CR>
nnoremap <silent> ,F :<C-u>VimFilerBufferDir -create -split -quit<CR>

" tab setting
" Anywhere SID.
function! s:SID_PREFIX()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID_PREFIX$')
endfunction

" Set tabline.
function! s:my_tabline()  "{{{
  let s = ''
  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let bufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears
    let no = i  " display 0-origin tabpagenr.
    let mod = getbufvar(bufnr, '&modified') ? '!' : ' '
    let title = fnamemodify(bufname(bufnr), ':t')
    let title = '[' . title . ']'
    let s .= '%'.i.'T'
    let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
    let s .= no . ':' . title
    let s .= mod
    let s .= '%#TabLineFill# '
  endfor
  let s .= '%#TabLineFill#%T%=%#TabLine#'
  return s
endfunction "}}}
let &tabline = '%!'. s:SID_PREFIX() . 'my_tabline()'
set showtabline=2 " 常にタブラインを表示

" The prefix key.
nnoremap    [Tag]   <Nop>
nmap    t [Tag]
" Tab jump
for n in range(1, 9)
  execute 'nnoremap <silent> [Tag]'.n  ':<C-u>tabnext'.n.'<CR>'
endfor

" tc 新しいタブを一番右に作る
map <silent> [Tag]c :tablast <bar> tabnew<CR>
" tx タブを閉じる
map <silent> [Tag]x :tabclose<CR>
" tn 次のタブ
map <silent> [Tag]n :tabnext<CR>
" tp 前のタブ
map <silent> [Tag]p :tabprevious<CR>

" VimShell settings
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
if has('win32') || has('win64')
  let g:vimshell_prompt = $USERNAME."% "
else
  let g:vimshell_prompt = $USER."% "
endif
let g:vimshell_interactive_update_time = 300


" syntastic settings
let g:syntastic_check_on_open = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['python', 'ocaml', 'javascript', 'coffee'],
                           \ 'passive_filetypes': ['html', 'rst', 'latex'] }
nnoremap <silent> ,s :<C-u>SyntasticToggleMode<CR>

" syntastic javascript settings
let g:syntastic_javascript_checkers = ['eslint']

" syntastic typescript settings
"let g:syntastic_typescript_tsc_args = '-t ES5 --noImplicitAny'
let g:syntastic_typescript_tsc_args = '-t ES5 --module commonjs'


" 行末の空白文字を可視化
highlight WhitespaceEOL cterm=underline ctermbg=red guibg=#ff0000
au BufWinEnter * let w:m1 = matchadd("WhitespaceEOL", ' \+$')
au WinEnter * let w:m1 = matchadd("WhitespaceEOL", ' \+$')

" 全角スペースの表示
"highlight ZenkakuSpace cterm=underline ctermbg=red guibg=#666666
"au BufWinEnter * let w:m3 = matchadd("ZenkakuSpace", '　')
"au WinEnter * let w:m3 = matchadd("ZenkakuSpace", '　')

" indent-guides
let g:indent_guides_enable_on_vim_startup = 1


" spell check
set spelllang=en,cjk
set spell

" vim-json
let g:vim_json_syntax_conceal = 0

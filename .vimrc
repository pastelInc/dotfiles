if 0 | endif

if &compatible
    set nocompatible
endif

filetype plugin indent off

set runtimepath+=~/.vim/bundle/neobundle.vim/

" Plugins
let g:neobundle_default_git_protocol = 'https'
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'msys' : 'make -f make_cygwin.mak',
\     'mac' : 'make',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }
NeoBundle 'https://github.com/flazz/vim-colorschemes.git'
NeoBundle 'https://github.com/scrooloose/syntastic'
NeoBundle 'https://github.com/Shougo/unite.vim'
NeoBundle 'https://github.com/ujihisa/unite-colorscheme'
NeoBundle 'https://github.com/nathanaelkane/vim-indent-guides.git'
NeoBundle 'https://github.com/bronson/vim-trailing-whitespace.git'
NeoBundle 'https://github.com/elzr/vim-json.git'
NeoBundle 'https://github.com/pangloss/vim-javascript.git'
if has('lua')
    NeoBundle 'https://github.com/Shougo/neocomplete'
endif
call neobundle#end()

NeoBundleCheck

if has('lua')
    " neocomplete setting
    let g:acp_enableAtStartup = 0
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

filetype plugin indent on
syntax on
colorscheme default
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
set backspace=eol,start,indent
set scrolloff=20

if exists('&ambiwidth')
    set ambiwidth=double
endif

" statusline settings
set laststatus=2
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" unite.vim setting
let g:unite_enable_start_insert=0
let g:unite_source_grep_default_opts = '-Hn'

nnoremap <silent> ,us :<C-u>Unite source -start-insert<CR>
nnoremap <silent> ,ub :<C-u>Unite buffer_tab<CR>
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
nnoremap <silent> ,uu :<C-u>Unite buffer file_mru<CR>
nnoremap <silent> ,ua :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file<CR>
nnoremap <silent> ,ug :<C-u>Unite grep<CR>
nnoremap <silent> ,qg :<C-u>Unite -no-quit grep<CR>
nnoremap <silent> ,ut :<C-u>Unite -immediately tab:no-current<CR>
nnoremap <silent> ,uw :<C-u>Unite -immediately window:no-current<CR>

au FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')

au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

nnoremap <silent> ,h :<C-u>VimShell<CR>
nnoremap <silent> H :<C-u>vne<CR>:<C-u>VimShell<CR>

" tab setting
nnoremap <silent> <C-t> :tabe<CR>

" syntastic settings
let g:syntastic_check_on_open = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['python', 'javascript', 'php'],
                           \ 'passive_filetypes': ['html'] }
nnoremap <silent> ,s :<C-u>SyntasticToggleMode<CR>

" syntastic javascript settings
let g:syntastic_javascript_checkers = ['standard']

" syntastic typescript settings
"let g:syntastic_typescript_tsc_args = '-t ES5 --noImplicitAny'
let g:syntastic_typescript_tsc_args = '-t ES5 --module commonjs'

" 行末の空白文字を可視化
highlight WhitespaceEOL cterm=underline ctermbg=red guibg=#ff0000
au BufWinEnter * let w:m1 = matchadd("WhitespaceEOL", ' \+$')
au WinEnter * let w:m1 = matchadd("WhitespaceEOL", ' \+$')

" 全角スペースの表示
highlight ZenkakuSpace cterm=underline ctermbg=red guibg=#666666
au BufWinEnter * let w:m3 = matchadd("ZenkakuSpace", '　')
au WinEnter * let w:m3 = matchadd("ZenkakuSpace", '　')

" indent-guides
let g:indent_guides_enable_on_vim_startup = 1

" spell check
set spelllang=en,cjk
set spell
hi clear SpellBad
hi SpellBad cterm=underline

let g:vim_json_syntax_conceal = 0
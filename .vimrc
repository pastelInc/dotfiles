set nocompatible
filetype plugin indent off

set runtimepath+=~/.vim/bundle/neobundle.vim/

" Plugins
let g:neobundle_default_git_protocol = 'https'
call neobundle#begin(expand('~/.vim/bundle/'))
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
NeoBundle 'https://github.com/Shougo/neobundle.vim'
NeoBundle 'https://github.com/Shougo/unite.vim'
NeoBundle 'https://github.com/Shougo/vimfiler'
NeoBundle 'https://github.com/Shougo/vinarise'
NeoBundle 'https://github.com/Shougo/neosnippet'
NeoBundle 'https://github.com/Shougo/neosnippet-snippets'
NeoBundle "https://github.com/thinca/vim-template"
NeoBundle 'https://github.com/ujihisa/unite-colorscheme'
NeoBundle 'https://github.com/vim-scripts/surround.vim'
NeoBundle 'https://github.com/vim-scripts/sudo.vim'
NeoBundle 'https://github.com/nathanaelkane/vim-indent-guides.git'
NeoBundle 'https://github.com/bronson/vim-trailing-whitespace.git'
NeoBundle 'https://github.com/elzr/vim-json.git'
NeoBundle 'https://github.com/pangloss/vim-javascript.git'
NeoBundle 'https://github.com/rust-lang/rust.vim'
NeoBundle 'https://github.com/mxw/vim-jsx'
NeoBundle 'https://github.com/leafgarland/typescript-vim'
if has('python')
    " NeoBundle "https://github.com/klen/python-mode"
endif
if has('lua')
    NeoBundle 'https://github.com/Shougo/neocomplete'
endif
call neobundle#end()

filetype plugin indent on

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

if exists('&ambiwidth')
    set ambiwidth=double
endif

" encoding settings
" source ~/dotfiles/encode.vim

" statusline settings
set laststatus=2
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" Python setting
let g:pymode_doc = 0
let g:pymode_folding = 0
let g:pymode_run = 0
let g:pymode_lint = 0
let g:pymode_syntax_print_as_function = 1
let g:pymode_rope_sorted_completions = 0
let g:pymode_rope_extended_complete = 0
let g:pymode_rope_vim_completion = 0

" OCaml setting

" C++ settings
autocmd Filetype c,cpp,cu set cindent

" case
set smartcase

" nobackup
set nowritebackup
set nobackup
set noswapfile
set noundofile

" hlsearch
set hlsearch
nmap <Esc><Esc> :nohlsearch<CR><Esc>

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

" VimFiler settings
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_directory_display_top = 0
let g:vimfiler_ignore_pattern = '^\%(.git\|.DS_Store\)$'
nnoremap <silent> ,f :<C-u>VimFilerBufferDir -create -quit <CR>
nnoremap <silent> ,F :<C-u>VimFilerBufferDir -create -split -quit<CR>

" tab setting
nnoremap <silent> <C-t> :tabe<CR>

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

" syntastic c++ settings
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
let g:syntastic_cpp_compiler_options = ' -std=c++0x -stdlib=libc++'
if executable('clang++')
    let g:syntastic_cpp_compiler = 'clang++'
endif

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

" crontabファイルの編集
autocmd BufRead /tmp/crontab.* :set nobackup nowritebackup

" indent-guides
let g:indent_guides_enable_on_vim_startup = 1


" spell check
set spelllang+=cjk
set spell

let g:vim_json_syntax_conceal = 0
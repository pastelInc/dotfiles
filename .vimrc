if &compatible
  set nocompatible
endif

set runtimepath+=~/.vim/bundles/repos/github.com/Shougo/dein.vim

" Plugins
if dein#load_state('~/.vim/bundles')
  call dein#begin('~/.vim/bundles')
  call dein#add('~/.vim/bundles/repos/github.com/Shougo/dein.vim')
  " call dein#add('flazz/vim-colorschemes.git')
  " call dein#add('scrooloose/syntastic')
  " call dein#add('Shougo/neobundle.vim')
  " call dein#add('Shougo/vimproc', {'build': 'make'})
  " call dein#add('Shougo/unite.vim')
  " call dein#add('Shougo/vimfiler')
  " call dein#add('Shougo/vinarise')
  " call dein#add('Shougo/neosnippet')
  " call dein#add('Shougo/neosnippet-snippets')
  " call dein#add("thinca/vim-template")
  " call dein#add("thinca/vim-quickrun")
  " call dein#add('ujihisa/unite-colorscheme')
  " call dein#add('vim-scripts/surround.vim')
  " call dein#add('vim-scripts/sudo.vim')
  " call dein#add('nathanaelkane/vim-indent-guides.git')
  " call dein#add('bronson/vim-trailing-whitespace.git')
  " call dein#add('elzr/vim-json.git')
  " call dein#add('pangloss/vim-javascript.git')
  " call dein#add('rust-lang/rust.vim')
  " call dein#add('racer-rust/vim-racer')
  " call dein#add('mxw/vim-jsx')
  " call dein#add('leafgarland/typescript-vim')
  " call dein#add('tpope/tpope-vim-abolish')
  " if has('lua')
  "   call dein#add('https://github.com/Shougo/neocomplete')
  " endif
  " call dein#end()
  " call dein#save_state()
endif

filetype plugin indent on
syntax enable
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

" spell check
set spelllang=en,cjk
set spell
hi clear SpellBad
hi SpellBad cterm=underline

let g:vim_json_syntax_conceal = 0

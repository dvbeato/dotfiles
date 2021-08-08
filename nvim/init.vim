" Diogo Beato vimrc configurations
" diogobeato.com - @dvbeato

call plug#begin('~/.config/nvim/plugged')
  "#### UTILITIES
  " git plugins
  Plug 'tpope/vim-fugitive'
  Plug 'airblade/vim-gitgutter'

  " tmux
  Plug 'preservim/vimux'

  " misc plugins
  Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'stsewd/fzf-checkout.vim'

  Plug 'ntpeters/vim-better-whitespace'
  Plug 'ap/vim-css-color'
  Plug 'tpope/vim-surround'
  Plug 'liuchengxu/vim-which-key'

  " Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  " IDE like plugins
  Plug 'neoclide/coc.nvim', {'branch': 'release'}

  "#### LANG AND FRAMEWORKS

  " golang
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

  " clojure plugins
  Plug 'Olical/conjure', {'tag': 'v4.22.1'}
"  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
"    map  <Leader>cen  :%Eval<CR>
"    map  <Leader>crt  :RunTests<CR>

"  Plug 'clojure-vim/clojure.vim', { 'for': 'clojure' }
"  let g:clojure_syntax_keywords = {
"        \'clojureMacro': ["schema.core/def"],
"        \'clojureFunc': ["schema.core/def"],
"        \}
  Plug 'guns/vim-sexp'
    let g:sexp_mappings = {
      \ 'sexp_raise_list': '<M-;>',
      \ 'sexp_indent':     '=-',
      \ 'sexp_indent_top': '==',
      \ }

  Plug 'guns/vim-clojure-highlight'

  "Plug 'tpope/vim-sexp-mappings-for-regular-people'
  "Plug 'clojure-vim/acid.nvim', {'for': 'clojure', 'do': ':UpdateRemotePlugins' }
  "Plug 'Vigemus/impromptu.nvim', { 'for': 'clojure' }
  "Plug 'clojure-vim/jazz.nvim', { 'for': 'clojure' }

  "#### COSMETICS
  Plug 'nanotech/jellybeans.vim'
  Plug 'joshdick/onedark.vim'
  Plug 'morhetz/gruvbox'
    let g:gruvbox_contrast_dark='medium'

  " air-line plugins
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
    let g:airline_powerline_fonts = 1

    if !exists('g:airline_symbols')
      let g:airline_symbols = {}
    endif

    " unicode symbols
    let g:airline_left_sep = '»'
    let g:airline_left_sep = '▶'
    let g:airline_right_sep = '«'
    let g:airline_right_sep = '◀'
    let g:airline_symbols.linenr = '␊'
    let g:airline_symbols.linenr = '␤'
    let g:airline_symbols.linenr = '¶'
    let g:airline_symbols.branch = '⎇'
    let g:airline_symbols.paste = 'ρ'
    let g:airline_symbols.paste = 'Þ'
    let g:airline_symbols.paste = '∥'
    let g:airline_symbols.whitespace = 'Ξ'

    " airline symbols
    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_symbols.branch = ''
    let g:airline_symbols.readonly = ''
    let g:airline_symbols.linenr = ''

  Plug 'junegunn/rainbow_parentheses.vim'
    let g:rainbow_active = 1
    let g:rainbow#colors = {
      \   'dark': [
      \     ['yellow',  'orange1'     ],
      \     ['green',   'yellow1'     ],
      \     ['cyan',    'greenyellow' ],
      \     ['magenta', 'green1'      ],
      \     ['red',     'springgreen1'],
      \     ['yellow',  'cyan1'       ],
      \     ['green',   'slateblue1'  ],
      \     ['cyan',    'magenta1'    ],
      \     ['magenta', 'purple1'     ]
      \   ],
      \   'light': [
      \     ['yellow',  'orange1'     ],
      \     ['green',   'yellow1'     ],
      \     ['cyan',    'greenyellow' ],
      \     ['magenta', 'green1'      ],
      \     ['red',     'springgreen1'],
      \     ['yellow',  'cyan1'       ],
      \     ['green',   'slateblue1'  ],
      \     ['cyan',    'magenta1'    ],
      \     ['magenta', 'purple1'     ]
      \   ]
      \ }
      augroup rainbow_lisp
        autocmd!
        autocmd FileType lisp,clojure,scheme RainbowParentheses
      augroup END

call plug#end()

syntax on                   " Enable syntax highlight
syntax enable
filetype plugin indent on

colorscheme default
highlight CursorLine gui=none cterm=none ctermbg=0
highlight ExtraWhitespace ctermbg=1

set clipboard+=unnamedplus
set nocompatible
set tabstop=2               " number of visual spaces per TAB
set softtabstop=2           " number of spaces in TAB when editing
set shiftwidth=2
set expandtab               " all tabs are spaces
set autoindent
set smartindent
set smarttab
set nowrap
set encoding=utf-8
set fileencoding=utf-8
set linespace=0             " don't insert extra pixels between rows
set number                  " show linenumbers
set cursorline              " highlight current line
set wildmenu                " visual autocomplete for command menu
set lazyredraw              " redraw only when necessary
set showmatch               " highlight match {[()]}
set incsearch               " Find the next match as we type the search
set hlsearch                " highlight matches search
set history=100
set backspace=indent,eol,start
set listchars=tab:>-,trail:.,extends:>,precedes:<
set list

if has('gui_running')
  set guioptions-=T       " remove toolbar
  set lines=999           " full height
  set columns=999         " full width
endif

let NERDTreeIgnore = ['\.pyc$']

" NERDTree
" Close Vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Shortcuts

nnoremap <SPACE> <Nop>
let mapleader=" "

nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
set timeoutlen=500

" panels
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h

" navigation
nmap <Leader>1  :NERDTreeToggle<CR>
map  <Leader>o  :GFiles<CR>
map  <Leader>ff :Files<CR>
map  <Leader>b  :Buffers<CR>
map  <Leader>C  :Commands<CR>
map  <Leader>F  :Rg<CR>

" exit term
tnoremap <ESC><ESC> <C-\><C-N>

" VIMFile
nmap <silent> <leader>rvf :source $MYVIMRC<CR>
nmap <silent> <leader>evf :edit! $MYVIMRC<CR>

" Git
nmap <Leader>g :Git<CR>
nmap <Leader>gb :Gblame<CR>

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> fr <Plug>(coc-references)


" Delete without copy
nnoremap x "_x
nnoremap X "_X
vnoremap p "_dP

"TODO
"fix <S-j>, it's being used by paredit and I'd like to use it as join lines
"learn how to scroll down without new lines
"learn how to add new line and keep parentheses as first char

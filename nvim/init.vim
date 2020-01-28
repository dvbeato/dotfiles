" Diogo Beato vimrc configurations
" diogobeato.com - @dvbeato

call plug#begin('~/.config/nvim/plugged')
  "#### UTILITIES
  " git plugins
"  Plug 'tpope/vim-fugitive'
"  Plug 'airblade/vim-gitgutter'

  " misc plugins
  Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'mileszs/ack.vim'
  Plug 'ntpeters/vim-better-whitespace'
  Plug 'ap/vim-css-color'
  Plug 'tpope/vim-surround'

  " IDE like plugins
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"  Plug 'neoclide/coc.nvim', {'branch': 'release'}

  "#### LANG AND FRAMEWORKS

  " golang
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

  " clojure plugins

  Plug 'clojure-vim/acid.nvim', {'for': 'clojure', 'do': ':UpdateRemotePlugins' }
  Plug 'vim-scripts/paredit.vim'
  Plug 'Vigemus/impromptu.nvim', { 'for': 'clojure' }
  Plug 'clojure-vim/jazz.nvim', { 'for': 'clojure' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

  " ruby plugins
  Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
  Plug 'thoughtbot/vim-rspec', { 'for': 'ruby' }

  "#### COSMETICS
  " colorschemas
"  Plug 'arcticicestudio/nord-vim'
"  Plug 'dracula/vim'
"  Plug 'nanotech/jellybeans.vim'
  Plug 'joshdick/onedark.vim'
"  Plug 'morhetz/gruvbox'
"  let g:gruvbox_contrast_dark='medium'

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

colorscheme onedark
highlight CursorLine gui=none cterm=none ctermbg=236
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

" Move between panels
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h

map <M-P> :Files<CR>
map <M-e> :Buffers<CR>
map <M-S-c> :Commits<CR>

if has('gui_running')
  set guioptions-=T       " remove toolbar
  set lines=999           " full height
  set columns=999         " full width
endif

let NERDTreeIgnore = ['\.pyc$']

" NERDTree
" Close Vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

map <C-n> :NERDTreeToggle<CR>


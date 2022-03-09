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
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'stsewd/fzf-checkout.vim'

  Plug 'ntpeters/vim-better-whitespace'
  Plug 'ap/vim-css-color'
  Plug 'tpope/vim-surround'
  Plug 'liuchengxu/vim-which-key'

  " IDE like plugins
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'dense-analysis/ale'

  "#### LANG AND FRAMEWORKS

  " Html
  Plug 'mattn/emmet-vim'

  " golang
  Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

  " clojure
  Plug 'Olical/conjure', {'tag': 'v4.30.1'}

  Plug 'guns/vim-sexp'
    let g:sexp_mappings = {
      \ 'sexp_raise_list': '<M-;>',
      \ 'sexp_indent':     '=-',
      \ 'sexp_indent_top': '==',
      \ }

  Plug 'guns/vim-clojure-highlight'

  "#### COSMETICS
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
  Plug 'p00f/nvim-ts-rainbow'

  Plug 'kyazdani42/nvim-web-devicons' " for file icons
  Plug 'kyazdani42/nvim-tree.lua'

  Plug 'ayu-theme/ayu-vim'

  Plug 'arzg/vim-colors-xcode'
  Plug 'phanviet/vim-monokai-pro'
  Plug 'sainnhe/gruvbox-material'
  Plug 'ryanoasis/vim-devicons'
  Plug 'chriskempson/base16-vim'
  Plug 'nanotech/jellybeans.vim'
  Plug 'doums/darcula'
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

    let g:airline_mode_map = {
          \ 'c': 'C',
          \ 'n': 'N',
          \ 'V': 'V',
          \ 'i': 'I'}

    let g:airline#extensions#tabline#enabled = 1
    let g:airline_section_z = '%2l/%L☰%2v'
    let g:airline#extensions#wordcount#enabled = 0
    let g:airline#extensions#tabline#formatter = 'unique_tail'

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

call plug#end()

syntax on                   " Enable syntax highlight
syntax enable
filetype plugin indent on

set termguicolors
colorscheme monokai_pro
" highlight CursorLine gui=none cterm=none ctermbg=0
highlight ExtraWhitespace ctermbg=2

set clipboard+=unnamedplus
set nocompatible
set tabstop=2               " number of visual spaces per TAB
set softtabstop=2           " number of spaces in TAB when editing
set shiftwidth=2
set expandtab               " all tabs are spaces
set splitright              " open split buffers at right
set autoindent
set smartindent
set smarttab
set noswapfile
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
set guifont=Fira\ Code\ Retina\ Nerd\ Font\ Complete\ Mono:h12

if has('gui_running')
  set guioptions-=T       " remove toolbar
  set lines=999           " full height
  set columns=999         " full width
endif

runtime coc-config.vim

highlight WhichKeyFloating guibg=#000000

"# NvimTree
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_indent_markers = 1
let g:nvim_tree_add_trailing = 1
let g:nvim_tree_show_icons = {
    \ 'git': 1,
    \ 'folders': 0,
    \ 'files': 1,
    \ 'folder_arrows': 0,
    \ }

"highlight NvimTreeNormal guibg=#2D2A2E
highlight NvimTreeFolderName gui=bold guifg=#78DCE8
highlight NvimTreeOpenedFolderName gui=bold guifg=#78DCE8

lua <<EOF
  require'nvim-tree'.setup {
    auto_close = true,
    view = {
      width = 36
    }
  }

  require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
    ignore_install = { "javascript" }, -- List of parsers to ignore installing
    highlight = {
      enable = true,              -- false will disable the whole extension
      -- disable = { "c", "rust" },  -- list of language that will be disabled
      -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
      -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
      -- Using this option may slow down your editor, and you may see some duplicate highlights.
      -- Instead of true it can also be a list of languages
      additional_vim_regex_highlighting = false,
    },
    rainbow = {
      enable = true,
      extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
      max_file_lines = nil, -- Do not enable for files with more than n lines, int
   --   colors = {}, -- table of hex strings
   --   termcolors = {} -- table of colour name strings
    }
  }
EOF

runtime keybinds.vim

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

  Plug 'dvbeato/morgana-theme', {'branch': 'main'}

call plug#end()

syntax on                   " Enable syntax highlight
syntax enable
filetype plugin indent on

set termguicolors
colorscheme morgana

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
" highlight CursorLine gui=none cterm=none ctermbg=0
" highlight ExtraWhitespace ctermbg=2
set listchars=tab:>-,trail:.,extends:>,precedes:<
set list
set guifont=Fira\ Code\ Retina\ Nerd\ Font\ Complete\ Mono:h12

let &titlestring = expand("%:@")
set title


if has('gui_running')
  set guioptions-=T       " remove toolbar
  set lines=999           " full height
  set columns=999         " full width
endif

let g:modelabel={
       \ 'n'  : 'NORMAL ',
       \ 'v'  : 'VISUAL ',
       \ 'V'  : 'V·Line ',
       \ "\<C-V>" : 'V·Block ',
       \ 'i'  : 'INSERT ',
       \ 'R'  : 'R ',
       \ 'Rv' : 'V·Replace ',
       \ 'c'  : 'Command ',
       \}

set laststatus=2
set statusline=
set statusline+=%#PmenuSel#
set statusline+=\ %{g:modelabel[mode()]}
set statusline+=%#CursorLine#
set statusline+=\ %f
set statusline+=%{&modified?'*':''}
set statusline+=\ %Y

runtime coc-config.vim

"# NvimTree
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_indent_markers = 1
let g:nvim_tree_add_trailing = 1
let g:nvim_tree_show_icons = {
    \ 'git': 1,
    \ 'folders': 1,
    \ 'files': 1,
    \ 'folder_arrows': 1,
    \ }

lua <<EOF
  require'nvim-tree'.setup {
    auto_close = true,
    open_on_setup = false,
    view = {
      width = 36
    },
    git = {
      enable = true,
      ignore = false,
      timeout = 400,
   },
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

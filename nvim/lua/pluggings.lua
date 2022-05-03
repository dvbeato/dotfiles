-- VIM PLUGGINS

local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.config/nvim/plugged')

  -- git plugins
  Plug 'tpope/vim-fugitive'
  Plug 'airblade/vim-gitgutter'

  -- tmux
  Plug 'preservim/vimux'

  -- misc plugins
  Plug('junegunn/fzf', { dir = '~/.fzf', ['do'] = vim.fn['fzf#install'] })
  Plug 'junegunn/fzf.vim'
  Plug 'stsewd/fzf-checkout.vim'

  Plug 'ntpeters/vim-better-whitespace'
  Plug 'ap/vim-css-color'
  Plug 'tpope/vim-surround'
  Plug 'liuchengxu/vim-which-key'

  -- IDE like plugins
  Plug('neoclide/coc.nvim', {branch = 'release'})
  Plug 'dense-analysis/ale'

  --#### LANG AND FRAMEWORKS
  -- Terraform
  Plug 'hashivim/vim-terraform'

  -- Html
  Plug 'mattn/emmet-vim'

  -- golang
  Plug('fatih/vim-go', {['do'] = vim.fn[':GoUpdateBinaries'] })

  -- clojure
  Plug('Olical/conjure', {tag = 'v4.30.1'})

  Plug 'guns/vim-clojure-highlight'

  --#### COSMETICS
  Plug('nvim-treesitter/nvim-treesitter', {['do'] = vim.fn[':TSUpdate']})  -- We recommend updating the parsers on update
  Plug 'p00f/nvim-ts-rainbow'

  Plug 'kyazdani42/nvim-web-devicons' -- for file icons
  Plug 'kyazdani42/nvim-tree.lua'

  Plug('dvbeato/morgana-theme', {branch = 'main'})

vim.call('plug#end')

-- NvimTree
require'nvim-tree'.setup {
  view = {
    width = 36
  },
  git = {
    enable = true,
    ignore = false,
    timeout = 400,
 },
}

-- NvimTreesitter
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

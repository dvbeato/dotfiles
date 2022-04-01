
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


vim.opt.termguicolors = true
vim.cmd('colorscheme morgana')

vim.opt.cursorline = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.autoindent = true

--vim.opt.clipboard+=unnamedplus
vim.opt.tabstop = 2               -- number of visual spaces per TAB
vim.opt.softtabstop = 2           -- number of spaces in TAB when editing
vim.opt.shiftwidth = 2
vim.opt.expandtab = true               -- all tabs are spaces
vim.opt.splitright = true              -- open split buffers at right
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.smarttab = true
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
vim.opt.linespace = 0             -- don't insert extra pixels between rows
vim.opt.number = true                  -- show linenumbers
vim.opt.cursorline = true              -- highlight current line
vim.opt.wildmenu = true                -- visual autocomplete for command menu
vim.opt.wrap = false
vim.opt.lazyredraw = true             -- redraw only when necessary
vim.opt.showmatch = true              -- highlight match {[()]}
vim.opt.incsearch = true              -- Find the next match as we type the search
vim.opt.hlsearch = true               -- highlight matches search
vim.opt.history = 100
vim.opt.backspace = "indent,eol,start"
vim.opt.swapfile = false


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

-- Keymap


local function map(mode, shortcut, command)
  vim.api.nvim_set_keymap(mode, shortcut, command, { noremap = true, silent = true })
end

-- Normal Map
local function nmap(shortcut, command)
  map('n', shortcut, command)
end

-- Terminal Map
local function tmap(shortcut, command)
  map('t', shortcut, command)
end

nmap("<SPACE>", "<Nop>")

vim.g.mapleader=" "
vim.g.maplocalleader=","

nmap("<C-k>", "<C-w>k")
nmap("<C-j>", "<C-w>j")
nmap("<C-l>", "<C-w>l")
nmap("<C-h>", "<C-w>h")

tmap("<ESC><ESC>", "<C-\\><C-N>")

nmap("<Leader>1", ":NvimTreeToggle<CR>")

nmap("<leader>ff", ":Files<CR>")
nmap("<leader>fb", ":Buffers<CR>")
nmap("<leader>fe", ":Rg<CR>")
nmap("<leader>fc", ":Commands<CR>")

nmap("<leader>gs", ":Git<CR>")
nmap("<leader>gb", ":Git blame<CR>")

nmap("<Leader>to", ":VimuxOpenRunner<CR>")
nmap("<Leader>tr", ":VimuxPromptCommand<CR>")

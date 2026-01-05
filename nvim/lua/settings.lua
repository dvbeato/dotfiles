vim.opt.autoindent   = true
vim.opt.backspace    = "indent,eol,start"
vim.opt.clipboard    = "unnamedplus"
vim.opt.cursorline   = true
vim.opt.cursorline   = true                 -- highlight current line
vim.opt.encoding     = 'utf-8'
vim.opt.expandtab    = true                 -- all tabs are spaces
vim.opt.fileencoding = 'utf-8'
vim.opt.filetype     = 'on'
vim.opt.hidden       = true
vim.opt.history      = 100
vim.opt.hlsearch     = true                 -- highlight matches search
vim.opt.incsearch    = true                 -- Find the next match as we type the search
vim.opt.lazyredraw   = true                 -- redraw only when necessary
vim.opt.linespace    = 0                    -- don't insert extra pixels between rows
vim.opt.number       = true                 -- show linenumbers
vim.opt.list         = true
vim.opt.shiftwidth   = 2
vim.opt.showmatch    = true                 -- highlight match {[()]}
vim.opt.showtabline  = 2
vim.opt.smartindent  = true
vim.opt.smarttab     = true
vim.opt.softtabstop  = 2
vim.opt.splitright   = true                 -- open split buffers at right
vim.opt.swapfile     = false
vim.opt.tabstop      = 2
vim.opt.updatetime   = 300
vim.opt.wildmenu     = true                 -- visual autocomplete for command menu
vim.opt.wrap         = false

vim.api.nvim_set_keymap('n', "<space>", "<nop>", { noremap = true, silent = true })
vim.g.mapleader=" "
vim.g.maplocalleader=","

vim.api.nvim_set_keymap('n', "<C-k>", "<C-w>k", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', "<C-j>", "<C-w>j", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', "<C-l>", "<C-w>l", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', "<C-h>", "<C-w>h", { noremap = true, silent = true })

vim.api.nvim_set_keymap('i', "jj", "<esc>", { noremap = true, silent = true })

vim.api.nvim_set_keymap('t', "<esc><esc>", "<C-\\><C-N>", { noremap = true, silent = true })

vim.cmd [[
  autocmd TermOpen * setlocal nonumber norelativenumber
]]

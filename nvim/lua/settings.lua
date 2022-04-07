-- VIM SETTINGS

local vim_set = vim.opt

vim_set.autoindent   = true
vim_set.backspace    = "indent,eol,start"
vim_set.cursorline   = true
vim_set.cursorline   = true                 -- highlight current line
vim_set.encoding     = 'utf-8'
vim_set.expandtab    = true                 -- all tabs are spaces
vim_set.fileencoding = 'utf-8'
vim_set.hidden       = true
vim_set.history      = 100
vim_set.hlsearch     = true                 -- highlight matches search
vim_set.incsearch    = true                 -- Find the next match as we type the search
vim_set.lazyredraw   = true                 -- redraw only when necessary
vim_set.linespace    = 0                    -- don't insert extra pixels between rows
vim_set.number       = true                 -- show linenumbers
vim_set.shiftwidth   = 2
vim_set.showmatch    = true                 -- highlight match {[()]}
vim_set.showtabline  = 2
vim_set.smartindent  = true
vim_set.smarttab     = true
vim_set.softtabstop  = 2
vim_set.splitright   = true                 -- open split buffers at right
vim_set.swapfile     = false
vim_set.tabstop      = 2
vim_set.updatetime   = 300
vim_set.wildmenu     = true                 -- visual autocomplete for command menu
vim_set.wrap         = false

vim.cmd [[
  autocmd TermOpen * setlocal nonumber norelativenumber
]]

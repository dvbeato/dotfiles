-- VIM KEYBINDS

local vim_api = vim.api
local vim_g = vim.g

local function noremap(m, shortcut, command)
  vim_api.nvim_set_keymap(m, shortcut, command, { noremap = true, silent = true })
end

local function map(m, shortcut, command)
  vim_api.nvim_set_keymap(m, shortcut, command, { noremap = false, silent = true })
end

-- Normal Map
local function nnoremap(shortcut, command)
  noremap('n', shortcut, command)
end

local function nmap(shortcut, command)
  map('n', shortcut, command)
end

-- Terminal Map
local function tmap(shortcut, command)
  noremap('t', shortcut, command)
end

nnoremap("<SPACE>", "<Nop>")

vim_g.mapleader=" "
vim_g.maplocalleader=","

-- Navigation
nnoremap("<C-k>", "<C-w>k")
nnoremap("<C-j>", "<C-w>j")
nnoremap("<C-l>", "<C-w>l")
nnoremap("<C-h>", "<C-w>h")

-- File Explorer
nnoremap("<Leader>1", ":NvimTreeToggle<CR>")

-- Find
nnoremap("<leader>ff", ":Files<CR>")
nnoremap("<leader>fb", ":Buffers<CR>")
nnoremap("<leader>fe", ":Rg<CR>")
nnoremap("<leader>fc", ":Commands<CR>")

-- Git
nnoremap("<leader>gs", ":Git<CR>")
nnoremap("<leader>gb", ":Git blame<CR>")

-- Terminal
nnoremap("<Leader>to", ":VimuxOpenRunner<CR>")
nnoremap("<Leader>tr", ":VimuxPromptCommand<CR>")
tmap("<ESC><ESC>", "<C-\\><C-N>")

-- Coc
nmap("gd", "<Plug>(coc-definition)")
nmap("gr", "<Plug>(coc-references)")
nmap("rn", "<Plug>(coc-rename)")

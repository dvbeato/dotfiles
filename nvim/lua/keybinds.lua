-- VIM KEYBINDS

local vim_api = vim.api
local vim_g = vim.g

local function merge(tableA, tableB)
  for k, v in pairs(tableB) do
    if k and v then
      tableA[k] = v
    end
  end
  return tableA
end

local function noremap(m, shortcut, command, opt)
  vim_api.nvim_set_keymap(m, shortcut, command, merge({ noremap = true, silent = true }, opt))
end

local function map(m, shortcut, command, opt)
  vim_api.nvim_set_keymap(m, shortcut, command, merge({ noremap = false, silent = true }, opt))
end

-- Normal Map
local function nnoremap(shortcut, command, opt)
  opt = opt or {}
  noremap('n', shortcut, command, opt)
end

local function inoremap(shortcut, command, opt)
  opt = opt or {}
  noremap('i', shortcut, command, opt)
end

local function nmap(shortcut, command, opt)
  opt = opt or {}
  map('n', shortcut, command, opt)
end

-- Terminal Map
local function tmap(shortcut, command, opt)
  opt = opt or {}
  noremap('t', shortcut, command, opt)
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

inoremap("<C-SPACE>", "coc#refresh()", {expr = true})

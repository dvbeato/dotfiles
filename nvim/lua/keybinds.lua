local function map(m, shortcut, command)
  vim.api.nvim_set_keymap(m, shortcut, command, { noremap = true, silent = true })
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

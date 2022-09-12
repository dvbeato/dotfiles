-- VIM STATUSLINE

local modes = {
  ["n"]  = "NORMAL",
  ["no"] = "NORMAL",
  ["v"]  = "VISUAL",
  ["V"]  = "VISUAL",
  [""] = "VISUAL",
  ["s"]  = "SELECT",
  ["S"]  = "SELECT",
  [""] = "SELECT",
  ["i"]  = "INSERT",
  ["ic"] = "INSERT",
  ["R"]  = "REPLCE",
  ["Rv"] = "VISUAL",
  ["c"]  = "COMMND",
  ["cv"] = "VIM EX",
  ["ce"] = "VIM EX",
  ["r"]  = "PROMPT",
  ["rm"] = "-MOAR-",
  ["r?"] = "CNFIRM",
  ["!"]  = "-SHLL-",
  ["t"]  = "I-TERM",
  ["nt"] = "N-TERM",
}

local api = vim.api
local fn = vim.fn

local function mode()
  local current_mode = api.nvim_get_mode().mode
  return string.format(" %s ", (modes[current_mode] or current_mode)):upper()
end

local function update_mode_colors()
  local current_mode = api.nvim_get_mode().mode
  local mode_color = "%#StatusLineAccent#"
  if current_mode == "n" then
      mode_color = "%#StatuslineAccent#"
  elseif current_mode == "i" or current_mode == "ic" then
      mode_color = "%#StatuslineInsertAccent#"
  elseif current_mode == "v" or current_mode == "V" or current_mode == "" then
      mode_color = "%#StatuslineVisualAccent#"
  elseif current_mode == "R" then
      mode_color = "%#StatuslineReplaceAccent#"
  elseif current_mode == "c" then
      mode_color = "%#StatuslineCmdLineAccent#"
  elseif current_mode == "t" then
      mode_color = "%#StatuslineTerminalAccent#"
  end
  return mode_color
end

local function show_modified()
  local bufnr = fn.bufnr()
  local bufmodified = fn.getbufvar(bufnr, '&mod')
  if bufmodified == 1 then
    return "*"
  else
    return ""
  end
end

local function filepath()
  local fpath = fn.fnamemodify(fn.expand "%", ":~:.:h")
  if fpath == "" or fpath == "." then
      return " "
  end

  return string.format(" %%<%s/", fpath)
end

local function filename()
  local fname = fn.expand "%:t"
  local modsignal = show_modified()
  if fname == "" then
      return ""
  end
  return fname .. modsignal .. " "
end

local function filetype()
  return string.format(" %s ", vim.bo.filetype):upper()
end

vim.g.coc_status_error_sign = "  "
vim.g.coc_status_warning_sign = "  "

local function cocStatus()
  local cocstatus = vim.fn["coc#status"]()
  return cocstatus
end

local function gitbranch()
  local branch = vim.fn["FugitiveHead"]()
  return "  " .. branch .. " "
end

Statusline = {}

Statusline.active = function()
  return table.concat {
    "%#Statusline#",
    update_mode_colors(),
    mode(),
    "%#StatusLineBranch#",
    gitbranch(),
    "%#StatusLineBranch#",
    "%#StatusLine#",
    filepath(),
    filename(),
    "%#StatusLine#",
    "%=%#StatusLineExtra#",
    cocStatus(),
    filetype(),
  }
end

function Statusline.inactive()
  return ""
end

vim.cmd [[
  augroup Statusline
  au!
  au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
  au WinLeave,BufLeave * setlocal statusline=%!v:lua.Statusline.inactive()
  au WinEnter,BufEnter,FileType NvimTree setlocal statusline=%!v:lua.Statusline.inactive()
  augroup END
]]

local function tabhi(index)
  if index == fn.tabpagenr() then
    return '%#TabLineSel#'
  else
    return '%#TabLine#'
  end
end

Tabline = {}

Tabline.active = function()
  local tline = {}

  for index = 1, fn.tabpagenr('$') do
    local winnr   = fn.tabpagewinnr(index)
    local buflist = fn.tabpagebuflist(index)
    local bufnr   = buflist[winnr]
    local rootpath = fn.fnamemodify(fn.getcwd(winnr, index),":t")
    local hili    = tabhi(index)

    local tab = {
      hili,
      rootpath,
      hili
    }

    table.insert(tline,  table.concat(tab, " "))
  end

  return table.concat(tline) .. '%#TabLineFill#'
end

vim.cmd [[
  set tabline=%!v:lua.Tabline.active()
]]


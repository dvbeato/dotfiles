-- VIM THEME

vim.opt.background = "dark"
vim.opt.termguicolors = true

local function bg(color)
  return string.format("guibg=%s", color)
end

local function fg(color)
  return string.format("guifg=%s", color)
end

local function hi(higroup, options)
  vim.cmd(string.format("highlight %s %s", higroup, table.concat(options, " ")))
end

--local blackDarker1 = "#171C1C"
--local blackDarker2 = "#092227"
--local black        = "#242a2e"
--local blackLight1  = "#2b363a"
--local blackLight2  = "#333D3D"
local blackDarker1 = "#111111"
local blackDarker2 = "#000000"
local black        = "#1c1c1c"
local blackLight1  = "#272727"
local blackLight2  = "#2D2D2D"
local gray         = "#77777A"
local white        = "#b4b4b4"
local red          = "#E15A60"
local green        = "#99c794"
local yellow       = "#FAC863"
local orange       = "#f3a451"
local blue         = "#73a6bb"
local magenta      = "#C594C5"
local cyan         = "#5FB3B3"

local background   = black
local foreground   = white
local comments     = gray
local cursorLine   = blackLight1
local visualSel    = blackLight2
local cursorlineNr = yellow

local s_preproc    = blue
local s_func       = blue
local s_statement  = magenta
local s_str     = green
local s_delimiter  = cyan
local s_identifier = magenta
local s_type       = foreground
local s_special    = orange
local s_number     = orange
local s_operator   = cyan
local s_boolean    = orange
local s_keyword    = magenta

hi("clear", {})
hi("Normal",            {bg(background), fg(foreground)})
hi("Error",             {bg(red)})
hi("Search",            {bg(orange),     fg(blackLight1)})
hi("Visual",            {bg(visualSel)})

-- Comment
hi("Comment",           {fg(comments)})
-- TODO
hi("Todo",              {bg(cyan),       fg(black)})
hi("NonText",           {fg(background)})

hi("EndOfBuffer",       {fg(background)})
hi("CursorLine",        {bg(cursorLine)})
hi("CursorLineNr",      {bg(cursorLine), fg(cursorlineNr)})
hi("CursorLineColumn",  {bg(cursorLine)})
hi("LineNr",            {fg(comments)})
hi("Folded",            {bg(blackDarker1), fg(gray)})

-- GUI
hi("VertSplit",                {bg(blackLight1), fg(background)})
hi("SignColumn",               {bg(background),  fg(comments)})
hi("StatusLine",               {bg(white),       fg(blackLight1)})
hi("StatusLineNC",             {"gui=underline", fg(blackLight1)})
hi("StatusLineAccent",         {bg(blue),        fg(black)})
hi("StatusLineBranch",         {bg(blackDarker1),fg(blue)})
hi("StatusLineInsertAccent",   {bg(green),       fg(black)})
hi("StatusLineVisualAccent",   {bg(magenta),     fg(black)})
hi("StatuslineCmdLineAccent",  {bg(yellow),      fg(black)})
hi("StatuslineTerminalAccent", {bg(blackDarker1),      fg(white)})
hi("StatusLineExtra",          {bg(gray),        fg(black)})
hi("TabLine",                  {bg(blackDarker1)})
hi("TabLineFill",              {bg(blackDarker1), fg(blackDarker1)})

-- AUTO COMPLETION
hi("Pmenu",        {bg(blackDarker1)})
hi("PmenuSel",     {bg(blackDarker1), fg(cyan)})

-- SYNTAX
hi("PreProc",      {fg(s_preproc)})
hi("Function",     {fg(s_func)})
hi("Statement",    {fg(s_statement)})
hi("String",       {fg(s_str)})
hi("Function",     {fg(s_func)})
hi("Delimiter",    {fg(s_delimiter)})
hi("Identifier",   {fg(s_identifier)})
hi("Type",         {fg(s_type)})
hi("Special",      {fg(s_special)})
hi("SpecialChar",  {fg(s_special)})
hi("Number",       {fg(s_number)})
hi("Operator",     {fg(s_operator)})
hi("Boolean",      {fg(s_boolean)})
hi("Keyword",      {fg(s_keyword)})

-- GitGutter
hi("GitGutterAdd",    {bg(background), fg(green)})
hi("GitGutterChange", {bg(background), fg(cyan)})
hi("GitGutterDelete", {bg(background), fg(red)})

-- NvimTree
hi("NvimTreeNormal",            {bg(blackDarker1), fg(white)})
hi("NvimTreeEndOfBuffer",       {fg(blackDarker1)})
hi("NvimTreeFolderName",        {fg(white)})
hi("NvimTreeFolderIcon",        {fg(blue)})
hi("NvimTreeOpenedFolderName",  {fg(white)})
hi("NvimTreeOpenedFile",        {fg(green)})

-- Coc
hi("CocUnusedHighlight", {bg("none"), fg(comments)})


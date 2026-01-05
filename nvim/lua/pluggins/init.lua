local utils   = require('utils')

local lazy    = require('pluggins.lazy')

local ui      = require('pluggins.ui')
local lsp     = require('pluggins.lsp')
local clojure = require('pluggins.clojure')
local golang  = require('pluggins.golang')
local webdev  = require('pluggins.webdev')
local devops  = require('pluggins.devops')


local git = {
  "airblade/vim-gitgutter",
  {
    "tpope/vim-fugitive",
    lazy=false,
    keys={
      {mode="n", "<leader>gs", ":Git<CR>"},
      {mode="n", "<leader>gb", ":Git blame<CR>"}
    }
  },
}

local tmux = {
  -- tmux integration
  "preservim/vimux",
}

local misc = {
  -- "vim-scripts/paredit.vim",
  { 
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {}
  },
  "tpope/vim-surround",
  "guns/vim-sexp",
  "tpope/vim-sexp-mappings-for-regular-people",
  "tpope/vim-repeat"
}

local treesitter = {
  {
    'nvim-treesitter/nvim-treesitter',
    cmd = "TSUpdate",
    dependencies = {
      "p00f/nvim-ts-rainbow"
    },
    config = function()
      require'nvim-treesitter.configs'.setup {
        ensure_installed = { "c", "lua", "clojure", "go", "javascript", "hcl", "python", "yaml", "markdown", "bash"},
        auto_install = true,
        highlight = {
          enable = true,              -- false will disable the whole extension
          -- disable = { "c", "rust" },  -- list of language that will be disabled
          -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
          -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
          -- Using this option may slow down your editor, and you may see some duplicate highlights.
          -- Instead of true it can also be a list of languages
--          additional_vim_regex_highlighting = false,
        },
        indent = {
         enable = true
        },
        rainbow = {
          enable = false,
          extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
          max_file_lines = 1000, -- Do not enable for files with more than n lines, int
          colors = {
            "#b4b4b4",
            "#FAC863",
            "#99c794",
            "#f3a451",
            "#73a6bb",
            "#C594C5",
          } -- table of hex strings
          --   termcolors = {} -- table of colour name strings
        }
      }
    end
  }
}

local pluggins = {}

utils.concat(pluggins, git)
utils.concat(pluggins, tmux)
utils.concat(pluggins, misc)
utils.concat(pluggins, treesitter)
utils.concat(pluggins, ui)
utils.concat(pluggins, lsp)
utils.concat(pluggins, clojure)
utils.concat(pluggins, golang)
utils.concat(pluggins, webdev)
utils.concat(pluggins, devops)

lazy.setup(pluggins)


-- VIM PLUGGINS

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local pluggins = {

  "folke/which-key.nvim",

  -- git 
  "tpope/vim-fugitive",
  "airblade/vim-gitgutter",

  -- tmux integration
  "preservim/vimux",

  "junegunn/goyo.vim",

  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v1.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {'williamboman/mason.nvim'},           -- Optional
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},         -- Required
      {'hrsh7th/cmp-nvim-lsp'},     -- Required
      {'hrsh7th/cmp-buffer'},       -- Optional
      {'hrsh7th/cmp-path'},         -- Optional
      {'saadparwaiz1/cmp_luasnip'}, -- Optional
      {'hrsh7th/cmp-nvim-lua'},     -- Optional

      -- status
      { 'j-hui/fidget.nvim',
        config=function()
          require"fidget".setup{}
        end}
    },

    config=function()
      local lsp = require('lsp-zero').preset({
        name = 'minimal',
        set_lsp_keymaps = true,
        manage_nvim_cmp = true,
        suggest_lsp_servers = false,
      })

      -- (Optional) Configure lua language server for neovim
      lsp.nvim_workspace()

      lsp.setup()
    end
  },

  { -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = false,
        --theme = 'onedark',
        component_separators = '|',
        section_separators = '',
      },
    },
  },

  --#### LANG AND FRAMEWORKS
  -- Terraform
  {'hashivim/vim-terraform',
  ft='hcl'},

  -- Html
  {'mattn/emmet-vim',
  ft='html' },

  -- golang
  {'fatih/vim-go', 
  ft='go',
  cmd = 'GoUpdateBinaries'},

  -- clojure
  {'Olical/conjure',
  ft='clojure'},

  -- Telescope
  {"nvim-telescope/telescope.nvim",
   tag = "0.1.0",
   dependencies = {
     "nvim-lua/plenary.nvim",
   }
  },

  {"kyazdani42/nvim-tree.lua",
  dependencies = {
    "kyazdani42/nvim-web-devicons", -- for file icons
  },
  config = function()

    require'nvim-tree'.setup {
      sync_root_with_cwd = true,
      open_on_setup = false,
      open_on_tab = true,
      view = {
        width = 36
      },
      git = {
        enable = true,
        ignore = false,
        timeout = 400,
      },
    }
  end
},

{'nvim-treesitter/nvim-treesitter', 
cmd = "TSUpdate",
config = function()
  require'nvim-treesitter.configs'.setup {
    ensure_installed = { "c", "lua", "clojure", "go", "javascript", "hcl"},
    auto_install = true,
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
  },  -- We recommend updating the parsers on update

  "p00f/nvim-ts-rainbow"
}

require("lazy").setup(pluggins)



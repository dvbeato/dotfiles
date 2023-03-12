local ui = {

  "folke/which-key.nvim",

  {
    "kyazdani42/nvim-tree.lua",
    dependencies = {
      "kyazdani42/nvim-web-devicons", -- for file icons
    },
    keys={
      {'<leader>1', ':NvimTreeToggle<CR>' }
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
  -- Telescope
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.0",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-file-browser.nvim"
    },
    keys = {
      {"<leader>ff", ":Telescope find_files<CR>"},
      {"<leader>fb", ":Telescope buffers<CR>"},
      {"<leader>fg", ":Telescope live_grep<CR>"},
      {"<leader>fc", ":Telescope commands<CR>"},
      {"<leader>of", ":Telescope file_browser<CR>"},
    },
    config=function()
      require('telescope').setup{}
      require('telescope').load_extension("file_browser")
    end
  },

  {
    'nvim-lualine/lualine.nvim',
    opts = {
      options = {
        icons_enabled = false,
        --theme = 'onedark',
        component_separators = '|',
        section_separators = '',
      },
    },
  },
  -- Theme
  {
    "catppuccin/nvim",
    name = "catppuccin",
    config=function()
      require('catppuccin').setup({
        flavour = "frappe", -- latte, frappe, macchiato, mocha
        background = { -- :h background
        light = "latte",
        dark = "mocha",
      },
      transparent_background = false,
      show_end_of_buffer = false, -- show the '~' characters after the end of buffers
      term_colors = false,
      dim_inactive = {
        enabled = false,
        shade = "dark",
        percentage = 0.15,
      },
      no_italic = false, -- Force no italic
      no_bold = false, -- Force no bold
      styles = {
        comments = { "italic" },
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
      },
      color_overrides = {
        frappe = {
          base = "#1c1e21",
          mantle = "#18191b",
          crust = "#111214",
          surface0 = "#292c31",
          surface1 = "#383c42",
        }
      },
      custom_highlights = {},
      integrations = {
        cmp = true,
        gitsigns = true,
        gitgutter = true,
        nvimtree = true,
        telescope = true,
        treesitter=true,
        notify = false,
        mini = false,
        -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
      }})
      vim.cmd.colorscheme 'catppuccin'
    end
  }
}

return ui

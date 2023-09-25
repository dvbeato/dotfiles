local lsp = {
  -- LSP Zero
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

      -- Snippets
      {'L3MON4D3/LuaSnip'},             -- Required
      {'rafamadriz/friendly-snippets'}, -- Optional

      -- status
      {
        'j-hui/fidget.nvim',
        tag = 'legacy',
        config=function()
          require"fidget".setup{}
        end
      }
    },

    config=function()
      local lsp = require('lsp-zero').preset({
        name = 'minimal',
        set_lsp_keymaps = true,
        manage_nvim_cmp = true,
        suggest_lsp_servers = false,
      })

      lsp.on_attach(function(client, bufnr)
        lsp.default_keymaps({buffer = bufnr})

        vim.keymap.set('n', 'rn', '<cmd>lua vim.lsp.buf.rename()<cr>', {buffer = true})
      end)
      -- (Optional) Configure lua language server for neovim
      lsp.nvim_workspace()

      lsp.setup()
    end
  },
  {
    'mfussenegger/nvim-dap',
    keys= {
      {"<Leader>B", function() require('dap').set_breakpoint() end },
      {"<Leader>b", function() require('dap').toggle_breakpoint() end },
      {"<F5>",      function() require('dap').continue() end },

      --  vim.keymap.set('n', '<F10>', function() require('dap').step_over() end)
      --  vim.keymap.set('n', '<F11>', function() require('dap').step_into() end)
      --  vim.keymap.set('n', '<F12>', function() require('dap').step_out() end)
      --  vim.keymap.set('n', '<Leader>lp', function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
      --  vim.keymap.set('n', '<Leader>dr', function() require('dap').repl.open() end)
      --  vim.keymap.set('n', '<Leader>dl', function() require('dap').run_last() end)
      --  vim.keymap.set({'n', 'v'}, '<Leader>dh', function()
      --    require('dap.ui.widgets').hover()
      --  end)
      --  vim.keymap.set({'n', 'v'}, '<Leader>dp', function()
      --    require('dap.ui.widgets').preview()
      --  end)
      --  vim.keymap.set('n', '<Leader>df', function()
      --    local widgets = require('dap.ui.widgets')
      --    widgets.centered_float(widgets.frames)
      --  end)
      --  vim.keymap.set('n', '<Leader>ds', function()
      --    local widgets = require('dap.ui.widgets')
      --    widgets.centered_float(widgets.scopes)
      --  end)
    },
    config=function()
      require('nvim-dap').setup{}
    end
  },
  {'rcarriga/nvim-dap-ui'},
}
return lsp

local lsp = {
  {
    "williamboman/mason-lspconfig.nvim",
    opts = {
      ensure_installed = {
        --web
        "html",
        "cssls",
        --langs
        "lua_ls",
        "ts_ls",
        "gopls",
        "clojure_lsp",
        --databases
        "postgres_lsp",
        --infra
        "ansiblels",
        "bashls",
        "docker-language-server",
        "terraformls",
      }
    },
    dependencies = {
      "neovim/nvim-lspconfig",
      {
        "mason-org/mason.nvim",
        opts = {},
      }
    }
  }
}

return lsp

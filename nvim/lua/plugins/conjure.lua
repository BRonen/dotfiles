return {
  {
    "Olical/conjure",
    ft = { "clojure" },
    lazy = true,
    init = function()
      -- Uncomment this to get verbose logging to help diagnose internal Conjure issues
      -- This is VERY helpful when reporting an issue with the project
      -- vim.g["conjure#debug"] = true
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "clojure" },
        callback = function()
          vim.g.maplocalleader = ","
        end,
      })
    end,
    dependencies = {
      "PaterJason/cmp-conjure",
      "tpope/vim-dispatch",
      "clojure-vim/vim-jack-in",
      "radenling/vim-dispatch-neovim",
    },
  },
  {
    "PaterJason/cmp-conjure",
    lazy = true,
    config = function()
      local cmp = require("cmp")
      local config = cmp.get_config()
      table.insert(config.sources, { name = "conjure" })
      return cmp.setup(config)
    end,
  },
}

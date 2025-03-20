return {
  "Verf/deepwhite.nvim", -- "catppuccin/nvim",
  lazy = false,
  priority = 1000,
  integrations = {
    treesitter = true,
  },
  config = function()
    -- vim.cmd.colorscheme "catppuccin-mocha"
    vim.cmd.colorscheme "deepwhite"
  end,
}

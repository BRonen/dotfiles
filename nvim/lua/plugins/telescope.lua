return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.8',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local builtin = require('telescope.builtin')
    vim.keymap.set('n', '<leader>f', builtin.find_files, { desc = "Find files" })
    vim.keymap.set('n', '<leader>g', builtin.live_grep, { desc = "Grep string" })
    vim.keymap.set('n', '<leader>b', builtin.buffers, { desc = "Buffers" })
    vim.keymap.set('n', '<leader>Fg', builtin.git_files, { desc = "Git files" })
    vim.keymap.set('n', '<leader>Fr', builtin.live_grep, { desc = "Live grep" })
    vim.keymap.set('n', '<leader>Fh', builtin.help_tags, { desc = "Help tags" })
    vim.keymap.set('n', '<leader>Ft', builtin.treesitter, { desc = "Treesitter" })
    require('telescope').setup{
      pickers = {
        find_files = { theme = "ivy" },
	live_grep = { theme = "ivy" },
	buffers = { theme = "ivy" },
      },
    }
  end,
}

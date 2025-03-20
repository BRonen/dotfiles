return {
  'mrcjkb/haskell-tools.nvim',
  version = '^4',
  lazy = false,
  config = function()
    local ht = require('haskell-tools')
    local bufnr = vim.api.nvim_get_current_buf()
    -- vim.keymap.set('n', '<leader>cl', vim.lsp.codelens.run, { desc = "Run Codelens" })
    vim.keymap.set('n', '<leader>rs', ht.hoogle.hoogle_signature, { desc = "Get Hoogle Signature" })
    vim.keymap.set('n', '<leader>re', ht.lsp.buf_eval_all, { desc = "Eval All Buffer Snippets" })
    vim.keymap.set('n', '<leader>rt', ht.repl.toggle, { desc = "Toggle Haskell Repl" })
    vim.keymap.set('n', '<leader>rr', ht.repl.reload, { desc = "Reload Haskell Repl" })
    vim.keymap.set('n', '<leader>rw', function ()
      vim.api.nvim_command('silent update')
      ht.repl.reload()
      local switch_split = vim.api.nvim_replace_termcodes('<C-w>wi', true, false, true)
      vim.api.nvim_feedkeys(switch_split, 'n', false)
    end)
    vim.keymap.set('n', '<leader>rf', function()
      ht.repl.toggle(vim.api.nvim_buf_get_name(0))
    end, { desc = "Toggle Buffer Repl" })
    vim.keymap.set('n', '<leader>rq', ht.repl.quit, { desc = "Quit Repl" })
  end
}

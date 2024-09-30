-- Notes
-- https://damien.pobel.fr/post/configure-neovim-vim-gf-javascript-import/

-- Open "2024 master" note
vim.keymap.set("n", "<F8>", ":cd ~/Documents/notes/ | e 202312302001\\ 2024\\ master.md<CR>")

-- Find word in file names
vim.keymap.set("n", "<leader>wf", ":FzfLua files query=<C-r><C-w><CR>")

-- Find word in text
vim.keymap.set("n", "<leader>wt", ":FzfLua live_grep search=<C-r><C-w><CR>")

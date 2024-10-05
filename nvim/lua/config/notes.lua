-- Notes
-- https://damien.pobel.fr/post/configure-neovim-vim-gf-javascript-import/

-- Open "2024 master" note
vim.keymap.set("n", "<F8>", ":cd ~/Documents/notes/ | e 202312302001\\ 2024\\ master.md<CR>")

-- Create new note with prompt (New Note)
vim.keymap.set("n", "<leader>nn", ":cd ~/Documents/notes/<CR>:e <C-r>=strftime(\"%Y%m%d%H%M\")<CR> .md<left><left><left>")

-- Insert filename (Complete File)
vim.keymap.set("n", "<leader>cf", ":FzfLua complete-file<CR>")

-- SEARCH

-- Find word in file names (Word in Files)
vim.keymap.set("n", "<leader>wf", ":FzfLua files query=<C-r><C-w><CR>")

-- Find word in text (Word in Text)
vim.keymap.set("n", "<leader>wt", ":FzfLua live_grep search=<C-r><C-w><CR>")

-- COPY ID

-- Copy file id (Id Yank)
vim.keymap.set("n", "<leader>iy", ":let @i = expand(\"%:t:r:s?\\\\s.*??\")<CR>")

-- Paste file id (Id Paste)
vim.keymap.set("n", "<leader>ip", "a§<ESC>\"ip")

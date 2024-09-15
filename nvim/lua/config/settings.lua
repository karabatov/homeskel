-- Leader key
vim.g.mapleader = ","
vim.g.maplocalleader = "\\"

-- Config file
vim.keymap.set("n", "<leader>ev", ":e $MYVIMRC<CR>")
vim.keymap.set("n", "<leader>rv", ":source $MYVIMRC<CR>")

-- Colorscheme
-- vim.o.termguicolors = true
vim.g.colorscheme = "desert"


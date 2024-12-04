-- Leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Config file
vim.keymap.set("n", "<leader>ev", ":e $MYVIMRC<CR>")
vim.keymap.set("n", "<leader>rv", ":source $MYVIMRC<CR>")

-- Colorscheme
vim.g.colorscheme = "desert"
-- vim.o.termguicolors = true -- Doesn't work on Terminal.app

-- Global options

vim.o.mouse = "a" -- Mouse usage in all modes
vim.o.syntax = "on"
vim.o.cursorline = true -- Highlight current line
vim.o.showmode = true -- Show mode in status line
vim.o.visualbell = true -- Bell visually
vim.o.langmenu = "none" -- Always English menus
vim.o.autochdir = false -- Change cwd to current file

-- Line numbers
vim.o.number = true
vim.o.relativenumber = true

-- Line wrapping
vim.o.linebreak = true -- Wrap on whole words
vim.o.wrap = true
vim.o.formatoptions = "qrn1"

-- Indentation
vim.o.autoindent = true
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true

-- File encodings
vim.o.fileencodings = "utf-8,cp1251,koi8-r,cp866"
vim.o.encoding = "utf-8"

-- Tab chars and trailing spaces with special characters \u2592\u2591 and \u2593
vim.o.lcs = "tab:▒░,trail:▓"
vim.o.list = true

-- Search
vim.o.incsearch = true
vim.o.showmatch = true
vim.o.hlsearch = true
vim.keymap.set("n", "<leader><space>", ":noh<CR>") -- Clear highlighting
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
        vim.highlight.on_yank { higroup="Visual", timeout=300 }
    end,
    desc = "Briefly highlight yanked text"
})

-- Projects
vim.o.exrc = true -- Enable loading of local .nvimrc and .exrc files

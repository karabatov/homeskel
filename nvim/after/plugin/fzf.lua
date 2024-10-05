require("fzf-lua").setup({
    fzf_colors = true
})

vim.keymap.set("n", "<C-p>", function()
        require("fzf-lua").files()
    end,
    { desc = "Fzf Files" }
)

vim.keymap.set("n", "<C-l>", function()
        require("fzf-lua").live_grep()
    end,
    { desc = "Fzf Live Grep" }
)

vim.keymap.set("n", "<leader>b", function()
        require("fzf-lua").buffers()
    end,
    { desc = "Fzf Buffers" }
)

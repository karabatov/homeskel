require("fzf-lua").setup({
    fzf_colors = true
})

vim.keymap.set("n", "<C-p>", function()
        require("fzf-lua").files({ multiprocess = false })
    end,
    { desc = "Fzf Files" }
)

vim.keymap.set("n", "<C-l>", function()
        require("fzf-lua").live_grep()
    end,
    { desc = "Fzf Live Grep" }
)

vim.keymap.set("n", "<leader>b", function()
        require("fzf-lua").buffers({ sort_lastused = false })
    end,
    { desc = "Fzf Buffers" }
)

local plugins = {
    { -- Neo-tree
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v3.x",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-tree/nvim-web-devicons",
            "MunifTanjim/nui.nvim",
            -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
        },
        lazy = false,
        after = "neotree",
    },

    { -- Zenburn
        "phha/zenburn.nvim",
    },

    { -- Fzf
        "ibhagwan/fzf-lua",
        -- optional for icon support
        dependencies = { "nvim-tree/nvim-web-devicons" },
        after = "fzf"
    },

    { -- Nvim-treesitter
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        after = "treesitter"
    },

    { -- vim-fugitive
        "tpope/vim-fugitive"
    },

    -- configure LazyVim
    {
        "folke/lazy.nvim",
        opts = {
            colorscheme = "desert",
        },
    }
}

-- Setup lazy.nvim
require("lazy").setup(plugins)


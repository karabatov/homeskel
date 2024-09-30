local configs = require("nvim-treesitter.configs")

configs.setup({
    ensure_installed = {
        "bash",
        "c",
        "cpp",
        "diff",
        "gdscript",
        "git_config",
        "gitcommit",
        "gitignore",
        "go",
        "gomod",
        "gosum",
        "json",
        "lua",
        "make",
        "markdown",
        "markdown_inline",
        "ssh_config",
        "toml",
        "vim",
        "vimdoc",
        "yaml"
    },
    sync_install = false,
    highlight = { enable = true },
    indent = { enable = true }
})

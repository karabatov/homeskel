if vim.g.neovide then
    -- Font
    vim.o.guifont = "0xProto,Symbols_Nerd_Font_Mono:h16:#e-subpixelantialias"

    -- Theme
    require("zenburn").setup()

    -- Padding
    vim.g.neovide_padding_top = 10
    vim.g.neovide_padding_bottom = 10
    vim.g.neovide_padding_right = 10
    vim.g.neovide_padding_left = 10

    -- Refresh rate when idle
    vim.g.neovide_refresh_rate_idle = 5

    -- Cursor
    vim.g.neovide_cursor_animation_length = 0.07
    vim.g.neovide_cursor_trail_size = 0.25

    -- Copy/paste
    vim.keymap.set('v', '<D-c>', '"+y') -- Copy
    vim.keymap.set('n', '<D-v>', '"+p') -- Paste normal mode
    vim.keymap.set('v', '<D-v>', '"+p') -- Paste visual mode
    vim.keymap.set('c', '<D-v>', '<C-r>+') -- Paste command mode
    vim.keymap.set('i', '<D-v>', '<ESC>l"+Pli') -- Paste insert mode
end

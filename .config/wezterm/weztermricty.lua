local wezterm = require('wezterm')
local mytable = require("lib/mystdlib").mytable
local theme = require('theme')
local configuration = require('configuration')

function font_with_fallback(name, params)
    local names = {name, "VictorMono Nerd Font Mono Bold Oblique", "Noto Color Emoji" , "JoyPixels"}
    return wezterm.font_with_fallback(names, params)
end

local cfg_misc = {
    -- OpenGL for GPU acceleration, Software for CPU
    front_end = "OpenGL",

    -- No updates, bleeding edge only
    check_for_updates = false,


    -- Font Stuff
    font = font_with_fallback("VictorMono Nerd Font Mono" , {bold = true , italic = true}),
    font_rules = {
        {
            italic = true,
            intensity="Bold",
            font = font_with_fallback("Ricty Nerd Font", {bold = true , italic = true})
        }, {
            italic = true,
            intensity = "Bold",
            font = font_with_fallback("Ricty Nerd Font",
                                      {bold = true, italic = true})
        },
        {
            intensity = "Bold",
            font = font_with_fallback("Ricty Nerd Font", {bold = true , italic = true})
        },
        {intensity = "Bold", font = font_with_fallback("Ricty Nerd Font")}
    },
    font_size = 12.0,
    font_shaper = "Harfbuzz",
    line_height = 1.0,
    freetype_load_target = "HorizontalLcd",
    freetype_render_target = "Normal",

    -- Cursor style
    default_cursor_style = "SteadyUnderline",

    -- X Bad
    enable_wayland = false,

    -- Pretty Colors
    bold_brightens_ansi_colors = true,

    --transparency
    window_background_opacity = 0.3,


    -- Get rid of close prompt
    window_close_confirmation = "NeverPrompt",

    -- Padding
    --window_padding = {left = 45, right = 45, top = 45, bottom = 45},

    -- No opacity
    inactive_pane_hsb = {saturation = 1.0, brightness = 1.0}
}

-- Colors
local cfg_colors = {colors = theme.colors}

-- Tab Style (like shape)
local cfg_tab_bar_style = theme.tab_style

-- Keys
local cfg_keys = configuration.keys

-- Merge everything and return
local config = mytable.merge_all(cfg_misc, cfg_colors, cfg_tab_bar_style,
                                 cfg_keys)
return config

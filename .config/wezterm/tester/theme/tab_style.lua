local wezterm = require('wezterm')

local tabs = {}
local tab_style = {}

-- Tab Bar Options
tabs.enable_tab_bar = true
tabs.hide_tab_bar_if_only_one_tab = true
tabs.show_tab_index_in_tab_bar = false
tabs.tab_max_width = 25

tab_style.inactive_tab_left = wezterm.format(
                                  {
        {Background = {Color = "#1a2026"}}, {Foreground = {Color = "#29343d"}},
        {Text = ""}
    })

tab_style.inactive_tab_right = wezterm.format(
                                   {
        {Background = {Color = "#1a2026"}}, {Foreground = {Color = "#29343d"}},
        {Text = " "}
    })

tab_style.active_tab_left = wezterm.format(
                                {
        {Background = {Color = "#1a2026"}}, {Foreground = {Color = "#3b4b58"}},
        {Text = ""}
    })

tab_style.active_tab_right = wezterm.format(
                                 {
        {Background = {Color = "#1a2026"}}, {Foreground = {Color = "#3b4b58"}},
        {Text = " "}
    })

tabs.tab_bar_style = tab_style

return tabs

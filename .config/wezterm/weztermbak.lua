local wezterm = require 'wezterm'

function font_with_fallback(name, params)
  local names = {name, 'Ricty Nerd Font'}

  return wezterm.font_with_fallback(names, params)
end

wezterm.on('plumb', function(window, pane)
  local sel = window:get_selection_text_for_pane(pane)
  local cwd = pane:get_current_working_dir()

  if cwd == nil or sel == nil then
    return
  end

  -- Extract current working directory from the file URI and make sure it
  -- actually contains an directory.
  cwd = cwd:gsub("^file://[^/]*", "")
  if #cwd == 0 then
    return
  end

  -- Trim whitespace from the selected text and make sure it's not empty.
  sel = sel:gsub("^%s*(.-)%s*$", "%1")
  if #sel == 0 then
    return
  end

  local success, stdout, stderr = wezterm.run_child_process({
    '/bin/sh',
    wezterm.config_dir .. '/bin/plumb',
    cwd,
    sel
  })

  if not success and #stderr > 0 then
    wezterm.log_error(stderr)
  end
end)

return {
  color_scheme = 'Afterglow',
  font = font_with_fallback('Ricty Nerd Font'),
  font_antialias = 'Subpixel',
  font_dirs = {'fonts'},
  font_rules = {
    {
      intensity = 'Bold',
      font = font_with_fallback('Ricty Nerd Font'),
    },
  },
  font_size = 13.0,
  keys = {
    {
      key = 'Insert',
      mods = 'SHIFT',
      action = 'Paste',
    },
    {
      key = 'c',
      mods = 'CTRL | SHIFT',
      action = 'Copy',
    },
    {
      key = 'v',
      mods = 'CTRL | SHIFT',
      action = 'Paste',
    },
    {
      key = 'x',
      mods = 'CTRL | SHIFT',
      action = 'ActivateCopyMode',
    },
    {
      key = 'z',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{EmitEvent='plumb'},
    },
    {
      key = 'PageUp',
      mods = 'SHIFT',
      action = wezterm.action{ScrollByPage=-1},
    },
    {
      key = 'PageDown',
      mods = 'SHIFT',
      action = wezterm.action{ScrollByPage=1},
    },
    {
      key = '+',
      mods = 'CTRL | SHIFT',
      action = 'IncreaseFontSize',
    },
    {
      key = '-',
      mods = 'CTRL | SHIFT',
      action = 'DecreaseFontSize',
    },
    {
      key = 't',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{SpawnTab='CurrentPaneDomain'},
    },
    {
      key = 'w',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{CloseCurrentTab={confirm=true}},
    },
    {
      key = 'LeftArrow',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{ActivateTabRelative=-1},
    },
    {
      key = 'RightArrow',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{ActivateTabRelative=1},
    },
    {
      key = 'PageUp',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{MoveTabRelative=-1},
    },
    {
      key = 'PageDown',
      mods = 'CTRL | SHIFT',
      action = wezterm.action{MoveTabRelative=1},
    },
  },
  mouse_bindings = {
    {
      event = {
        Down = {
          streak = 1,
          button = 'Left',
        },
      },
      mods = 'NONE',
      action = wezterm.action{SelectTextAtMouseCursor='Cell'},
    },
    {
      event = {
        Down = {
          streak = 2,
          button = 'Left',
        },
      },
      mods = 'NONE',
      action = wezterm.action{SelectTextAtMouseCursor='Word'},
    },
    {
      event = {
        Down = {
          streak = 3,
          button = 'Left',
        },
      },
      mods = 'NONE',
      action = wezterm.action{SelectTextAtMouseCursor='Line'},
    },
    {
      event = {
        Up = {
          streak = 1,
          button = 'Left',
        },
      },
      mods = 'NONE',
      action = wezterm.action{CompleteSelectionOrOpenLinkAtMouseCursor='PrimarySelection'},
    },
    {
      event = {
        Down = {
          streak = 1,
          button = 'Middle',
        },
      },
      mods = 'NONE',
      action = 'Paste',
    },
    {
      event = {
        Down = {
          streak = 1,
          button = 'Middle',
        },
      },
      mods = 'CTRL',
      action = wezterm.action{EmitEvent='plumb'},
    },
  },
}

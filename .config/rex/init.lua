-- Pane focus
rex.bind("cmd+ctrl+j", "pane.focus", { direction = "down" })
rex.bind("cmd+ctrl+k", "pane.focus", { direction = "up" })
rex.bind("cmd+ctrl+h", "pane.focus", { direction = "left" })
rex.bind("cmd+ctrl+l", "pane.focus", { direction = "right" })
rex.bind("cmd+ctrl+f", "pane.zoom")

-- Floats
rex.bind("cmd+p", "pane.new_floating")

---------------------------
-- Agentic import from Tmux
-- Migrated from tmux (~/.config/tmux/tmux.conf) and Herdr (~/.config/herdr/config.toml).

-- tmux: prefix ctrl+a
-- ctrl+a starts sequences, so pass it through on a double press.

rex.bind("ctrl+a>ctrl+a", "pane.send_key", { key = "ctrl+a" })
-- Tabs and splits
rex.bind("ctrl+a>c", "client.tab.new")
rex.bind("ctrl+a>b", "pane.split", { direction = "right" })
rex.bind("ctrl+a>v", "pane.split", { direction = "down" })
rex.bind("ctrl+a>shift+b", "pane.move_to_new_tab")
rex.bind("ctrl+a>r", "client.config.reload")

-- tmux `bind -r` bindings, as plain sequences
rex.bind("ctrl+a>left", "pane.resize", { amount = 1, direction = "left" })
rex.bind("ctrl+a>right", "pane.resize", { amount = 1, direction = "right" })
rex.bind("ctrl+a>down", "pane.resize", { amount = 1, direction = "down" })
rex.bind("ctrl+a>up", "pane.resize", { amount = 1, direction = "up" })
rex.bind("ctrl+a>tab", "client.tab.next")
rex.bind("ctrl+a>shift+tab", "client.tab.previous")

-- ...and in a repeat mode: ctrl+a ctrl+r enters, enter or escape leaves
rex.mode("repeat", { exclusive = true })
rex.bind("ctrl+a>ctrl+r", "client.mode.enter", { name = "repeat" })
rex.bind("repeat/enter", "client.mode.exit")
rex.bind("repeat/escape", "client.mode.exit")
rex.bind("repeat/left", "pane.resize", { amount = 1, direction = "left" })
rex.bind("repeat/right", "pane.resize", { amount = 1, direction = "right" })
rex.bind("repeat/down", "pane.resize", { amount = 1, direction = "down" })
rex.bind("repeat/up", "pane.resize", { amount = 1, direction = "up" })
rex.bind("repeat/tab", "client.tab.next")
rex.bind("repeat/shift+tab", "client.tab.previous")

-- Herdr: prefix ctrl+b
rex.bind("ctrl+b>ctrl+b", "pane.send_key", { key = "ctrl+b" })
rex.bind("ctrl+b>shift+n", "session.next")
rex.bind("ctrl+b>shift+p", "session.previous")

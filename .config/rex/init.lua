require("sessions")
local popup = require("popup")
local dispensary = require("dispensary")

rex.bind("cmd+ctrl+r", "client.config.reload")

-- Sessions
rex.bind("cmd+shift+l", "session_last")
rex.bind("cmd+k", "session_switch_tracked")
rex.bind("cmd+ctrl+4", "session.rename") -- '4' similar to '$' in tmux
dispensary.register("dispensary")
rex.bind("cmd+shift+k", "dispensary")

-- Tabs
rex.bind("cmd+ctrl+,", "client.tab.rename")

-- Panes
rex.bind("cmd+ctrl+j", "pane.focus", { direction = "down" })
rex.bind("cmd+ctrl+k", "pane.focus", { direction = "up" })
rex.bind("cmd+ctrl+h", "pane.focus", { direction = "left" })
rex.bind("cmd+ctrl+l", "pane.focus", { direction = "right" })
rex.bind("cmd+ctrl+f", "pane.zoom")
rex.bind("cmd+ctrl+1", "pane.move_to_new_tab") -- '1' similar to '!' in tmux

-- Floats
rex.bind("cmd+ctrl+p", "pane.new_floating")

popup.register("jjtui", "jjtui", { "jjtui" })
popup.register("prstack", "PR Stack", { "prstack" })
rex.bind("cmd+j", "jjtui")
rex.bind("cmd+p", "prstack")

---------------------------
-- Agentic import from Tmux
-- Migrated from tmux (~/.config/tmux/tmux.conf) and Herdr (~/.config/herdr/config.toml).

-- tmux: prefix ctrl+a
-- ctrl+a starts sequences, so pass it through on a double press.

rex.bind("ctrl+a>ctrl+a", "pane.send_key", { key = "ctrl+a" })

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

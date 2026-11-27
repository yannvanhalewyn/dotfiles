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
-- resizing
rex.bind("cmd+ctrl+left", "pane.resize", { amount = 1, direction = "left" })
rex.bind("cmd+ctrl+right", "pane.resize", { amount = 1, direction = "right" })
rex.bind("cmd+ctrl+down", "pane.resize", { amount = 1, direction = "down" })
rex.bind("cmd+ctrl+up", "pane.resize", { amount = 1, direction = "up" })

-- Floats
rex.bind("cmd+ctrl+p", "pane.new_floating")

popup.register("jjtui", "jjtui", { "jjtui" })
popup.register("prstack", "PR Stack", { "prstack" })
popup.register("yazi", "Yazi", { "yazi" })
rex.bind("cmd+j", "jjtui")
rex.bind("cmd+p", "prstack")
rex.bind("cmd+y", "yazi")

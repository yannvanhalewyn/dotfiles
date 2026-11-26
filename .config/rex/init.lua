rex.bind("cmd+ctrl+r", "client.config.reload")

-- Sessions

-- tmux-style `switch-client -l`: cmd+shift+l jumps to the session you came
-- from, so repeated presses toggle between two sessions. Rex has no event for
-- the app changing sessions, so the previous session is recorded by the
-- actions below (the toggle itself and cmd+k); sidebar clicks aren't seen.
local state_dir = os.getenv("HOME") .. "/.local/state/rex"
local last_session_file = state_dir .. "/last_session"
rex.exec({ "mkdir", "-p", state_dir })

local function read_last_session()
  local f = io.open(last_session_file, "r")
  if not f then return nil end
  local id = f:read("*l")
  f:close()
  return id
end

local function remember_session(id)
  if not id then return end
  local f = io.open(last_session_file, "w")
  if not f then return end
  f:write(id)
  f:close()
end

rex.action{
  name  = "session_last",
  title = "Go to Last Session",
  run   = function(ctx)
    local last = read_last_session()
    if last == nil or last == ctx.session_id then
      rex.client.queue("session.previous")
      remember_session(ctx.session_id)
      return
    end
    remember_session(ctx.session_id)
    rex.client.queue("session.select", { session_id = last })
  end,
}

rex.action{
  name  = "session_switch_tracked",
  title = "Change Session (remember last)",
  run   = function(ctx)
    remember_session(ctx.session_id)
    rex.client.queue("session.switch")
  end,
}

rex.bind("cmd+shift+l", "session_last")
rex.bind("cmd+k", "session_switch_tracked")

-- Pane focus
rex.bind("cmd+ctrl+j", "pane.focus", { direction = "down" })
rex.bind("cmd+ctrl+k", "pane.focus", { direction = "up" })
rex.bind("cmd+ctrl+h", "pane.focus", { direction = "left" })
rex.bind("cmd+ctrl+l", "pane.focus", { direction = "right" })
rex.bind("cmd+ctrl+f", "pane.zoom")

-- Floats
rex.bind("cmd+ctrl+p", "pane.new_floating")

-- The focused terminal's directory as a plain path, or nil when its shell
-- hasn't reported one. `pwd` answers with a file://host/path URL.
local function focused_dir()
  local result = rex.block.call("com.superlogical.terminal", "pwd", {})
  local url = type(result) == "table" and result.pwd or result
  if type(url) ~= "string" then return nil end

  local path = url:match("^file://[^/]*(/.*)$") or url
  if path:sub(1, 1) ~= "/" then return nil end
  return (path:gsub("%%(%x%x)", function(hex)
    return string.char(tonumber(hex, 16))
  end))
end

local function popup(name, title, command)
  rex.action{
    name = name,
    title = title,
    run = function(ctx)
      local _, err = rex.session.new_layer{
        bounds = { 0.025, 0.025, 0.95, 0.95 },
        focus  = true,
        layout = rex.layout.block{
          flavor  = "com.superlogical.terminal.shell",
          options = { cwd = focused_dir(), command = command },
        },
      }
      if err ~= nil then error(err, 0) end
    end,
  }
end

popup("jjtui", "jjtui", { "jjtui" })
popup("prstack", "PR Stack", { "prstack" })
rex.bind("cmd+j", "jjtui")
rex.bind("cmd+p", "prstack")

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

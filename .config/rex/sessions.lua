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


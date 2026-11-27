-- Jump back to where focus was before, like tmux's `switch-client -l` and
-- `last-window`. The server already keeps the registry: focus_history records
-- every focus change this user makes (app tab and session switches, CLI focus
-- moves), and focus_history.reverse steps back through it. Stepping back is
-- itself a focus change, so pressing the same key again returns to where you
-- started: repeated presses toggle between the two most recent places.
local M = {}

-- Step focus back to the most recent place `filter` allows and show it.
-- Returns false when the history holds no such place.
local function back(filter)
  local res, err = rex.call("focus_history.reverse", filter)
  if err ~= nil then error(err, 0) end
  if not res.reversed then return false end
  rex.client.queue("session.select", { session_id = res.session_id, window_id = res.window_id })
  return true
end

local function active_window(session_id)
  local view, err = rex.call("session.view", { session_id = session_id })
  if err ~= nil then error(err, 0) end
  return view.active_window_id
end

-- Most recent tab in the current session other than the one shown.
function M.tab(ctx)
  local filter = {
    session_id = ctx.session_id,
    not_window_id = active_window(ctx.session_id)
  }
  if not back(filter) then rex.client.queue("client.tab.previous") end
end

-- Most recent session other than the current one.
function M.session(ctx)
  if not back({ not_session_id = ctx.session_id }) then rex.client.queue("session.previous") end
end

function M.register()
  rex.action({ name = "tab_last", title = "Go to Last Tab", run = M.tab })
  rex.action({ name = "session_last", title = "Go to Last Session", run = M.session })
end

return M

local M = {}

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

function M.register(name, title, command)
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

return M

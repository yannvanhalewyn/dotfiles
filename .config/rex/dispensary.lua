local M = {}

local HOME = os.getenv("HOME")

local parent_dirs = {
  HOME .. "/spronq",
  HOME .. "/code",
  HOME .. "/repos",
}

local extra_dirs = {
  { dir = HOME .. "/dotfiles" },
  { dir = HOME .. "/mediquest" },
  { dir = HOME .. "/wolftrap" },
  { dir = HOME .. "/Library/Mobile Documents/iCloud~md~obsidian/Documents", name = "Obsidian" },
}

local function basename(path)
  return path:match("([^/]+)/*$")
end

local function short(path)
  return (path:gsub("^" .. HOME:gsub("%p", "%%%0") .. "/", ""))
end

-- Immediate subdirectories of `parent`, via find (always on PATH, unlike fd).
local function subdirs(parent)
  local res = rex.exec({ "find", parent, "-mindepth", "1", "-maxdepth", "1", "-type", "d", "-not", "-name", ".*" })
  local dirs = {}
  if not res.ok then return dirs end
  for line in res.stdout:gmatch("[^\n]+") do dirs[#dirs + 1] = line end
  table.sort(dirs)
  return dirs
end

-- label -> session_id for every running session.
local function sessions_by_label()
  local res, err = rex.call("session.list")
  if err ~= nil then error(err, 0) end
  local by_label = {}
  for _, s in ipairs(res.sessions or {}) do by_label[s.label] = s.session_id end
  return by_label
end

function M.projects()
  local projects, seen = {}, {}
  local function add(dir, name)
    if seen[dir] then return end
    seen[dir] = true
    projects[#projects + 1] = { dir = dir, name = name or basename(dir) }
  end
  for _, e in ipairs(extra_dirs) do add(e.dir, e.name) end
  for _, parent in ipairs(parent_dirs) do
    for _, dir in ipairs(subdirs(parent)) do add(dir) end
  end
  return projects
end

-- rex.ui.pick answers with the chosen id; accept the table forms too.
local function picked_id(answer)
  if type(answer) ~= "table" then return answer end
  return answer.id or (answer.ids and answer.ids[1]) or answer[1]
end

function M.run(ctx)
  local open = sessions_by_label()
  local projects = M.projects()

  local items = {}
  for i, p in ipairs(projects) do
    items[i] = {
      id       = p.dir,
      title    = p.name,
      subtitle = short(p.dir:match("(.*)/[^/]+/*$") or p.dir),
      icon     = open[p.name] and "terminal" or "folder",
      detail   = open[p.name] and "open" or nil,
    }
  end

  local answer, err = rex.ui.pick{ title = "Select project", confirm = "Open", items = items }
  if err ~= nil then
    if err == "cancelled" then return end
    error(err, 0)
  end

  local dir = picked_id(answer)
  if dir == nil then return end
  local name
  for _, p in ipairs(projects) do
    if p.dir == dir then name = p.name break end
  end

  local session_id = open[name]
  if session_id == nil then
    local created, cerr = rex.call("session.create", {
      label = name,
      initial_windows = { {
        window_label = name,
        layout = rex.layout.block{
          flavor  = "com.superlogical.terminal.shell",
          options = { cwd = dir },
        },
      } },
    })
    if cerr ~= nil then error(cerr, 0) end
    session_id = created.session_id
  end

  rex.client.queue("session.select", { session_id = session_id })
end

function M.register(name)
  rex.action{ name = name, title = "Open Project Session (dispensary)", run = M.run }
end

return M

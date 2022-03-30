-- Useful docs / functions:
-- https://www.hammerspoon.org/docs/hs.fnutils.html
hs.window.animationDuration = 0

function moveWindowToNextScreen(toEast)
  local window = hs.window.focusedWindow()
  local screen = window:screen()
  if toEast then
    screen = screen:toEast()
  else
    screen = screen:toWest()
  end
  window:moveToScreen(screen)
end

function resizeToScreen(f)
  local window = hs.window.focusedWindow()
  local frame = hs.window.focusedWindow():frame()
  local max = hs.window.focusedWindow():screen():frame()

  local new_frame = f(frame, max)
  hs.window.focusedWindow():setFrame(new_frame)
end

--------------------------------------------------------------------------------
-- Debug

hs.hotkey.bind({"cmd", "alt", "shift"}, "/", function()
    local window = hs.window.focusedWindow()
    local frame = window:frame()
    local screen = window:screen()
    local screen_frame = screen:frame()
    local application = window:application()
    hs.alert.show(string.format(
            "%s\t\t[%s, %s], [%s, %s]\n%s\t\t[%s, %s]",
            application:name(), frame.x, frame.y, frame.w, frame.h,
            screen:name(), screen_frame.w, screen_frame.h
        ),
    10)
end)

--------------------------------------------------------------------------------
-- Moving between screens

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "left", function()
    moveWindowToNextScreen(false)
    -- hs.window.focusedWindow():moveOneScreenWest(false, true, 0)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "right", function()
    moveWindowToNextScreen(true)
    -- hs.window.focusedWindow():moveOneScreenEast(false, true, 0)
end)

--------------------------------------------------------------------------------
-- Halves

hs.hotkey.bind({"cmd", "alt"}, "left", function()
    resizeToScreen(function(frame, max)
      frame.x = max.x
      frame.y = max.y
      frame.w = max.w / 2
      frame.h = max.h
      return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt"}, "right", function()
    resizeToScreen(function(frame, max)
      frame.x = max.x + max.w / 2
      frame.y = max.y
      frame.w = max.w / 2
      frame.h = max.h
      return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt"}, "up", function()
    resizeToScreen(function(frame, max)
      frame.x = max.x
      frame.y = max.y
      frame.w = max.w
      frame.h = max.h / 2
      return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt"}, "down", function()
    resizeToScreen(function(frame, max)
      frame.x = max.x 
      frame.y = max.y + max.h / 2
      frame.w = max.w
      frame.h = max.h / 2
      return frame
    end)
end)

--------------------------------------------------------------------------------
-- Full

hs.hotkey.bind({"cmd", "alt"}, "f", function()
    resizeToScreen(function(frame, max) return max end)
end)

--------------------------------------------------------------------------------
-- Thirds

hs.hotkey.bind({"cmd", "alt"}, "1", function()
    resizeToScreen(function(frame, max)
        frame.x = max.x
        frame.y = max.y
        frame.w = max.w / 3
        frame.h = max.h
        return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt", "shift"}, "1", function()
    resizeToScreen(function(frame, max)
        frame.x = max.x
        frame.y = max.y
        frame.w = max.w * 2 / 3
        frame.h = max.h
        return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt"}, "2", function()
    resizeToScreen(function(frame, max)
        frame.x = max.x + (max.w / 3)
        frame.y = max.y
        frame.w = max.w / 3
        frame.h = max.h
        return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt"}, "3", function()
    resizeToScreen(function(frame, max)
        frame.x = max.x + (max.w * 2 / 3)
        frame.y = max.y
        frame.w = max.w / 3
        frame.h = max.h
        return frame
    end)
end)

hs.hotkey.bind({"cmd", "alt", "shift"}, "3", function()
    resizeToScreen(function(frame, max)
        frame.x = max.x + (max.w / 3)
        frame.y = max.y
        frame.w = max.w * 2 / 3
        frame.h = max.h
        return frame
    end)
end)


-- 7 for 720p, resizes the window to fit 720p screen-recording
hs.hotkey.bind({"cmd", "alt"}, "7", function()
    resizeToScreen(function(frame, max)
        frame.x = max.x + ((max.w - 1280) / 2)
        frame.y = max.y + ((max.h - 720) / 2)
        frame.w = 1280
        frame.h = 720
        return frame
    end)
end)


-- hs.hotkey.bind({"cmd", "alt"}, "1", function()
--     local all = hs.screen.allScreens()
--     local primary = hs.screen.primaryScreen()
--     local secondary = all[#all]

--     local windowLayout = {
--         {"iTerm2",         nil, primary,   hs.layout.maximized,    nil, nil},
--         {"Google Chrome",  nil, secondary, hs.layout.left50,    nil, nil},
--         {"Emacs",          nil, secondary, hs.layout.right50,   nil, nil},
--     }
--     hs.layout.apply(windowLayout)
-- end)


--------------------------------------------------------------------------------
-- Mouse button toggle

function toggleButton(buttonCode)
  return function()
    local application = hs.application.frontmostApplication()
    local mousePosition = hs.mouse.absolutePosition()
    eventType = hs.eventtap.event.types[buttonCode .. "Down"]
    event = hs.eventtap.event.newMouseEvent(eventType, mousePosition, {})
    event:post()
  end
end

hs.hotkey.bind({}, "f10", toggleButton("rightMouse"))

function keyStrokes(strings)
  return function()
    -- hs.eventtap.keyStrokes is cleaner but won't submit strokes to minecraft
    -- for some reason
    (strings):gsub(".", function(c)
        hs.eventtap.keyStroke({}, c)
    end)
  end
end

function keyStroke(to)
  return function()
    print("keycode", to)
    hs.eventtap.keyStroke({}, to)
  end
end

function all(fns)
  return function()
    for i, f in ipairs(fns) do
      f()
    end
  end
end

hs.hotkey.bind({}, "f12", all({
      keyStroke("/"),
      keyStroke("up"),
      keyStroke("return")
}))

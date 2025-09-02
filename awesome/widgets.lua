pcall(require, "luarocks.loader")
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")

-- ---------------------------------------------------
--  UTILS
-- ---------------------------------------------------

local function mix(color1, color2, ratio)
   ratio = ratio or 0.5
   local r1, g1, b1 = color1:match("#(%x%x)(%x%x)(%x%x)")
   local r2, g2, b2 = color2:match("#(%x%x)(%x%x)(%x%x)")
   
   r1, g1, b1 = tonumber(r1, 16), tonumber(g1, 16), tonumber(b1, 16)
   r2, g2, b2 = tonumber(r2, 16), tonumber(g2, 16), tonumber(b2, 16)
   
   local r = math.floor(r1 * (1 - ratio) + r2 * ratio)
   local g = math.floor(g1 * (1 - ratio) + g2 * ratio)
   local b = math.floor(b1 * (1 - ratio) + b2 * ratio)
   
   return string.format("#%02X%02X%02X", r, g, b)
end


local function get_rounded_rect(roundess)
   local rounded_rect = function(cr, w, h)
      gears.shape.rounded_rect(cr, w, h, roundess)
   end
   return rounded_rect
end

local function get_volumetric_border_gradient(height, width)
   local volumetric_border_gradient = {
      type = "linear",
      from = {0, 0},
      to =  {height, width},
      stops = {
	 {0, "#FFFFFFAA"},
	 {0.1, "#FFFFFFAA"},
	 {1, "#222222AA"}
      }
   }
   return volumetric_border_gradient
end

local function get_volumetric_plane_gradient(x, y, color)
   local volumetric_plane_gradient = {
      type = "linear",
      from = {0, 0},
      to = {x, y},
      stops = {
	 {0, mix(color, "#FFFFFF", 0.3)},
	 {0.5, color},
	 {1, mix(color, "#000000", 0.3)}
      }
   }
   return volumetric_plane_gradient 
end

function set_wallpaper(s)
   -- Wallpaper
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
	 wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end


-- ---------------------------------------------------
--  KEYBOARD LAYOUT
-- ---------------------------------------------------
mykeyboardlayout_wrapped =
   awful.widget.keyboardlayout

-- wibox.widget {
--    { widget = wibox.container.margin,
--      left = 1.5,
--      right = 1.5,
--      awful.widget.keyboardlayout,
--    },
--    widget = wibox.container.background,

--    border_width = 1,
--    border_color = get_volumetric_border_gradient(40,30),

--    shape = get_rounded_rect(10),

--    bg = get_volumetric_plane_gradient(10, 30, beautiful.taglist_bg_occupied),
-- }


-- ---------------------------------------------------
--  CLOCK
-- ---------------------------------------------------
mytextclock_wrapped =
   wibox.widget.textclock

-- wibox.widget {
--    { widget = wibox.container.margin,
--      left = 1.5,
--      right = 1.5,
--      wibox.widget.textclock,
--    },
--    widget = wibox.container.background,

--    shape = get_rounded_rect(10),

--    border_width = 1,
--    border_color = get_volumetric_border_gradient(40, 30),

--    bg = get_volumetric_plane_gradient(5, 40, beautiful.taglist_bg_occupied),
-- }


-- ---------------------------------------------------
--  SEPARATOR
-- ---------------------------------------------------
separator = wibox.widget.separator {
   orientation = 'vertical',
   forced_width = 5,
   color = "#FF00FF00"
}


-- ---------------------------------------------------
--  MENU
-- ---------------------------------------------------
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu( {
      items = {
	 { "awesome", myawesomemenu, beautiful.awesome_icon },
	 { "open terminal", terminal }
      }
} )

mylauncher = awful.widget.launcher(
   { image = beautiful.awesome_icon,
     menu = mymainmenu } )

menubar.utils.terminal = terminal


-- ---------------------------------------------------
--  TASKLIST
-- ---------------------------------------------------
local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 if c == client.focus then
	    c.minimized = true
	 else
	    c:emit_signal(
	       "request::activate",
	       "tasklist",
	       {raise = true}
	    )
	 end
   end),
   awful.button({ }, 3, function()
	 awful.menu.client_list({ theme = { width = 250 } })
   end),
   awful.button({ }, 4, function ()
	 awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
	 awful.client.focus.byidx(-1)
end))

local function create_tasklist(s)
   local tasklist = awful.widget.tasklist {
      screen  = s,
      filter  = awful.widget.tasklist.filter.currenttags,
      buttons = tasklist_buttons
   }
   return 
      tasklist
end


-- ---------------------------------------------------
--  TAGLIST
-- ---------------------------------------------------
local taglist_buttons = gears.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
	 if client.focus then
	    client.focus:move_to_tag(t)
	 end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
	 if client.focus then
	    client.focus:toggle_tag(t)
	 end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)


beautiful.taglist_shape = function(cr, w, h)
   gears.shape.rounded_rect(cr, w, h, 10 )
end

beautiful.taglist_font = beautiful.font
beautiful.taglist_spacing = beautiful.useless_gap

beautiful.taglist_bg_focus = {
   type = "linear",
   from = {10, 30},
   to = {0, 0},
   stops = {
      {0, mix(beautiful.bg_focus, "#FFFFFF", 0.9)},
      {0.5, beautiful.bg_focus},
      {1, mix(beautiful.bg_focus, "#000000", 0.4)},
   }
}

beautiful.taglist_bg_occupied = {
   type = "linear",
   from = {0, 0},
   to = {10, 30},
   stops = {
      {0, mix(beautiful.taglist_bg_occupied, "#FFFFFF", 0.3)},
      {0.5, beautiful.taglist_bg_occupied},
      {1, mix(beautiful.taglist_bg_occupied, "#000000", 0.3)}
   } 
}

beautiful.taglist_bg_empty = {
   type = "linear",
   from = {0, 0},
   to = {0, 15},
   stops = {
      {0, mix(beautiful.taglist_bg_empty, "#FFFFFF", 0.05)},
      {1, beautiful.taglist_bg_empty}
   }
}

beautiful.taglist_shape_border_width = 1

beautiful.taglist_shape_border_color = {
   type = "linear",
   from = {0, 0},
   to = {40, 30},
   stops = {
      {0, "#FFFFFF"},
      {0.1, "#FFFFFF"},
      {1, "#000000"}
   }
}
beautiful.taglist_shape_border_color_focus = {
   type = "linear",
   from = {0, 0},
   to = {40, 30},
   stops = {
      {0, "#FFFFFF"},
      {0.1, "#FFFFFF"},
      {1, "#000000"}
   }
}

beautiful.taglist_shape_border_color_occupied = beautiful.taglist_bg_occupied 
beautiful.taglist_shape_border_color_empty = "#888888"
beautiful.taglist_shape_border_color_urgent = "#FF5555"

local function create_taglist(s, filter, buttons)
   return awful.widget.taglist {
      screen  = s,
      filter  = awful.widget.taglist.filter.noempty,
      buttons = buttons or taglist_buttons,
      shadow = true,
      widget_template = {
	 {
	    {
	       id = 'text_role',
	       widget = wibox.widget.textbox,
	    },
	    margins = 8,
	    widget = wibox.container.margin,
	 },
	 id = 'background_role',
	 widget = wibox.container.background,
	 shape = beautiful.taglist_shape,
	 
      },
   }
end


-- ---------------------------------------------------
--  EACHSCREEN
-- ---------------------------------------------------
awful.screen.connect_for_each_screen(function(s)
      set_wallpaper(s)
      awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

      s.mylayoutbox = awful.widget.layoutbox(s)
      
      s.mylayoutbox:buttons(
	 gears.table.join(
	    awful.button({ }, 1, function () awful.layout.inc( 1) end),
	    awful.button({ }, 3, function () awful.layout.inc(-1) end),
	    awful.button({ }, 4, function () awful.layout.inc( 1) end),
	    awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      
      s.mytaglist = create_taglist(s)
      s.mytasklist = create_tasklist(s) 
      s.mypromptbox = awful.widget.prompt()

      s.mywibox = awful.wibar({ position = "top", screen = s, })

      s.mywibox:setup {
	 layout = wibox.layout.align.horizontal,
	 { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            -- mylauncher,
            -- s.mytaglist,
	 },
	 s.mytasklist, -- Middle widget
	 { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(), separator,
	    s.mytaglist,            separator,
            mykeyboardlayout_wrapped,       separator,
	    mytextclock_wrapped,            separator,
            s.mylayoutbox,          separator,
            s.mypromptbox,
	    -- mylauncher,             separator
	 },
      }
end)



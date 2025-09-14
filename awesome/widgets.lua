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

-- ---------------------------------------------------
--  CLOCK
-- ---------------------------------------------------
mytextclock_wrapped =
   wibox.widget.textclock

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
      buttons = tasklist_buttons,
   }
   return 
      tasklist
end

-- ---------------------------------------------------
--  TAGLIST
-- ---------------------------------------------------

beautiful.taglist_shape = function(cr, w, h)
   gears.shape.
      rectangle(cr, w, h)
end

beautiful.taglist_spacing = beautiful.useless_gap

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

local function create_taglist(s, filter, buttons)
   local taglist = awful.widget.taglist {
      screen  = s,
      filter  = awful.widget.taglist.filter.all,
      buttons = buttons or taglist_buttons,
      widget_template = {
	 {
	    {
	       id = 'text_role',
	       widget = wibox.widget.textbox,
	    },
	    margins = 4,
	    widget = wibox.container.margin,
	 },
	 id = 'background_role',
	 widget = wibox.container.background,
      },
   }
   return
      taglist
end

-- ---------------------------------------------------
--  EACHSCREEN
-- ---------------------------------------------------
local gap_y = beautiful.useless_gap * 2
local gap_accum_x = 0

awful.screen.connect_for_each_screen(function(s)

      menubar.geometry = {
	 x = beautiful.useless_gap * 2,
	 y = beautiful.useless_gap * 4 + 32,
	 width = s.geometry.width - beautiful.useless_gap * 4,
	 height = 32
      }
      beautiful.menubar_border_width = 0

      set_wallpaper(s)
      awful.tag({ "1", "2", "3", "4", "5", }, s, awful.layout.layouts[1])

      s.mylayoutbox =
	 awful.widget.layoutbox(s)
      
      s.mylayoutbox:buttons(
	 gears.table.join(
	    awful.button({ }, 1, function () awful.layout.inc( 1) end),
	    awful.button({ }, 3, function () awful.layout.inc(-1) end),
	    awful.button({ }, 4, function () awful.layout.inc( 1) end),
	    awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      
      s.mytaglist = create_taglist(s)
      s.mytasklist = create_tasklist(s) 
      s.mypromptbox = awful.widget.prompt()

      -- layoutbox
      s.wibox_layoutbox = wibox ({
	    screen = s,
	    width = 32,
	    height = 32,
	    shape = gears.shape.rect,
	    visible = true,
      })
      gap_accum_x = gap_accum_x + ( s.wibox_layoutbox.width + beautiful.useless_gap * 2 ) 
      s.wibox_layoutbox:geometry ({
	    x = s.geometry.width - gap_accum_x,
	    y = gap_y,
      })
      s.wibox_layoutbox:struts({
	    top = 32 + beautiful.useless_gap * 2,
      })
      s.wibox_layoutbox:setup ({
	    s.mylayoutbox,
	    halign = "center",
	    widget = wibox.container.place
      })


      -- clock
      s.wibox_clock = wibox ({
	    screen = s,
	    width = 196,
	    height = 32,
	    shape = gears.shape.rect,
	    visible = true,
      })
      gap_accum_x = gap_accum_x + ( s.wibox_clock.width + beautiful.useless_gap * 2 ) 
      s.wibox_clock:geometry ({
	    x = s.geometry.width - gap_accum_x,
	    y = gap_y,
      })
      s.wibox_clock:struts({
	    top = 32 + beautiful.useless_gap * 2,
      })
      s.wibox_clock:setup ({
	    mytextclock_wrapped,
	    halign = "center",
	    widget = wibox.container.place
      })


      -- keyboardlayout
      s.wibox_keyboardlayout = wibox ({
	    screen = s,
	    width = 32,
	    height = 32,
	    shape = gears.shape.rect,
	    visible = true,
     })
      gap_accum_x = gap_accum_x + ( s.wibox_keyboardlayout.width + beautiful.useless_gap * 2 ) 
      s.wibox_keyboardlayout:geometry ({
	    x = s.geometry.width - gap_accum_x,
	    y = gap_y,
      })
      s.wibox_keyboardlayout:struts({
	    top = 32 + beautiful.useless_gap * 2,
      })
      s.wibox_keyboardlayout:setup ({
	    mykeyboardlayout_wrapped,
	    halign = "center",
	    widget = wibox.container.place
      })

      
      -- taglist
      s.wibox_taglist = wibox ({
	    screen = s,
	    width = 128,
	    height = 32,
	    shape = gears.shape.rect,
	    visible = true,
      })
      gap_accum_x = gap_accum_x + ( s.wibox_taglist.width + beautiful.useless_gap * 2 ) 
      s.wibox_taglist:geometry ({
	    x = s.geometry.width - gap_accum_x,
	    y = gap_y,
      })
      s.wibox_taglist:struts({
	    top = 32 + beautiful.useless_gap * 2,
      })
      
      s.wibox_taglist:setup ({
	    s.mytaglist,
	    halign = "center",
	    widget = wibox.container.place
      })


      -- tasklist
      s.wibox_tasklist = wibox ({
	    screen = s,
	    width = s.geometry.width - gap_accum_x - beautiful.useless_gap * 4,
	    height = 32,
	    shape = gears.shape.rect,
	    visible = true,
      })
      gap_accum_x = gap_accum_x + ( s.wibox_tasklist.width + beautiful.useless_gap * 2 ) 
      s.wibox_tasklist:geometry ({
	    x = s.geometry.width - gap_accum_x,
	    y = gap_y,
      })
      s.wibox_tasklist:struts({
	    top = 32 + beautiful.useless_gap * 2,
      })
      s.wibox_tasklist:setup ({
	    space,
	    s.mytasklist,
	    layout = wibox.layout.align.horizontal,
      })

end)



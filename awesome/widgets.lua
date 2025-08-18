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
--  WIDGETS ( EACHSCREEN )
-- ---------------------------------------------------

mykeyboardlayout = awful.widget.keyboardlayout()
mytextclock      = wibox.widget.textclock()

separator = wibox.widget.separator {
   orientation = 'vertical',
   forced_width = 21,
}


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
    return awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }
end



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
    return awful.widget.taglist {
        screen  = s,
        filter  = filter or awful.widget.taglist.filter.all,
        buttons = buttons or taglist_buttons
    }
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

      s.mywibox = awful.wibar({ position = "top", screen = s })

      s.mywibox:setup {
	 layout = wibox.layout.align.horizontal,
	 { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            -- mylauncher,
            -- s.mytaglist,
            -- s.mypromptbox,
	 },
	 s.mytasklist, -- Middle widget
	 { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(), separator,
	    s.mytaglist,            separator,
            mykeyboardlayout,       separator,
	    mytextclock,            separator,
            s.mylayoutbox,          separator,
	    -- mylauncher,             separator
	 },
      }
end)



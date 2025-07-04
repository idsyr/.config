-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

local lain = require("lain")


-- ---------------------------------------------------
--  ERORR HANDLING
-- ---------------------------------------------------
if awesome.startup_errors then
   naughty.notify({
	 preset = naughty.config.presets.critical,
	 title = "Oops, there were errors during startup!",
	 text = awesome.startup_errors })
end

do
   local in_error = false
   awesome.connect_signal(
      "debug::error", function (err)
	 if in_error then return end
	 in_error = true

	 naughty.notify({
	       preset = naughty.config.presets.critical,
	       title = "Oops, an error happened!",
	       text = tostring(err) })
	 in_error = false
   end)
end



-- ---------------------------------------------------
--  VARIABLE DEFINITIONS
-- ---------------------------------------------------
beautiful.init("/home/ids/.config/awesome/themes/theme.lua")
terminal = "alacritty"
editor = os.getenv("EDITOR") or "emacs"
editor_cmd = terminal .. " -e " .. editor
modkey = "Mod4"

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.corner.nw,
    awful.layout.suit.fair,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}

local widget_font = "3270 Nerd Font Mono 14"


-- ---------------------------------------------------
--  LAUNCHER WIDGET + MAIN MENU (unused)
-- ---------------------------------------------------
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({
      items = {
	 { "awesome", myawesomemenu, beautiful.awesome_icon },
	 { "open terminal", terminal }
      }
})

mylauncher = awful.widget.launcher({
      image = beautiful.awesome_icon,
      menu = mymainmenu,
      direction = 'east'
})



-- ---------------------------------------------------
--  ICONS, LINES, STYLES
-- ---------------------------------------------------

local function create_underline(height, color)
    return wibox.widget {
        forced_height = height,  
        forced_width = 21,       
        color = color or "#FFFFFF55", 
        widget = wibox.widget.separator
    }
end

local underline_widget = create_underline(16)



function create_v_icon(icon, font)
    local icon_widget = wibox.widget {
        widget = wibox.widget.textbox,
        text   = icon,
        font   = font or "3270 Nerd Font Mono 24",
	forced_height = 20,
        align  = "center",
        valign = "center"
    }

    local rotated_icon = icon_widget

    local centered_rotated_icon = wibox.container.place(
       rotated_icon,
       "center",
       "center"
    )
    return centered_rotated_icon 
end

local cpu_icon = create_v_icon("󰍛")
local net_r_icon = create_v_icon("⇊")
local net_s_icon = create_v_icon("⇈")
local disk_icon = create_v_icon("󰋊");
local mem_icon = create_v_icon("");
local volume_icon = create_v_icon("󰕾");

local function to_center(widget)
   return wibox.container.place(
      widget,
      "center",
      "center"
   )
end


-- ---------------------------------------------------
--  WIDGETS (UNUSED)
-- ---------------------------------------------------

local ar_cpu_widget = to_center(
   lain.widget.cpu {
      settings = function()
	 widget:set_markup([[<span font="3270 Nerd Font Mono 14" >]] ..
	    cpu_now.usage .. [[</span>]])
      end
   }.widget
)


local net_received_widget = to_center(
      lain.widget.net{
	 units = 1024*1024,
	 timeout = 1,
	 settings = function()
	    widget:set_markup([[<span font="3270 Nerd Font Mono 14" >]] ..
	        net_now.received .. [[</span>]])
	 end
      }.widget
)


local net_sent_widget = to_center(
      lain.widget.net{
	 units = 1024*1024,
	 timeout = 1,
	 settings = function()
	    widget:set_markup([[<span font="3270 Nerd Font Mono 14" >]] ..
	        net_now.sent .. [[</span>]])
	 end
      }.widget
)


local fs_widget = to_center(
      lain.widget.fs{
	 partition = "/",
	 forced_height = 42,
	 settings = function()
	    widget:set_markup([[<span font="3270 Nerd Font Mono 14" >]] ..
	       fs_now["/"].percentage .. [[</span>]])
	 end
      }.widget
)


local mem_widget = to_center(
      lain.widget.mem{
	 settings = function()
	    widget:set_markup([[<span font="3270 Nerd Font Mono 14" >]] ..
	       mem_now.perc .. [[</span>]])
	 end
      }.widget
)




-- ---------------------------------------------------
--  WIDGETS
-- ---------------------------------------------------


local ar_mykeyboardlayout; do
    local kb = awful.widget.keyboardlayout()
    kb.widget.font = "3270 Nerd Font Mono 14"
    ar_mykeyboardlayout = wibox.container.place(kb, "center", "center")
end



clock_h = wibox.widget.textclock("%H")
clock_h.font = "3270 Nerd Font Mono 20"
clock_h.align = "center"

clock_m = wibox.widget.textclock("%M")
clock_m.font = "3270 Nerd Font Mono 20"
clock_m.align = "center"


volume = wibox.widget.textbox()
local command = "pamixer --get-volume > /tmp/voltemp"
awful.spawn.easy_async_with_shell(
   command, function()
      awful.spawn.easy_async_with_shell(
	 "cat /tmp/voltemp", function(out)
	    volume.markup = '<span font="3270 Nerd Font Mono 14">'..out.."</span>"
	end)
end)


-- ---------------------------------------------------
--  TASK- N TAG- LIST STUFF
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
	 end   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

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
   awful.button({ }, 5, function ()	 awful.client.focus.byidx(-1)
end))



-- ---------------------------------------------------
--  WALLPAPER?
-- ---------------------------------------------------

local function set_wallpaper(s)
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      if type(wallpaper) == "function" then
	 wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

screen.connect_signal("property::geometry", set_wallpaper)



-- ---------------------------------------------------
--  EACH SCREEN
-- ---------------------------------------------------

awful.screen.connect_for_each_screen(function(s)
      set_wallpaper(s)
      awful.tag({ "♈︎", "♉︎", "♊︎", "♋︎", "♌︎", }, s, awful.layout.layouts[1])      
      s.mypromptbox = awful.widget.prompt()
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(
	 gears.table.join(
	    awful.button({ }, 1, function () awful.layout.inc( 1) end),
	    awful.button({ }, 3, function () awful.layout.inc(-1) end),
	    awful.button({ }, 4, function () awful.layout.inc( 1) end),
	    awful.button({ }, 5, function () awful.layout.inc(-1) end)))

      s.mytaglist = awful.widget.taglist {
	 screen  = s,
	 filter  = awful.widget.taglist.filter.all,
	 layout = wibox.layout.fixed.vertical,
	 buttons = taglist_buttons,
	 style   = {
            shape = gears.shape.circle,
	    font = "3270 Nerd Font Mono 12",
	 },
	 widget_template =
	    {
	       {
		  widget = wibox.widget.textbox,
		  id     = 'text_role',
		  align  = "center",
		  valign = "center",
	       },
	       id     = 'background_role',
	       widget = wibox.container.background,
	       forced_width = 23,
	       forced_height = 23,
	    },
      }
      
   
      s.mytasklist = awful.widget.tasklist {
	 screen  = s,
	 filter  = awful.widget.tasklist.filter.currenttags,
	 buttons = tasklist_buttons,
	 widget_template = {
	    {
	       {
		  id     = 'clienticon',
		  widget = awful.widget.clienticon,
	       },
	       widget = wibox.container.rotate,
	       direction = "east",
	    },
	    id              = 'background_role',
	    widget          = wibox.container.background,
	    create_callback = function(self, c, index, objects)
	       self:get_children_by_id('clienticon')[1].client = c
	    end,
	 },
      }
      
      s.newbar = wibox({
	    screen = s,
	    width = 32,
	    height = 512,
	    shape = gears.shape.rounded_bar,
	    ontop = true,
	    type = "dock",
	    visible = true,
	    position = "right",
      })
      s.newbar:geometry({
	    x = s.geometry.width - 64,
	    y = (s.geometry.height - 512) / 2 + 200
      })
      s.newbar:setup({
	    layout = wibox.layout.align.vertical,
	    { -- Left widgets
	       layout = wibox.layout.fixed.vertical,

	       underline_widget,
	       s.mytaglist,
	    },
	    { -- Middle widget
	       layout = wibox.layout.fixed.vertical,

	       underline_widget,
	       wibox.container.rotate(s.mytasklist, "west"),
	       s.mypromptbox,
	       wibox.widget.systray(),
	    },
	    { -- Right widgets
	       layout = wibox.layout.fixed.vertical,

	       underline_widget,
	       s.mylayoutbox,

	       underline_widget,
	       volume_icon,
	       to_center(volume),

	       underline_widget,
	       to_center(
		  ar_mykeyboardlayout
	       ),

	       underline_widget,
	       clock_h,
	       clock_m,
	       underline_widget,
	    },
      })      

  -- s.fs_on_back = wibox({
  -- 	    screen = s,
  -- 	    width = 32,
  -- 	    height = 32,
  -- 	    shape = gears.shape.rounded_rect,
  -- 	    ontop = false,
  -- 	    focusable = false,
  -- 	    below = true,
  -- 	    visible = true,
  -- 	    position = "center",
  -- })
  --  s.fs_on_back:geometry({
  --        x = (s.geometry.width - 64) / 2,
  --        y = (s.geometry.height - 64) / 2
  --     })
  --     s.fs_on_back:setup({
  -- 	    layout = wibox.layout.align.vertical,
  -- 	    { -- Right widgets
  -- 	       layout = wibox.layout.flex.vertical,
  -- 		   disk_icon,
	      
  -- 		  fs_widget,
  -- 	    },
  --     }) 
     
end)


-- ---------------------------------------------------
--  KEY BINDINGS
-- ---------------------------------------------------

root.buttons(
   gears.table.join(
      awful.button({ }, 3, function () mymainmenu:toggle() end),
      awful.button({ }, 4, awful.tag.viewnext),
      awful.button({ }, 5, awful.tag.viewprev)
))

globalkeys = gears.table.join(
     -- default
   awful.key(
      { modkey, }, "s",
      hotkeys_popup.show_help,
      {description="show help", group="awesome"}
   ),
   awful.key(
      { modkey, }, "Left",
      awful.tag.viewprev,
      {description = "view previous", group = "tag"}
   ),
   awful.key(
      { modkey, }, "Right",
      awful.tag.viewnext,
      {description = "view next", group = "tag"}
   ),
   awful.key(
      { modkey, }, "Escape",
      awful.tag.history.restore,
      {description = "go back", group = "tag"}
   ),
   
   awful.key(
      { modkey, }, "j",
      function () awful.client.focus.byidx( 1) end,
      {description = "focus next by index", group = "client"}
   ),
   awful.key(
      { modkey, }, "k",
      function () awful.client.focus.byidx(-1) end,
      {description = "focus previous by index", group = "client"}
    ),
   awful.key(
      { modkey, }, "w",
      function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}
   ),
    
    -- Layout manipulation
   awful.key(
      { modkey, "Shift" }, "j",
      function () awful.client.swap.byidx(  1)    end,
      {description = "swap with next client by index", group = "client"}
   ),
   awful.key(
      { modkey, "Shift" }, "k",
      function () awful.client.swap.byidx( -1)    end,
      {description = "swap with previous client by index", group = "client"}
   ),
   awful.key( 
      { modkey, "Control" }, "j",
      function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}
   ),
   awful.key(
      { modkey, "Control" }, "k",
      function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}
   ),
   awful.key(
      { modkey, }, "u",
      awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}
   ),
   awful.key(
      { modkey, }, "Tab",
      function () awful.client.focus.history.previous()
	 if client.focus then
	    client.focus:raise()
	 end
      end,
      {description = "go back", group = "client"}
   ),

    -- Standard program
   awful.key(
      { modkey, }, "Return",
      function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}
   ),
   awful.key(
      { modkey, "Control" }, "r",
      awesome.restart,
      {description = "reload awesome", group = "awesome"}
   ),
   awful.key(
      { modkey, "Shift" }, "q",
      awesome.quit,
      {description = "quit awesome", group = "awesome"}
   ),
    
   awful.key(
      { modkey, }, "l",
      function () awful.tag.incmwfact( 0.05) end,
      {description = "increase master width factor", group = "layout"}
   ),
   awful.key(
      { modkey, }, "h",
      function () awful.tag.incmwfact(-0.05) end,
      {description = "decrease master width factor", group = "layout"}
   ),
   awful.key(
      { modkey, "Shift"   }, "h",
      function () awful.tag.incnmaster( 1, nil, true) end,
      {description = "increase the number of master clients", group = "layout"}
   ),
   awful.key(
      { modkey, "Shift"   }, "l",
      function () awful.tag.incnmaster(-1, nil, true) end,
      {description = "decrease the number of master clients", group = "layout"}
   ),
   awful.key(
      { modkey, "Control" }, "h",
      function () awful.tag.incncol( 1, nil, true) end,
      {description = "increase the number of columns", group = "layout"}
   ),
   awful.key(
      { modkey, "Control" }, "l",
      function () awful.tag.incncol(-1, nil, true) end,
      {description = "decrease the number of columns", group = "layout"}
   ),
   awful.key({ modkey, }, "space",
      function () awful.layout.inc( 1) end,
      {description = "select next", group = "layout"}
   ),
   awful.key(
      { modkey, "Shift" }, "space",
      function () awful.layout.inc(-1) end,
      {description = "select previous", group = "layout"}
   ),

    awful.key({ modkey, "Control" }, "n",
              function ()
		 local c = awful.client.restore()
		 -- Focus restored client
		 if c then
                    c:emit_signal(
		       "request::activate", "key.unminimize", {raise = true}
                    )
		 end
              end,
              {description = "restore minimized", group = "client"}),
    
    
    awful.key(
       { modkey }, "x",
       function ()
	  awful.prompt.run {
	     prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
       end,
       {description = "lua execute prompt", group = "awesome"}
    ),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
       {description = "show the menubar", group = "launcher"}),

     
   awful.key(
      { modkey }, "m",
      function () awful.util.spawn("dmenu_run") end,
      {description = "dmenu", group = "suckless"}
   ),
   awful.key({ modkey }, "z",
      function ()
	 myscreen = awful.screen.focused()
	 myscreen.newbar.visible = not myscreen.newbar.visible
      end,
      {description = "toggle newbar"}
   ),
   awful.key(
      { modkey }, "d",
      function()
	 awful.util.spawn("pamixer -d 5")
	 local command = "pamixer --get-volume > /tmp/voltemp"
	 awful.spawn.easy_async_with_shell(
	    command, function()
	       awful.spawn.easy_async_with_shell(
		  "cat /tmp/voltemp",
		  function(out) -- TODO: redraw
		     volume.markup = '<span font="3270 Nerd Font Mono 14">'..out.."</span>"
	       end)
	 end)
      end,
      {description = "decreasure volume", group = "custom"}
   ),
   awful.key(
      { modkey }, "i",
      function()
	 awful.util.spawn("pamixer -i 5")
	 local command = "pamixer --get-volume > /tmp/voltemp"
	 awful.spawn.easy_async_with_shell(
	    command, function()
	       awful.spawn.easy_async_with_shell(
		  "cat /tmp/voltemp", function(out)
		     volume.markup = '<span font="3270 Nerd Font Mono 14">'..out.."</span>"
	       end)
	 end)
      end,
      {description = "increasure volume", group = "launcher"}
   )


)


-- ---------------------------------------------------
--  KEY BINDINGS 2
-- ---------------------------------------------------

clientkeys = gears.table.join(

   
   awful.key(
      { modkey, }, "f",
      function (c)
	 c.fullscreen = not c.fullscreen
	 c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}
   ),
   awful.key(
      { modkey, "Shift" }, "c",
      function (c) c:kill() end,
      {description = "close", group = "client"}
   ),
   awful.key(
      { modkey, "Control" }, "space",
      awful.client.floating.toggle,
      {description = "toggle floating", group = "client"}
   ),
   awful.key(
      { modkey, "Control" }, "Return",
      function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}
   ),
   awful.key(
      { modkey,}, "o",
      function (c) c:move_to_screen()
      end,
      {description = "move to screen", group = "client"}
   ),
   awful.key(
      { modkey, }, "t",
      function (c) c.ontop = not c.ontop
      end,
      {description = "toggle keep on top", group = "client"}
   )
   -- ,
   -- awful.key({ modkey, }, "n",
   --     function (c)
   -- 	  -- The client currently has the input focus, so it cannot be
   -- 	  -- minimized, since minimized clients can't have the focus.
   -- 	  c.minimized = true
   --     end ,
   --     {description = "minimize", group = "client"}
   -- ),
   -- awful.key({ modkey, }, "m",
   --    function (c)
   -- 	 c.maximized = not c.maximized
   -- 	 c:raise()
   --    end ,
   --    {description = "(un)maximize", group = "client"}),
   -- awful.key({ modkey, "Control" }, "m",
   --     function (c)
   -- 	  c.maximized_vertical = not c.maximized_vertical
   -- 	  c:raise()
   --     end ,
   --     {description = "(un)maximize vertically", group = "client"}),
   --  awful.key({ modkey, "Shift"   }, "m",
   --      function (c)
   --          c.maximized_horizontal = not c.maximized_horizontal
   --          c:raise()
   --      end ,
   --      {description = "(un)maximize horizontally", group = "client"})
)


-- ---------------------------------------------------
--  SOME STUFF
-- ---------------------------------------------------
-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(globalkeys,
				 -- View tag only.
				 awful.key({ modkey }, "#" .. i + 9,
				    function ()
				       local screen = awful.screen.focused()
				       local tag = screen.tags[i]				       if tag then
					  tag:view_only()
				       end
				    end,
				    {description = "view tag #"..i, group = "tag"}),
				 -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
	   function ()
	      if client.focus then
		 local tag = client.focus.screen.tags[i]
		 if tag then
		    client.focus:move_to_tag(tag)
		 end	      end
	   end,
	   {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
	   function ()
	      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
	      end
	   end,
	   {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.move(c)   end),
   awful.button({ modkey }, 3, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.resize(c)
   end)
)

-- Set keys
root.keys(globalkeys)


-- ---------------------------------------------------
--  RULES
-- ---------------------------------------------------
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
		    border_color = beautiful.border_normal,
		    focus = awful.client.focus.filter,
		    raise = true,
		    keys = clientkeys,
		    buttons = clientbuttons,
		    screen = awful.screen.preferred,
		    -- size_hint_honor = true,
		    placement = awful.placement.no_overlap+awful.placement.no_offscreen     }
   },
   
   -- Floating clients.
   { rule_any = {
        instance = {
	   "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
	   "Arandr",
	   "Blueman-manager",
	   "Gpick",
	   "Kruler",	   "MessageWin",  -- kalarm.
	   "Sxiv",
	   "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
	   "Wpa_gui",
	   "veromix",
	   "xtightvncviewer"},
	
        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
	   "Event Tester",  -- xev.
        },
        role = {
	   "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},
   
   -- Add titlebars to normal clients and dialogs
   { rule_any = {type = { "normal", "dialog" }
		}, properties = { titlebars_enabled = false }
   },
   
   -- Set Firefox to always map on the tag named "2" on screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { screen = 1, tag = "2" } },
}

-- ---------------------------------------------------
--  SIGNALS
-- ---------------------------------------------------
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
			 -- Set the windows at the slave,
			 -- i.e. put it at the end of others instead of setting it master.
			 -- if not awesome.startup then awful.client.setslave(c) end
			 
			 if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
			 end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
			 -- buttons for the titlebar
			 local buttons = gears.table.join(
			    awful.button({ }, 1, function()
				  c:emit_signal("request::activate", "titlebar", {raise = true})
				  awful.mouse.client.move(c)
			    end),
			    awful.button({ }, 3, function()
				  c:emit_signal("request::activate", "titlebar", {raise = true})
				  awful.mouse.client.resize(c)
			    end)
			 )
			 
			 awful.titlebar(c) : setup {
			    { -- Left
			       awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
	   { -- Title
	      align  = "center",
	      widget = awful.titlebar.widget.titlewidget(c)
	   },
	   buttons = buttons,
	   layout  = wibox.layout.flex.horizontal
        },
        { -- Right
	   awful.titlebar.widget.floatingbutton (c),
	   awful.titlebar.widget.maximizedbutton(c),
	   awful.titlebar.widget.stickybutton   (c),
	   awful.titlebar.widget.ontopbutton    (c),
	   awful.titlebar.widget.closebutton    (c),
	   layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
						   }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
			 c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}



-- ---------------------------------------------------
--  AUTOSTART
-- ---------------------------------------------------

awful.spawn.with_shell("picom --daemon &")
awful.spawn.with_shell("nitrogen --restore")

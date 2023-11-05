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
local vicious = require("vicious")
--require('couth.alsa')



------------------------------------------------------------------------------|  Error handlinig |--------
if awesome.startup_errors then 
	naughty.notify({
	        preset = naughty.config.presets.critical,
                title = "Oops, there were errors during startup!",
                text = awesome.startup_errors }) 
end


do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true

        naughty.notify({ 
		preset = naughty.config.presets.critical,
                title = "Oops, an error happened!",
                text = tostring(err) })
        in_error = false
    end)
end

----------------------------------------------------------------------------| Variable definitions |------
--beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
--beautiful.init("home/ids/.config/awesome/theme.lua")
beautiful.init(gears.filesystem.get_configuration_dir().."theme.lua")
terminal = "alacritty"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor
modkey = "Mod4"

------------------------------------------------------------------------------| Table of layouts |--------
awful.layout.layouts = {
    --awful.layout.suit.floating,
    awful.layout.suit.tile,
    --awful.layout.suit.tile.left,
    --awful.layout.suit.tile.bottom,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.max,
    --awful.layout.suit.max.fullscreen,
    --awful.layout.suit.magnifier,
    --awful.layout.suit.corner.nw,
}

------------------------------------------------------------------------------------| Menu |--------------
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
	    --
                                     menu = mymainmenu })

menubar.utils.terminal = terminal

-----------------------------------------------------------------| Keyboard map indicator and switcher|---
mykeyboardlayout = awful.widget.keyboardlayout()

-------------------------------------------------------------------------------------| Wibar |------------
mytextclock = wibox.widget.textclock()

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

awful.screen.connect_for_each_screen(function(s)

    set_wallpaper(s)

    awful.tag({ "A", "W", "E", "S", "O", "M", "E" }, s, awful.layout.layouts[1])

    s.mypromptbox = awful.widget.prompt()
local stdout = { }
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))

    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
--[[	style   = {shape = gears.powerline},
	layout  = {
		spacing = -12,
		spacing_widget = {
			color = '#dddddd',
			shape = gears.shape.powerline,
			widget = wibox.widget.separator,
		},
		layout = wibox.layout.fixed.horizontal
	},
	widget_template = {
	    {
		 {
		      {
			   {
			        {
				    id = 'index_role',
		                    widget = wiox.widget.textbox,
	                        },
		                margins = 4,
		                widget = wibox.container.margin,
	                   },
			   bg = '#dddddd',
			   shape = gears.shape.circle,
			   widget = wibox.container.background,
		     },
		     {
			  {
			        id = 'icon_role',
				widget = wibox.widget.imagebox,
			  },
			  margins = 2,
			  widget = wibox.container.margin,
		     },
		     {
			  id = 'text_role',
			  widget = wibox.widget.textbox,
	             },
		     layout = wibox.layout.fixed.horizontal,
		 },
		 left = 18,
		 right = 18,
		 widget = wibox.container.margin
	    },
	    id = 'background_role',
	    widget = wibox.contsiner.background,

	    crerate_callback = function(self, c3, index, objects)
		self::get_children_by_id('index_role')[1].markup = '<b> '..index..'</b>'


	}]]
        buttons = taglist_buttons
    }

    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }
----------------------------------------------------------------------------------| temp    |-------------
hddtempwidget = wibox.widget.textbox()
vicious.register(hddtempwidget, vicious.widgets.hddtemp, "${/dev/sda} C", 19)
----------------------------------------------------------------------------------| battery |-------------
batwidget = wibox.widget.progressbar()

batbox = wibox.layout.margin(
	wibox.widget{ { max_value = 1, widget = batwidget,
			border_width = 0.5, border_color = "#000000",
			color ={type = "linear",
				from = {0, 0},
				to = {0, 5},
				stops ={{0, "#AECF96"},
					{1, "#FF5656"}}
				}
		        },
			forced_height = 10, forced_width = 8,
			direction = 'east', color = beautiful.fg_widget,
			layout = wibox.container.rotate},
	1,1,3,3)
vicious.register(batwidget, vicious.widgets.bat, "$2", 61, "BAT0")
----------------------------------------------------------------------------------| cpu usage |----------
cpuwidget = awful.widget.graph()
cpuwidget:set_width(50)
cpuwidget:set_background_color"#494B4F"
cpuwidget:set_color{
type = "linear", from = {0,0}, to = {50, 0},
stops ={ {0, "#FF5656" },
{0.5, "#88A175"},
{1, "AECF96"}}}
vicious.register(cpuwidget, vicious.widgets.cpu, "$1", 3)
----------------------------------------------------------------------------------| cpu usage 2 |--------
cpuwidget2 = wibox.widget.textbox()
--cpuwidget2 = widget({type = "textbox"})
vicious.register(cpuwidget2, vicious.widgets.cpu,
'<span background="#222222"> $1% </span>', 3)
----------------------------------------------------------------------------------| sensors    |---------
--sensors = wibox.widget.textbox()
--vicious.register(sensors, vicious.widgets.sensors)

----------------------------------------------------------------------------------| cpuinf   |-----------
cpuinfwid = wibox.widget.textbox()
vicious.register(cpuinfwid, vicious.widgets.cpufreq,
'<span background="#222222"> $1 </span>', 1, "cpu0")

----------------------------------------------------------------------------------| battery2 |-----------
battery2 = wibox.widget.textbox()--widget({type = "textbox"})
vicious.register(battery2, vicious.widgets.bat, '$1 | 2$ | 3$ | $4 | $5', 10, "BAT0" )

----------------------------------------------------------------------------------| volume |--------------
volume = wibox.widget.textbox()
local command = "pamixer --get-volume > /tmp/voltemp"
awful.spawn.easy_async_with_shell(command, function()
	awful.spawn.easy_async_with_shell("cat /tmp/voltemp", function(out)
		volume.markup = "<span background='#222222'> "..out.."volume</span>"
	end)
end)
vul = wibox.widget.textbox()
vul.markup = "<span background='#222222'>% </span>" 

--------------------------------------------------------------------------------| Date widget |-----------
datewidget = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date, "<span background='#222222'> %b %d %R </span>")

--------------------------------------------------------------------------------| MEM widget |------------
local memwidget1 = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget1, vicious.widgets.mem, 
"<span background='#222222'><span font='Terminus 9'> $2MiB </span></span>", 15)
--------------------------------------------------------------------------------| net |-------------------
netwidget = wibox.widget.textbox()
vicious.register(netwidget, vicious.widgets.net,
'<span background="#222222">${enp3s0 down_mb}mb | ${enp3s0 up_mb}mb </span>', 3)
--netwidget:set_forced_width(140)
--netwidget.align = "center"


--------------------------------------------------------------------------------| SP widgets|-------------
yeline = wibox.widget.textbox()
yeline.markup = "<span background = '#ff9333'> </span>"
------------------------------------------------------------------------------------| Panel |-------------
    s.mywibox = awful.wibar({ position = "top", screen = s })

    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            s.mypromptbox,
        },
	s.mytasklist,
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
	    --hddtempwidget,
	    --batwidget,
	    --battery2,
            mykeyboardlayout, yeline,
	    --sensors,
	    --netwidget,
	    --cpiinfwid,
	    --cpuwidget2,
	    volume, vul, yeline,
	    --memwidget1,
	    --wibox.widget.systray(),
            datewidget,
        },
    }
end)

---------------------------------------------------------------------------------| Mouse bindings |-------
root.buttons(gears.table.join(
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

----------------------------------------------------------------------------------|  Key bindings |-------
globalkeys = gears.table.join(

    -- Awesome
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

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

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),
    ----------------------------------------------------------------------------------------------------
    --make screenshot
    awful.key({modkey}, "Print", function () awful.util.spawn("scrot '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f'") end), 
    -- dmenu	      
    awful.key({modkey},		     "m",      function() awful.util.spawn_with_shell("dmenu_run") end,
              {description = "->show the menubar", group = "launcher"}),
    -- pamixer kill battery and change volume
--    awful.key({"Shift"}, "Alt_L", function() mykeybourdlayout.next_layout() end,
--	      {description = "chan ru", group = "client"}),

   awful.key({modkey},		    "d", function() awful.util.spawn("pamixer -d 5")
local command = "pamixer --get-volume > /tmp/voltemp"
awful.spawn.easy_async_with_shell(command, function()
	awful.spawn.easy_async_with_shell("cat /tmp/voltemp", function(out)
		volume.markup = "<span background='#222222'> "..out.."</span>"
	end)
end)
    end,
    {description = "decreasure volume", group = "launcher"}
    ),

    awful.key({modkey},		    "i", function() awful.util.spawn("pamixer -i 5")
local command = "pamixer --get-volume > /tmp/voltemp"
awful.spawn.easy_async_with_shell(command, function()
	awful.spawn.easy_async_with_shell("cat /tmp/voltemp", function(out)
		volume.markup = "<span background='#222222'> "..out.."</span>"
	end)
end)
    end,
    {description = "increasure volume", group = "launcher"}
    ),


    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"})
        
	
)

for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,

        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
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
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused cliendt.
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
       ,
       awful.key({"Shift"}, "Alt_L", function () mykeyboardlayout.next_layout(); end)
      
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

root.keys(globalkeys)

------------------------------------------------------------------------------------| Rules |-------------
awful.rules.rules = {
    { rule = {  },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    { rule_any = {
        instance = {
          "DTA",  
          "copyq",  
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  
          "Sxiv",
          "Tor Browser",
          "Wpa_gui",
          "veromix",
          "xtightvncviewer"},

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
}

-------------------------------------------------------------------------------------| Signals |----------
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
      -- awful.key({"Shift"}, "Alt_L", function() mykeyboardlayout.next_layout() end,
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
--
---------------------------------------------------------------------| Autostart application |------------
awful.spawn.with_shell("picom --daemon &")
awful.spawn.with_shell("nitrogen --restore")
--awful.spawn.with_shell('setxbmap -model pc 105 -option "grp:shifts_toggle, compose:sclk" "us, ru(phonetic_YASHERTY"')
awful.spawn.with_shell("setxbkmap -model pc105 -layout us,ru, -option grp:alt_shift_toggle")
awful.spawn.with_shell("xinput --set-prop 11 'libinput Accel Speed' -1")

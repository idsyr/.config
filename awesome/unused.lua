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



      -- s.mywibox =
      -- 	 awful.wibar(
      -- 	 { position = "top", screen = s,
      -- 	   height = 32,
      -- 	   margins = {
      -- 	      top = beautiful.useless_gap * 2,
      -- 	      left = beautiful.useless_gap * 2,
      -- 	      right = beautiful.useless_gap * 2
      -- 	   }
      -- 	 } )

      -- s.mywibox:setup {
      -- 	 {
      -- 	    layout = wibox.layout.fixed.horizontal,
      -- 	    { -- Left widgets
      -- 	       layout = wibox.layout.fixed.horizontal,
      -- 	       -- mylauncher,
      -- 	       -- s.mytaglist,
      -- 	    },
      -- 	    --s.mytasklist, -- Middle widget
      -- 	    { -- Right widgets
      -- 	       layout = wibox.layout.fixed.horizontal,
      -- 	       wibox.widget.systray(), separator,
      -- 	       s.mytaglist,            separator,
      -- 	       mykeyboardlayout_wrapped,       separator,
      -- 	       mytextclock_wrapped,            separator,
      -- 	       s.mylayoutbox,          separator,
      -- 	       s.mypromptbox,
      -- 	       -- mylauncher,             separator
      -- 	    },
      -- 	 },
      -- 	 separator, separator,
      -- 	 -- wibox.widget {
      -- 	 --    {
      -- 	 --       layout = wibox.layout.align.horizontal,
      -- 	 --       separator,
      -- 	 --       s.mytasklist, -- Middle widget
      -- 	 --       separator
      -- 	 --    },
      -- 	 --    widget = wibox.container.background,
	    
      -- 	 --    shape = get_rounded_rect(10),
	    
      -- 	 --    border_width = 0,
      -- 	 --    border_color = get_volumetric_border_gradient(40, 30),
	    
      -- 	 --    bg = get_volumetric_plane_gradient(5, 40, beautiful.taglist_bg_occupied),
      -- 	 -- },
	 
      -- 	 layout = wibox.layout.fixed.horizontal,
      -- }

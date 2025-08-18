-- EXPEXPEXPEXPEXPEXPEXPEXPEXPEXPEXPEXPEXPEXP --
local function get_buttons(c)
  return awful.util.table.join(
    awful.button({ }, 1, function()
      client.focus = c
      c:raise()
      awful.mouse.client.move(c)
    end),
    awful.button({ }, 2, function()
      client.focus = c
      c:raise()
      c.maximized = not c.maximized
    end),
    awful.button({ }, 3, function()
      client.focus = c
      c:raise()
      awful.mouse.client.resize(c)
    end)
  )
end
local function get_style_for_client(c)
  local client_is_focused = client.focus == c
  return {
    border = (
      client_is_focused
      and beautiful.titlebar_bg_focus
      or beautiful.titlebar_bg_normalv
    ),
    shadow = (
      client_is_focused
      and beautiful.titlebar_shadow_focus
      or beautiful.titlebar_shadow_normal
    ),
    font = (
      client_is_focused
      and (beautiful.titlebar_font_focus or beautiful.titlebar_font)
      or (beautiful.titlebar_font_normal or beautiful.titlebar_font)
    ),
  }
end

local TRANSPARENT = "#00000000"
local function make_border_with_shadow(c, args)
  args = args or {}
  local is_titlebar = args.is_titlebar

  local style = get_style_for_client(c)
  local border_clr = style.border
  local shadow_clr = style.shadow

  --               Top border                --
  local tbt
  if not is_titlebar then
    tbt = awful.titlebar(c, {
      size = beautiful.base_border_width or 5,
      position="top"
    })
    tbt:setup {
        {
          nil,
          {
            {
              top   = beautiful.border_shadow_width,
              layout = wibox.container.margin,
            },
            -- bg=TRANSPARENT,
            widget = wibox.container.background,
          },
          {
            {
              left   = beautiful.border_shadow_width,
              layout = wibox.container.margin,
            },
            -- bg=TRANSPARENT,
            widget = wibox.container.background,
          },
          layout = wibox.layout.align.horizontal,
        },
      buttons = get_buttons(c),
      id     = "main_layout",
      layout = wibox.container.background,
      -- bg=TRANSPARENT
    }
  end

  --               Left border                --
  local tbl = awful.titlebar(c, {
    size = beautiful.base_border_width or 5,
    position = "left"
  })
  tbl:setup {
          {
            left   = beautiful.base_border_width,
            layout = wibox.container.margin,
          },
    buttons = get_buttons(c),
    bg=beautiful.true_border_bg_normal, -- tight line SUCCESS
    widget    = wibox.container.background
  }

  --               Right border                --
  local tbr = awful.titlebar(c, {
    size = (beautiful.base_border_width or 5)+(beautiful.border_shadow_width or 0),
    position = "right"
  })
  tbr:setup{
    buttons = get_buttons(c),
    id="main_layout",
    {
      {
        {
          left   = beautiful.base_border_width,
          layout = wibox.container.margin,
        },
        bg=beautiful.true_border_bg_normal, -- tight line SUCCESS
        widget = wibox.container.background,
      },
      layout = wibox.container.margin,
    },
    {
      not is_titlebar and {
        {
          {
            top   = beautiful.border_shadow_width,
            layout = wibox.container.margin,
          },
          bg=shadow_clr, -- не используется из за условия (всегда одно -> удалить)
          widget = wibox.container.background,
        },
        height   = beautiful.base_border_width + beautiful.border_shadow_width,
        width   = beautiful.border_shadow_width,
        layout = wibox.container.constraint,
      },
      {
        {
          { text   = ' ', widget = wibox.widget.textbox, },
          bg=shadow_clr, -- почти вся правая линия тени
          widget = wibox.container.background,
        },
        width   = beautiful.border_shadow_width,
        layout = wibox.container.constraint,
      },
      layout = wibox.layout.align.vertical,
    },
    layout = wibox.layout.align.horizontal,
  }

  --               Bottom border                --
  local tbb = awful.titlebar(c, {
    size = (beautiful.base_border_width or 5) + (beautiful.border_shadow_width or 0),
    position = "bottom"
  })
  tbb:setup{
    buttons = get_buttons(c),
    id="main_layout",
    {
      nil,
      {
        {
          top   = beautiful.base_border_width,
          layout = wibox.container.margin,
        },
        bg=beautiful.true_border_bg_normal, -- tight line SUCCESS
        widget = wibox.container.background,
      },
      {
        {
          {
            left   = beautiful.border_shadow_width,
            top   = beautiful.base_border_width,
            layout = wibox.container.margin,
          },
          bg=shadow_clr, -- писюлька на углу
          widget = wibox.container.background,
        },
        height   = beautiful.base_border_width,
        layout = wibox.container.constraint,
      },
      layout = wibox.layout.align.horizontal,
    },
    {
      {
        {
          { text   = ' ', widget = wibox.widget.textbox, },
          bg=TRANSPARENT, -- отступ снизу (эффект от тени)
          widget = wibox.container.background,
        },
        width   = beautiful.base_border_width + beautiful.border_shadow_width,
        height   = beautiful.border_shadow_width,
        layout = wibox.container.constraint,
      },
      {
        {
          { text   = ' ', widget = wibox.widget.textbox, },
          bg=shadow_clr, -- почти вся нижняя линия
          widget = wibox.container.background,
        },
        height   = beautiful.border_shadow_width,
        layout = wibox.container.constraint,
      },
      layout = wibox.layout.align.horizontal,
    },
    layout = wibox.layout.align.vertical,
  }
  return {
    top = tbt,
    left = tbl,
    right = tbr,
    bottom = tbb,
  }
end
-- Add a titlebar if titlebars_enabled is set to true in the rules.
local testcolor = '#FF1234'

   client.connect_signal("focus:titlebars", function(c)
	testcolor = beautiful.val_focus
	c.focus = client.focus
	-- c:set_bg(testcolor)
    end)
    client.connect_signal("unfocus:titlebars", function(c)
	testcolor = beautiful.val_normal
	c.focus = client.focus
	-- c:set_bg(testcolor)
    end)



client.connect_signal(
   "request::titlebars", function(c)
local right_color = c.border_color
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

      make_border_with_shadow(c, {is_titlebar=true})
      local tbt = awful.titlebar(c, {
				    size=beautiful.titlebar_height or 16,
				    position = beautiful.titlebar_position,
				    opacity = beautiful.titlebar_opacity,
				    }
      )

      local default_titlebar = {
	  {
        {
          {
            awful.titlebar.widget.closebutton(c),
            awful.titlebar.widget.minimizebutton(c),
            --awful.titlebar.widget.maximizedbutton(c)),
            layout = wibox.layout.fixed.horizontal,
          }, {
            {
              widget = awful.titlebar.widget.titlewidget(c),
              halign = "center",
              font = font,
            },
            layout = wibox.layout.flex.horizontal,
            buttons = get_buttons(c),
          }, {
            awful.titlebar.widget.ontopbutton(c),
            awful.titlebar.widget.stickybutton(c),
            layout = wibox.layout.fixed.horizontal,
          },
          layout = wibox.layout.align.horizontal,
        },
        widget = wibox.container.background,
      },
      top   = beautiful.base_border_width,
      left   = beautiful.base_border_width,
      right   = beautiful.base_border_width,
      layout = wibox.container.margin,
      }                                -- default_titlebar
 
  local style = get_style_for_client(c)
  local border_clr = style.border
  local shadow_clr = style.shadow
     
      tbt : setup
      {  layout = wibox.container.background,
	 id     = "main_layout",
	 {  layout = wibox.layout.align.horizontal,
	    nil,
	    {  widget = wibox.container.background,
	       default_titlebar,
	       bg = beautiful.true_border_bg_normal  -- верхняя с вырезом
	    },
	    {  layout = wibox.layout.align.vertical,
	       {  widget = wibox.container.background,
		  bg =TRANSPARENT, -- отступ тени
		  {  layout = wibox.container.margin,
		     top = beautiful.border_shadow_width + beautiful.base_border_width,
		     left = beautiful.border_shadow_width,
		  }
	       },
	       {  widget = wibox.container.background,
		  bg = shadow_clr, -- кусок до отступа тени
		  {  layout = wibox.container.margin,
		     left = beautiful.border_shadow_width
		  }
	       }
	    }
	 }
      }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
			 c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c)
	c.border_color = beautiful.border_focus
	--beautiful.test_focus = beautiful.val_focus
	--c.focus_st = true
end)
client.connect_signal("unfocus", function(c)
	c.border_color = beautiful.border_normal
	--beautiful.test_focus = beautiful.val_normal
	--c.focus_st = false
end)
-- }}}



-- {{{ Signals
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

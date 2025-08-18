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
--  VARIABLE DEFINITIONS
-- ---------------------------------------------------
beautiful.init(
   gears.filesystem.get_themes_dir()
   -- "~/.config/awesome/themes/"
   .. "gtk/theme.lua")


terminal   = "alacritty"
editor     = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor
modkey     = "Mod4"

awful.layout.layouts = {
   awful.layout.suit.tile,
   -- awful.layout.suit.tile.left,
   -- awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   -- awful.layout.suit.floating,
   awful.layout.suit.fair,
   -- awful.layout.suit.fair.horizontal,
   -- awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   -- awful.layout.suit.max,            -- | WIN
   -- awful.layout.suit.max.fullscreen, -- | + F
   -- awful.layout.suit.magnifier,
   awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
}

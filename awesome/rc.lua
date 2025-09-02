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
--  ERROR HANDLING
-- ---------------------------------------------------
dofile(".config/awesome/error_handling.lua")

-- ---------------------------------------------------
--  VARIABLE DEFINITIONS
-- ---------------------------------------------------
dofile(".config/awesome/variable_definitions.lua")

-- ---------------------------------------------------
--  WIDGETS ( EACHSCREEN )
-- ---------------------------------------------------
dofile(".config/awesome/widgets.lua")

-- ---------------------------------------------------
--  KEYBINDS
-- ---------------------------------------------------
dofile(".config/awesome/keybinds.lua")

-- ---------------------------------------------------
--  RULES
-- ---------------------------------------------------
dofile(".config/awesome/rules.lua")

-- ---------------------------------------------------
--  SIGNALS
-- ---------------------------------------------------
dofile(".config/awesome/signals.lua")

-- ---------------------------------------------------
--  AUTOSTART
-- ---------------------------------------------------
awful.spawn.with_shell("setxkbmap -layout us,ru -option grp:alt_shift_toggle")
awful.spawn.with_shell("picom --daemon &")

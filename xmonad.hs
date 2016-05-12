import System.IO
import System.Exit

import XMonad
import Data.Monoid
import System.Exit

import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Actions.UpdatePointer
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators hiding ( (|||) )

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Actions.WindowBringer

import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "terminator"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
workspacesList = (map show [ 1..9 ]) -- ++ [ "0", "-", "+", "=" ])
getWorkspaces x = withScreens x workspacesList

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#A22"
myFocusedBorderColor = "#2A2"

myComboKeys :: [ (String, X()) ]
myComboKeys =
    [ ("M-c r", spawn "~/bin/p.spotify -d --pause"),
      ("M-c p", spawn "~/bin/p.spotify -d --play"),
      ("M-c n", spawn "~/bin/p.spotify -n"),
      ("M-c b", spawn "~/bin/p.spotify -b"),
      ("M-c s", spawn "~/bin/p.spotify -s"),
      ("M-c l", spawn "/usr/bin/spotify"),
      ("M-z l", spawn "xscreensaver-command -lock"),
      ("M-z 7", spawn "virtualbox --startvm W7"),
      ("M-z f", spawn "firefox"),
      ("M-z o", spawn "poweroff"),
      ("M-z r", spawn "reboot"),
      ("M-z m", spawn "pactl set-sink-mute 0 toggle")
    ]
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Toggle the keyboard layout
    , ((modm .|. shiftMask, xK_period ), spawn "~/bin/toggle.layout")

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm .|. shiftMask, xK_g     ), gotoMenu)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    {-, ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))-}

    -- TwoPane layout for 4k Monitor
    -- , ((modm .|. shiftMask, xK_b     ), layoutScreens 2 (TwoPane 0.8 0.2))
    , ((modm .|. controlMask .|. shiftMask, xK_r ), rescreen)
    -- Increase/Decrease the volume
    , ((modm .|. shiftMask, xK_Up), spawn "pactl set-sink-volume 0 +5%")
    , ((modm .|. shiftMask, xK_Down), spawn "pactl set-sink-volume 0 -5%")
    -- XF86AudioMute toggle
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- custom launcher
myLauncher = "$(~/.cabal/bin/yeganesh -x -- -nb '#000' -nf '#f00' -sb '#a00' -sf '#111'  -p 'What?: ')"
------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = nob ||| avoidStruts nob ||| fullavoid ||| avoidStruts pidginLayout
  where
     nob = noBorders (fullscreenFull Full)
     fullavoid = avoidStruts (Full)
     gridLayout = spacing 8 $ Grid
     pidginLayout = withIM (18/100) (Role "buddy_list") gridLayout

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "gvim"           --> doFloat
    , className =? "Gvim"           --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "stalonetray"    --> doIgnore
    , className =? "Pavucontrol"    --> doFloat
    , className =? "plugin-container" --> doFloat
    , isFullscreen                  --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
-- hs    <- mapM (spawnPipe . xmobarCommand) [ 0 .. nScreens - 1 ]
main = do
    nScreens <- countScreens

    hs   <- mapM (spawnPipe . xmobarCommand) [ 0 .. nScreens - 1 ]

    xmonad $ defaultConfig {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      clickJustFocuses   = myClickJustFocuses,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = getWorkspaces nScreens,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      keys               = \c -> myKeys c `M.union` mkKeymap c myComboKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook <+> manageDocks,
      handleEventHook    = myEventHook,
      logHook            = mapM_ dynamicLogWithPP $ zipWith pp hs [ 0..nScreens ],
      startupHook        = ewmhDesktopsStartup >> setWMName "LG3D"
    }

-- xmobarCommand (S s) = unwords ["xmobar", "-x", show s, "-t", template s, "-c", config s ] where
xmobarCommand (S s) = unwords ["xmobar", "-x", show s, "~/.xmonad/xmobar.hs" ]

bright = "#80c0ff"
dark   = "#13294e"

pp h s = marshallPP s defaultPP {
    ppCurrent           = xmobarColor xmobarCurrentWorkspaceColor "",
    ppTitle             = xmobarColor xmobarTitleColor "" . shorten 100,
    ppVisible           = xmobarColor "white" "",
    ppHiddenNoWindows   = xmobarColor dark "",
    ppUrgent            = xmobarColor "red" "",
    ppSep               = "  ",
    ppOutput            = hPutStrLn h
}

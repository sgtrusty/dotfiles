--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import XMonad

import XMonad.Actions.UpdatePointer ( updatePointer )
import XMonad.Actions.Submap

import XMonad.ManageHook ( liftX )

import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.Ungrab
import XMonad.Util.Run ( runProcessWithInput )

import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks
    ( avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Hooks.RefocusLast ( isFloat)

import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.BoringWindows (boringWindows, focusUp, focusDown)
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Renamed (renamed, Rename(Replace))

import Control.Monad ( join, when, liftM )
import Data.Monoid
import Data.Maybe ( maybeToList, isJust )
import Data.List (isSuffixOf)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
import System.Exit ()
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

--Looks to see if focused window is floating and if it is the places it in the stack
--else it makes it floating but as full screen
toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1) })

-- I'd like to have a setup where I have a short list of layouts that can be toggled with the usual "meta+space" combo,
-- but then have a keybingding that allows me to select from a larger range of configured layouts with dmenu.
-- https://www.reddit.com/r/xmonad/comments/e8h3dm/getting_a_larger_selection_of_layouts_with_dmenu/
-- https://github.com/liskin/dotfiles/blob/3b194dc4ef84bb44510c319f1e218469f835d5f5/.xmonad/xmonad.hs#L171-L177
runSelectedAction :: String -> [(String, X ())] -> X ()
runSelectedAction prompt actions = do
    unGrab
    out <- lines <$> runProcessWithInput "rofi" ["-dmenu", "-p", prompt] (unlines $ map fst actions)
    case out of
        [sel] -> maybe (pure ()) id (sel `lookup` actions)
        _ -> pure ()

-- https://mail.haskell.org/pipermail/xmonad/2011-March/011157.html
-- https://github.com/xmonad/xmonad/issues/300
-- willFloat::Query Bool
-- willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> do
--   sh <- io $ getWMNormalHints d w
--   let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
--   isTransient <- isJust <$> io (getTransientForHint d w)
--   return (isFixedSize || isTransient)
  
-- floating = (ask >>= liftX . willFloat)
--             -- panel applets make everything shift around when
--             -- shifted to master.
--             <&&> (liftM (not . isSuffixOf "-panel")) resource
--             <&&> (liftM (not . isSuffixOf "-applet")) resource

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
myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor = "#bc96da"

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
--clipboardy :: MonadIO m => m () -- Don't question it 
--clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"

centerlaunch = spawn "exec eww open-many blur_full weather profile quote search_full disturb-icon vpn-icon home_dir screenshot power_full reboot_full lock_full logout_full suspend_full"
sidebarlaunch = spawn "exec eww open-many weather_side time_side smol_calendar player_side sys_side sliders_side"
ewwclose = spawn "exec eww close-all"
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
maimsave = spawn "maim -s ~/Pictures/screenshot/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"
rofiLauncher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ 
    -- native keys
    -- Audio keys
      ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")

    -- Brightness keys
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")

    -- launch a terminal
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- Screenshot
    , ((0,                    xK_Print), maimcopy)
    , ((modm,                 xK_Print), maimsave)

    -- , ((0, xK_Super_L), submap . M.fromList $
    --   [ ((0, xK_e),    submap . M.fromList $
    --     [ ((0, xK_f),  spawn "notify-send \"wef combo detected!\"" ) ])
    --   ])

    -- lock screen
    , ((modm,               xK_F1    ), spawn "betterlockscreen -l")

    -- launch rofi and dashboard
    , ((modm,               xK_period), spawn "rofimoji")
    , ((modm,               xK_o     ), rofiLauncher)
    , ((modm,               xK_p     ), centerlaunch)
    , ((modm .|. shiftMask, xK_p     ), ewwclose)

    -- launch eww sidebar
    , ((modm,               xK_s     ), sidebarlaunch)
    , ((modm .|. shiftMask, xK_s     ), ewwclose)

    -- My Stuff
    , ((modm,               xK_b     ), spawn "exec ~/.config/xmonad/scripts/bartoggle")
    , ((modm,               xK_z     ), spawn "exec ~/.config/xmonad/scripts/inhibit_activate")
    , ((modm .|. shiftMask, xK_z     ), spawn "exec ~/.config/xmonad/scripts/inhibit_deactivate")
    --, ((modm .|. shiftMask, xK_a     ), clipboardy)
    -- Turn do not disturb on and off
    , ((modm,               xK_d     ), spawn "exec ~/.config/xmonad/scripts/do_not_disturb.sh")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- GAPS!!!
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- toggle all gaps
    , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps [(L,30), (R,30), (U,40), (D,60)]) -- reset the GapSpec
    
    -- , ((modm .|. controlMask, xK_t), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
    -- , ((modm .|. shiftMask, xK_t     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap
    
    -- , ((modm .|. controlMask, xK_y), sendMessage $ IncGap 10 U)              -- increment the top gap
    -- , ((modm .|. shiftMask, xK_y     ), sendMessage $ DecGap 10 U)           -- decrement the top gap
    
    -- , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
    -- , ((modm .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap

    -- , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
    -- , ((modm .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap

    -- https://hackage.haskell.org/package/xmonad-contrib-0.9/docs/XMonad-Layout-MouseResizableTile.html
    , ((modm,               xK_u), sendMessage ShrinkSlave) -- %! Shrink a slave area
    , ((modm,               xK_i), sendMessage ExpandSlave) -- %! Expand a slave area

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), runSelectedAction "layout" laySels)
    , ((modm .|. controlMask, xK_space ), sendMessage NextLayout)

    -- https://www.reddit.com/r/xmonad/comments/npdtxs/toggle_full_screen_in_xmonad/
    -- , ((modm,               xK_f     ), toggleFull)
    --, ((modm,               xK_f     ), withFocused (sendMessage . maximizeRestore))
    , ((modm,               xK_f     ), do
        sendMessage $ ToggleGaps
        sendMessage $ JumpToLayout "Full"
        )

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)

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

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_semicolon ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_apostrophe), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "~/.config/xmonad/scripts/powermenu.sh")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
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
myLayout = avoidStruts (
            renamed [Replace "Full"]     ( noBorders . maximize . minimize . boringWindows $ Full )
        ||| renamed [Replace "Tiled"]    ( smartBorders . maximize . minimize . boringWindows $ mouseResizableTile )
        ||| renamed [Replace "Mirror"]   ( smartBorders . maximize . minimize . boringWindows $ mouseResizableTileMirrored )
        ||| renamed [Replace "Acordion"] ( smartBorders . maximize . minimize . boringWindows $ Accordion )
        ||| renamed [Replace "Simple"]   ( smartBorders . maximize . minimize . boringWindows $  simpleFloat )
        ||| renamed [Replace "Circle"]     ( noBorders . maximize . minimize . boringWindows $ Circle )
        -- ||| renamed [Replace "Mirror"] ( common $ Mirror tiled )
        -- ||| renamed [Replace "Tiled"] ( common tiled )
        -- ||| renamed [Replace "Acordion"] ( common Accordion )
        -- ||| renamed [Replace "Columns"] ( common $ ThreeColMid 1 0.03 0.5 )
        -- ||| renamed [Replace "Simple"] ( common simpleFloat )
    )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

laySels = [ (s, sendMessage $ JumpToLayout s) | s <- l ]
    where l = [ "Full"
              , "Tiled"
              , "Mirror"
              , "Accordion"
              , "Simple"
              , "Circle" ]

------------------------------------------------------------------------
-- Event Masks:


-- | The client events that xmonad is interested in
-- clientMask :: EventMask
-- clientMask = focusChangeMask .|. clientMask def

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
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ 
      -- className =? "smplayer"       --> doFloat,
      className =? "smplayer"       --> hasBorder False
    , className =? "Gimp"           --> doFloat
    , className =? "tint2"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> hasBorder False
    , isFullscreen --> doFullFloat
    -- , floating --> doF W.shiftMaster
                                 ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty
-- https://stackoverflow.com/questions/23314584/xmonad-focus-hook
-- myEventHook e@(CrossingEvent {ev_event_type=t, ev_window=win}) 
--         | t == enterNotify = do
--                    focus win >> windows W.shiftMaster
--                    return (All True)
--         | t == leaveNotify = do
--                   --  updatePointer (0.5, 0.5) (0, 0)
--                    return (All True)
--         | otherwise = return $ All True


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()
-- myLogHook = updatePointer (0.5, 0.5) (0, 0)

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawnOnce "exec ~/.config/xmonad/scripts/bartoggle"
  spawnOnce "exec eww daemon"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "exec ~/.config/xmonad/scripts/lock.sh"
  spawn "setxkbmap 'us,es' -option grp:alt_shift_toggle"
  spawnOnce "feh --bg-fill Pictures/wallpaper/wallhaven-y8oqgl.png"
  spawnOnce "picom --experimental-backends"
--  spawnOnce "greenclip daemon"
  spawnOnce "dunst"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook = myManageHook, 
        layoutHook = gaps [(L,30), (R,30), (U,40), (D,60)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen,

        clientMask = focusChangeMask .|. clientMask def
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]

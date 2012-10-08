-- my xmonad.hs
-- adapted from many sources, thanks to everyone!
--

-- IMPORTS {{{

import Data.Monoid
-- import List
import System.Exit
import System.IO
import XMonad hiding ( (|||) )

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Prompt 	 as P

import Dzen
import Graphics.X11.ExtraTypes.XF86

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.OnScreen
import XMonad.Actions.Promote
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

-- import XMonad.Hooks.DynamicLog
-- }}}

-- defines and variables {{{

myTerminal = "urxvt"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth = 2
myModMask = mod4Mask

myWorkspaces = ["c&c", "web", "code", "music", "misc", "office", "gfx", "media", "virtual", "TX"]

-- myWorkspaces    = ["一 巣","二 くも","三 著す","四 参照","五","六 曲","七 絵","八 映画館","九 仮想"]
-- Japanese meanings {{{
-- ws 1: su
-- 1: nest; rookery; breeding place; hive;
-- 2: den;
-- 3: haunt;
-- 4: (spider's) web
--
-- ws 2: kumo
-- = spider
--
-- ws 3: arawasu
-- = to write; to publish
--
-- ws 4: sanshou
-- = reference; bibliographical reference; consultation; browsing (e.g. when selecting a file to upload on a computer); checking out 
--
-- ws 5:
--
-- ws 6: kyoku
-- = tune; piece of music
--
-- ws 7: e (!)
-- picture; drawing; painting; sketch
--
-- ws 8: eigakan
-- cinema; movie theatre
--
-- ws 9: kasou
-- 	imagination; supposition; virtual; potential (enemy)
-- }}}

-- workspace variables
cliWs     = (myWorkspaces !! 0)
webWs     = (myWorkspaces !! 1)
codeWs    = (myWorkspaces !! 2)
musicWs   = (myWorkspaces !! 3)
miscWs    = (myWorkspaces !! 4)
officeWs  = (myWorkspaces !! 5)
gimpWs    = (myWorkspaces !! 6)
mediaWs   = (myWorkspaces !! 7)
virtualWs = (myWorkspaces !! 8)
txWs      = (myWorkspaces !! 9)

myNormalBorderColor = "#000000"
-- myFocusedBorderColor = "#306EFF"
myFocusedBorderColor = "#222222"

myJapFontName = "IPAGothic"
myJapFontSize = "10"
myJapaneseFont = myJapFontName ++ "-" ++ myJapFontSize

-- }}}

-- XPConfig {{{
myXPConfig = defaultXPConfig                                    
    { 
	XMonad.Prompt.font  = "-*-terminus-*-*-*-*-13-*-*-*-*-*-*-u" 
	--,fgColor = "#0096d1"
	, fgColor = "#D37E2C"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Bottom
    , historySize = 512
    , showCompletionOnTab = True
    , historyFilter = deleteConsecutive
    }
-- }}}

-- Keybindings {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    ((modm,                 xK_Return),    spawn $ XMonad.terminal conf),
    ((modm .|. shiftMask,   xK_Return),    spawn "urxvt -e screen"),
    ((modm .|. controlMask, xK_Return),    spawn "urxvt -e /home/rolf/bin/tm"),
    ((modm,                 xK_c),         spawn "crawl-tiles"),
    ((modm,                 xK_f),         spawn "firefox"),
    ((modm,                 xK_g),         spawn "gnucash"),
--    ((modm,                 xK_o),         spawn "libreoffice"),
    ((modm,                 xK_s),         spawn "grsync"),
    ((modm,                 xK_v),         spawn "gvim"),
    ((modm,                 xK_w),         spawn "vimprobable2"),
    ((modm,                 xK_x),         spawn "xbmc"),
    ((modm,                 xK_oslash),    spawn "beersmith2"),

    ((modm,                 xK_space),     sendMessage NextLayout),
    ((modm,                 xK_Tab),       windows W.focusDown),
    ((modm,                 xK_b),         withFocused toggleBorder),
    ((modm .|. shiftMask,   xK_c),         kill),
    ((modm,                 xK_h),         sendMessage Shrink),
    ((modm,                 xK_j),         windows W.focusDown),
    ((modm .|. shiftMask,   xK_j),         windows W.swapDown),
    ((modm,                 xK_k),         windows W.focusUp),
    ((modm .|. shiftMask,   xK_k),         windows W.swapUp),
    ((modm,                 xK_l),         sendMessage Expand),
    ((modm,                 xK_m),         windows W.focusMaster),
    ((modm .|. shiftMask,   xK_m),         windows W.swapMaster),
    ((modm,                 xK_n),         refresh),
    ((modm .|. shiftMask,   xK_o),         goToSelected defaultGSConfig),
    ((modm,                 xK_o),         spawnSelected defaultGSConfig ["urxvt", "firefox", "gimp", "libreoffice", "gvim", "xcalc", "gnucash", "beersmith2", "truecrypt", "xbmc"]),
--  ((modm,                 xK_o),         spawn "/home/rolf/bin/dmenumount"),
    ((modm,                 xK_p),         shellPrompt myXPConfig),
    ((modm .|. shiftMask,   xK_p),         runOrRaisePrompt myXPConfig),
    ((modm .|. shiftMask,   xK_t),         sinkAll),
    ((modm,                 xK_Escape),    toggleWS),
    ((modm,                 xK_bar),       scratchpadSpawnActionTerminal myTerminal),

    -- keybindings for controlling MPD
    ((modm,                 xK_Home),      spawn "mpc toggle"),
    ((modm,                 xK_Page_Down), spawn "mpc next"),
    ((modm,                 xK_Page_Up),   spawn "mpc prev"),
    ((modm,                 xK_Insert),    spawn "mpc volume +2"),
    ((modm,                 xK_Delete),    spawn "mpc volume -2"),

    -- multimedia keys etc. on my wireless keyboard
    ((0, xF86XK_Back),                     prevWS),
    ((0, xF86XK_Forward),                  nextWS),
    ((0, xF86XK_Reload),                   spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
    ((0, xK_Cancel),                       spawn "xmessage cancel"),
--  ((0, xK_Kana_Shift),                   spawn "luakit"),

    ((0, xF86XK_AudioStop),                spawn "mpc stop"),
    ((0, xF86XK_AudioPrev),                spawn "mpc prev"),
    ((0, xF86XK_AudioNext),                spawn "mpc next"),
    ((0, xF86XK_AudioPlay),                spawn "mpc toggle"),
    ((0, xF86XK_AudioLowerVolume),         spawn "amixer set Master 5%- unmute"),
    ((0, xF86XK_AudioRaiseVolume),         spawn "amixer set Master 5%+ unmute"),
    ((0, xF86XK_AudioMedia),               spawn "xbmc"),
    ((0, xF86XK_AudioMute),                spawn "amixer set Master mute"),

    ((0, xF86XK_MyComputer),               spawn "xdotool key super+1 alt+7"),
    ((0, xF86XK_Calculator),               sinkAll),
    ((0, xF86XK_Mail),                     spawn "xdotool key super+1 alt+6"),
    ((0, xF86XK_Sleep),                    io (exitWith ExitSuccess)),

    ((modm,               xK_q),           spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
    ((modm .|. shiftMask, xK_q),           io (exitWith ExitSuccess))
    ]
        -- ++
        -- [((m .|. modm, k), windows $ f i)
        --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++
        [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e] [0,1]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
        ++
        [ ((m .|. modm, k), windows (f i))
          | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
          , (f, m) <- [ (viewOnScreen 0, 0)
                      , (viewOnScreen 1, controlMask)
                      , (W.greedyView, 0), (W.shift, shiftMask) ]
        ]
        ++
        [((m .|. controlMask, k), windows $ f i)
            | (i, k) <- zip (workspaces conf) numPadKeys
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ]
        
        -- ++
        --[ ((m .|. modm, k), windows (f i))
        --  | (i, k) <- zip (workspaces conf) ([xK_KP_1 .. xK_KP_9] ++ [xK_KP_0])
        --  , (f, m) <- [ (viewOnScreen 0, 0)
        --              , (viewOnScreen 1, controlMask)
        --              , (W.greedyView, 0), (W.shift, shiftMask) ]
        --]

-- Non-numeric num pad keys, sorted by number 
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert] -- 0
-- }}}

-- Mouse bindings {{{
--
myMouseBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
    ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
    ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)),
    ((modm, button4), (\w -> nextWS)),
    ((modm, button5), (\w -> prevWS))
    ]

-- }}}

-- Layout {{{
tiled x = Tall nmaster delta ratio
    where
        nmaster = x
--        ratio = 1/2
--        Golden ratio:
        ratio = toRational ( 2 / (1 + sqrt 5 :: Double))
        delta = 3/100

fullLayout = (noBorders $ Full) ||| Grid
-- gimpLayout = withIM (0.14) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.22) (Role "gimp-dock") Full
defaultLayout = Grid ||| (tiled 1) ||| Mirror (tiled 1) ||| fullLayout

myLayout = avoidStruts $ smartBorders $ onWorkspace cliWs fullLayout $ onWorkspace webWs defaultLayout $ onWorkspace mediaWs fullLayout $ onWorkspace gimpWs fullLayout $ defaultLayout

-- }}}

-- floatClickFocusHandler {{{
floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent { ev_window = w } = do
        withWindowSet $ \s -> do
                if isFloat w s
                        then (focus w >> promote)
                        else return ()
                return (All True)
                where isFloat w ss = M.member w $ W.floating ss
floatClickFocusHandler _ = return (All True)
-- }}}

-- ManageHook (rules for programs) {{{
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat,
      className =? "Smplayer"       --> doFloat,
      className =? "Vlc"            --> doFloat,
      className =? "Gnucash"        --> doShift officeWs,
      className =? "Firefox"        --> doShift webWs,
      className =? "xbmc.bin"       --> doShift mediaWs <+> doFullFloat,
      className =? "Gimp"           --> doShift gimpWs,
      className =? "Ardour"         --> doShift musicWs,
      className =? "VirtualBox"     --> doShift virtualWs <+> doFullFloat,
      className =? "feh"            --> doFullFloat,
      className =? "Mcomix"         --> doFullFloat,
      className =? "Xmessage"       --> doFloat,
      resource  =? "desktop_window" --> doIgnore,
      className =? "Xmessage"       --> doFloat,
      className =? "Transmission-remote-gtk" --> doShift txWs,
      className =? "Gvim"           --> doShift codeWs
--    className =? "Pcmanfm"        --> doShift fmWs,
--    className =? "Chromium"       --> doShift webWs,
    ] <+> manageDocks <+> manageScratchPad <+> (isFullscreen --> doFullFloat)
-- }}}

-- {{{ Scratchpad

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.1    -- terminal height, 10%
        w = 1      -- terminal width, 100%
        t = 1 - h  -- distance from top edge, 90%
        l = 1 - w  -- distance from left edge, 0% 
-- }}}

-- Other hooks {{{
myEventHook = floatClickFocusHandler

-- myLogHook = return ()

myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-bg", "'#000000'" , "-fg", "'#FF0000'"] } 

myStartupHook = return ()
-- }}}

-- Loghook (PP) {{{
-- 
-- 
myLogHook h = dynamicLogWithPP $ defaultPP -- the h here...
    -- display current workspace as darkgrey on light grey (opposite of default colors)
--    { ppCurrent         = wrapFont myJapaneseFont . dzenColor "#306EFF" "#202020" . pad 
    { ppCurrent         = dzenColor "#306EFF" "#202020" . pad 
--    { ppCurrent         = dzenColor "#D37E2C" "#202020" . pad 

    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad . noScratchPad
--    , ppHidden          = wrapFont myJapaneseFont . dzenColor "#909090" "" . pad 

    -- display other workspaces with no windows as a normal grey
--    , ppHiddenNoWindows = wrapFont myJapaneseFont . dzenColor "#606060" "" . pad 
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 

    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 

    -- if a window on a hidden workspace needs my attention, color it so
    --, ppUrgent          = wrapFont myJapaneseFont . dzenColor "#ff0000" "" . pad . dzenStrip
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    -- shorten if it goes over 100 characters
    , ppTitle           = dzenColor "#306EFF" "#202020" . shorten 150

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "  "

    , ppOutput          = hPutStrLn h -- ... must match the h here
    }
    where
        wrapFont font = wrap ("^fn(" ++ font ++ ")") "^fn()"
        noScratchPad ws = if ws == "NSP" then "" else ws
-- }}}

-- Statusbars {{{
-- 
myTopBar :: DzenConf
myTopBar = defaultDzen
    -- use the default as a base and override width and
    -- colors
    { width       = Just 1920,
      -- x_position  = Just 1280,
      fg_color    = Just "#909090",
      bg_color    = Just "#202020",
      dz_font   = Just "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-u" 
    }

myBottomLeftBar :: DzenConf
myBottomLeftBar = myTopBar
-- use Top Bar as base!
    { y_position = Just 1080,
      width      = Just 960,
      -- x_position = Just 1280,
      alignment  = Just LeftAlign,
      dz_font   = Just "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
    }

myBottomRightBar :: DzenConf
myBottomRightBar = myTopBar
-- use Top Bar as base!
    { y_position = Just 1080,
      -- x_position = Just 2240,
      x_position = Just 960,
      width      = Just 960,
      alignment  = Just RightAlign,
      dz_font   = Just "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
    }
-- }}}

-- {{{ Main
-- main = xmonad =<< dzen defaults
main = do
    d <- spawnDzen myTopBar
    spawnToDzen "conky -c ~/.config/.dzen2conkyrcleft" myBottomLeftBar
    spawnToDzen "conky -c ~/.config/.dzen2conkyrcright" myBottomRightBar
    xmonad $ myUrgencyHook $ ewmh defaultConfig {
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys = myKeys,
        mouseBindings = myMouseBindings,
        layoutHook = myLayout,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        logHook = myLogHook d,
        startupHook = myStartupHook
    }


--- old main {{{
-- main = do
--    conf <- dzen defaultConfig
--    xmonad $ conf 
--        { manageHook = manageDocks <+> manageHook defaultConfig
--        , layoutHook = avoidStruts  $  layoutHook defaultConfig
--        , logHook = dynamicLogWithPP $ xmobarPP
--                        { ppOutput = hPutStrLn xmproc
--                        , ppTitle = xmobarColor "green" "" . shorten 50
--                        }
--        } 
-- }}}
-- }}}
--
-- vim: fdm=marker ts=4 sw=4 sts=4 et:

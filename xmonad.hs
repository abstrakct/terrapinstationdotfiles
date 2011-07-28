-- my xmonad.hs
-- adapted from many sources, thanks to everyone!
--

-- IMPORTS {{{

import XMonad hiding ( (|||) )
import List
import Data.Monoid
import System.Exit
import System.IO

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote

import qualified XMonad.Prompt 		as P
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise

import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators

import Dzen
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Scratchpad

-- import XMonad.Hooks.DynamicLog
-- }}}

-- defines and variables {{{

myTerminal = "urxvt"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth = 1
myModMask = mod4Mask

myWorkspaces = ["I:cli", "II:web", "III:div", "IV:fm", "V", "VI:musikk", "VII:gimp", "VIII:media", "IX:virtuelt"]

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
skriveWs  = (myWorkspaces !! 2)
fmWs      = (myWorkspaces !! 3)
musicWs   = (myWorkspaces !! 5)
gimpWs    = (myWorkspaces !! 6)
mediaWs   = (myWorkspaces !! 7)
virtualWs = (myWorkspaces !! 8)

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
	XMonad.Prompt.font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
	--,fgColor = "#0096d1"
	,fgColor = "#D37E2C"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Bottom
    , historySize = 512
    , showCompletionOnTab = True
    , historyFilter = deleteConsecutive
    }
-- }}}

-- Key bindings {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    ((modm,               xK_Return), spawn $ XMonad.terminal conf),
    ((modm .|. shiftMask, xK_Return), spawn "urxvt -e screen"),
    ((modm,               xK_x),      spawn "xbmc"),
    ((modm,               xK_w),      spawn "firefox"),

    ((modm,               xK_p),      shellPrompt myXPConfig),
    ((modm,               xK_space),  sendMessage NextLayout),
    ((modm,               xK_Tab),    windows W.focusDown),
    ((modm,               xK_j),      windows W.focusDown),
    ((modm,               xK_k),      windows W.focusUp),
    ((modm .|. shiftMask, xK_j),      windows W.swapDown),
    ((modm .|. shiftMask, xK_k),      windows W.swapUp),
    ((modm,               xK_h),      sendMessage Shrink),
    ((modm,               xK_l),      sendMessage Expand),
    ((modm,               xK_m),      windows W.focusMaster),
    ((modm .|. shiftMask, xK_m),      windows W.swapMaster),
    ((modm .|. shiftMask, xK_c),      kill),
    ((modm,               xK_n),      refresh),
    ((modm,               xK_Escape), toggleWS),
    ((modm,               xK_bar),    scratchpadSpawnActionTerminal myTerminal),

    -- keybindings for controlling MPD
    ((modm,               xK_Home),      spawn "mpc toggle"),
    ((modm,               xK_Page_Down), spawn "mpc next"),
    ((modm,               xK_Page_Up),   spawn "mpc prev"),
    ((modm,               xK_Insert),    spawn "mpc volume +2"),
    ((modm,               xK_Delete),    spawn "mpc volume -2"),

    -- multimedia keys etc. on my wireless keyboard

    ((0, xF86XK_Back),       prevWS),
    ((0, xF86XK_Forward),    nextWS),
    ((0, xF86XK_Reload),     spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
    ((0, xF86XK_MyComputer), spawn "urxvt -e ranger"),
    ((0, xF86XK_Sleep),      io (exitWith ExitSuccess)),


    ((modm,               xK_q),      spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
    ((modm .|. shiftMask, xK_q),      io (exitWith ExitSuccess))
    ]
        ++
        [((m .|. modm, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
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
gimpLayout = withIM (0.14) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.22) (Role "gimp-dock") Full
defaultLayout = Grid ||| (tiled 1) ||| Mirror (tiled 1) ||| fullLayout

myLayout = avoidStruts $ onWorkspace cliWs fullLayout $ onWorkspace webWs defaultLayout $ onWorkspace skriveWs fullLayout $ onWorkspace mediaWs fullLayout $ onWorkspace gimpWs gimpLayout $ defaultLayout

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
      className =? "Firefox"        --> doShift webWs,
--      className =? "Chromium"       --> doShift webWs,
      className =? "xbmc.bin"       --> doShift mediaWs,
      className =? "Gimp"           --> doShift gimpWs,
      className =? "Pcmanfm"        --> doShift fmWs,
      className =? "Ardour"         --> doShift musicWs,
      className =? "Gvim"           --> doShift skriveWs,
      className =? "VirtualBox"     --> doShift virtualWs <+> doFullFloat,
      className =? "feh"            --> doFullFloat,
      className =? "Xmessage"       --> doFloat,
      resource  =? "desktop_window" --> doIgnore
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
    , ppTitle           = shorten 100  

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
    { width       = Just 1260
    , fg_color    = Just "#909090"
    , bg_color    = Just "#202020"
--    , Dzen.font   = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
    }

myBottomLeftBar :: DzenConf
myBottomLeftBar = myTopBar
-- use Top Bar as base!
    { y_position = Just 1024,
      width      = Just 640,
      alignment  = Just LeftAlign
    }

myBottomRightBar :: DzenConf
myBottomRightBar = myTopBar
-- use Top Bar as base!
    { y_position = Just 1024,
      x_position = Just 640,
      width      = Just 640,
      alignment  = Just RightAlign
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

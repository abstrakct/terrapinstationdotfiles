-- custom!


--- IMPORTS

import XMonad
-- import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import qualified XMonad.Prompt 		as P
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

import Dzen
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.UrgencyHook

--- CUSTOMIZATIONS

myTerminal = "urxvt"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth = 1
myModMask = mod4Mask

myWorkspaces = ["I:main","II:web","III:fm","IV:media", "V:gimp", "VI", "VII", "VIII", "IX"]

-- workspace variables
mainWs  = (myWorkspaces !! 0)
webWs   = (myWorkspaces !! 1)
fmWs    = (myWorkspaces !! 2)
mediaWs = (myWorkspaces !! 3)
gimpWs  = (myWorkspaces !! 4)

myNormalBorderColor = "#000000"
-- myFocusedBorderColor = "#306EFF"
myFocusedBorderColor = "#222222"


myXPConfig = defaultXPConfig                                    
    { 
	XMonad.Prompt.font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
	,fgColor = "#0096d1"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Bottom
        , historySize = 512
        , showCompletionOnTab = True
        , historyFilter = deleteConsecutive
    }


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        [ ((modm, xK_Return), spawn $ XMonad.terminal conf),
          ((modm .|. shiftMask, xK_Return), spawn "urxvt -e screen"),
          ((modm, xK_x), spawn "xbmc"),
--          ((modm, xK_p), spawn "dodmenu"),
        
          ((modm, xK_p), shellPrompt myXPConfig),
          ((modm, xK_space), sendMessage NextLayout),
          ((modm, xK_Tab), windows W.focusDown),
          ((modm, xK_j), windows W.focusDown),
          ((modm, xK_k), windows W.focusUp),
          ((modm .|. shiftMask, xK_j), windows W.swapDown),
          ((modm .|. shiftMask, xK_k), windows W.swapUp),
          ((modm, xK_h), sendMessage Shrink),
          ((modm, xK_l), sendMessage Expand),

          ((modm, xK_q), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart"),
          ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
        ]
        ++
        [((m .|. modm, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


tiled x = Tall nmaster delta ratio
    where
        nmaster = x
        ratio = 1/2
        delta = 2/100

fullLayout = noBorders $ Full
gimpLayout = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
defaultLayout = (tiled 1) ||| Mirror (tiled 1) ||| fullLayout

myLayout = avoidStruts $ onWorkspace webWs fullLayout $ onWorkspace mediaWs fullLayout $ onWorkspace gimpWs gimpLayout $ defaultLayout


myManageHook = (composeAll
    [ className =? "Mplayer" --> doFloat,
      className =? "Smplayer" --> doFloat,
      className =? "Firefox" --> doShift "II:web",
      className =? "Namoroka" --> doShift "II:web",
      className =? "Pentadactyl" --> doShift "II:web",
      className =? "XBMC" --> doShift "IV:media",
      className =? "Gimp" --> doShift "V:gimp",
      className =? "pcmanfm" --> doShift "III:fm"
    ]) <+> manageDocks

-- myEventHook = mempty

-- myLogHook = return ()

myStartupHook = return ()

-- 
-- Loghook
-- 
-- note: some of these colors may differ from what's in the
-- screenshot, it changes daily
-- 
myLogHook h = dynamicLogWithPP $ defaultPP -- the h here...
    -- display current workspace as darkgrey on light grey (opposite of default colors)
    { ppCurrent         = dzenColor "#306EFF" "#202020" . pad 

    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad 

    -- display other workspaces with no windows as a normal grey
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 

    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 

    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100  

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "  "

    , ppOutput          = hPutStrLn h -- ... must match the h here
    }

-- 
-- StatusBars                                                                                                      
-- 
myTopBar :: DzenConf
myTopBar = defaultDzen
    -- use the default as a base and override width and
    -- colors
    { width       = Just 1200
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

--- LET'S GO!!!
--

-- main = xmonad =<< dzen defaults
main = do
    d <- spawnDzen myTopBar
    spawnToDzen "conky -c ~/.dzen2conkyrcleft" myBottomLeftBar
    spawnToDzen "conky -c ~/.dzen2conkyrcright" myBottomRightBar
--    myFm <- spawn "pcmanfm"
--    terminal1 <- spawn myTerminal
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- keybindings
        keys = myKeys,
        -- mouseBindings = myMouseBindings,
        layoutHook = myLayout,
        manageHook = myManageHook,
--        handleEventHook = myEventHook,
        logHook = myLogHook d,
        startupHook = myStartupHook
    }



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

--------------------------------------------------------------------------------------------
-- ORIGINAL AUTHOR: nnoell <nnoell3@gmail.com>                                            --
--
-- Customized/modified quite a bit by myself.                                             --
--------------------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs                                                                    --
--------------------------------------------------------------------------------------------

-- {{{ Misc
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses #-}
-- }}}
-- {{{Imported libraries
import XMonad
import XMonad.Core

import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Grid

import XMonad.StackSet (RationalRect (..), currentTag)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

import XMonad.Util.Replace
import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad

import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import XMonad.Actions.NoBorders
import XMonad.Actions.UpdateFocus
import XMonad.Actions.Submap

import Data.Monoid
import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (Handle, hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
-- }}}
-- {{{ Variables
myTerminal = "urxvtc"
myBorderWidth = 2
myModKey = modMask
-- }}}
-- {{{ Main
main :: IO ()
main = do
	workspaceBar            <- spawnPipe myWorkspaceBar
	bottomStatusBar         <- spawnPipe myBottomStatusBar
	topStatusBar            <- spawnPipe myTopStatusBar
	secondTopStatusBar      <- spawnPipe mySecondTopBar
	secondBottomStatusBar   <- spawnPipe mySecondBottomBar
	replace
	xmonad $ myUrgencyHook $ ewmh defaultConfig
		{ terminal           = "urxvtc"
		, modMask            = mod4Mask
		, focusFollowsMouse  = True
		, borderWidth        = myBorderWidth
		, normalBorderColor  = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		, layoutHook         = myLayoutHook
		, workspaces         = myWorkspaces
		, manageHook         = manageDocks <+> myManageHook
		, logHook            = (myLogHook workspaceBar) <+> ewmhDesktopsLogHook >> setWMName "LG3D" --ewmh needed so that chromium gain focus
		, handleEventHook    = focusOnMouseMove <+> fullscreenEventHook                             --needed for chromium full screen
		, keys               = myKeys
		, mouseBindings      = myMouseBindings
		, startupHook        = do 
		    setDefaultCursor xC_left_ptr
		    setWMName "LG3D"
		    adjustEventInput
		    ewmhDesktopsStartup
		}
-- }}}

--------------------------------------------------------------------------------------------
-- APPEARANCE CONFIG                                                                      --
--------------------------------------------------------------------------------------------
-- {{{ Colors and fonts
myFont               = "-xos4-terminus-medium-r-normal-*-12-*-*-*-*-*-*-*"
dzenFont             = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#222222" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#3e546a"
colorRed             = "#d74b73"
colorGreen           = "#99cc66"
colorCurrent         = "#64ae38"
myArrow              = "" -- "^fg(" ++ colorWhiteAlt ++ ")>^fg(" ++ colorBlue ++ ")>^fg(" ++ colorGray ++ ")>"
myNormalBorderColor  = colorGray
myFocusedBorderColor = "#0f2a46"
-- }}}
-- {{{ Tab theme
myTabTheme :: Theme
myTabTheme = defaultTheme
	{ fontName            = myFont
	, inactiveBorderColor = colorBlackAlt
	, inactiveColor       = colorBlack
	, inactiveTextColor   = colorGray
	, activeBorderColor   = colorGray
	, activeColor         = colorBlackAlt
	, activeTextColor     = colorWhiteAlt
	, urgentBorderColor   = colorGray
	, urgentTextColor     = colorGreen
	, decoHeight          = 14
	}
-- }}}
-- {{{Prompt theme
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
	{ font                = myFont
	, bgColor             = colorBlack
	--, fgColor             = colorWhite
	, fgColor             = colorBlue
	, bgHLight            = colorBlue
	, fgHLight            = colorWhite
	, borderColor         = colorGrayAlt
	, promptBorderWidth   = 1
	, height              = 16
	, position            = Top
	, historySize         = 100
	, historyFilter       = deleteConsecutive
	, autoComplete        = Nothing
	--, completionKey       = xK_Return
	}
-- }}}
-- {{{ GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
	(0x00,0x00,0x00) -- lowest inactive bg
	(0x1C,0x1C,0x1C) -- highest inactive bg
	(0x39,0x55,0xC4) -- active bg
	(0xBB,0xBB,0xBB) -- inactive fg
	(0x00,0x00,0x00) -- active fg

-- GridSelect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
	{ gs_cellheight  = 50
	, gs_cellwidth   = 200
	, gs_cellpadding = 10
	, gs_font        = myFont
	}
-- }}}
-- {{{ Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:c&c", "2:web", "3:code", "4:skriv", "5:misc", "6:rec", "7:gfx", "8:xbmc", "9:virt", "0:tx"]
-- }}}

--------------------------------------------------------------------------------------------
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------
-- {{{ Layouts
myTile = named "T"  $ smartBorders $ ResizableTall 1 0.03 0.5 []
myMirr = named "MT" $ smartBorders $ Mirror myTile
myMosA = named "M"  $ smartBorders $ MosaicAlt M.empty
myObig = named "O"  $ smartBorders $ OneBig 0.75 0.65
myTabs = named "TS" $ smartBorders $ tabbed shrinkText myTabTheme
myTabM = named "TM" $ smartBorders $ mastered 0.01 0.4 $ tabbed shrinkText myTabTheme
myGimp = named "G"  $ withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") myMosA
myChat = named "C"  $ withIM (0.20) (Title "Buddy List") $ Mirror $ ResizableTall 1 0.03 0.5 []
myFull = named "F"  $ noBorders $ Full

myFloat = named "FL" $ smartBorders $ simplestFloat
myFullscr = named "FS" $ avoidStruts $ smartBorders $ Full 

-- Transformers (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFull (\_ -> x)

-- Layout hook
myLayoutHook = gaps [(U,16), (D,16), (L,0), (R,0)]
	$ avoidStruts
	$ minimize
	$ smartSpacing 6
	-- $ mkToggle (single TABBED)
	-- $ mkToggle (single MIRROR)
	-- $ mkToggle (single REFLECTX)
	-- $ mkToggle (single REFLECTY)
	$ onWorkspace (myWorkspaces !! 0) ccLayouts   --Workspace 1 layouts
	$ onWorkspace (myWorkspaces !! 1) webLayouts  --Workspace 2 layouts
	$ onWorkspace (myWorkspaces !! 2) allLayouts  --Workspace 3 layouts
	$ onWorkspace (myWorkspaces !! 4) allLayouts  --Workspace 5 layouts
	$ onWorkspace (myWorkspaces !! 6) gimpLayouts --Workspace 7 layouts
	$ onWorkspace (myWorkspaces !! 7) fullscreenLayout --Workspace 8 (xbmc)
	-- $ onWorkspace (myWorkspaces !! 4) chatLayouts --Workspace 4 layouts
	$ allLayouts
	where
		allLayouts  = Grid ||| myTile ||| myFull ||| myObig ||| myMirr ||| myMosA ||| myTabM ||| myFloat
		ccLayouts   = myFull ||| Grid ||| myTile ||| myObig ||| myMirr ||| myMosA ||| myTabM ||| myFloat
		webLayouts  = myFull ||| Grid ||| myTabs ||| myTabM ||| myObig
		codeLayouts = myTabM ||| myTile ||| myFull
		gimpLayouts = myGimp ||| Grid ||| myFull
		fullscreenLayout = myFullscr ||| myFull
		--chatLayouts = myChat
-- }}}

--------------------------------------------------------------------------------------------
-- MANAGEHOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------
-- {{{ Scratchpads

myScratchPads = [ NS "ncmpcpp"  spawnNcmpcpp findNcmpcpp manageNcmpcpp
                , NS "terminal" spawnTerm    findTerm    manageTerm
                ]
    where
                spawnNcmpcpp  = myTerminal ++ " -name musicscratch -e ncmpcpp"
                findNcmpcpp   = resource =? "musicscratch"
                manageNcmpcpp = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.8
                        w = 0.8
                        t = (1 - h) / 2
                        l = (1 - w) / 2

                spawnTerm  = myTerminal ++ " -name scratchpad -bg black"
                findTerm   = resource =? "scratchpad"
                manageTerm = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.15   -- terminal height, 10%
                        w = 1      -- terminal width, 100%
                        t = 0.98 - h  -- distance from top edge, 90%
                        l = 1 - w  -- distance from left edge, 0% 


--manageScratchPad :: ManageHook
----manageScratchPad = scratchpadManageHook (W.RationalRect (0.15) (1/50) (1) (3/4))
--scratchPad = scratchpadSpawnActionCustom "urxvtc -name scratchpad -bg black"
--manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
--    where
--        h = 0.15   -- terminal height, 10%
--        w = 1      -- terminal width, 100%
--        t = 1 - h  -- distance from top edge, 90%
--        l = 1 - w  -- distance from left edge, 0% 
--
--manageMusicPad :: ManageHook
--musicPad = scratchpadSpawnActionCustom "urxvt -name music -bg black -e ncmpcpp"
--manageMusicPad = scratchpadManageHook (W.RationalRect l t w h)
--    where
--        h = 0.5    -- terminal height, 50%
--        w = 0.8    -- terminal width, 100%
--        t = 1 - h  -- distance from top edge
--        l = 1 - w  -- distance from left edge
-- }}}
-- {{{ Managehook (rules for programs)
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
	[ [resource     =? r     --> doIgnore                             | r <- myIgnores] --ignore desktop
	, [className    =? c     --> doShift (myWorkspaces !! 1)          | c <- myWebS   ] --move myWebS windows to workspace 1 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 2)          | c <- myCodeS  ]
	, [className    =? c     --> doShift (myWorkspaces !! 6)          | c <- myGfxS   ] --move myGfxS windows to workspace 4 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 7)          | c <- myXBMC   ]
	, [className    =? c     --> doFullFloat                          | c <- myFullscr]
	, [className    =? c     --> doShift (myWorkspaces !! 9)          | c <- myOtherS ] --move myOtherS windows to workspace 5 by classname and shift (was doShiftAndGo)
	, [className    =? c     --> doCenterFloat                        | c <- myFloatCC] --float center geometry by classname
	, [name         =? n     --> doSideFloat NW                       | n <- myFloatSN] --float side NW geometry by name
	, [className    =? c     --> doF W.focusDown                      | c <- myFocusDC] --dont focus on launching by classname
	, [isFullscreen          --> doFullFloat]
    , [name         =? n     --> doCenterFloat                        | n <- myFloatCN] --float center geometry by name
	]) <+> namedScratchpadManageHook myScratchPads -- <+> manageScratchPad
	where
		doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
		role            = stringProperty "WM_WINDOW_ROLE"
		name            = stringProperty "WM_NAME"
		myIgnores       = ["desktop","desktop_window"]
		myWebS          = ["Firefox"]
		myGfxS          = ["Gimp", "gimp", "GIMP"]
		myCodeS         = ["Gvim"]
		myChatS         = ["Pidgin", "Xchat"]
		myGameS         = ["zsnes"]
		myXBMC          = ["xbmc"]
		myOtherS        = ["Transmission-remote-gtk"]
		myFloatCC       = ["sun-applet-AppletViewer", "G15-config", "cataclysm-tiles", "Dogecoin-qt", "Bitcoin-qt", "Steam", "Thunar", "ds", "t-engine", "MPlayer", "Smplayer", "mplayer2", "Smplayer2", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1", "Gksu", "PSX", "Galculator", "Nvidia-settings", "Vidalia", "XFontSel", "XCalc", "XClock"]
		myFloatSN       = ["Event Tester"]
		myFocusDC       = ["Event Tester", "Notify-osd"]
		myFloatCN       = ["Volume Control", "PlayOnLinux"]
		myFullscr       = ["t-engine64", "xbmc"]
-- }}}

--------------------------------------------------------------------------------------------
-- STATUSBARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------
-- {{{ UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }
-- }}}
-- {{{ StatusBars
myWorkspaceBar, myBottomStatusBar, myTopStatusBar, mySecondTopBar, mySecondBottomBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y '0' -h '16' -w '1510' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -p -e ''"
myBottomStatusBar = "/home/rolf/bin/statusbar.first.bottom.sh"
myTopStatusBar    = "/home/rolf/bin/statusbar.first.top.sh"
mySecondBottomBar   = "/home/rolf/bin/statusbar.second.bottom.sh"
mySecondTopBar      = "/home/rolf/bin/statusbar.second.top.sh"
-- }}}
-- {{{ myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) -- hide "NSP" from workspace list
	, ppOrder           = orderText
	, ppExtras          = []
	, ppSep             = "^fg(" ++ colorGray ++ ")|"
	, ppWsSep           = ""
	, ppCurrent         = dzenColor colorCurrent  colorBlack . pad . wrap "[" "]" 
	, ppUrgent          = dzenColor colorGreen    colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppVisible         = dzenColor colorBlue     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppHidden          = dzenColor colorWhiteAlt colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppHiddenNoWindows = dzenColor colorGrayAlt  colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
	, ppLayout          = dzenColor colorBlue     colorBlack . pad . wrapClickLayout . layoutText
	, ppTitle           = dzenColor colorWhiteAlt colorBlack . pad . wrapClickTitle . titleText . dzenEscape
	}
	where
		--display config
		orderText (ws:l:t:_) = [ws,l,t]
		titleText [] = "Desktop " ++ myArrow
		titleText x = (shorten 120 x) ++ " " ++ myArrow
		layoutText "Minimize T"  = "ResizeTall"
		layoutText "Minimize O"  = "One Big"
		layoutText "Minimize TS" = "Tabbed"
		layoutText "Minimize TM" = "Master"
		layoutText "Minimize M"  = "Mosaic"
		layoutText "Minimize MT" = "Mirror"
		layoutText "Minimize G"  = "Mosaic"
		layoutText "Minimize C"  = "Mirror"
		layoutText "Minimize F"  = "Full"
		layoutText "Minimize FS" = "Fullscreen"
		layoutText "Minimize FL" = "Float"
		layoutText "Minimize SmartSpacing 6 T"  = "ResizeTall"
		layoutText "Minimize SmartSpacing 6 O"  = "One Big"
		layoutText "Minimize SmartSpacing 6 TS" = "Tabbed"
		layoutText "Minimize SmartSpacing 6 TM" = "Master"
		layoutText "Minimize SmartSpacing 6 M"  = "Mosaic"
		layoutText "Minimize SmartSpacing 6 MT" = "Mirror"
		layoutText "Minimize SmartSpacing 6 G"  = "Mosaic"
		layoutText "Minimize SmartSpacing 6 C"  = "Mirror"
		layoutText "Minimize SmartSpacing 6 F"  = "Full"
		layoutText "Minimize SmartSpacing 6 FS" = "Fullscreen"
		layoutText "Minimize SmartSpacing 6 FL" = "Float"
		layoutText "Minimize SmartSpacing 6 Grid"  = "Grid"
		--layoutText "Minimize ReflectX T"  = "^fg(" ++ colorGreen ++ ")ReTall X^fg()"
		--layoutText "Minimize ReflectX O"  = "^fg(" ++ colorGreen ++ ")OneBig X^fg()"
		--layoutText "Minimize ReflectX TS" = "^fg(" ++ colorGreen ++ ")Tabbed X^fg()"
		--layoutText "Minimize ReflectX TM" = "^fg(" ++ colorGreen ++ ")Master X^fg()"
		--layoutText "Minimize ReflectX M"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
		--layoutText "Minimize ReflectX MT" = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
		--layoutText "Minimize ReflectX G"  = "^fg(" ++ colorGreen ++ ")Mosaic X^fg()"
		--layoutText "Minimize ReflectX C"  = "^fg(" ++ colorGreen ++ ")Mirror X^fg()"
		--layoutText "Minimize ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall Y^fg()"
		--layoutText "Minimize ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig Y^fg()"
		--layoutText "Minimize ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed Y^fg()"
		--layoutText "Minimize ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master Y^fg()"
		--layoutText "Minimize ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
		--layoutText "Minimize ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
		--layoutText "Minimize ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic Y^fg()"
		--layoutText "Minimize ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror Y^fg()"
		--layoutText "Minimize ReflectX ReflectY T"  = "^fg(" ++ colorGreen ++ ")ReTall XY^fg()"
		--layoutText "Minimize ReflectX ReflectY O"  = "^fg(" ++ colorGreen ++ ")OneBig XY^fg()"
		--layoutText "Minimize ReflectX ReflectY TS" = "^fg(" ++ colorGreen ++ ")Tabbed XY^fg()"
		--layoutText "Minimize ReflectX ReflectY TM" = "^fg(" ++ colorGreen ++ ")Master XY^fg()"
		--layoutText "Minimize ReflectX ReflectY M"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
		--layoutText "Minimize ReflectX ReflectY MT" = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
		--layoutText "Minimize ReflectX ReflectY G"  = "^fg(" ++ colorGreen ++ ")Mosaic XY^fg()"
		--layoutText "Minimize ReflectX ReflectY C"  = "^fg(" ++ colorGreen ++ ")Mirror XY^fg()"
		layoutText x = "^fg(" ++ colorGreen ++ ")" ++ x ++ "^fg()"
		--clickable config
		wrapClickLayout content = "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"                                                           --clickable layout
		wrapClickTitle content = "^ca(1,xdotool key super+j)" ++ content ++ "^ca()"                                                                --clickable title
		--wrapClickWorkSpace (idx,str) = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "e;" ++ xdo index ++ ")" ++ str ++ "^ca()^ca()" --clickable workspaces
		wrapClickWorkSpace (idx,str) = "^ca(1," ++ xdo index ++ ")" ++ "^ca(3," ++ xdo index ++ ")" ++ str ++ "^ca()^ca()" --clickable workspaces
			where
				wsIdxToString Nothing = "1"
				wsIdxToString (Just n) = show (n+1)
				index = wsIdxToString (elemIndex idx myWorkspaces)
				xdo key = "xdotool key super+" ++ key
-- }}}

--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------
-- {{{ Key bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modMask,                 xK_p),      spawn "dmenu_run -dim 0.7 -nb black -nf \\#3955c4 -sb \\#3955c4 -sf white -fn -*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*")
	, ((modMask .|. shiftMask,   xK_p),      xmonadPrompt myXPConfig)                                              --Launch Xmonad prompt
	, ((modMask,                 xK_bar),    scratchTerm)                                                    --Scratchpad
	, ((modMask,                 xK_q),      scratchMusic)
	, ((0,              xF86XK_AudioMedia),  scratchMusic)
	, ((modMask,                 xK_space),  sendMessage NextLayout)                                            --Rotate through the available layout algorithms
	, ((modMask .|. shiftMask,   xK_space),  setLayout $ XMonad.layoutHook conf)                 --Reset the layouts on the current workspace to default
	, ((modMask,                 xK_Tab),    windows W.focusDown)
    , ((modMask,                 xK_b),      withFocused toggleBorder)
	, ((modMask .|. shiftMask,   xK_c),      kill)                                                                  --Close focused window
	, ((modMask,                 xK_f),      spawn "dmenulocate")                     -- Use 'locate' with dmenu!
	--, ((modMask .|. controlMask, xK_g),      sendMessage $ ToggleGaps) 
	, ((modMask,                 xK_i),      goToSelected $ myGSConfig myColorizer)                                 --Launch GridSelect
	--, ((modMask,                 xK_n),      refresh)                                                               --Resize viewed windows to the correct size
	, ((modMask,                 xK_h),      sendMessage Shrink)                                                    --Shrink the master area
	, ((modMask .|. shiftMask,   xK_h),      sendMessage MirrorShrink)                                --MirrorShrink the master area
	, ((modMask .|. controlMask, xK_h),      withFocused (keysMoveWindow (-1,0)))
	, ((modMask .|. mod1Mask,    xK_h),      withFocused (keysResizeWindow (-1,0) (0,0)))
	, ((modMask,                 xK_j),      windows W.focusDown)
	, ((modMask .|. shiftMask,   xK_j),      windows W.swapDown  )                                    --Swap the focused window with the next window
	, ((modMask .|. controlMask, xK_j),      withFocused (keysMoveWindow (0,1)))
	, ((modMask .|. mod1Mask,    xK_j),      withFocused (keysResizeWindow (0,1) (0,1)))
	, ((modMask,                 xK_k),      windows W.focusUp)                                                     --Move focus to the previous window
	, ((modMask .|. shiftMask,   xK_k),      windows W.swapUp    )                                    --Swap the focused window with the previous window
	, ((modMask .|. controlMask, xK_k),      withFocused (keysMoveWindow (0,-1)))
	, ((modMask .|. mod1Mask,    xK_k),      withFocused (keysResizeWindow (0,-1) (0,0)))
	, ((modMask,                 xK_l),      sendMessage Expand)                                                    --Expand the master area
	, ((modMask .|. shiftMask,   xK_l),      sendMessage MirrorExpand)                                --MirrorExpand the master area
	, ((modMask .|. controlMask, xK_l),      withFocused (keysMoveWindow (1,0)))
	, ((modMask .|. mod1Mask,    xK_l),      withFocused (keysResizeWindow (1,0) (0,1)))
	, ((modMask,                 xK_m),      withFocused minimizeWindow)                                            --Minimize window
	, ((modMask .|. shiftMask,   xK_m),      sendMessage RestoreNextMinimizedWin)                     --Restore window
	, ((modMask,                 xK_n),      sendMessage RestoreNextMinimizedWin)                     --Restore window
	, ((modMask .|. shiftMask,   xK_s),      spawn "xscreensaver-command -lock")                                   --Lock screen
	, ((modMask,                 xK_t),      withFocused $ windows . W.sink)                                        --Push window back into tiling
	, ((modMask .|. shiftMask,   xK_t),      rectFloatFocused)                                        --Push window into float
	, ((modMask .|. shiftMask,   xK_Left),   sendMessage Shrink)
	, ((modMask .|. shiftMask,   xK_Right),  sendMessage Expand)
	, ((modMask .|. shiftMask,   xK_Down),   sendMessage MirrorShrink)
	, ((modMask .|. shiftMask,   xK_Up),     sendMessage MirrorExpand)
	, ((modMask .|. controlMask, xK_Left),   withFocused (keysResizeWindow (-30,0) (0,0)))       --Shrink floated window horizontally by 50 pixels
	, ((modMask .|. controlMask, xK_Right),  withFocused (keysResizeWindow (30,0) (0,0)))       --Expand floated window horizontally by 50 pixels
	, ((modMask .|. controlMask, xK_Up),     withFocused (keysResizeWindow (0,-30) (0,0)))         --Shrink floated window verticaly by 50 pixels
	, ((modMask .|. controlMask, xK_Down),   withFocused (keysResizeWindow (0,30) (0,0)))        --Expand floated window verticaly by 50 pixels
	, ((modMask,                 xK_period), sendMessage (IncMasterN (-1)))                                    --Deincrement the number of windows in the master area
    , ((modMask .|. controlMask, xK_q),      spawn "killall dzen2; xmonad --recompile; xmonad --restart")
	, ((modMask .|. shiftMask,   xK_q),      io (exitWith ExitSuccess))                               --Quit xmonad
	, ((modMask,                 xK_comma),  toggleWS)                                                          --Toggle to the workspace displayed previously
    , ((0, xF86XK_Reload),                   spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")
	 
	 -- mpdmenus
    --, ((modMask,                 xK_o),      spawn "/home/rolf/bin/dmenumpd -a")
    --, ((modMask .|. shiftMask,   xK_o),      spawn "/home/rolf/bin/dmenumpd -l")
    --, ((modMask .|. controlMask, xK_o),      spawn "/home/rolf/bin/dmenumpd -j")
    , ((modMask,                 xK_o), submap . M.fromList $
        [ ((0, xK_a),                        spawn "/home/rolf/bin/dmenumpd -a")  -- artists
        , ((0, xK_b),                        spawn "/home/rolf/bin/dmenumpd -l")  -- albums
        , ((0, xK_c),                        spawn "/home/rolf/bin/dmenumpd -c")  -- current playlist
        ])

	-- Keys to launch programs
	, ((modMask,                 xK_Return), spawn $ XMonad.terminal conf)                       --Launch a terminal
	, ((modMask .|. shiftMask,   xK_Return), spawn "urxvt -e /home/rolf/bin/tm")                 --Launch a terminal with tmux
	, ((modMask .|. controlMask, xK_Return), spawn "terminology")
	, ((modMask,                 xK_s),      spawn "steam")
	, ((modMask,                 xK_c),      spawn "crawl-tiles")
	, ((modMask,                 xK_d),      spawn "dogecoin-qt")
	, ((modMask,                 xK_e),      spawn "eclipse")
	, ((modMask,                 xK_g),      spawn "gnucash")
	, ((modMask,                 xK_v),      spawn "gvim")
	, ((modMask,                 xK_x),      spawn "/home/rolf/bin/launch-xbmc")

	-- Utilities on F-keys
	, ((modMask,                 xK_F1),     spawn "/home/rolf/bin/dmenufiles")

     -- keybindings for controlling MPD
    -- , ((modMask,                 xK_Home),      spawn "mpc -p 6660 toggle")
    -- , ((modMask,                 xK_Page_Down), spawn "mpc next")
    -- , ((modMask,                 xK_Page_Up),   spawn "mpc prev")

	, ((modMask, xK_Left), prevWS)
	, ((modMask, xK_Right), nextWS)                                                            --Move to next Workspace
	, ((0, xF86XK_AudioRaiseVolume), spawn "sh /home/rolf/bin/dzen/dzen_vol.sh + -d")                --Raise volume
	, ((0, xF86XK_AudioLowerVolume), spawn "sh /home/rolf/bin/dzen/dzen_vol.sh - -d")                --Lower volume
	, ((0, xF86XK_AudioMute),        spawn "sh /home/rolf/bin/dzen/dzen_vol.sh t -d")                --Toggle mute
	, ((0, xF86XK_AudioNext), spawn "mpc -p 6660 next")                                            --next song
	, ((0, xF86XK_AudioPrev), spawn "mpc -p 6660 prev")                                            --prev song
	, ((0, xF86XK_AudioPlay), spawn "mpc -p 6660 toggle")                                          --toggle song
	, ((0, xF86XK_AudioStop), spawn "mpc -p 6660 stop")                                            --stop song
	, ((mod1Mask, xK_Up),     spawn "sh /home/rolf/bin/dzen/dzen_vol.sh + -d")
	, ((mod1Mask, xK_Down),   spawn "sh /home/rolf/bin/dzen/dzen_vol.sh - -d")
	--, ((mod1Mask, xK_Right),  spawn "ncmpcpp next")
	--, ((mod1Mask, xK_Left),   spawn "ncmpcpp prev")
	--, ((mod1Mask .|. controlMask, xK_Down), spawn "ncmpcpp toggle")
	--, ((mod1Mask .|. controlMask, xK_Up), spawn "ncmpcpp stop")
	--, ((0, xK_Cancel), spawn "xscreensaver-command -lock")                            --Lock screen
	--, ((0, xK_Print), spawn "scrot '%Y-%m-%d_%s_$wx$h.png'")                                      --Take a screenshot
	--, ((0, xF86XK_Search),    spawn "/home/rolf/bin/dmenulocate")                     -- Use 'locate' with dmenu!
	--, ((0, xF86XK_WWW),       spawn "firefox")
    
    -- Old/unused crap
  --, ((modMask,                 xK_f),      spawn "firefox")
  --, ((modMask, xK_q), restart "xmonad" True)                                                 --Restart xmonad
  --, ((modMask,                 xK_q),      spawn "killall dzen2; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
  --, ((modMask,                 xK_a),      windows W.focusMaster)                                                 --Move focus to the master window
  --, ((modMask .|. shiftMask,   xK_a),      windows W.swapMaster)                                    --Swap the focused window and the master window
  --, ((modMask .|. shiftMask,   xK_f),      fullFloatFocused)                                        --Push window into full screen
  --, ((modMask,                 xK_f),      sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED)                 --Push layout into tabbed
  --, ((modMask,                 xK_comma),  sendMessage (IncMasterN 1))                                        --Increment the number of windows in the master area
  --, ((0, xF86XK_AudioMedia),               spawn "/home/rolf/bin/launch-xbmc")
  --  ((modMask,                 xK_p),      shellPrompt myXPConfig)                                              --Launch Xmonad shell prompt
  --, ((modMask,                 xK_Insert),    spawn "mpc volume +2")
  --, ((modMask,                 xK_Delete),    spawn "mpc volume -2")
  --, ((modMask .|. shiftMask,   xK_x),      sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX) --Reflect layout by X
  --, ((modMask .|. shiftMask,   xK_y),      sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY) --Reflect layout by Y
  --, ((modMask .|. shiftMask,   xK_z),      sendMessage $ Toggle MIRROR)                             --Push layout into mirror
  --, ((mod1Mask, xK_masculine), toggleOrView (myWorkspaces !! 0))                             --if ws != 0 then move to workspace 0, else move to latest ws I was
  --, ((modMask .|. controlMask, xK_Left),  prevWS)                                           --Move to previous Workspace
  --, ((modMask .|. controlMask, xK_Right), nextWS)
	]
	++
	[((m .|. modMask, k), windows $ f i)                                                       --Switch to n workspaces and send client to n workspaces
		| (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
		, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	++
	[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))                --Switch to n screens and send client to n screens
	    -- | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
	    | (key, sc) <- zip [xK_w, xK_e] [0..]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	where
		fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
		rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ RationalRect 0.05 0.05 0.9 0.9) f
		scratchTerm  = namedScratchpadAction myScratchPads "terminal"
		scratchMusic = namedScratchpadAction myScratchPads "ncmpcpp"
-- }}}
-- {{{ Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- set the window to floating mode and move by dragging
	, ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))                      -- raise the window to the top of the stack
	, ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))                   -- set the window to floating mode and resize by dragging
	, ((modMask, button4), (\_ -> prevWS))                                                -- switch to previous workspace
	, ((modMask, button5), (\_ -> nextWS))                                                -- switch to next workspace
	]
-- }}}

-- vim: fdm=marker ts=4 sw=4 sts=4 et:

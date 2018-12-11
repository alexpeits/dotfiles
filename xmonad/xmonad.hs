{-# LANGUAGE CPP #-}
-- #define JOB
import System.IO
import System.Exit

import Data.Maybe (isJust)

import Control.Monad.IO.Class

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import qualified XMonad.Hooks.DynamicBars as Bars

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed as Tabs
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen
import XMonad.Layout.Named
import qualified XMonad.Layout.Combo as Combo
import qualified XMonad.Layout.Decoration as Deco
import qualified XMonad.Layout.TwoPane as TwoPane
import qualified XMonad.Layout.WindowNavigation as WNav
import qualified XMonad.Layout.IndependentScreens as IndS
import qualified XMonad.Layout.Reflect as Reflect
import qualified XMonad.Layout.NoFrillsDecoration as NoFrills

import XMonad.Util.Run (spawnPipe, safeSpawn, runProcessWithInput)
import XMonad.Util.NamedWindows
#ifndef JOB
import XMonad.Util.Ungrab
#endif
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS as Cycle
import XMonad.Actions.WindowBringer
import qualified XMonad.Actions.CopyWindow as CopyW

import qualified XMonad.StackSet as W

import qualified Data.Map        as M
import Graphics.X11.ExtraTypes.XF86


------------------------------------------------------------------------
-- # Variables

myTerminal = "/usr/bin/gnome-terminal"
myScreensaver = "xscreensaver-command -lock"
mySelectScreenshot = "gnome-screenshot -a"
myWindowScreenshot = "scrot ~/Pictures/Screenshots/%Y-%m-%d_%H-%M-%S.png -s"
myScreenshot = "gnome-screenshot"
myLauncher = unwords
  [ "rofi -modi drun,run -show drun"
  , "-matching fuzzy -no-levenshtein-sort -sort"
  , "-theme lb -show-icons -kb-mode-next Alt+m"
  ]
rofiGoToWinArgs =
  [ "-dmenu"
  , "-i", "-p", "Go to window"
  , "-matching", "fuzzy", "-no-levenshtein-sort", "-sort"
  , "-theme", "lb"
  ]
firefoxDocs = unwords
  [ "firefox"
  , "--new-instance --class Docs -P Simple"
  , "https://hoogle.haskell.org"
  , "https://www.haskell.org/hoogle"
  , "https://pursuit.purescript.org"
  ]

------------------------------------------------------------------------
-- # Colors

myNormalBorderColor  = "#5b5b5b"
myFocusedBorderColor = "#db7272"
myBorderWidth = 1

xmobarTitleColor = "#d58966"
xmobarInactiveTitleColor = "#656565"
xmobarCurrentWorkspaceColor = "#2ec8a2"
xmobarVisibleWorkspaceColor = "#3b7887"
xmobarInactiveCurrentWorkspaceColor = "#656565"
xmobarInactiveVisibleWorkspaceColor = "#656565"
xmobarInactiveWorkspaceColor = "#656565"
xmobarLayoutColor = "#676767"
xmobarSepColor = xmobarLayoutColor

------------------------------------------------------------------------
-- # Scratchpads

myScratchpads =
  [ NS "scratch" "gedit --class=Scratch ~/.scratch.txt" (className =? "Scratch") smallRectBR
  -- , NS "scratch" "gnome-terminal --role=scratch -- em ~/.scratch.org" (role =? "scratch") smallRectBR
  -- , NS "zeal" "zeal" (className =? "Zeal") largeRectM
  , NS "docs" firefoxDocs (className =? "Docs") medRectBR
  , NS "dropTerm" "gnome-terminal --role=dropTerm" (role =? "dropTerm") dropDown
  ]
  where role = stringProperty "WM_WINDOW_ROLE"
        title = stringProperty "WM_NAME"

myScratchAction = namedScratchpadAction myScratchpads  -- helper

myTopMargin =
#ifdef JOB
  28 / 2160  -- depends on xmobar height
#else
  20 / 1080  -- depends on xmobar height
#endif
middleRR w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h
topRightRR w h = W.RationalRect (1 - w) myTopMargin w h
topLeftRR w h = W.RationalRect 0 myTopMargin w h
botRightRR w h = W.RationalRect (1 - w) (1 - h) w h
botLeftRR w h = W.RationalRect 0 (1 - h) w h
dropDownRR w h = W.RationalRect 0 myTopMargin w h

largeRectM = customFloating $ middleRR 0.8 0.8
medRectM = customFloating $ middleRR 0.65 0.75
medRectBR = customFloating $ botRightRR 0.4 0.45
smallRectTR = customFloating $ topRightRR 0.25 0.3
smallRectBR = customFloating $ botRightRR 0.3 0.4
dropDown = customFloating $ dropDownRR 1 0.35

------------------------------------------------------------------------
-- # Workspaces

#ifdef JOB
myWorkspaces = IndS.withScreens 2 $ map show [1..9]
#else
myWorkspaces =
    [ "1:main"
    , "2:term"
    , "3:emacs"
    , "4:chat"
    , "5:music"
    , "6:vm"
    ] ++ map show [7..9]
#endif

nWorkspace = (myWorkspaces !!) . pred  -- helper

------------------------------------------------------------------------
-- # Windows

myManageHook = composeAll
    [
#ifndef JOB
      className =? "Emacs" --> doShift (nWorkspace 3)
    , className =? "Slack" --> doShift (nWorkspace 4)
    , className =? "Skype" --> doShift (nWorkspace 4)
    , className =? "Pidgin" --> doShift (nWorkspace 4)
    , className =? "vlc" --> doShift (nWorkspace 5)
    , className =? "Spotify" --> doShift (nWorkspace 5)
    , className =? "spotify" --> doShift (nWorkspace 5)
    , className =? "VirtualBox" --> doShift (nWorkspace 6)
    , className =? "VirtualBox Manager" --> doShift (nWorkspace 6)
    ,
#endif

    -- , className =? "Nautilus" --> medRectM
     className =? "Gnome-calculator" --> smallRectTR

    , className =? "Indicator.py" --> doFloatAt 0.43 0.43
    , className =? "Zenity" --> doFloatAt 0.43 0.43
    , className =? "Gsimplecal" --> doFloatAt 0.815 0.022

    , className =? "stalonetray" --> doIgnore

    , isFullscreen --> doFullFloat
    ]

------------------------------------------------------------------------
-- # Layouts

myLayout = avoidStruts $
#ifdef JOB
  named "Tall" (deco myTall)
  ||| named "Left" (deco myLeft)
  ||| named "Right" (deco myRight)
  ||| named "Focus" (deco (Mirror (Tall nmaster delta bigMasterRatio)))
#else
  myTall
  ||| named "Focus" (Mirror (Tall nmaster delta bigMasterRatio))
  ||| named "3Col" (ThreeColMid nmaster delta halfRatio)
  ||| named "Tabs" (Tabs.tabbed Deco.shrinkText tabTheme)
#endif
  -- ||| named "VTall" (Mirror myTall)
  -- ||| named "Split" (Combo.combineTwo (TwoPane.TwoPane delta 0.295) (Mirror myTall) simpleTabbed)
  -- ||| named "RSplit" (Combo.combineTwo (TwoPane.TwoPane delta 0.295) (Mirror myTall) myTabbed)
  -- ||| named "LSplit" (Combo.combineTwo (TwoPane.TwoPane delta 0.71) myTabbed (Mirror myTall))
  -- ||| named "BSplit" (Combo.combineTwo (Mirror (TwoPane.TwoPane delta 0.63)) (Mirror myTall) myTabbed)
  where
    myTall = Tall nmaster delta halfRatio
    myLeft = named "Left" (Tall nmaster delta 0.75)
    myRight = named "Right" (Reflect.reflectHoriz myLeft)
    -- theme
    tabTheme = def
      { activeColor = "#245361"
      , activeBorderColor = "#245361"
      , inactiveColor = "#091f2e"
      , inactiveBorderColor = "#245361"
      , decoHeight = 18
      , fontName = "xft:DejaVu Sans Mono for Powerline:size=8"
      }
    -- The default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment by when resizing panes
    delta = 3/100
    -- Ratios
    halfRatio = 1/2
    bigMasterRatio = 75/100
    deco = NoFrills.noFrillsDeco shrinkText topBarTheme
    topBarTheme = def
     { inactiveBorderColor   = "#002b36"
     , inactiveColor         = "#002b36"
     , inactiveTextColor     = "#002b36"
     , activeBorderColor     = "#268bd2"
     , activeColor           = "#268bd2"
     , activeTextColor       = "#268bd2"
     , decoHeight            = 10
     }

------------------------------------------------------------------------
-- # Keybindings

-- helpers
isActiveWS :: WindowSpace -> Bool
isActiveWS ws@(W.Workspace tag _ _) = isActive && isNotScratch
  where isActive = isJust $ W.stack ws
        isNotScratch = tag /= "NSP"

getScreenshot :: X ()
getScreenshot = do
  scrStr <-
    runProcessWithInput "rofi-dmenu-args.sh"
    ["Screenshot type", "Area", "Window", "Full"] ""
  case filter (/= '\n') scrStr of
    "Area"   -> spawn mySelectScreenshot
#ifndef JOB
    "Window" -> unGrab >> spawn myWindowScreenshot
#endif
    "Full"   -> spawn myScreenshot
    _        -> return ()

myModMask = mod4Mask  -- win key

customKeys conf@(XConfig{XMonad.modMask = modMask}) =
  -- terminal
  [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

  -- screen lock
  , ((modMask .|. controlMask, xK_l), spawn myScreensaver)

  -- launcher
  , ((modMask, xK_p), spawn myLauncher)
  -- window select
  , ((modMask .|. shiftMask, xK_p) , gotoMenuArgs' "rofi" rofiGoToWinArgs)
  -- screenshots
  , ((modMask .|. controlMask .|. shiftMask, xK_p), getScreenshot)

  -- scratchpads
  , ((modMask, xK_z), myScratchAction "dropTerm")
  , ((0, xK_F12), myScratchAction "dropTerm")
  , ((modMask .|. shiftMask, xK_n), myScratchAction "scratch")
  , ((modMask .|. shiftMask, xK_d), myScratchAction "docs")

  -- toggle xmobar
  , ((modMask .|. shiftMask, xK_f), sendMessage ToggleStruts)

  -- mute/unmute
  -- There is a bug with this one:
  -- , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
  , ((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master 1+ toggle")
  -- volume down
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
  -- volume up
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
  -- mic
#ifndef JOB
  , ((0, xF86XK_AudioMicMute), spawn "amixer -q set Capture toggle")
#endif
  -- , ((modMask, xK_F5), spawn "amixer -q set Master 5%-")
  -- , ((modMask, xK_F6), spawn "amixer -q set Master toggle")
  -- , ((modMask, xK_F7), spawn "amixer -q set Master 5%+")

  -- next track
  , ((modMask, xK_F2), spawn "playerctl previous")
  -- previous track
  , ((modMask, xK_F3), spawn "playerctl play-pause")
  -- play/pause
  , ((modMask, xK_F4), spawn "playerctl next")

  -- brightness up
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5%")
  -- brightness down
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5%")

  -- move windows to sublayouts
  , ((modMask .|. shiftMask, xK_Left), sendMessage $ WNav.Move L)
  , ((modMask .|. shiftMask, xK_Right), sendMessage $ WNav.Move R)
  , ((modMask .|. shiftMask, xK_Up), sendMessage $ WNav.Move U)
  , ((modMask .|. shiftMask, xK_Down), sendMessage $ WNav.Move D)

  -- mycle monitors
  , ((modMask, xK_o), Cycle.nextScreen)
  , ((modMask .|. shiftMask, xK_o), Cycle.shiftNextScreen)
  -- cycle workspaces
  -- , ((modMask .|. shiftMask, xK_o), moveTo Prev (WSIs (return isActiveWS)))
#ifdef JOB
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown >> windows W.focusUp)
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp >> windows W.focusDown)
#endif
  ]

  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
#ifdef JOB
  [((m .|. modMask, k), windows $ IndS.onCurrentScreen f i)
      | (i, k) <- zip (IndS.workspaces' conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
#else
  [((m .|. modMask, k), windows $  f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
#endif

myKeys x = M.union (M.fromList (customKeys x)) (keys defaultConfig x)

------------------------------------------------------------------------
-- # xmobar

barCreator (S sid) = spawnPipe $
  "xmobar --screen " ++ show sid
  ++ " ~/.xmonad/xmobar" ++ show sid ++ ".hs"

#ifdef JOB
tLength = 200
#else
tLength = 100
#endif

myLogPPActive copies = (myLogPP copies)
  { ppTitle = xmobarColor xmobarTitleColor "" . shorten tLength
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
  , ppVisible = xmobarColor xmobarVisibleWorkspaceColor "" . wrap "(" ")"
  , ppHidden = \ws -> if ws == "NSP" then "" else ws
  }

myLogPP copies = defaultPP
  { ppTitle = xmobarColor xmobarInactiveTitleColor "" . shorten tLength
  , ppCurrent = xmobarColor xmobarInactiveCurrentWorkspaceColor "" . wrap "[" "]"
  , ppVisible = xmobarColor xmobarInactiveVisibleWorkspaceColor "" . wrap "(" ")"
  , ppHidden = \ws -> if ws == "NSP" then "" else ws
  , ppSep = xmobarColor xmobarSepColor "" " | "
  , ppLayout = xmobarColor xmobarLayoutColor ""
  , ppUrgent = xmobarColor "red" "yellow"
  }

------------------------------------------------------------------------
-- # Startup

main = do
  -- xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ fullscreenSupport $ defaultConfig
    { terminal = myTerminal
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = myBorderWidth
    , modMask = myModMask
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys = myKeys
    , logHook = do
        copies <- CopyW.wsContainingCopies
        Bars.multiPP (myLogPPActive copies) (myLogPP copies)
        -- dynamicLogWithPP $ xmobarPP
        --   { ppOutput = hPutStrLn xmproc
        --   , ppTitle = xmobarColor xmobarTitleColor "" . shorten 80
        --   , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
        --   , ppSep = xmobarColor xmobarSepColor "" " | "
        --   , ppLayout = xmobarColor xmobarLayoutColor ""
        --   , ppHidden = \ws -> if ws == "NSP" then "" else ws
        --   , ppUrgent = xmobarColor "red" "yellow"
        --   }
    , manageHook =
        manageDocks
        <+> myManageHook
        -- <+> insertPosition Below Newer
        -- insert new windows below the newest but also handle floating ones
        <+> composeOne
          [ isDialog -?> doCenterFloat
          , stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat
          , transience -- Move transient windows to their parent.
          , pure True -?> insertPosition Below Newer
          ]
        <+> namedScratchpadManageHook myScratchpads
    , startupHook = do
        setWMName "LG3D"
        Bars.dynStatusBarStartup barCreator (pure ())
    , layoutHook = smartBorders myLayout
    , handleEventHook =
        Bars.dynStatusBarEventHook barCreator (pure ())
        <+> handleEventHook defaultConfig
        <+> docksEventHook
    }

------------------------------------------------------------------------

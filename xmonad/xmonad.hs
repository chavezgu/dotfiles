import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.UpdatePointer

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = defaultConfig
        { terminal    = "st -e zsh"
        , modMask     = mod4Mask
        , borderWidth = 3
        , logHook = updatePointer (Relative 0.5 0.5)
        }

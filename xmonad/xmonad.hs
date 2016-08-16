import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.UpdatePointer
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.1
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = defaultConfig
        { terminal    = "st -e bash"
        , modMask     = mod4Mask
        , borderWidth = 3
        , logHook = updatePointer (0.5, 0.5) (0, 0)
        }

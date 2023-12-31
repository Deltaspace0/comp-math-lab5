module Main (main) where

import Monomer

import Model
import UI

main :: IO ()
main = do
    let model = initModel
        config =
            [ appWindowState $ MainWindowNormal (1000, 600)
            , appWindowTitle "Лабораторная работа №5"
            , appTheme darkTheme
            , appFontDef "Regular" "./assets/font/laconic.otf"
            , appInitEvent AppInit
            ]
    startApp model handleEvent buildUI config

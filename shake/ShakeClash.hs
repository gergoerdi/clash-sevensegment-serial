{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ShakeClash (ClashProject(..), mainFor) where

import Development.Shake hiding ((~>))
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Control.Monad.Trans

import Text.Mustache
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

import Clash.Driver.Types

data XilinxTarget = XilinxTarget
    { targetFamily :: String
    , targetDevice :: String
    , targetPackage :: String
    , targetSpeed :: String
    }

instance ToMustache XilinxTarget where
    toMustache = object . targetMustache

targetMustache XilinxTarget{..} =
    [ "targetFamily" ~> T.pack targetFamily
    , "targetDevice" ~> T.pack targetDevice
    , "targetPackage" ~> T.pack targetPackage
    , "targetSpeed" ~> T.pack targetSpeed
    ]

boards :: M.Map String XilinxTarget
boards = M.fromList
    [ ("papilio-pro", XilinxTarget "Spartan6" "xc6slx9" "tqg144" "-2")
    ]

data ClashProject = ClashProject
    { projectName :: String
    , clashModule :: String
    , clashTopName :: String
    , ipCores :: [String]
    , vhdlSrcs :: [String]
    }

buildDir = "_build"
shakeDir = "shake"

mainFor :: ClashProject -> IO ()
mainFor ClashProject{..} = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

    let manifest = buildDir </> "vhdl" </> clashModule </> clashTopName </> clashTopName <.> "manifest"
    let manifestSrcs = do
            need [manifest]
            Manifest{..} <- read <$> readFile' manifest
            let clashTypes = map toLower clashTopName <> "_types"
                clashSrcs = clashTypes : map TL.unpack componentNames
            return $ mconcat
              [ [ "vhdl" </> clashModule </> clashTopName </> c <.> "vhdl" | c <- clashSrcs ]
              , [ "src-vhdl" </> projectName <.> "ucf" ]
              , [ "src-vhdl" </> vhdl <.> "vhdl" | vhdl <- vhdlSrcs ]
              ]

    want [ buildDir </> projectName <.> "bit" ]

    let clash src = do
            clashExe <- fromMaybe ("clash") <$> getConfig "CLASH"
            cmd_ clashExe ["-i" <> "src-clash", "-outputdir", buildDir, "--vhdl", src]
        xilinx tool args = do
            xilinxRoot <- fromMaybe (error "XILINX_ROOT missing") <$> getConfig "XILINX_ROOT"
            cmd_ (Cwd buildDir) (xilinxRoot </> "ISE/bin/lin64" </> tool) args

    buildDir </> "vhdl" <//> "*.manifest" %> \out -> do
        let src = "src-clash" </> clashModule <.> "hs" -- TODO
        need [ src ]
        clash src

    buildDir </> projectName <.> "bit" %> \_out -> do
        srcs <- manifestSrcs
        need $ mconcat
          [ [ buildDir </> projectName <.> "tcl" ]
          , [ buildDir </> src | src <- srcs ]
          , [ buildDir </> "ipcore_dir" </> core <.> "xco" | core <- ipCores ]
          ]
        xilinx "xtclsh" [projectName <.> "tcl", "rebuild_project"]

    buildDir <//> "*.tcl" %> \out -> do
        let src = shakeDir </> "project.tcl.mustache"
        s <- T.pack <$> readFile' src
        alwaysRerun

        board <- fromMaybe "papilio-pro" <$> getConfig "BOARD"
        let target = fromMaybe (error $ unwords ["Unknown target board:", board]) $ M.lookup board boards

        srcs <- manifestSrcs

        template <- case compileTemplate src s of
            Left err -> fail (show err)
            Right template -> return template
        let values = object . mconcat $
                     [ [ "project" ~> T.pack clashModule ]
                     , targetMustache target
                     , [ "srcs" ~> [ object [ "fileName" ~> src ] | src <- srcs ] ]
                     , [ "ipcores" ~> [ object [ "name" ~> core ] | core <- ipCores ] ]
                     ]
        writeFileChanged out . T.unpack $ substitute template values

    buildDir <//> "*.xco" %> \out -> do
        let src = "ise" </> dropDirectory1 out
        copyFileChanged src out

    buildDir </> "src-vhdl" <//> "*" %> \out -> do
        let src = "src-vhdl" </> (dropDirectory1 . dropDirectory1 $ out)
        copyFileChanged src out

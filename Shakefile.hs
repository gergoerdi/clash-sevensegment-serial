{-# OPTIONS_GHC -ishake #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "SerialTX"
    , clashModule = "SerialTX"
    , clashTopName = "SerialTX"
    , ipCores = []
    , vhdlSrcs = []
    }

main :: IO ()
main = mainFor clashProject

module TranslateConfigFiles.ConfigFilesCommon where

import Data.Map.Strict as Map

-- ===================================================================================================
-- | Use a map `String -> [(String, String)]` to store a config file.
-- ===================================================================================================
type Configs = Map String [(String, String)]

insertConfig :: String -> (String, String) -> Configs -> Configs
insertConfig section setting configs = alter f section configs
    where
        f Nothing = Just [setting]
        f (Just settings') = Just (settings' ++ [setting])

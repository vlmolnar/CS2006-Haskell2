module File where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.IO.Unsafe

import Board

-- Save file path, the path where the game state is used
jsonFile :: FilePath
jsonFile = "../data/game_features.json"

--Reads in save file
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

--Writes to save file
writeJSON :: B.ByteString -> IO ()
writeJSON string = B.writeFile jsonFile string

-- This function returns a Save object - mirror of the Play object nested in
-- data world
-- The unsafePerformIO is used to bring down the moand IO Save type back to just
-- Save
readSave :: Maybe Save
readSave = decode $ unsafePerformIO $ getJSON

-- This function writes a save object to the file
writeSave :: Save -> World
writeSave s =  unsafePerformIO $ do writeJSON (encode s)
                                    return $ saveToWorld (Just s)

-- This function converts the world Play type to the Save type
worldToSave :: World -> Save
worldToSave (Play b t a m) = File b t a m

-- This function converts a Save object to a World object
saveToWorld :: Maybe Save -> World
saveToWorld (Just (File b t a m)) = Play b t a m 

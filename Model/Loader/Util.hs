module Model.Loader.Util where

import qualified Control.Exception.Lifted as E
import           Control.Monad.IO.Class
import           Import
import           System.Directory
import           System.FilePath

-- | Load multiple instances of a given object from a directory. Does
-- not die if any individual loader dies, but prints a debug message
-- to console.
loadMulti :: (MonadIO m, MonadBaseControl IO m) =>
             (FilePath -> m ()) -> FilePath -> m ()
loadMulti loader dir = do
  directories <- liftIO . getDirectoryContents $ dir
  let safeLoad objDir = loader objDir `E.catch` \e -> liftIO . putStrLn $
        "While loading " ++ objDir ++ " caught " ++ show (e :: E.SomeException)
      valid = map (dir </>) . filter (`notElem` [".", ".."]) $ directories
  mapM_ safeLoad valid


module Util where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import System.IO


unlessMaybe :: (Monad m) => Bool -> m (Maybe a) -> m (Maybe a)
unlessMaybe True  _   = return Nothing
unlessMaybe False act = act

split :: (Eq a) => a -> [a] -> [[a]]
split _     [] = []
split delim xs = case span (/= delim) xs of
                   (prefix, []) -> [prefix]
                   (prefix, (_:rest)) -> prefix : split delim rest


processLinesFromHandle :: Handle -> [(String, (String -> IO ()))] -> IO ()
processLinesFromHandle handle dispatchAList = do
  messageStream <- lines <$> hGetContents handle
  mapM_ dispatchMessageByPrefix messageStream
    where dispatchMessageByPrefix msg =
              case catMaybes (map g dispatchAList) of
                [] -> return ()
                (act:_) -> act
              where g (header, fn) = fn <$> stripPrefix (header ++ " ") msg

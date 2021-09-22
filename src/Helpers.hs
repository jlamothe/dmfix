{-|

Module     : Helpers
Copyright  : Jonathan Lamothe
License    : GPL-3
Maintainer : jonathan@jlamothe.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Helpers (
  textToUrl,
  urlToText,
  ) where

import qualified Data.List as L
import qualified Data.Text as T

import Types

-- | Convert a text to a 'Url' (if possible)
textToUrl :: T.Text -> Maybe Url
textToUrl text = case T.splitOn "#" text of
  [sub, anchorT] -> do
    url <- subToUrl sub
    Just $ url { anchor = Just $ T.unpack anchorT }
  [sub] -> subToUrl sub
  _ -> Nothing

-- | Convert a 'Url' to text
urlToText :: Url -> T.Text
urlToText url = T.pack $
  protocol url ++ "://" ++
  host url ++ "/" ++
  pathStr ++ paramsStr ++ anchorStr
  where
    pathStr = L.intercalate "/" $ path url
    paramsStr = case params url of
      [] -> ""
      ps -> '?' : L.intercalate "&"
        ( map
          ( \case
            (name, Just val) -> name ++ "=" ++ val
            (name, Nothing)  -> name
          ) ps
        )
    anchorStr = case anchor url of
      Just str -> "#" ++ str
      Nothing  -> ""

subToUrl :: T.Text -> Maybe Url
subToUrl text = case T.splitOn "://" text of
  [protT, raw] -> do
    let prot = T.unpack protT
    url <- getParts raw
    Just url { protocol = prot }
  _ -> Nothing

getParts :: T.Text -> Maybe Url
getParts text = do
  (host', path', params') <- case T.splitOn "?" text of
    [pathT, paramsT] -> do
      (host', path') <- getPath pathT
      params'       <- getParams paramsT
      Just (host', path', params')
    [pathT] -> do
      (host', path') <- getPath pathT
      Just (host', path', [])
    _ -> Nothing
  Just newUrl
    { host   = host'
    , path   = path'
    , params = params'
    }

getPath :: T.Text -> Maybe (String, [String])
getPath text = case T.splitOn "/" (removeTrailing '/' text) of
  hostT : dirs -> Just
    ( T.unpack hostT
    , map T.unpack dirs
    )
  _ -> Nothing

getParams :: T.Text -> Maybe [(String, Maybe String)]
getParams = mapM getParam . T.splitOn "&"

getParam :: T.Text -> Maybe (String, Maybe String)
getParam text = case T.splitOn "=" text of
  [name, val] -> Just
    ( T.unpack name
    , Just $ T.unpack val
    )
  [name] -> Just
    ( T.unpack name
    , Nothing
    )
  _ -> Nothing

removeTrailing :: Char -> T.Text -> T.Text
removeTrailing ch text
  | T.null text = ""
  | otherwise   = if T.last text == ch
    then T.init text
    else text

--jl

{-|

Module     : Handler.Home
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

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Handler.Home where

import Import
import Helpers

title :: Text
title = "DiscussMormonism.com Link Fixer"

linkForm :: Form Text
linkForm = renderDivs $
  areq urlField "Old URL" Nothing

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost linkForm
  defaultLayout $ do
    setTitle $ toHtml title
    $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
  ((result, _), _) <- runFormPost linkForm
  case result of
    FormSuccess l -> case updateLink l of
      Just l' -> redirect l'
      Nothing -> do
        setMessage "Invalid link"
        redirect HomeR
    _ -> do
      setMessage "Something went wrong"
      redirect HomeR

--jl

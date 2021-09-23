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

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Welcome To Yesod!"
  $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = defaultLayout $ do
  setTitle "Welcome To Yesod!"
  $(widgetFile "homepage")

--jl

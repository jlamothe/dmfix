{-|

Module     : Types
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

module Types (
  Url (..),
  newUrl
  ) where

data Url = Url
  { protocol :: String
  , host     :: String
  , path     :: [String]
  , params   :: [(String, Maybe String)]
  , anchor   :: Maybe String
  } deriving (Eq, Show)

newUrl :: Url
newUrl = Url "" "" [] [] Nothing

--jl

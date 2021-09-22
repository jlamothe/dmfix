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

-- | Defines the basic parts of a URL
data Url = Url
  { protocol :: String
  -- ^ the protocol, e.g.: HTTPS
  , host     :: String
  -- ^ the hostname (and optional port)
  , path     :: [String]
  -- ^ the path as a list of nested directories
  , params   :: [(String, Maybe String)]
  -- ^ the parameters and their optional values
  , anchor   :: Maybe String
  -- ^ the anchor text
  } deriving (Eq, Show)

-- | default value for a 'Url'
newUrl :: Url
newUrl = Url "" "" [] [] Nothing

--jl

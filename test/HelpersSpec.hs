{-

dmfix

Copyright (C) Jonathan Lamothe <jonathan@jlamothe.net>

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

{-# LANGUAGE OverloadedStrings #-}

module HelpersSpec (spec) where

import qualified Data.Text as T
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Helpers
import Types

spec :: Spec
spec = do
  textToUrlSpec
  urlToTextSpec
  editHostSpec

textToUrlSpec :: Spec
textToUrlSpec = describe "textToUrl" $ mapM_
  ( \(input, expected) -> context (show input) $
    it ("should be " ++ show expected) $
      textToUrl input `shouldBe` expected
  )

  --  input,    expected
  [ ( "",        Nothing        )
  , ( "foo",     Nothing        )
  , ( simpleTxt, Just simpleUrl )
  , ( pathTxt,   Just pathUrl   )
  , ( paramsTxt, Just paramsUrl )
  , ( anchorTxt, Just anchorUrl )
  ]

urlToTextSpec :: Spec
urlToTextSpec = describe "urlToText" $ mapM_
  ( \(input, expected) -> context (show input) $
    it ("should be " ++ show expected) $
      urlToText input `shouldBe` expected
  )

  --  input,     expected
  [ ( simpleUrl, simpleTxt )
  , ( pathUrl,   pathTxt   )
  , ( paramsUrl, paramsTxt )
  , ( anchorUrl, anchorTxt )
  ]

editHostSpec ::Spec
editHostSpec = describe "editHost" $ mapM_
  ( \(desc, f, url, expected) -> context desc $
    it ("should be " ++ show expected) $
      editHost f url `shouldBe` expected
  )

  --  description, function,       url,       expected
  [ ( "reverse",   Just . reverse, simpleUrl, reversed )
  , ( "fail",      const Nothing,  simpleUrl, Nothing  )
  ]

  where
    reversed = Just simpleUrl
      { host = reverse $ host simpleUrl }

simpleTxt :: T.Text
simpleTxt = "http://example.com/"

simpleUrl :: Url
simpleUrl = newUrl
  { protocol = "http"
  , host     = "example.com"
  }

pathTxt :: T.Text
pathTxt = simpleTxt `T.append` "foo/bar"

pathUrl :: Url
pathUrl = simpleUrl { path = ["foo", "bar"] }

paramsTxt :: T.Text
paramsTxt = pathTxt `T.append` "?a=1&b=2&c"

paramsUrl :: Url
paramsUrl = pathUrl
  { params =
    [ ( "a", Just "1" )
    , ( "b", Just "2" )
    , ( "c", Nothing  )
    ]
  }

anchorTxt :: T.Text
anchorTxt = paramsTxt `T.append` "#abc"

anchorUrl :: Url
anchorUrl = paramsUrl { anchor = Just "abc" }

--jl

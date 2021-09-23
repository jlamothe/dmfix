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
  makeHttpsSpec
  editHostSpec
  dropParamSpec
  editParamSpec
  editAnchorSpec
  incParamBySpec
  incAnchorBySpec
  incStrBySpec

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

makeHttpsSpec :: Spec
makeHttpsSpec = describe "makeHttps" $ mapM_
  ( \(desc, input, expected) -> context desc $
    it ("should be " ++ show expected) $
      makeHttps input `shouldBe` expected
  )

  --  description, input,         expected
  [ ( "HTTP",      simpleUrl,     Just https )
  , ( "HTTPS",     https,         Just https )
  , ( "FTP",       urlWith "ftp", Nothing    )
  ]

  where
    https     = urlWith "https"
    urlWith p = simpleUrl { protocol = p }

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

dropParamSpec :: Spec
dropParamSpec = describe "dropParam" $ mapM_
  ( \(desc, p, expected) -> context desc $
    it ("should be " ++ show expected) $
      dropParam p url `shouldBe` expected
  )

  --  description,   param, expected
  [ ( "with val",    "a",   withVal    )
  , ( "without val", "b",   withoutVal )
  , ( "not present", "c",   url        )
  ]

  where
    url = simpleUrl
      { params =
        [ ("a", Just "1")
        , ("b", Nothing)
        ]
      }
    withVal    = simpleUrl { params = [("b", Nothing)] }
    withoutVal = simpleUrl { params = [("a", Just "1")] }

editParamSpec :: Spec
editParamSpec = describe "editParam" $ mapM_
  ( \(desc, pName, f, expected) -> context desc $
    it ("should be " ++ show expected) $
      editParam pName f url `shouldBe` expected
  )

  --  description,     param name, function,       expected
  [ ( "reverse",       "a",        Just . reverse, Just reversed )
  , ( "fail",          "a",        const Nothing,  Nothing       )
  , ( "reverse empty", "c",        Just . reverse, Just url      )
  , ( "fail empty",    "c",        const Nothing,  Just url      )
  , ( "no reverse",    "d",        Just . reverse, Just url      )
  , ( "no fail",       "d",        const Nothing,  Just url      )
  ]

  where
    url      = simpleUrl { params = mkParams "foo" }
    reversed = simpleUrl { params = mkParams "oof" }
    mkParams val =
      [ ( "a", Just val   )
      , ( "b", Just "bar" )
      , ( "c", Nothing    )
      ]

editAnchorSpec :: Spec
editAnchorSpec = describe "editAnchor" $ mapM_
  ( \(desc, f, url, expected) -> context desc $
    it ("should be " ++ show url) $
      editAnchor f url `shouldBe` expected
  )

  --  description,     function,       url,       expected
  [ ( "reverse",       Just . reverse, fooUrl,    Just reversedUrl )
  , ( "fail",          const Nothing,  fooUrl,    Nothing          )
  , ( "reverse empty", Just . reverse, simpleUrl, Just simpleUrl   )
  , ( "fail empty",    const Nothing,  simpleUrl, Just simpleUrl   )
  ]

  where
    fooUrl      = urlWith "foo"
    reversedUrl = urlWith "oof"
    urlWith a   = simpleUrl { anchor = Just a }

incParamBySpec :: Spec
incParamBySpec = describe "incParamBy" $ mapM_
  ( \(desc, n, pName, expected) -> context desc $
    it ("should be " ++ show expected) $
      incParamBy n pName url `shouldBe` expected
  )

  --  description,   number, param, expected
  [ ( "+1",          1,      "a",   Just $ urlWith "3" )
  , ( "+2",          2,      "a",   Just $ urlWith "4" )
  , ( "non-numeric", 1,      "c",   Nothing            )
  , ( "missing",     1,      "d",   Just url           )
  ]

  where
    url        = urlWith "2"
    urlWith n  = simpleUrl { params = mkParams n }
    mkParams n =
      [ ( "a", Just n     )
      , ( "b", Just "2"   )
      , ( "c", Just "foo" )
      ]

incAnchorBySpec :: Spec
incAnchorBySpec = describe "incAnchorBy" $ mapM_
  ( \(desc, n, url, expected) -> context desc $
    it ("should be " ++ show expected) $
      incAnchorBy n url `shouldBe` expected
  )

  --  description,   number, url,           expected
  [ ( "+1",          1,      urlWith "1",   Just $ urlWith "2" )
  , ( "+2",          2,      urlWith "1",   Just $ urlWith "3" )
  , ( "empty",       1,      simpleUrl,     Just simpleUrl     )
  , ( "non-numeric", 1,      urlWith "foo", Nothing            )
  ]

  where urlWith str = simpleUrl { anchor = Just str }

incStrBySpec :: Spec
incStrBySpec = describe "incStrBy" $ mapM_
  ( \(n, str, expected) ->
    context (show str ++ " + " ++ show n) $
      it ("should be " ++ show expected) $
        incStrBy n str `shouldBe` expected
  )

  --  number, string, expected
  [ ( 2,      "3",    Just "5" )
  , ( 2,      "foo",  Nothing  )
  , ( 2,      "",     Nothing  )
  ]

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

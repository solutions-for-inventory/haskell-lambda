{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main,
  )
where

import Prelude
import Data.Morpheus (runApp)
import Data.Morpheus.Types (GQLRequest (..), GQLResponse (..))
import Test.Morpheus
  ( FileUrl,
    cd,
    mainTest,
    mkUrl,
    scan,
    testApi,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

main :: IO ()
main =
  mainTest
    "Morpheus Graphql Tests"
    []

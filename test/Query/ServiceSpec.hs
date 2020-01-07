{-# LANGUAGE OverloadedStrings #-}

{-| Test module for Service module.

Test creation of Service types.
In most cases this indirectly also verifies the related functions in Query.Load
and Query.Variables.

-}

module Query.ServiceSpec
  ( -- * Top level spec function.
    spec
  ) where

import Data.Text (Text)
import Test.Hspec
import Query.Datatypes (Service, variables)
import Query.Service
import Query.Variables (variablesPsuedoAuthUser)

spec :: Spec
spec = do
  servicePsuedoAuthUserSpec

-- | Checks if the variables are the same.
servicePsuedoAuthUserSpec :: Spec
servicePsuedoAuthUserSpec = before (servicePsuedoAuthUser "Drens5") $ do
  describe "servicePsuedoAuthUser" $ do
    it "has the right variables" $ \service -> do
      variables service `shouldBe` variablesPsuedoAuthUser "Drens5"
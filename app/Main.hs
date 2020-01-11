{-# LANGUAGE OverloadedStrings #-}

{-| Main module of project.

A command line application which gives anime recommendations.
Currently the only implemented algorithm for obtaining recommendations is
CoupledGT.
Main's interface is currently tailored solely to this algorithm, however this
is likely to change in the future with other recommendation algorithms.

Run the program with --help for the most recent usage and help text.
-}

module Main where

import CommandLineInterface
import Interface
import Options.Applicative
import Presentation
import Recco.CoupledGT

main :: IO ()
main = do
  cgto <- execParser optsCoupledGT  -- ^ Defined in CommandLineInterface
  pAuth <- psuedoAuthenticateUser (cgtoUserName cgto)
  reccos <- minimal <$> coupledGT pAuth (cgtoGenres cgto) (cgtoTags cgto)
    (cgtoAmount cgto) (cgtoRecommendations cgto)
  print reccos
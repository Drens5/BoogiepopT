{-# LANGUAGE OverloadedStrings #-}

{-| Command line options with optparse-applicative

Defines the necessary functions and structures to parse command line arguments
and provide a command line interface using optparse-applicative.

-}

module CommandLineInterface
  ( -- * Command line argument parsers to be run
    optsCoupledGT
    -- * Datatypes to hold parsed arguments
  , CoupledGTOptions (..)
  ) where

import Condition (Coupled (..), Genre, Tag)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Options.Applicative
import Query.Datatypes hiding (userName)

data CoupledGTOptions = CoupledGTOptions
  { cgtoUserName :: Text
  , cgtoGenres :: [Coupled Genre]
  , cgtoTags :: [Coupled Tag]
  , cgtoAmount :: Int  -- ^ optional: default 10, least amount of users considered in aggregation
  , cgtoRecommendations :: Int  -- ^ optional: default: 3, amount of selected anime for each aggregation
  }

optsCoupledGT :: ParserInfo CoupledGTOptions
optsCoupledGT = info (coupledGTOptions <**> helper)
  (  fullDesc
  <> progDesc "Get anime recommendations for USERNAME based on GENRES and TAGS (GT).\n More recommendation methods coming soon."
  <> header "BoogiepopT - an anime recommendation program."
  )

coupledGTOptions :: Parser CoupledGTOptions
coupledGTOptions = CoupledGTOptions <$> userName <*> genres <*> tags <*>
  amount <*> recommendations

userName :: Parser Text
userName = option auto
  (  long "username"
  <> short 'n'
  <> metavar "USERNAME"
  <> help "Anilist username"
  )

genres :: Parser [Coupled Genre]
genres = option auto
  (  long "genres"
  <> short 'g'
  <> metavar "GENRES"
  <> help "A list of genres in a coupled sense e.g: [Strong \"Comedy\", Strong \"Drama\", Weak [\"Adventure\", \"Action\"]]."
  )

tags :: Parser [Coupled Tag]
tags = option auto
  (  long "tags"
  <> short 't'
  <> metavar "TAGS"
  <> help "A list of tags in a coupled sense e.g: [Strong \"Surreal Comedy\", Weak [\"Slapstick\", \"Satire\", \"Meta\"]]"
  )

amount :: Parser Int
amount = option auto
  (  long "amount"
  <> short 'a'
  <> metavar "N"
  <> value 10
  <> help "Least amount of users considered in aggregation. DEFAULT: 10"
  )

recommendations :: Parser Int
recommendations = option auto
  (  long "recommendations"
  <> short 'r'
  <> metavar "K"
  <> value 3
  <> help "Amount of media to select for each aggregation method. DEFAULT: 3"
  )
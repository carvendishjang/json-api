{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty

-- | Types for our api
data User = User { userId :: Int, userName :: String } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }


allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

-- | Route parameters
routes :: ScottyM ()
routes = do
  get "/hello/:name" $ do
    name <- param "name"
    text ("hello" <> name <> "!")

  get "/users" $ do
    json allUsers

-- | return users to the api
  get "/users/:id" $ do
    id <- param "id"
    json (filter (matchesId id) allUsers)


-- | A simple web server
main :: IO ()
main = do
  putStrLn "Starting server"
  scotty 3000 routes
    
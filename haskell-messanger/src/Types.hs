{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-matches -Wno-name-shadowing -Wno-unused-imports -Wno-compat-unqualified-imports -Wno-dodgy-imports -Wno-orphans -Wno-unused-local-binds  #-}
{-|
Module      : Haskell Messanger
Description : Initiating messages between multithreaded users
Maintainer  : Shivani Hooda
-}
module Types (
    UserDetails (..),
    MessageDetails (..)
) where 

import GHC.Generics (Generic)
import Data.Time.Clock

{-|
    > UserDetails is a record type with a single field, username, which is a String.
	It also derives instances of the Show, Eq, and Generic type classes.
-}
data UserDetails = UserDetails {
    username :: String
} deriving (Show, Eq, Generic)

{-|
    > MessageDetails is a record type with five fields: message (a String), timestamp (a UTCTime),
	sender and receiver (both String), and total_message_count (an Int). 
	It also derives instances of the Show, Eq, and Generic type classes.
-}
data MessageDetails = MessageDetails {
	message :: String,
	timestamp :: UTCTime,
	sender :: String,
	receiver :: String,
	total_message_count :: Int
} deriving (Show, Eq, Generic)

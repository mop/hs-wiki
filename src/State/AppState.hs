{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction, 
    ScopedTypeVariables, TypeFamilies, FlexibleInstances, 
    MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
{-# OPTIONS -XPatternSignatures #-}

module State.AppState
where

import HAppS.State
import Data.Generics

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import qualified Data.Map as M

data User = User {
    userName     :: B.ByteString
  , userPassword :: B.ByteString
  , userMail     :: B.ByteString
} deriving (Show, Read, Eq, Ord, Typeable, Data)
instance Version User
$(deriveSerialize ''User)

data Article = Article {
    htmlContent       :: B.ByteString
  , markupContent     :: B.ByteString
  , authorName        :: B.ByteString
  , articleCategories :: [B.ByteString]
} deriving (Show, Read, Eq, Ord, Typeable, Data)
instance Version Article
$(deriveSerialize ''Article)

data Session = Session {
    sessionUser :: B.ByteString
} deriving (Read, Show, Eq, Ord, Typeable, Data)
instance Version Session
$(deriveSerialize ''Session)

type UserMap    = M.Map B.ByteString User
type ArticleMap = M.Map B.ByteString Article
type SessionMap = M.Map Integer Session

data AppState  = AppState {
    users    :: UserMap
  , articles :: ArticleMap
  , sessions :: SessionMap
} deriving (Show, Read, Eq, Ord, Typeable, Data)
instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
    type Dependencies AppState = End
    initialValue = AppState M.empty M.empty M.empty

$(mkMethods ''AppState [])

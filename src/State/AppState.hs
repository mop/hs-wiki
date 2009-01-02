{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction, 
    ScopedTypeVariables, TypeFamilies, FlexibleInstances, 
    MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
{-# OPTIONS -XPatternSignatures #-}

module State.AppState
where

import HAppS.State
import Data.Generics
import Data.Maybe

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)

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

getUser :: B.ByteString -> Query AppState User
getUser name = do
    (app :: AppState) <- ask 
    return $ fromJust $ M.lookup name $ users app

existUser :: B.ByteString -> Query AppState Bool
existUser name = do
    (app :: AppState) <- ask
    return $ isJust $ M.lookup name $ users app

createUser :: B.ByteString -> B.ByteString -> B.ByteString -> 
              Update AppState ()
createUser name mail pwd = do
        appState <- get
        put $ modUsers appState name user
    where   user    = User name pwd mail
            modUsers appState key val = let users' = M.insert 
                                                     key val (users appState)
                                        in appState { users = users' }

createSession :: B.ByteString -> Update AppState Integer
createSession name = do
    (state :: AppState) <- ask
    key <- getRandom
    case M.lookup key (sessions state) of
        Nothing -> (put $ modifySessions state (M.insert key sessionData)) >> 
                   return key
        Just x  -> createSession name
    where   sessionData   = Session name

deleteSession :: B.ByteString -> Update AppState ()
deleteSession key = do
    (appState :: AppState) <- get
    put $ modifySessions appState (M.delete key') 
    where    key' = read $ B.unpack key

modifySessions :: AppState -> (SessionMap -> SessionMap) -> AppState
modifySessions state fun = let sessions' = fun (sessions state)
                           in state { sessions = sessions' }

instance Component AppState where
    type Dependencies AppState = End
    initialValue = AppState M.empty M.empty M.empty

$(mkMethods ''AppState [
      'getUser
    , 'existUser
    , 'createUser
    , 'createSession
    , 'deleteSession
    ])

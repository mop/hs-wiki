module SessionController
    (sessionController)
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers
import Control.Exception (try)
import Control.Monad (liftM2, mplus, fmap)
import Control.Monad.Trans (lift)

import State.AppState
import Utils

import qualified Data.ByteString.Char8 as B

data SessionForm = SessionForm {
    sessionFormName     :: B.ByteString
  , sessionFormPassword :: B.ByteString
}

instance FromData SessionForm where
    fromData = liftM2 SessionForm (fmap B.pack $ look "name" `mplus` return "")
                                  (fmap B.pack $ look "password" 
                                   `mplus` return "")

sessionController :: STDirGroups String -> ServerPartT IO Response
sessionController tpls = multi $ [
        dir "login" [
            methodSP GET $ newSession tpls
          , methodSP POST $ doCreateSession tpls
        ]
      , dir "logout" [
            methodSP GET $ doDeleteSession tpls
        ]
    ]

newSession :: STDirGroups String -> ServerPartT IO Response
newSession tpls = renderSessionForm tpls []

renderSessionForm :: STDirGroups String -> [(String, String)] -> 
                     ServerPartT IO Response
renderSessionForm tpls args = withRequest $ \req -> 
                              ok . toResponse . HtmlString $ 
                              renderLayout tpls [("content", renderedSession)]
    where   renderedSession = renderSession args layout
            renderSession   = renderTemplateGroup (template tpls)
            layout          = "session-new"

lookupUserData :: B.ByteString -> B.ByteString -> IO User
lookupUserData name pwd = do
    user <- query $ GetUser name
    if (userPassword user) /= pwd 
        then fail "wrong password"
        else return user

doCreateSession :: STDirGroups String -> ServerPartT IO Response
doCreateSession tpls = withData $ \(SessionForm name pwd) -> [
        ServerPartT $ \req -> do
            result <- lift $ try (lookupUserData name pwd) 
            case result of
                Right user -> handleCreateSession name >> 
                              ((found "/") . toResponse . HtmlString $ "login")
                Left  err  -> unServerPartT (renderSessionForm tpls errorArgs)
                              req
    ]
    where   errorArgs    = [("errorMessages", errorMessage)]
            errorMessage = "Login failed. " ++ 
                           "The username/password combination entered by " ++
                           "you is incorrect."

handleCreateSession :: B.ByteString -> WebT IO ()
handleCreateSession name = do 
    sid <- update $ CreateSession name
    addCookie (3600) (mkCookie "sid" $ show sid)

doDeleteSession :: STDirGroups String -> ServerPartT IO Response
doDeleteSession tpls = error "undefined"

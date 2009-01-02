module UserController
    (userController)
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers
import Control.Monad (liftM4, when, mplus)
import Control.Monad.Trans (lift)

import State.AppState
import Utils

import qualified Data.ByteString.Char8 as B

data RegisterUserForm = RegisterUserForm {
      registerUserFormName                 :: String
    , registerUserFormMail                 :: String
    , registerUserFormPassword             :: String
    , registerUserFormPasswordConfirmation :: String
}

instance FromData RegisterUserForm where
    fromData = liftM4 RegisterUserForm  (look "name" `mplus` (return "")) 
                                        (look "mail" `mplus` (return ""))
                                        (look "password" `mplus` (return ""))
                                        (look "password_confirmation" 
                                            `mplus` (return ""))

userController :: STDirGroups String -> ServerPartT IO Response
userController tpls = multi $ [
        dir "register" [
            methodSP GET $ newUser tpls
          , methodSP POST $ createNewUser tpls
        ]
    ]

newUser :: STDirGroups String -> ServerPartT IO Response
newUser tpls = renderNewUserForm tpls []

renderNewUserForm :: STDirGroups String -> [(String, String)] -> ServerPartT IO Response
renderNewUserForm tpls args = withRequest $ \req -> 
    ok . toResponse . HtmlString $ result
    where   userForm = renderTemplateGroup (template tpls) args "user-new"
            result   = renderLayout tpls [("content", userForm)]

checkForm :: RegisterUserForm -> Either String Bool
checkForm (RegisterUserForm name mail pwd pwd_confirm) = do
    when (name == "") (Left "Name cannot be blank.")
    when (mail == "") (Left "Mail cannot be blank.")
    when (pwd == "") (Left "Password cannot be blank.")
    when (pwd /= pwd_confirm) (Left confirmErrMsg)
    return True

    where   confirmErrMsg = "Password confirmation doesn't match password."

insertUser :: String -> String -> String -> IO (Either String ())
insertUser name mail pwd = do
    res <- query $ ExistUser (B.pack name)
    if (res == True) 
        then return (Left errMsg)
        else (update $ CreateUser (B.pack name) (B.pack mail) (B.pack pwd)) >> 
             (return $ Right ())
    where   errMsg = "Another user with the same name already exists" ++ 
                     ", please choose another name!"

handleCreation :: RegisterUserForm -> IO (Either String ())
handleCreation form@(RegisterUserForm name mail pwd pwd_confirm) = do
    case checkForm form of
        Right _  -> insertUser name mail pwd
        Left err -> return $ Left err

createNewUser :: STDirGroups String -> ServerPartT IO Response
createNewUser tpls = withData $ \(form@(RegisterUserForm name mail pwd pwd_confirm)) -> [
    ServerPartT $ \req -> do
            result <- lift $ handleCreation form
            case result of 
                Right _  -> found ("/") . toResponse . HtmlString $ "create"
                Left err -> unServerPartT (renderNewUserForm tpls (errMsg err))
                            req
    ]

    where   errMsg msg = [("errorMessages", msg)]



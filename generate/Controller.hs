{-# LANGUAGE NoMonomorphismRestriction #-}
module Controller
    (generateController)
where

import Text.StringTemplate
import Text.StringTemplate.Helpers
import Control.Monad (when)
import Data.Char (toLower, toUpper)

generateController :: [String] -> IO String
generateController args = do
    when (length args < 1) (fail "controller name must be given!") 
    tpls <- getTemplates
    return (render' tpls (head args))

getTemplates :: IO (STGroup String)
getTemplates = do
    grp  <- directoryGroups "Controller" 
    return $ getTemplateGroup "." grp

render' :: STGroup String -> String -> String
render' tpls name = renderTemplateGroup tpls args "controller"
    where   name'  = (toName1 name) ++ "Controller"
            name'' = (toName2 name) ++ "Controller"
            args   = [("name", name'), ("smallname", name'')]

toName1 :: String -> String
toName1 (x:xs) = (toUpper x) : xs

toName2 :: String -> String
toName2 (x:xs) = (toLower x) : xs

test :: IO ()
test = generateController ["zomg"] >>= print

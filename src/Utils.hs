module Utils
    ( HtmlString(..)
    , renderLayout
    , renderLayoutSP
    , template
    , resource
    , fetchSession
    , fetchSessionId
    , tryRender
    , tryRenderSP
    )
where

import HAppS.Server
import HAppS.State (query)
import Text.StringTemplate
import Text.StringTemplate.Helpers
import Maybe (isJust)
import Control.Monad.Trans (lift)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

import State.AppState

newtype HtmlString = HtmlString { htmlString :: String }
instance ToMessage HtmlString where
    toContentType _          = B.pack "text/html"
    toMessage (HtmlString s) = L.pack s

renderLayoutSP :: STDirGroups String -> [(String, String)] -> 
                  ServerPartT IO Response
renderLayoutSP tpls args = ServerPartT $ \req -> do
    sess <- lift $ fetchSession req
    ok . toResponse . HtmlString $ renderLayout tpls (args' sess)
    where   args' (Just (Session name)) = ("currentUser", B.unpack name) : args
            args' Nothing               = args

renderLayout :: STDirGroups String -> [(String, String)] -> String
renderLayout tpls args = renderTemplateGroup template' args' "layout"
    where   template' = template tpls
            args' | not editedSelected && not historySelected = args ++ 
                    [("articleSelected", "True")]
                  | otherwise = args
            editedSelected  = isJust $ lookup "editedSelected"  args
            historySelected = isJust $ lookup "historySelected" args
    
template = getTemplateGroup "."

resource :: [(String, Method, String -> ServerPartT IO Response)] -> 
            ServerPartT IO Response
resource dispatch = ServerPartT $ \req -> do
    case rqPaths req of 
        (id:action:xs) -> maybe noHandle 
                            (\x -> unServerPartT (x id) $ req'' req) 
                            (lookup' action (rqMethod req) dispatch)
        (id:xs)        -> maybe noHandle 
                            (\x -> unServerPartT (x id) $ req' req) 
                            (lookup' "" (rqMethod req) dispatch)
        otherwise      -> noHandle
    where   id r      = head $ rqPaths r
            paths' r  = drop 1 $ rqPaths r
            paths'' r = drop 2 $ rqPaths r
            req' r    = r { rqPaths = paths' r }
            req'' r   = r { rqPaths = paths'' r }
    
lookup' :: String -> Method -> [(String, Method, a)] -> Maybe a
lookup' _ _ [] = Nothing
lookup' str meth ((str', meth', res'):xs) 
    | str == str' && meth == meth' = Just res'
    | otherwise                    = lookup' str meth xs

fetchSession :: Request -> IO (Maybe Session)
fetchSession req = maybe (return Nothing) (id) 
                 ((lookup "sid" $ rqCookies req) >>= 
                   (Just . query . GetSession . read . cookieValue))

fetchSessionId :: Request -> (Maybe String)
fetchSessionId req = (lookup "sid" $ rqCookies req) >>= (Just . cookieValue)

tryRenderSP :: (a -> ServerPartT IO Response) -> Maybe a -> 
             ServerPartT IO Response
tryRenderSP = maybe (ServerPartT $ \req -> noHandle)

tryRender :: (a -> WebT IO Response) -> Maybe a -> WebT IO Response
tryRender = maybe (noHandle)


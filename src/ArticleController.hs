module ArticleController
    (articleController)
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers
import Control.Monad (mplus, liftM3, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Exception (try, userErrors)
import Maybe (isNothing, fromJust)
import Data.List (isPrefixOf)
import System.Random (randomIO)

import State.AppState
import Utils

import qualified Data.ByteString.Char8 as B

data ArticleForm = ArticleForm {
    articleFormName       :: String
  , articleFormContent    :: String
  , articleFormCategories :: [String]
}

splitSpliced :: String -> String -> [String]
splitSpliced split allStr = work allStr ""
    where   work "" accum = [accum]
            work allStr accum 
                | split `isPrefixOf` allStr = accum : 
                                              (work (dropLength' allStr) "")
                | otherwise = work (drop 1 allStr) (accum ++ (take 1 allStr))
            length' = length split
            dropLength' = drop length' 


lookSplicedList :: String -> RqData [String]
lookSplicedList name = 
    look name >>= (return . splitSpliced ", ")

instance FromData ArticleForm where
    fromData = liftM3 ArticleForm (look "name" `mplus` return "")
                                  (look "content" `mplus` return "")
                                  (lookSplicedList "categories" `mplus` return [])

translationTable :: STDirGroups String -> 
                    [(String, HAppS.Server.Method, String -> 
                        ServerPartT IO Response)]
translationTable tpls = [ ("delete",  GET,  doDeleteArticle tpls)
                        , ("edit",    GET,  doEditArticle tpls)
                        , ("edit",    POST, doUpdateArticle tpls)
                        , ("history", GET,  doShowHistoryArticle tpls)
                        , ("version", GET,  doShowVersionArticle tpls)
                        , ("",        GET,  doShowArticle tpls)
                        ]

articleController :: STDirGroups String -> ServerPartT IO Response
articleController tpls = multi $ [
        dir "articles" [
            methodSP GET $ viewArticles tpls
          , dir "new" [
                methodSP GET $ doNewArticle tpls
              , methodSP POST $ doCreateArticle tpls
            ]
          , dir "random" [
                methodSP GET $ doShowRandomArticle tpls
            ]
          , resource (translationTable tpls)
        ]
    ]

viewArticles :: STDirGroups String -> ServerPartT IO Response
viewArticles tpls = withRequest $ \req -> do
    articles <- query GetArticles
    unServerPartT 
        (renderLayoutSP tpls [("content", renderArticleIndex articles)]) req
    where   renderArticleIndex xs = renderTemplateGroup template' 
                                    (articlesArgs xs) "articles-index"
            articlesArgs xs = [("articles", renderArticles xs)]
            renderArticles = concatMap renderArticle
            renderArticle x = renderTemplateGroup template' (articleArgs x)
                                "article-index"
            template' = template tpls
            articleArgs (name, a) = [ ("name",    B.unpack   name)
                                    , ("author",  B.unpack $ authorName a)
                                    , ("content", B.unpack $ htmlContent a)
                                    ]
    

doNewArticle :: STDirGroups String -> ServerPartT IO Response
doNewArticle tpls = doRenderNewArticle tpls []

doRenderNewArticle :: STDirGroups String -> [(String, String)] -> 
                      ServerPartT IO Response
doRenderNewArticle tpls args = renderNewArticle
    where   renderNewArticle = renderLayoutSP tpls [("content", renderPartial)]
            renderPartial = renderTemplateGroup template' args
                            "article-new"
            template' = template tpls

tryCreateArticle :: String -> ArticleForm -> IO (Either String ())
tryCreateArticle author (ArticleForm name content cats) = do
    result <- query $ GetArticle (B.pack name)
    case result of
        Just _  -> return $ Left errMsg
        Nothing -> (update $ CreateArticle (B.pack name) 
                                           (B.pack author) 
                                           (map B.pack cats)
                                           (B.pack content)) >> 
                    return (Right ())
    where   errMsg = "Error, an article with the same name is already existing"
doCreateArticle :: STDirGroups String -> ServerPartT IO Response
doCreateArticle tpls = withData $ \form@(ArticleForm name content cats) -> [ 
        withRequest $ \req -> do
            result <- lift $ try (do 
                sess <- fetchSession req
                when (isNothing sess) 
                    (fail "You must be logged in to write new articles.")
                when (name == "") (fail "You must enter a valid name.")
                (Session author) <- return (fromJust sess)
                res <- tryCreateArticle (B.unpack author) form
                case res of 
                    Left str -> fail str
                    Right x  -> return x
                )
            case result of 
                Left err -> unServerPartT (doRenderNewArticle 
                            tpls [("errorMessages",  show err)])
                            req
                Right result -> (found $ "/articles/" ++ name) .
                                 toResponse . HtmlString $ "created"
    ]
    

doDeleteArticle :: STDirGroups String -> String -> ServerPartT IO Response
doDeleteArticle tpls id = error "undefined"

doEditArticle :: STDirGroups String -> String -> ServerPartT IO Response
doEditArticle tpls id = withRequest $ \req -> do
    article <- query $ GetArticle (B.pack id)
    unServerPartT (
        maybe (ServerPartT $ \req -> noHandle) 
              (doRenderEditArticle tpls id) 
               article
            ) req

doRenderEditArticle :: STDirGroups String -> String -> Article -> 
                       ServerPartT IO Response
doRenderEditArticle tpls id article = withRequest $ \req -> do
    sess <- lift $ fetchSession req
    case sess of 
        Nothing -> unServerPartT (doRenderEditArticleForm tpls id article
                        [("errorMessages", errorMsg)]) req
        Just (Session s) -> unServerPartT 
                    (doRenderEditArticleForm tpls id article []) req

    where errorMsg = "You must be logged in to edit articles"

doRenderEditArticleForm :: STDirGroups String -> String -> Article -> 
                           [(String, String)] -> ServerPartT IO Response
doRenderEditArticleForm tpls id article args = renderLayoutSP tpls 
                                            [ ("content", articleForm)
                                            , ("editedSelected", "True")
                                            , ("articleName", id)
                                            ]
    where   template'   = template tpls
            articleForm = renderTemplateGroup template' args' "article-edit"
            args' = args ++ [ ("articleContent" , 
                                (B.unpack $ markupContent article))
                            , ("articleName", id) 
                            , ("articleCategories", splice ", " $ 
                                map B.unpack $ articleCategories article)
                            ]

formToArticle :: ArticleForm -> Article
formToArticle (ArticleForm name content cats) = Article (B.pack name) 
                                                        (B.pack content) 
                                                        (B.pack content) 
                                                        (B.pack "") 
                                                        (map B.pack cats)

doUpdateArticle :: STDirGroups String -> String -> ServerPartT IO Response
doUpdateArticle tpls id = withData $ \form@(ArticleForm name content cats) -> [
        withRequest $ \req -> do
            result <- liftIO $ try ( do
                sess <- fetchSession req
                when (isNothing sess) 
                    (fail "You must be logged in to upate an article.")
                when (name == "") 
                    (fail "Please enter a valid name")
                (Just (Session author)) <- return sess
                update $ RenameArticle (B.pack id) (B.pack name)
                update $ CreateArticle (B.pack name)
                                       (author)
                                       (map B.pack cats)
                                       (B.pack content)
                return ()
                )
            case result of
                Left err -> unServerPartT (doRenderEditArticleForm 
                                            tpls id (formToArticle form) 
                                            [("errorMessages", 
                                              fromJust . userErrors $ err)]
                                          ) req
                Right _  -> (found $ "/articles/" ++ name) . toResponse . 
                             HtmlString $ ""
        ]

doShowArticle :: STDirGroups String -> String -> ServerPartT IO Response
doShowArticle tpls id = withRequest $ \req -> do
    article <- lift $ query $ GetArticle (B.pack id)
    maybe (noHandle) (\x -> unServerPartT (doRenderArticle tpls id x) req) 
          article

splice :: String -> [String] -> String
splice _ [] = []
splice _ (x:[]) = x
splice str (x:xs) = x ++ str ++ (splice str xs)

doRenderArticle :: STDirGroups String -> String -> Article -> 
                   ServerPartT IO Response
doRenderArticle tpls id article = renderLayoutSP
                                  tpls $ [("content", articleTemplate)] ++ args
    where   articleTemplate = renderTemplateGroup template' args "article-show"
            template' = template tpls
            args      = [ ("articleName", id)
                        , ("articleContent", B.unpack $ htmlContent article)
                        , ("articleAuthor", B.unpack $ authorName article)
                        , ("articleCategories", 
                            splice ", " $ map B.unpack $ 
                            articleCategories article)
                        ]

doShowHistoryArticle :: STDirGroups String -> String -> ServerPartT IO Response
doShowHistoryArticle tpls id = withRequest $ \req -> do
    history <- lift $ query $ GetArticleHistory (B.pack id)
    unServerPartT (tryRenderSP (doRenderHistoryArticle tpls id) history) req

doRenderHistoryArticle :: STDirGroups String -> String -> [Article] -> 
                          ServerPartT IO Response
doRenderHistoryArticle tpls id articles = result
    where   result    = renderLayoutSP tpls args
            args      = [ ("content", content)
                        , ("historySelected", "True")
                        , ("articleName", id)
                        ]
            content   = renderTemplateGroup template' 
                            [ ("articles", articlesStr)
                            , ("articleName", id)
                            ] "articles-history"
            articlesStr = concatMap renderArticle $ zip articles ids
            renderArticle (a, i) = renderTemplateGroup template'
                                [ ("articleName", B.unpack $ articleName a)
                                , ("articleAuthor", B.unpack $ authorName a)
                                , ("articleVersion", show i)
                                , ("articleContent", B.unpack $ htmlContent a)
                                ] "article-history"
            template' = template tpls
            ids       = reverse $ take (length articles) [1..]

doShowVersionArticle :: STDirGroups String -> String -> ServerPartT IO Response
doShowVersionArticle tpls id = withRequest $ \req -> do
    case rqPaths req of
        (version:xs) -> unServerPartT 
                            (doRenderShowVersionArticle tpls id version)
                            (req' req)
        otherwise    -> noHandle
    where   req' r = let paths' = drop 1 $ rqPaths r
                     in r { rqPaths = paths' }

doRenderShowVersionArticle :: STDirGroups String -> String -> String -> 
                              ServerPartT IO Response
doRenderShowVersionArticle tpls id version = withRequest $ \req -> do
    articles' <- query $ GetArticleHistory $ B.pack id
    when (isNothing articles') noHandle
    let article = (reverse $ fromJust articles') !! ((read version) - 1)
    unServerPartT (doRenderArticle tpls id article) req

doShowRandomArticle :: STDirGroups String -> ServerPartT IO Response
doShowRandomArticle tpls = do
    articles <- query $ GetArticles
    key <- liftIO $ fmap (`mod` (length articles)) randomIO
    let element = articles !! key
    doRenderArticle tpls (B.unpack . fst $ element)  (snd element)

module ArticleController
    (articleController)
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers
import Control.Monad (mplus, liftM2, when)
import Control.Monad.Trans (lift)
import Control.Exception (try)
import Maybe (isNothing, fromJust)

import State.AppState
import Utils

import qualified Data.ByteString.Char8 as B

data ArticleForm = ArticleForm {
    articleFormName    :: String
  , articleFormContent :: String
}

instance FromData ArticleForm where
    fromData = liftM2 ArticleForm (look "name" `mplus` return "")
                                  (look "content" `mplus` return "")

translationTable :: STDirGroups String -> 
                    [(String, HAppS.Server.Method, String -> 
                        ServerPartT IO Response)]
translationTable tpls = [ ("delete", GET,  doDeleteArticle tpls)
                        , ("edit",   GET,  doEditArticle tpls)
                        , ("edit",   POST, doUpdateArticle tpls)
                        , ("",       GET,  doShowArticle tpls)
                        ]

articleController :: STDirGroups String -> ServerPartT IO Response
articleController tpls = multi $ [
        dir "articles" [
            methodSP GET $ viewArticles tpls
          , dir "new" [
                methodSP GET $ doNewArticle tpls
              , methodSP POST $ doCreateArticle tpls
            ]
          , resource (translationTable tpls)
        ]
    ]

viewArticles :: STDirGroups String -> ServerPartT IO Response
viewArticles tpls = withRequest $ \req -> do
    articles <- query GetArticles
    ok . toResponse . HtmlString $ 
        renderLayout tpls [("content", renderArticleIndex articles)]
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
doRenderNewArticle tpls args = withRequest $ \req -> do
    ok . toResponse . HtmlString $ renderNewArticle
    where   renderNewArticle = renderLayout tpls [("content", renderPartial)]
            renderPartial = renderTemplateGroup template' args
                            "article-new"
            template' = template tpls

tryCreateArticle :: String -> ArticleForm -> IO (Either String ())
tryCreateArticle author (ArticleForm name content) = do
    result <- query $ GetArticle (B.pack name)
    case result of
        Just _  -> return $ Left errMsg
        Nothing -> (update $ CreateArticle (B.pack name) 
                                           (B.pack author) 
                                           (B.pack content)) >> 
                    return (Right ())
    where   errMsg = "Error, an article with the same name is already existing"
doCreateArticle :: STDirGroups String -> ServerPartT IO Response
doCreateArticle tpls = withData $ \form@(ArticleForm name content) -> [ 
        withRequest $ \req -> do
            result <- lift $ try (do 
                sess <- fetchSession req
                when (isNothing sess) 
                    (fail "You must be logged in to write new articles.")
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
        Nothing -> doRenderEditArticleForm tpls id article
                    [("errorMessages", errorMsg)]
        Just (Session s) -> doRenderEditArticleForm tpls id article []

    where errorMsg = "You must be logged in to edit articles"

doRenderEditArticleForm :: STDirGroups String -> String -> Article -> 
                           [(String, String)] -> WebT IO Response
doRenderEditArticleForm tpls id article args = ok . toResponse . HtmlString $ 
                                           renderLayout tpls 
                                            [("content", articleForm)]
    where   template'   = template tpls
            articleForm = renderTemplateGroup template' args' "article-edit"
            args' = args ++ [ ("articleContent" , (B.unpack $ markupContent article))
                            , ("articleName", id) 
                            ]

doUpdateArticle :: STDirGroups String -> String -> ServerPartT IO Response
doUpdateArticle tpls id = withData $ \(ArticleForm name content) -> [
        withRequest $ \req -> do
            sess <- lift $ fetchSession req
            case sess of
                Nothing -> unServerPartT (doEditArticle tpls id) req
                Just (Session author) -> do
                    update $ CreateArticle (B.pack name)
                                           (author)
                                           (B.pack content)
                    (found $ "/articles/" ++ name) . toResponse . HtmlString $ 
                        ""
        ]

doShowArticle :: STDirGroups String -> String -> ServerPartT IO Response
doShowArticle tpls id = withRequest $ \req -> do
    article <- lift $ query $ GetArticle (B.pack id)
    maybe (noHandle) (doRenderArticle tpls id) article

doRenderArticle :: STDirGroups String -> String -> Article -> WebT IO Response
doRenderArticle tpls id article = ok . toResponse . HtmlString $ renderLayout 
                                  tpls $ [("content", articleTemplate)] ++ args
    where   articleTemplate = renderTemplateGroup template' args "article-show"
            template' = template tpls
            args      = [ ("articleName", id)
                        , ("articleContent", B.unpack $ htmlContent article)
                        , ("articleAuthor", B.unpack $ authorName article)
                        , ("articleCategories", 
                            concatMap B.unpack $ articleCategories article)
                        ]

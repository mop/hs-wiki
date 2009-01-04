module CategoryController
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers
import Control.Monad.Trans (lift)

import State.AppState
import Utils

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

categoryController :: STDirGroups String -> ServerPartT IO Response
categoryController tpls = multi $ [
        dir "categories" [
            methodSP GET $ doShowCategories tpls
        ]
    ]

doShowCategories :: STDirGroups String -> ServerPartT IO Response
doShowCategories tpls = withRequest $ \req -> do
    categories <- query $ GetCategoryTree
    sess <- lift $ fetchSession req
    let sessParams = (case sess of 
                        Nothing -> []
                        Just (Session name) -> [("currentUser", B.unpack name)]
                     )
    unServerPartT (result categories sessParams) req
    
    where   result cs s = renderLayoutSP tpls 
                                          [ ("content", categoriesPartial cs s)
                                          , ("articleName", "categories")
                                          ]
            categoriesPartial cs s = renderTemplateGroup template'
                                     [ ("categories", categoriesStr cs s) ] 
                                     "categories"
            categoriesStr cs s   = concatMap (renderCategory s) $ M.toList cs
            renderCategory s (cat, articles) = renderTemplateGroup template' 
                                               [ ("category", B.unpack cat)
                                               , ("articles", 
                                                  renderArticles s articles)
                                               ] "category"
            renderArticles s = concatMap (renderArticle s)
            renderArticle s a = renderTemplateGroup template' 
                                ([ ("articleName", B.unpack $ articleName a) 
                                 , ("articleAuthor", B.unpack $ authorName a)
                                 , ("articleContent", B.unpack $ htmlContent a)
                                 ] ++ s) "category-article"
            template' = template tpls


module CategoryController
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers

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
    unServerPartT (result categories) req
    
    where   result cs = renderLayoutSP tpls [ ("content", categoriesPartial cs)
                                          , ("articleName", "categories")
                                          ]
            categoriesPartial cs = renderTemplateGroup template'
                                   [ ("categories", categoriesStr cs) ] 
                                   "categories"
            categoriesStr cs     = concatMap renderCategory $ M.toList cs
            renderCategory (cat, articles) = renderTemplateGroup template' 
                                             [ ("category", B.unpack cat)
                                             , ("articles", 
                                                renderArticles articles)
                                             ] "category"
            renderArticles = concatMap renderArticle
            renderArticle a = renderTemplateGroup template' 
                                [ ("articleName", B.unpack $ articleName a) 
                                , ("articleAuthor", B.unpack $ authorName a)
                                , ("articleContent", B.unpack $ htmlContent a)
                                ] "category-article"
            template' = template tpls


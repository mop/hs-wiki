module Utils
    ( HtmlString(..)
    , renderLayout
    )
where

import HAppS.Server
import Text.StringTemplate
import Text.StringTemplate.Helpers

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

newtype HtmlString = HtmlString { htmlString :: String }
instance ToMessage HtmlString where
    toContentType _          = B.pack "text/html"
    toMessage (HtmlString s) = L.pack s

renderLayout :: STDirGroups String -> [(String, String)] -> String
renderLayout tpls args = renderTemplateGroup template args "layout"
    where   template = getTemplateGroup "." tpls

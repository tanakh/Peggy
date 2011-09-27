#! /usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Text.XHtml.Strict

import Hakyll
import Text.Pandoc

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler
      
  match "bootstrap/**" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "google-code-prettify/*" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match (list ["index.md"]) $ do
    route   $ setExtension "html"
    compile $ pageCompilerWithPandoc defaultHakyllParserState
                                     defaultHakyllWriterOptions { writerHtml5 = True }
                                     googlePrettify
      >>> applyTemplateCompiler "templates/default.hamlet"
      >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
  where deploy = "cp _site/*.html . && ./hakyll.hs clean && git commit -am 'deploy'"

googlePrettify :: Pandoc -> Pandoc
googlePrettify doc = bottomUp f doc where
  f (CodeBlock _ code) =
    RawBlock "html" $ "<pre class=\"prettyprint\">" ++ stringToHtmlString code ++ "</pre>"
  f b = b

#! /usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad
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

  let srcs  = ["index.md", "tutorial.md", "syntax.md", "example.md", "about.md"]
      langs = ["", "ja/"]
  
  forM_ langs $ \lan -> do
    match (list $ map (parseIdentifier . (lan++)) srcs) $ do
      route   $ setExtension "html"
      compile $ pageCompilerWithPandoc defaultHakyllParserState
                                       defaultHakyllWriterOptions { writerHtml5 = True }
                                       googlePrettify
        >>> applyTemplateCompiler "templates/default.hamlet"
        >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
  where deploy = "./hakyll.hs rebuild && cp _site/*.html . && ./hakyll.hs clean && git commit -am 'deploy'"

googlePrettify :: Pandoc -> Pandoc
googlePrettify doc = bottomUp f doc where
  f (CodeBlock _ code) =
    RawBlock "html" $ "<pre class=\"prettyprint\">" ++ stringToHtmlString code ++ "</pre>"
  f b = b

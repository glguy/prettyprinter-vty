{-|
Module      : Data.Text.Prettyprint.Doc.Render.Vty
Description : Rending prettyprinter Doc to Vty image
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Data.Text.Prettyprint.Doc.Render.Vty
  ( render, renderSDS
  , VtyDocError(..)
  ) where

import Control.Exception
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import Data.Text.Prettyprint.Doc

-- | Errors that can occur due to bugs in the layout logic
data VtyDocError
  = VtyDocFail   -- ^ SimpleDocStream contained 'SFail'
  | VtyDocBadAnn -- ^ SimpleDocStream had too many 'SAnnPop'
  deriving (Eq, Ord, Show, Read)

-- | 'displayException' provides human readable description.
instance Exception VtyDocError where
  displayException VtyDocFail   = "Encountered failure in simple document stream while rendering to VTY"
  displayException VtyDocBadAnn = "Encountered mismatched annotations while rendering to VTY"

-- | Render a document as a VTY image using 'defaultLayoutOptions'.
--
-- Throws: 'VtyDocError'
render :: Doc Attr -> Image
render = renderSDS . layoutPretty defaultLayoutOptions

-- | Render a document stream as a VTY image.
--
-- Throws: 'VtyDocError'
renderSDS :: SimpleDocStream Attr -> Image
renderSDS = go defAttr [] emptyImage

go :: Attr -> [Attr] -> Image -> SimpleDocStream Attr -> Image
go attr stack line d =
  case d of
    SFail         -> throw VtyDocFail
    SEmpty        -> line
    SChar c x     -> go attr stack (line <|> char attr c) x
    SText _ txt x -> go attr stack (line <|> text' attr txt) x
    SLine n x     -> line <-> go attr stack (string mempty (replicate n ' ')) x
    SAnnPush a x  -> go a (attr:stack) line x
    SAnnPop x     ->
      case stack of
        []   -> throw VtyDocBadAnn
        a:as -> go a as line x

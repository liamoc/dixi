module Dixi.Pandoc.Wikilinks (wikilinks) where

import Text.Pandoc
import Text.Pandoc.Walk
import Network.URI

wikilinks :: String -> Pandoc -> IO Pandoc
wikilinks s = return . wikilinks' s

wikilinks' :: String -> Pandoc -> Pandoc
wikilinks' s = walk (wikify s)

wikify :: String -> Inline -> Inline
wikify base (Link a s ("",t)) = Link a s (url, t)
  where url = base ++ "/" ++ inlinesToURL s
wikify base (Link a s (l,"wikilink")) = Link a s (base ++ "/" ++ l, "wikilink")
wikify _ x = x

-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL = escapeURIString isUnescapedInURI . inlinesToString

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s                   -> s
               Emph xs                 -> concatMap go xs
               Strong xs               -> concatMap go xs
               Strikeout xs            -> concatMap go xs
               Superscript xs          -> concatMap go xs
               Subscript xs            -> concatMap go xs
               SmallCaps xs            -> concatMap go xs
               Quoted DoubleQuote xs   -> '"' : (concatMap go xs ++ "\"")
               Quoted SingleQuote xs   -> '\'' : (concatMap go xs ++ "'")
               Cite _ xs               -> concatMap go xs
               Code _ s                -> s
               Space                   -> "_"
               LineBreak               -> "_"
               Math DisplayMath s      -> "$$" ++ s ++ "$$"
               Math InlineMath s       -> "$" ++ s ++ "$"
               RawInline (Format "tex") s -> s
               RawInline _ _           -> ""
               Link _ xs _             -> concatMap go xs
               Image _ xs _            -> concatMap go xs
               Note _                  -> ""
               Span _ xs               -> concatMap go xs
               SoftBreak {}            -> ""

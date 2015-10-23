{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
import Dixi.API
import Dixi.Forms()
import Dixi.Markup()
import Servant.Docs
import Servant.API
import System.Environment
import Data.Text (Text)
instance ToSample RevReq RevReq where
  toSample _ = Nothing
instance ToSample RawPage RawPage where
  toSample _ = Nothing
instance ToSample PrettyPage PrettyPage where
  toSample _ = Nothing
instance ToSample NewBody NewBody where
  toSample _ = Nothing
instance ToSample History History where
  toSample _ = Nothing
instance ToSample DiffPage DiffPage where
  toSample _ = Nothing
instance ToParam (QueryParam "from" Int) where
  toParam _ = DocQueryParam "from" [] "Version to diff from, starting from 0 to N-1" Normal
instance ToParam (QueryParam "to" Int) where
  toParam _ = DocQueryParam "to" [] "Version to diff to, from 1 to N" Normal
instance ToCapture (Capture "page" Text) where
  toCapture _ = DocCapture "page" "Title of the page, using underscores for spaces."
instance ToCapture (Capture "version" Int) where
  toCapture _ = DocCapture "version" "A page version to examine. All pages have a zeroth version, which is empty."

main :: IO ()
main = do
  filename <- fmap (\x -> if null x then "docs.md" else head x) getArgs
  writeFile filename (markdown (docs dixi))

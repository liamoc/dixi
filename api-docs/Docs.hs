{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
import Data.Proxy
import Data.Patch(HunkStatus (..))
import Dixi.API
import Dixi.Forms()
import Dixi.Markup()
import Servant.Docs
import Servant.API
import System.Environment
import Data.Text (Text)
import Dixi.Config
import Dixi.Page
import Data.Time
import Data.Monoid
import Control.Lens
import qualified Data.Vector as V
import Text.Hamlet

instance ToSample RevReq RevReq where
  toSample _ = Just $ DR 5 7 (Just "Revert changes 5-6, 6-7")

instance ToSample RawPage RawPage where
  toSample _ = Just $ RP defaultRenders "Page_Title" 3 $ Page "Some page content, in input format (e.g org mode)"
                                                              (Last (Just "An optional comment"))
                                                              (Last (Just (UTCTime (ModifiedJulianDay 0) 0)))
instance ToSample PrettyPage PrettyPage where
  toSample _ = Just $ PP defaultRenders "Page_Title" 3 $ Page [shamlet|Some page content, in <b>HTML</b> from Pandoc.|]
                                                              (Last (Just "An optional comment"))
                                                              (Last (Just (UTCTime (ModifiedJulianDay 0) 0)))
instance ToSample NewBody NewBody where
  toSample _ = Just $ NB "Some new content, in input format (e.g org mode)" (Just "An optional comment")

instance ToSample History History where
  toSample _ = Just $ H  defaultRenders "Page_Title"
                 [ Page (4, 9, 14) (Last (Just "Change 1")) (Last (Just (UTCTime (ModifiedJulianDay 0) 0)))
                 , Page (12, 3, 1) (Last (Just "Change 2")) (Last (Just (UTCTime (ModifiedJulianDay 1) 0)))
                 , Page (12, 0, 0) (Last (Just "Change 3")) (Last (Just (UTCTime (ModifiedJulianDay 2) 0)))
                 ]

instance ToSample DiffPage DiffPage where
  toSample _ = Just $ DP defaultRenders "Page_Title" 5 7
                         (Page [ (V.fromList "This is the ", Unchanged)
                               , (V.fromList "new", Replaced)
                               , (V.fromList "ginal", Deleted)
                               , (V.fromList " document.", Unchanged)
                               ]
                            (Last (Just "An optional comment"))
                            (Last (Just (UTCTime (ModifiedJulianDay 0) 0))))

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
  writeFile filename (markdown $ postprocess $ docs dixi)

postprocess :: API -> API
postprocess = over (apiEndpoints . traverse . rqbody)              (filter $ (== contentType (Proxy :: Proxy JSON)) . view _1)
            . over (apiEndpoints . traverse . response . respBody) (filter $ (== contentType (Proxy :: Proxy JSON)) . view _2)

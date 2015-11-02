{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
import Data.Proxy
import Data.Patch(HunkStatus (..))
import Dixi.API
import Dixi.Forms()
import Dixi.Markup()
import Servant.Docs
import Servant.API
import Servant.HTML.Blaze
import System.Environment
import Data.Text (Text)
import Dixi.Config
import Dixi.Page
import Data.Time
import Dixi.Common
import Data.Monoid
import Control.Lens
import qualified Data.Vector as V
import Text.Hamlet
import Data.Aeson.Parser
import Data.Attoparsec.ByteString.Lazy
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LBS
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
  writeFile filename (markdown $ postprocess $ docsWith intro extra dixi)

postprocess :: API -> API
postprocess = over (apiEndpoints . traverse . rqbody)              (filter ((== contentType (Proxy :: Proxy JSON)) . view _1) . fmap (over _2 prettify))
            . over (apiEndpoints . traverse . response . respBody) (filter ((== contentType (Proxy :: Proxy JSON)) . view _2) . fmap (over _3 prettify))



diffNote, revertNote, historyNote, amendNote :: String -> [DocNote]
diffNote page = [ DocNote "Description" [ "Compute the diff between two versions of " ++ page ++ "."
                                        , "If the `from` version is greater than the `to` version, they are swapped."
                                        ]
                , DocNote "Reponse details" ["Each diff hunk uses a single character string to " ++
                                             "indicate the type of change, where " ++
                                             "`+` is used for additions, `-` for deletions, and `~` for replacements."]
                ]

revertNote page = [ DocNote "Description" [ "Apply a new patch which reverts any still-extant changes made in a certain version range of " ++ page ++ "."
                                          , "If the `from` version is greater than the `to` version they are swapped."
                                          ]
                  , DocNote "Response details" ["The response is just the most recent version of the page, " ++
                                                "with the revert patch applied."]
                  ]
historyNote page = [ DocNote "Description" [   "Returns the complete version history of " ++ page
                                            ++ ", with patch summaries, times, and change comments."]]
amendNote page = [ DocNote "Description" [ "Submit a new version to be applied to the " ++ page ++ ", based on the given version."
                                         , "The change will be transformed and applied to the latest version."
                                         ]
                 ]
prettyNote, rawNote :: String -> String -> [DocNote]
prettyNote version page = [ DocNote "Description" [   "Returns the " ++ version ++ " of the " ++ page
                                                   ++ ", in HTML format as processed by Pandoc."]]
rawNote version page = [ DocNote "Description" [   "Returns the " ++ version ++ " of the " ++ page
                                                ++ ", in the input format (e.g org mode, markdown)."]]

intro :: [DocIntro]
intro = [ DocIntro "Dixi API Documentation"
                  ["What follows is the documentation for the JSON API of `dixi`."
                  ,"Unlike other wikis `dixi` supports editing of any version of a page, and the changes will be propagated through to the latest version in a way that makes sense."
                  ,"It is also able to revert changes (or the composition of several changes) from deep in a document's history and preserve every other change."
                  ]
        , DocIntro "Structure"
                  ["In general, there are two of each API action, one applying to a particular, specified page and one applying to the page entitled `Main_Page`"
                  ,"The behaviour of these actions is identical, except for the page to which they apply."]
        ]

prettify :: LBS.ByteString -> LBS.ByteString
prettify x = go (parse json x)
  where
    go (Done _ r) = encodePretty r
    go _ = error "Error, cannot parse json value! Something is seriously wrong here."

extra :: ExtraInfo Dixi
extra = mconcat
      [ extraInfo (Proxy :: Proxy (Capture "page" Key :> "history" :> "diff" :> QueryParam "from" Version :> QueryParam "to" Version :> Get '[HTML,JSON] DiffPage)) $
                  defAction & notes <>~ diffNote "the given page"
      , extraInfo (Proxy :: Proxy ("history" :> "revert" :> ReqBody '[FormUrlEncoded, JSON] RevReq :> Post '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ revertNote "the `Main_Page`"
      , extraInfo (Proxy :: Proxy ("history" :> "diff" :> QueryParam "from" Version :> QueryParam "to" Version :> Get '[HTML,JSON] DiffPage)) $
                  defAction & notes <>~ diffNote "the `Main_Page`"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> "history" :> "revert" :> ReqBody '[FormUrlEncoded, JSON] RevReq :> Post '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ revertNote "the given page"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> "history" :> Get '[HTML, JSON] History)) $
                  defAction & notes <>~ historyNote "the given page"
      , extraInfo (Proxy :: Proxy ("history" :> Get '[HTML, JSON] History)) $
                  defAction & notes <>~ historyNote "the page entitled `Main_Page`"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> "history" :> Capture "version" Version :> Get '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ prettyNote "given version" "given page"
      , extraInfo (Proxy :: Proxy ("history" :> Capture "version" Version :> Get '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ prettyNote "given version" "page entitled `Main_Page`"
      , extraInfo (Proxy :: Proxy (Get '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ prettyNote "latest version" "page entitled `Main_Page`"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> Get '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ prettyNote "latest version" "given page"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> "history" :> Capture "version" Version :> "raw" :> Get '[HTML, JSON] RawPage)) $
                  defAction & notes <>~ rawNote "given version" "given page"
      , extraInfo (Proxy :: Proxy ("history" :> Capture "version" Version :> "raw" :> Get '[HTML, JSON] RawPage)) $
                  defAction & notes <>~ rawNote "given version" "page entitled `Main_Page`"
      , extraInfo (Proxy :: Proxy ("raw" :> Get '[HTML, JSON] RawPage)) $
                  defAction & notes <>~ rawNote "latest version" "page entitled `Main_Page`"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> "raw" :> Get '[HTML, JSON] RawPage)) $
                  defAction & notes <>~ rawNote "latest version" "given page"
      , extraInfo (Proxy :: Proxy (Capture "page" Key :> "history" :> Capture "version" Version :> ReqBody '[FormUrlEncoded, JSON] NewBody :> Post '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ amendNote "given page"
      , extraInfo (Proxy :: Proxy ("history" :> Capture "version" Version :> ReqBody '[FormUrlEncoded, JSON] NewBody :> Post '[HTML, JSON] PrettyPage)) $
                  defAction & notes <>~ amendNote "`Main_Page`"
      ]

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


import Alfred
import Hoogle
import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as M
import Network.HTTP.Simple
import qualified Data.Text as T
import Data.Maybe
import Control.Exception
import qualified Text.HTML.TagSoup as TS

main :: IO ()
main = alfMain hoogleSearch

prevSearchKey :: String
prevSearchKey = "prevSearchTerm"

hoogleSearch :: AlfM () Return
hoogleSearch = do
  args <- alfArgs
  let argsString = concat args
  doLocalSearch'  <- envVariableThrow "local_search"
  doLocalSearch   <- readBool doLocalSearch'
  doOnlineSearch' <- envVariableThrow "online_search"
  doOnlineSearch  <- readBool doOnlineSearch'
  prioLocal'      <- envVariableThrow "prio_offline"
  prioLocal       <- readBool prioLocal'
  prevSearch      <- envVariable prevSearchKey

  let isSecondRun = case prevSearch of
        Nothing   -> False
        (Just ps) -> ps == argsString

  localResults' <- searchLocal argsString
  let localResults = targetToReturn "Local search failed"
                                    "hask_local.png"
                                    localResults'
  onlineResults' <- searchOnline argsString
  let onlineResults = targetToReturn
        "Error: 'hoogle.haskell.org' not reachable"
        "hask_web.png"
        onlineResults'

  -- | The first run if both are searched just does local results, then sets the argument to rerun the
  -- script, on the second run both are searched and the result is updated in Alfed when they are done
  if
    | isSecondRun -> return $ unSetPrevSearch $ merge prioLocal localResults onlineResults
    | doLocalSearch && doOnlineSearch -> return $ setPrevSearch argsString $ setRerun localResults
    | doOnlineSearch -> return onlineResults
    | doLocalSearch -> return localResults
    | otherwise -> throwAlfE $ OtherError "Both 'online_search' and 'offline_search' are false"

targetToReturn :: String -> FilePath -> Maybe [Target] -> Return
targetToReturn  _ iconFp (Just []) =
  defaultReturn { items = [addIcon iconFp $ defaultItem { title = "No results" , valid = False }] }
targetToReturn _ iconFp (Just ts) =
  defaultReturn { items = addIcon iconFp . targetToItem <$> ts }
targetToReturn conErr _ Nothing =
  defaultReturn { items = [defaultItem { title = conErr, valid = False }] }

merge :: Bool -> Return -> Return -> Return
merge prioLocal local online = if prioLocal
  then local { items = newItems }
  else online { items = newItems }
 where
  newItems =
    concat $ zipWithTails (: []) (: []) choose (items local) (items online)
  itemsEqual a b = title a == title b && subtitle a == subtitle b
  choose a b | a `itemsEqual` b && prioLocal = [a]
             | a `itemsEqual` b = [b]
             | otherwise = if prioLocal then [a, b] else [b, a]

targetToItem :: Target -> Item
targetToItem t = defaultItem
  { title        = titleS
  , subtitle     = liftA2 (maybeSeparator ".")
                          (fst <$> targetPackage t)
                          (fst <$> targetModule t)
  , arg          = Just $ targetURL t
  , autocomplete = Just nameS
  , quicklookurl = Just $ targetURL t
  , text         = Just $ RetText (Just nameS) (Just docsS)
  , mods = defaultMods { shift = doMod "package" <$> targetPackage t
                       , alt   = doMod "module" <$> targetModule t
                       , ctrl  = Just $ defaultMod { modSubtitle = Just docsS }
                       }
  }
 where
  stripHtmlToList =
    fmap (T.replace "<0>" "" . TS.fromTagText)
      . filter TS.isTagText
      . TS.parseTags
      . T.pack
  htmlListToString = T.unpack . T.concat
  titleS           = htmlListToString $ stripHtmlToList $ targetItem t
  docsS            = htmlListToString $ stripHtmlToList $ targetDocs t
  nameS = htmlListToString $ headToList $ stripHtmlToList $ targetItem t
  doMod modType (modName, modUrl) = defaultMod
    { modValid    = Just True
    , modSubtitle = Just
                    $  "Open "
                    <> modType
                    <> " '"
                    <> modName
                    <> "' in browser"
    , modArg      = Just modUrl
    }

searchLocal :: String -> AlfM () (Maybe [Target])
searchLocal _ = return Nothing -- TODO using shell json?

-- | returns Nothing if unable to connect or reponse code /= 200
searchOnline :: String -> AlfM () (Maybe [Target])
searchOnline term = do
  let req = buildRequest term 10
  mResp <- liftIO $ try $ httpJSONEither req
  case mResp of
    Left  (_ :: HttpException) -> return Nothing
    Right resp                 -> case getResponseStatusCode resp of
      200 -> case getResponseBody resp of
        Left e -> throwAlfE $ OtherError $ "Failed to decode JSON: " <> show e
        Right body -> return $ Just body
      _ -> return Nothing

buildRequest :: String -> Int -> Request
buildRequest term n =
  setRequestQueryString
      [ ("mode"  , Just "json")
      , ("start" , Just "1")
      , ("count" , Just $ C8.pack $ show n)
      , ("hoogle", Just $ C8.pack term)
      ]
    $ parseRequest_ "https://hoogle.haskell.org"

addIcon :: FilePath -> Item -> Item
addIcon fp i = i { icon = Just $ Icon False fp }

setPrevSearch :: String -> Return -> Return
setPrevSearch str ret = ret { retVars = M.insert prevSearchKey str (retVars ret)}

unSetPrevSearch :: Return -> Return
unSetPrevSearch  ret = ret { retVars = M.insert prevSearchKey "" (retVars ret)}

-- | The smallest value possible
setRerun :: Return -> Return
setRerun ret = ret { rerun = Just 0.1 }

readBool :: String -> AlfM s Bool
readBool str = case str of
  "true"  -> return True
  "false" -> return False
  _       -> throwAlfE $ OtherError "Bool value other then 'true' or 'false'"

maybeSeparator :: (Foldable t, Semigroup (t a)) => t a -> t a -> t a -> t a
maybeSeparator s ms ms' | not (null ms) && not (null ms') = ms <> s <> ms'
                        | otherwise                       = ms <> ms'

headToList :: [a] -> [a]
headToList (a : _) = [a]
headToList _       = []

zipWithTails :: (a -> c) -> (b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithTails l r f as bs = catMaybes . takeWhile isJust $ zipWith fMaybe
                                                                  (extend as)
                                                                  (extend bs)
 where
  extend xs = map Just xs ++ repeat Nothing
  fMaybe a b = liftA2 f a b <|> fmap l a <|> fmap r b


-- | Hoping to merge this instance to hoogle
instance FromJSON Target where
  parseJSON = withObject "Target" $ \o ->
    Target <$> o .: "url"
           <*> o `namedUrl` "package"
           <*> o `namedUrl` "module"
           <*> o .: "type"
           <*> o .: "item"
           <*> o .: "docs"
    where namedUrl o' n = do
             mObj <- o' .: n
             if null mObj then return Nothing
                        else do
                           pkName <- mObj .: "name"
                           pkUrl  <- mObj .: "url"
                           return $ Just (pkName ,pkUrl)

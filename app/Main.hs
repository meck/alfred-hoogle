{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import           Alfred
import           Alfred.Updater
import           Hoogle
import           Control.Applicative
import qualified Data.ByteString.Char8         as C8
import qualified Data.Map.Strict               as M
import           Data.Binary                    ( Binary )
import           Text.Read                      ( readMaybe )
import           System.IO.Silently
import           System.Directory               ( doesFileExist )
import           Network.HTTP.Simple
import qualified Data.Text                     as T
import           Data.Maybe
import           Control.Exception
import           Text.HTML.TagSoup              ( parseTags
                                                , innerText
                                                , isTagCloseName
                                                , isTagOpenName
                                                )
import           GHC.Generics                   ( Generic )

updateRerunVal :: String
updateRerunVal = "rerun_for_update"

updateItem' :: AlfM s (Maybe Item)
updateItem' = updateItem "meck" "alfred-hoogle"

data AlfHState = AlfHState { localSearchActive :: Bool
                           , onlineSearchActive :: Bool
                           , useLocalServer :: Bool
                           , localFilePath :: Maybe FilePath
                           , pathOfCurrentDb :: Maybe FilePath
                           , preferLocal :: Bool
                           } deriving Generic

instance Binary AlfHState
instance AlfStatable AlfHState where
  defaultState = AlfHState False True False Nothing Nothing True

main :: IO ()
main = alfMain $ envVariable "do_cmd" >>= \case
    (Just "true") -> handleCmd
    _             -> hoogleSearch

handleCmd :: AlfM AlfHState (Maybe Return)
handleCmd = envVariableThrow "cmd" >>= \case
    "settings"           -> cmdShowSettings
    "update_db"          -> cmdUpdateDb
    "unset_local_folder" -> cmdUnsetLocal
    "toggle_bool"        -> envVariableThrow "cmd_arg" >>= cmdToggleBool
    "set_local_folder"   -> envVariableThrow "cmd_arg" >>= cmdSetLocalFolder
    _                    -> throwAlfE $ OtherError "Unknown command"

cmdShowSettings :: AlfM AlfHState (Maybe Return)
cmdShowSettings = do
    (AlfHState lsa osa ls lfp _ plo) <- get
    lSaddr                           <- envVariableThrow "alternate_server_addr"
    setRR                            <- setRerunWithTag updateRerunVal
    isRR                             <- isRerunWithTag updateRerunVal
    updateI                          <- fmap setNoCmd . maybeToList <$> updateItem'
    return $ Just $ setRR $ defaultReturn
        { retVars = M.fromList [("do_cmd", "true")]
        , items   =
            [ itemWithTSV
                    ("Web search: " <> bTt osa)
                    "Disable/Enable online stackage search"
                    [("cmd", "toggle_bool"), ("cmd_arg", "online_search_active")]
                , itemWithTSV
                    ("Alternate server: " <> bTt ls)
                    ("Use '" <> lSaddr <> "' as server")
                    [("cmd", "toggle_bool"), ("cmd_arg", "local_server")]
                , itemWithTSV
                    ("Local search: " <> bTt lsa)
                    "Disable/Enable local search"
                    [("cmd", "toggle_bool"), ("cmd_arg", "local_search_active")]
                , itemWithTSV
                    ("Prefer local results: " <> bTt plo)
                    "Local result is shown when there is a equal one from web search"
                    [("cmd", "toggle_bool"), ("cmd_arg", "prefer_local")]
                , itemWithTSV "Reindex local database"
                              "This might take a minute, will notify when done"
                              [("cmd", "update_db")]
                ]
                ++ maybe
                       []
                       (\fp ->
                           [ itemWithTSV "Reset local path"
                                         ("current path: " <> fp)
                                         [("cmd", "unset_local_folder")]
                           ]
                       )
                       lfp
                ++ (if isRR then updateI else [])
        }
  where
    itemWithTSV t s v =
        defaultItem { title = t, subtitle = Just s, itemVars = M.fromList v }

-----------------------
--  Command Runners  --
-----------------------
-- These just are just run as a scipt therefore we dont output a `Return`
cmdUpdateDb :: AlfM AlfHState (Maybe Return)
cmdUpdateDb = do
    dbF                          <- databasePath
    st@(AlfHState _ _ _ mFp _ _) <- get
    let args = filter
            (not . null)
            [ "generate"
            , "--database=" <> dbF
            , maybe [] ("--local=" <>) mFp
            , "--download"
            ]
    -- Handle exeptions and print to stdout for a done notification
    -- and capture any stdout with silence
    eErr <- liftIO $ silence $ try $ hoogle args
    case eErr of
        (Left (_ :: SomeException)) -> liftIO $ do
            putStrLn "Indexing failed!"
            return Nothing
        (Right _) -> do
            put st { pathOfCurrentDb = mFp }
            pkgs <- searchDB "is:package"
            liftIO
                $  putStrLn
                $  "Indexing finished, nr of packets in database: "
                <> show (length pkgs)
            return Nothing

cmdSetLocalFolder :: FilePath -> AlfM AlfHState (Maybe Return)
cmdSetLocalFolder fp = do
    st <- get
    put $ st { localFilePath = Just fp }
    return Nothing

cmdUnsetLocal :: AlfM AlfHState (Maybe Return)
cmdUnsetLocal = do
    st <- get
    put $ st { localFilePath = Nothing }
    return Nothing

cmdToggleBool :: String -> AlfM AlfHState (Maybe Return)
cmdToggleBool bKey = do
    st@(AlfHState lsa osa ls _ _ plo) <- get
    case bKey of
        "local_search_active" ->
            (put $ st { localSearchActive = not lsa }) >> return Nothing
        "online_search_active" ->
            (put $ st { onlineSearchActive = not osa }) >> return Nothing
        "prefer_local" ->
            (put $ st { preferLocal = not plo }) >> return Nothing
        "local_server" ->
            (put $ st { useLocalServer = not ls }) >> return Nothing
        _ -> throwAlfE $ OtherError $ bKey <> " is not a valid bool"

-----------------
--  searching  --
-----------------

hoogleSearch :: AlfM AlfHState (Maybe Return)
hoogleSearch = Just <$> do
    (AlfHState doLocalSearch doOnlineSearch _ _ _ prefLoc) <- get
    let doBoth = doLocalSearch && doOnlineSearch
    query       <- getQuery
    isSecondRun <- isRerunWithTag query
    setRR       <- setRerunWithTag query

    let localResults  = addIcon "hask_local.png" . itemsToResult <$> searchLocal
    let onlineResults = addIcon "hask_web.png" . itemsToResult <$> searchOnline

    -- | The first run if both are searched just does local results, then sets the argument to rerun the
    -- script, on the second run both are searched and the result is updated in Alfed when they are done
    if
        | isSecondRun -> liftA2 (merge prefLoc) localResults onlineResults
        | doBoth -> setRR <$> localResults
        | doOnlineSearch -> onlineResults
        | doLocalSearch -> localResults
        | otherwise -> return $ itemsToResult $ errorItems
            "Online and offline search is disabled"
  where
    itemsToResult [] = defaultReturn { items = errorItems "No results" }
    itemsToResult is = defaultReturn { items = is }

searchLocal :: AlfM AlfHState [Item]
searchLocal = do
    query                         <- getQuery
    (AlfHState _ _ _ lFp lDbFp _) <- get
    exists                        <- databasePath >>= liftIO . doesFileExist
    nSearches'                    <- envVariableThrow "n_local_search"
    case readMaybe nSearches' of
        Nothing -> throwAlfE $ EnvVarError $ "n_local_search" <> " not valid"
        Just nSearches -> if lFp /= lDbFp || not exists
            then return
                [ defaultItem
                      { title    = "Local database needs updating"
                      , subtitle =
                          Just "select to now, might take a minute or two"
                      , itemVars =
                          M.fromList [("do_cmd", "true"), ("cmd", "update_db")]
                      , arg      = Just " " -- Hacky needs a argument to be executable in alfred
                      }
                ]
            else fmap targetToItem . take nSearches <$> searchDB query

searchDB :: String -> AlfM AlfHState [Target]
searchDB query = databasePath >>= liftIO . flip
    withDatabase
    (return . flip searchDatabase query)

-- | returns a Item with error if unable to connect or reponse code /= 200
searchOnline :: AlfM AlfHState [Item]
searchOnline = do
    query                    <- getQuery
    (AlfHState _ _ ls _ _ _) <- get
    nSearches                <- envVariableThrow "n_web_search"
    lAdd                     <- envVariableThrow "alternate_server_addr"
    let req =
            setRequestQueryString
                    [ ("mode"  , Just "json")
                    , ("start" , Just "1")
                    , ("count" , Just $ C8.pack nSearches)
                    , ("hoogle", Just $ C8.pack query)
                    ]
                $ parseRequest_
                $ if ls then lAdd else "https://hoogle.haskell.org"
    mResp <- liftIO $ try $ httpJSONEither req
    case mResp of
        Left  (_ :: HttpException) -> return $ errorItems "Connection error"
        Right resp                 -> case getResponseStatusCode resp of
            200 -> case getResponseBody resp of
                Left e ->
                    throwAlfE $ OtherError $ "Failed to decode JSON: " <> show e
                Right body -> return $ targetToItem <$> body
            _ -> return $ errorItems "Connection error"

---------------------------------
--  Item and Return functions  --
---------------------------------

-- Merges two Returns by zipping them and eliminating
-- items that have equal title and subtitle
merge :: Bool -> Return -> Return -> Return
merge prioLocal local online = if prioLocal
    then local { items = items' }
    else online { items = items' }
  where
    items' =
        concat $ zipWithTails (: []) (: []) choose (items local) (items online)
    itemsEqual a b = title a == title b && subtitle a == subtitle b
    choose a b | a `itemsEqual` b && prioLocal = [a]
               | a `itemsEqual` b = [b]
               | otherwise = if prioLocal then [a, b] else [b, a]

targetToItem :: Target -> Item
targetToItem Target { targetURL, targetPackage, targetModule, targetType, targetItem, targetDocs }
    = defaultItem
        { title        = targetItem'
        , subtitle     = case targetType of
                             "" -> ("Module: " <>) <$> liftA2
                                 (maybeSeparator ".")
                                 (fst <$> targetPackage)
                                 (fst <$> targetModule)
                             "module" -> ("Package: " <>) . fst <$> targetPackage
                             _ -> Nothing
        , arg          = Just targetURL
        , autocomplete = Just targetName'
        , quicklookurl = Just targetURL
        , text         = Just $ RetText (Just targetName') (Just targetDocs')
        , mods         = defaultMods
                             { shift = packageMod
                             , ctrl  = stackPkgMod
                             , alt   = moduleMod
                             , cmd   = Just
                                           $ defaultMod { modSubtitle = Just targetDocs' }
                             }
        }
  where
    targetItem'    = T.unpack $ innerText $ parseTags $ T.pack targetItem
    targetDocs'    = T.unpack $ innerText $ parseTags $ T.pack targetDocs
    targetName' =
        T.unpack
            $ innerText
            $ takeWhile (not . isTagCloseName "s0")
            $ dropWhile (not . isTagOpenName "s0")
            $ parseTags
            $ T.pack targetItem
    packageMod = case targetType of
        ""        -> bNo $ makeMod pacS False =<< targetPackage
        "module"  -> bNo $ makeMod pacS False =<< targetPackage
        "package" -> makeMod pacS False (targetName', targetURL)
        _         -> Nothing
    stackPkgMod = case targetType of
        ""        -> bNo $ makeMod pacS True . dupUrl =<< targetPackage
        "module"  -> bNo $ makeMod pacS True . dupUrl =<< targetPackage
        "package" -> makeMod pacS True (targetName', stackAddr targetName')
        _         -> Nothing
    moduleMod = case targetType of
        ""        -> bNo $ makeMod modS False =<< targetModule
        "module"  -> makeMod modS False (targetName', targetURL)
        "package" -> bNo Nothing
        _         -> Nothing
    makeMod tType onStack (tName, tUrl) = Just $ defaultMod
        { modArg      = Just tUrl
        , modSubtitle = Just
                            (  "View "
                            <> tType
                            <> " '"
                            <> tName
                            <> "'"
                            <> if onStack then " on stackage" else mempty
                            )
        }
    dupUrl (n, _) = (n, stackAddr n)
    pacS      = "package"
    modS      = "module"
    stackAddr = mappend "https://www.stackage.org/package/"
    -- | Needed to disable defaults in alfred
    bNo       = maybe (Just $ defaultMod { modSubtitle = Just "" }) pure

--------------------------------
--  Item and Return modifers  --
--------------------------------

addIcon :: FilePath -> Return -> Return
addIcon fp r =
    r { items = (\i -> i { icon = Just (Icon False fp) }) <$> items r }

getQuery :: AlfM AlfHState String
getQuery = unwords <$> alfArgs

errorItems :: String -> [Item]
errorItems e = [defaultItem { title = e, valid = False }]

setNoCmd :: Item -> Item
setNoCmd i = i { itemVars = M.insert "do_cmd" "false" (itemVars i)}

-----------------
--  Misc Util  --
-----------------

databasePath :: AlfM AlfHState FilePath
databasePath = (<> "/alfredHoogleDB.hoo") <$> workflowDataDir

bTt :: Bool -> String
bTt True  = "on"
bTt False = "off"

maybeSeparator :: (Foldable t, Semigroup (t a)) => t a -> t a -> t a -> t a
maybeSeparator s ms ms' | not (null ms) && not (null ms') = ms <> s <> ms'
                        | otherwise                       = ms <> ms'

zipWithTails :: (a -> c) -> (b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithTails l r f as bs = catMaybes . takeWhile isJust $ zipWith
    fMaybe
    (extend as)
    (extend bs)
  where
    extend xs = map Just xs ++ repeat Nothing
    fMaybe a b = liftA2 f a b <|> fmap l a <|> fmap r b

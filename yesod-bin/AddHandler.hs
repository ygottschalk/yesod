{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
module AddHandler (addHandler, AddHandlerConfig(..)) where

import Prelude hiding (readFile)
import System.IO  (hFlush, stdout)
import Data.Char  (isLower, toLower, isSpace)
import Data.List  (isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Default.Class (Default(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Control.Monad (unless)

data RouteError = EmptyRoute
                | RouteCaseError
                | RouteExists FilePath
                deriving Eq

instance Show RouteError where
    show EmptyRoute         = "No name entered. Quitting ..."
    show RouteCaseError     = "Name must start with an upper case letter"
    show (RouteExists file) = "File already exists: " ++ file


data AddHandlerConfig = AddHandlerConfig
                      { handlerDir      :: FilePath
                      , applicationFile :: FilePath
                      , routesFile      :: FilePath
                      , testHandlerDir  :: FilePath
                      } deriving (Show, Eq)

instance Default AddHandlerConfig where
        def = AddHandlerConfig { handlerDir      = "Handler/"
                               , applicationFile = "Application.hs"
                               , routesFile      = "config/routes"
                               , testHandlerDir  = "test/Handler/"
                               }

-- strict readFile
readFile :: FilePath -> IO String
readFile = fmap T.unpack . TIO.readFile

cmdLineArgsError :: String
cmdLineArgsError = "You have to specify a route name if you want to add handler with command line arguments."

addHandler :: Maybe String -> Maybe String -> [String] -> AddHandlerConfig -> IO ()
addHandler (Just route) pat met config = do
    _ <- checkConfig config
    cabal <- getCabal
    checked <- checkRoute route config
    let routePair = case checked of
          Left err@EmptyRoute -> (error . show) err
          Left err@RouteCaseError -> (error . show) err
          Left err@(RouteExists _) -> (error . show) err
          Right p -> p

    addHandlerFiles cabal routePair pattern methods config
  where
    pattern = fromMaybe "" pat -- pattern defaults to ""
    methods = unwords met      -- methods default to none

addHandler Nothing (Just _) _ _ = error cmdLineArgsError
addHandler Nothing _ (_:_)    _ = error cmdLineArgsError
addHandler _ _ _ config = addHandlerInteractive config

addHandlerInteractive :: AddHandlerConfig -> IO ()
addHandlerInteractive config = do
    cabal <- getCabal
    let routeInput = do
        putStr "Name of route (without trailing R): "
        hFlush stdout
        name <- getLine
        checked <- checkRoute name config
        case checked of
            Left err@EmptyRoute -> (error . show) err
            Left err@RouteCaseError -> print err >> routeInput
            Left err@(RouteExists _) -> do
              print err
              putStrLn "Try another name or leave blank to exit"
              routeInput
            Right p -> return p

    routePair <- routeInput
    putStr "Enter route pattern (ex: /entry/#EntryId): "
    hFlush stdout
    pattern <- getLine
    putStr "Enter space-separated list of methods (ex: GET POST): "
    hFlush stdout
    methods <- getLine
    addHandlerFiles cabal routePair pattern methods config

addHandlerFiles :: FilePath -> (String, FilePath) -> String -> String
                -> AddHandlerConfig -> IO ()
addHandlerFiles cabal (name, handlerFile) pattern methods AddHandlerConfig{..} = do
    modify applicationFile $ fixApp name
    modify cabal $ fixCabal name
    modify routesFile $ fixRoutes name pattern methods
    writeFile handlerFile $ mkHandler name pattern methods
    specExists <- doesFileExist specFile
    unless specExists $
      writeFile specFile $ mkSpec name pattern methods
  where
    specFile = testHandlerDir ++ name ++ "Spec.hs"
    modify fp f = readFile fp >>= writeFile fp . f

getCabal :: IO FilePath
getCabal = do
    allFiles <- getDirectoryContents "."
    case filter (".cabal" `isSuffixOf`) allFiles of
        [x] -> return x
        [] -> error "No cabal file found"
        _ -> error "Too many cabal files found"

checkRoute :: String -> AddHandlerConfig -> IO (Either RouteError (String, FilePath))
checkRoute name AddHandlerConfig{ handlerDir } =
    case name of
        [] -> return $ Left EmptyRoute
        c:_
            | isLower c -> return $ Left RouteCaseError
            | otherwise -> do
                -- Check that the handler file doesn't already exist
                let handlerFile = concat [handlerDir, name, ".hs"]
                exists <- doesFileExist handlerFile
                if exists
                    then (return . Left . RouteExists) handlerFile
                    else return $ Right (name, handlerFile)

-- This will check the config and return it untouched
-- or crashes with an error message
checkConfig :: AddHandlerConfig -> IO AddHandlerConfig
checkConfig AddHandlerConfig{..} = do
    valididateHandlerDir
    valididateRoutesFile
    valididateApplicationFile
    valididateTestHandlerDir
    return AddHandlerConfig{..}

    where
      (<>) = mappend
      valididateHandlerDir = do
        exists <- doesDirectoryExist handlerDir
        if exists && '/' == last handlerDir
           then return ()
           else error $ "Handler Directory does not exist or is maleformed (missing a trailing / ?) '" <> handlerDir <> "'"
      valididateApplicationFile = do
        exists <- doesFileExist applicationFile
        if exists
           then return ()
           else error $ "Application file does not exist '" <> applicationFile <> "'"
      valididateRoutesFile = do
        exists <- doesFileExist routesFile
        if exists
           then return ()
           else error $ "Routes file does not exist '" <> routesFile <> "'"
      valididateTestHandlerDir = do
        exists <- doesDirectoryExist testHandlerDir
        if exists && '/' == last testHandlerDir
           then return ()
           else error $ "Test Handler Directory does not exist or is maleformed (missing a trailing / ?) '" <> testHandlerDir <> "'"

fixApp :: String -> String -> String
fixApp name =
    unlines . reverse . go . reverse . lines
  where
    l spaces = "import " ++ spaces ++ "Handler." ++ name

    go [] = [l ""]
    go (x:xs)
        | Just y <- stripPrefix "import " x, "Handler." `isPrefixOf` dropWhile (== ' ') y = l (takeWhile (== ' ') y) : x : xs
        | otherwise = x : go xs

fixCabal :: String -> String -> String
fixCabal name orig =
    unlines $ (reverse $ go $ reverse libraryLines) ++ restLines
  where
    origLines = lines orig

    (libraryLines, restLines) = break isExeTestBench origLines

    isExeTestBench x = any
        (\prefix -> prefix `isPrefixOf` x)
        [ "executable"
        , "test-suite"
        , "benchmark"
        ]

    l = "                  Handler." ++ name

    go [] = [l]
    go (x:xs)
        | "Handler." `isPrefixOf` x' = (spaces ++ "Handler." ++ name) : x : xs
        | otherwise = x : go xs
      where
        (spaces, x') = span isSpace x

fixRoutes :: String -> String -> String -> String -> String
fixRoutes name pattern methods fileContents =
    fileContents ++ l
  where
    l = concat
        [ startingCharacter
        , pattern
        , " "
        , name
        , "R "
        , methods
        , "\n"
        ]
    startingCharacter = if "\n" `isSuffixOf` fileContents then "" else "\n"

mkSpec :: String -> String -> String -> String
mkSpec name _ methods = unlines
    $ ("module Handler." ++ name ++ "Spec (spec) where")
    : ""
    : "import TestImport"
    : ""
    : "spec :: Spec"
    : "spec = withApp $ do"
    : concatMap go (words methods)
  where
    go method =
        [ ""
        , "    describe \"" ++ func ++ "\" $ do"
        , "        error \"Spec not implemented: " ++ func ++ "\""
        , ""]
      where
        func = concat [map toLower method, name, "R"]

mkHandler :: String -> String -> String -> String
mkHandler name pattern methods = unlines
    $ ("module Handler." ++ name ++ " where")
    : ""
    : "import Import"
    : concatMap go (words methods)
  where
    go method =
        [ ""
        , concat $ func : " :: " : map toArrow types ++ ["Handler Html"]
        , concat
            [ func
            , " "
            , concatMap toArgument types
            , "= error \"Not yet implemented: "
            , func
            , "\""
            ]
        ]
      where
        func = concat [map toLower method, name, "R"]

    types = getTypes pattern

    toArrow t = concat [t, " -> "]
    toArgument t = concat [uncapitalize t, " "]

    getTypes "" = []
    getTypes ('/':rest) = getTypes rest
    getTypes (c:rest) | c `elem` "#*" =
        typ : getTypes rest'
      where
        (typ, rest') = break (== '/') rest
    getTypes rest = getTypes $ dropWhile (/= '/') rest

uncapitalize :: String -> String
uncapitalize (x:xs) = toLower x : xs
uncapitalize "" = ""

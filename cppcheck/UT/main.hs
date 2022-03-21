{-# LANGUAGE DoAndIfThenElse #-}
import App.Data.Parser
import App.Data.Comment.Parser
import App.Data.Define.Parser
import App.Data.File.Parser
import App.Data.Include.Parser
import App.Data.Spaces.Parser
import App.Data.TheType.Parser
import App.Data.Using.Parser
import App.TestData
import App.XmlReader
import Control.Exception (SomeException, catch)
import Control.Monad (filterM)
import Data.Either (lefts, rights)
import Data.List (sort, sortBy)
import Prelude hiding (catch)
import System.Directory (getDirectoryContents, doesFileExist)
import Text.Format
import Text.Regex.Posix ((=~))

main :: IO ()
main = do
    totalResult <- return True
        <&&> checkAllFiles "comment" comment
        <&&> checkAllFiles "spaces" spaces
        <&&> checkAllFiles "include" include
        <&&> checkAllFiles "define" define
        <&&> checkAllFiles "using" using
        <&&> checkAllFiles "namespace" namespace

        <&&> checkAllFiles "types" theType
        <&&> checkAllFiles "types/arrays" theType
        <&&> checkAllFiles "types/const" theType
        <&&> checkAllFiles "types/refptr" theType
        <&&> checkAllFiles "types/shortlong" theType
        <&&> checkAllFiles "types/signedunsigned" theType

        <&&> checkAllFiles "file" file
    putStrLn "---------"
    if totalResult 
        then putStrLn "All tests passed successfully"
        else putStrLn "Some test(s) failed!"
    where
    (<&&>) :: IO Bool -> IO Bool -> IO Bool
    (<&&>) ioA ioB = do
        a <- ioA
        b <- ioB
        return $ a && b

--------------------------------------------------------------------------------
data FlatFileInfo = FlatFileInfo {
    fiPath::FilePath,
    fiNum::Int,
    fiExpResult::ExpResult }

data ExpResult = ResultFail | ResultSuccess deriving (Eq, Ord)

checkAllFiles :: FilePath -> Parser a -> IO Bool
checkAllFiles path p = do
    a <- checkXmlFiles path
    b <- checkFlatFiles path p
    return (a && b)

checkXmlFiles :: FilePath -> IO Bool
checkXmlFiles i_path = do
    path <- return $ "tests/" ++ i_path
    files <- getFiles path (\file -> file =~ "\\.xml$" :: Bool) >>= return . sort
    result <- flip mapM files $ \file -> do
        fullpath <- return $ path ++ "/" ++ file
        putStr $ format "checking \"{0}\"... " [fullpath]
        flip catch printError $ do
            content <- readFile fullpath
            testData <- return $ xmlToTestData content
            if not . null . lefts $ testData then do
                putStrLn "Failed!!! With following errors:"
                mapM_ (\a -> putStr "* " >> putStrLn a) . lefts $ testData
                return False
            else do
                (strItems, testItems) <- return . unzip . rights $ testData
                checkResults <- flip mapM testItems $ \item -> checkTestItem item
                testResults <- return $ zip3 checkResults testItems strItems
                if False `elem` checkResults then do
                    putStrLn "Failed!!! In following cases:"
                    mapM_ (\(_,_,str) -> putStrLn str) . filter (\(a,_,_) -> not a) $ testResults
                    return False
                else do
                    putStrLn $ format "Done (all {0} case(s))" [show . length $ testItems]
                    return True
    return $ False `notElem` result
    where
    printError :: SomeException -> IO Bool
    printError e = do
        putStrLn $ format "Failed!!! {0}." [show e]
        return False
    checkTestItem :: TestItem -> IO Bool
    checkTestItem (TestItem object input output) = case output of
        Just toExpected -> case tryParse object input of
            Just toResult -> return $ toExpected == toResult
            Nothing -> return False
        Nothing -> case tryParse object input of
            Just _ -> return False
            Nothing -> return True

tryParse :: String -> String -> Maybe TestObject
tryParse parserName str = case parserName of
    "Comment" -> parse comment TO_Comment
    "Define" -> parse define TO_Define
    a -> error $ format "main.tryParse: add rule for '{0}'" [a]
    where
    parse :: Parser a -> (a -> TestObject) -> Maybe TestObject
    parse p ctrTO = case runParser p (str,emptySettings) of
        Left _ -> Nothing
        Right (result,(input,_)) -> if null input then Just $ ctrTO result else Nothing

checkFlatFiles :: FilePath -> Parser a -> IO Bool
checkFlatFiles i_path parser = do
    path <- return $ "tests/" ++ i_path
    files <- getFiles path (\file -> file =~ "[[:digit:]]+\\.(success|fail)$" :: Bool) >>=
        return . map (toFileInfo path) >>=
        return . sortBy cmpFileInfo
    results <- flip mapM files $ \fileInfo -> do
        putStr $ format "checking \"{0}\"... " [fiPath fileInfo]
        content <- readFile $ fiPath fileInfo
        input <- return $ (content,emptySettings)
        case runParser parser input of
            Left err -> if fiExpResult fileInfo == ResultFail 
                then putStrLn "Done" >> return True
                else putStrLn ("Failed!!! " ++ (printErr input err)) >> return False
            Right (_,input) -> if not . null . fst $ input then putStrLn "Failed!!! Not fully processed the file!" >> return False
                else if fiExpResult fileInfo == ResultSuccess 
                    then putStrLn "Done" >> return True
                    else putStrLn "Failed!!! Unexpected result. Expecting failing, but test passed successfully." >> return False
    return $ False `notElem` results
    where
    printErr input err = let
        (ln,col) = errorPlace_LineColumn input err
        reason = errorDescription err
        in format "at (Ln {0}, Col {1}): {2}" [show ln, show col, reason]

getFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
getFiles path fnFilter = do
    files <- getDirectoryContents path >>= 
        return . filter (\file -> file `notElem` [".",".."]) >>=
        filterM (\file -> doesFileExist (path ++ "/" ++ file)) >>=
        return . filter fnFilter
    return files

toFileInfo :: FilePath -> FilePath -> FlatFileInfo
toFileInfo path file = let
    breakPoint = (\ch -> ch == '.')
    num = (\s -> read s :: Int) $ fst $ break breakPoint file
    expectedResult = tail $ snd $ break breakPoint file
    in FlatFileInfo (path ++ "/" ++ file) num (toResult expectedResult)
    where
    toResult "success" = ResultSuccess
    toResult "fail" = ResultFail
    toResult s = error $ format "toFileInfo.toResult: unexpected result '{0}'" [s]

cmpFileInfo :: FlatFileInfo -> FlatFileInfo -> Ordering
cmpFileInfo file1 file2 = if fiNum file1 < fiNum file2 then LT else GT

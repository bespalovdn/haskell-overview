module App.XmlReader (
    xmlToTestData -- | NOTE: May throw an exception!
    ) where

import App.TestData
import Data.Maybe (fromJust, isJust)
import Prelude hiding (catch)
import Text.Format
import Text.XML.Light

type Result a = Either String a

xmlToTestData :: String -> [Result (String, TestItem)]
xmlToTestData str = let
    elems = parseXML str
    in map fromJust . filter isJust . map toTestItem $ elems

toTestItem :: Content -> Maybe (Result (String, TestItem))
toTestItem (Elem elem) = if (qName . elName $ elem) /= "test" then error "toTestItem: root elem is not a 'test'" else let
    tObject = getAttrValue (elAttribs elem) "object"
    resultSuccess = isResultSuccess $ getAttrValue (elAttribs elem) "result"
    tInput = getText $ getElem (elContent elem) "input"
    to = if not resultSuccess then return Nothing :: Result (Maybe TestObject) else
        return . Just =<< (toObject tObject $ getElem (elContent elem) "output")
    in case to of
        Left err -> Just $ Left err
        Right tOutput -> Just $ Right (showElement elem, TestItem tObject tInput tOutput)
toTestItem (Text _) = Nothing
toTestItem a = error $ format "toTestItem: unexpected input parameter: {0}" [show a] 

isResultSuccess :: String -> Bool
isResultSuccess "success" = True
isResultSuccess "fail" = False
isResultSuccess a = error $ format "isResultSuccess: unexpected test result: '{0}'" [a]

toObject :: String -> Element -> Result TestObject
toObject name elem = case readObject name (getText elem) of
    Just to -> Right to
    Nothing -> Left $ 
        format "toObject: unable to read object '{0}' in string: {1}" [name, getText elem]

getAttrValue :: [Attr] -> String -> String
getAttrValue [] key = error $ format "getAttrValue: no such attributes: '{0}'" [key]
getAttrValue (a:as) key = if (qName . attrKey $ a) == key then attrVal a else getAttrValue as key

getText :: Element -> String
getText e = get (elContent e)
    where
    get [] = error "getText: does not contain a text item for elem: '{0}'" [qName $ elName e]
    get ((Text dat):_) = cdData dat
    get (_:xs) = get xs 

getElem :: [Content] -> String -> Element
getElem [] s = error $ format "getElem: no such elems: '{0}'" [s]
getElem (c:cs) s = case c of
    Elem e -> if (qName . elName $ e) == s then e else getElem cs s
    _ -> getElem cs s

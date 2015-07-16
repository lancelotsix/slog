--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (forM_)
import           Hakyll
import           Hakyll.Core.Identifier (Identifier(..), toFilePath)
import           System.FilePath ((</>))
import           Data.Time.Format (TimeLocale(..))
import           Data.List (find)
import           Data.Maybe (Maybe(..), fromMaybe)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    -- All the static resources
    match (    fromGlob "lancelot_six.pdf"
          .||. fromGlob "images/**"
          .||. fromGlob "publications/*"
          ) $ do
            route idRoute
            compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown"
                    ,"recherche.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/slog.html" blogCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/slog.html" postCtx
            >>= relativizeUrls

    create ["archives.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    blogCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/slog.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Home"                  `mappend`
                    blogCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/slog.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler



--------------------------------------------------------------------------------
data License = License { name :: String
                       , logoUrl :: String
                       , refUrl :: String
                       }

licenseList :: [License]
licenseList = [ License "cc-by" "/images/cc-by.png" "https://creativecommons.org/licenses/by/4.0"
              , License "cc-by-sa" "/images/cc-by-sa.png" "https://creativecommons.org/licenses/by-sa/4.0"
              , License "cc-by-nd" "/images/cc-by-nd.png" "https://creativecommons.org/licenses/by-nd/4.0"
              , License "cc-by-nc" "/images/cc-by-nc.png" "https://creativecommons.org/licenses/by-nc/4.0"
              , License "cc-by-nc-sa" "/images/cc-by-nc-sa.png" "https://creativecommons.org/licenses/by-nc-sa/4.0"
              , License "cc-by-nc-nd" "/images/cc-by-nc-nd.png" "https://creativecommons.org/licenses/by-nc-nd/4.0"
              ]
defaultLicense :: License
defaultLicense = head licenseList

findLicense :: Maybe String -> Maybe License
findLicense (Just lname) = case find (\l -> name l == lname) licenseList of
                             Just l -> Just l
                             Nothing -> Nothing
findLicense Nothing = Just defaultLicense
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    field "license-desc-url" (licenseElt refUrl) `mappend`
    field "license-name" (licenseElt name)  `mappend`
    field "license-logo-url" (licenseElt logoUrl)  `mappend`
    dateFieldWith frenchTime "date" "%e %B %Y" `mappend`
    blogCtx
  where licenseElt :: (License -> String) -> Item String -> Compiler String
        licenseElt accessor i =  do
          lname <- getMetadataField (itemIdentifier i) "license"
          case findLicense lname of
                  Nothing -> error $ "Unknown license " ++ fromMaybe "" lname ++ " for item " ++ show (itemIdentifier i)
                  Just l -> return $ accessor l

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

blogCtx :: Context String
blogCtx = constField "blogtitle" "S.Log" `mappend`
          -- constField "blogtitle" "σλog" `mappend`
          defaultContext

--------------------------------------------------------------------------------
frenchTime :: TimeLocale
frenchTime = TimeLocale { wDays = [ ("Lundi", "Lun"), ("Mardi", "Mar"), ("Mercredi", "Mer")
                                  , ("Mercredi", "Mer"), ("Jeudi", "Jeu"), ("Vendredi", "Ven")
                                  , ("Samedi", "Sam"), ("Dimanche", "Dim")]
                        , months = [ ("janvier", "Jan"), ("février", "Fev"), ("mars", "mar")
                                   , ("avril", "avr"), ("mai", "mai"), ("juin", "jun"), ("juillet", "jul")
                                   , ("aout", "aou"), ("septembre", "sept"), ("octobre", "oct")
                                   , ("novembre", "nov"), ("décembre", "dec")]
                        , amPm = ("AM", "PM")
                        , dateTimeFmt = "%A %e %B %Y à %H:%M:%S"
                        , dateFmt = "%m/%d/%y"
                        , timeFmt = "%H:%M:%S"
                        , time12Fmt = "%I:%M:%S %p"
                        , knownTimeZones = []
                        }

config :: Configuration
config = defaultConfiguration { deployCommand = "rsync -rv --delete " ++ destinationDirectory defaultConfiguration </> "."
                                                                      ++ " sitedeploy:/var/www/"
                              }

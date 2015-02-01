--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Monad (forM_)
import           Hakyll
import           System.FilePath ((</>))
import           System.Locale (TimeLocale(..))


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    -- All the static resources
    forM_ ["lancelot_six.pdf"
          ,"publications/*"
          ,"images/*"] $ \pat ->  match pat $ do
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
postCtx :: Context String
postCtx =
    dateFieldWith frenchTime "date" "%e %B %Y" `mappend`
    blogCtx

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
                                  , ("Samedi", "Sam"), ("Dimanche", "Dim")],
                          months = [ ("janvier", "Jan"), ("février", "Fev"), ("mars", "mar")
                                   , ("avril", "avr"), ("mai", "mai"), ("juin", "jun"), ("juillet", "jul")
                                   , ("aout", "aou"), ("septembre", "sept"), ("octobre", "oct")
                                   , ("novembre", "nov"), ("décembre", "dec")],
                          intervals = [ ("an", "ans"), ("moi", "mois"), ("jour", "jours"), ("heure", "heures")
                                      , ("minute", "minutes"), ("seconde", "secondes")
                                      , ("mili seconde", "mili secondes")],
                          amPm = ("AM", "PM"),
                          dateTimeFmt = "%A %e %B %Y à %H:%M:%S",
                          dateFmt = "%m/%d/%y",
                          timeFmt = "%H:%M:%S",
                          time12Fmt = "%I:%M:%S %p"
                        }

config :: Configuration
config = defaultConfiguration { deployCommand = "rsync -rva --delete " ++ destinationDirectory defaultConfiguration </> "."
                                                                       ++ " lancelotsix@home.lancelotsix.com:/home/www/blog.home.lancelotsix.com/"
                              }

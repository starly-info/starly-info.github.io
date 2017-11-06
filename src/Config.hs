module Config where

import Multilang
import Hakyll (FeedConfiguration(..), constField)

proVersion  = [
                  constField "versionW" "426c7fc212"
                , constField "versionD" "865813558f"
              ]
freeVersion = [
                  constField "versionW" "23af82aff2"
                , constField "versionD" "d8c7673dbb"
              ]

feedConfig :: Language -> FeedConfiguration
feedConfig lang = FeedConfiguration
  { feedTitle = "starly"
  , feedDescription = "Learning Sessions Available"
  , feedAuthorName = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname at gmail"
  , feedRoot = "https://starly-info.github.io"
  }

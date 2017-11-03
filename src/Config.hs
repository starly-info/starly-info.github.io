module Config where

import Multilang
import Hakyll (FeedConfiguration(..), constField)

proVersion  = constField "version" "865813558f"
freeVersion = constField "version" "d8c7673dbb"

feedConfig :: Language -> FeedConfiguration
feedConfig lang = FeedConfiguration
  { feedTitle = "starly"
  , feedDescription = "Learning Sessions Available"
  , feedAuthorName = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname at gmail"
  , feedRoot = "https://starly-info.github.io"
  }

module Config where

import Multilang
import Hakyll (FeedConfiguration(..))

feedConfig :: Language -> FeedConfiguration
feedConfig lang = FeedConfiguration
  { feedTitle = "starly"
  , feedDescription = "Learning Sessions Available"
  , feedAuthorName = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname at gmail"
  , feedRoot = "https://starly-info.github.io"
  }

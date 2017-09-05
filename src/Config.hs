module Config where

import Multilang
import Hakyll (FeedConfiguration(..))

feedConfig :: Language -> FeedConfiguration
feedConfig lang = FeedConfiguration
  { feedTitle = "starly"
  , feedDescription = "Starly available newsletters"
  , feedAuthorName = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname on gmail"
  , feedRoot = "https://starly-info.github.io"
  }

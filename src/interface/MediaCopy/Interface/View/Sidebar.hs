module MediaCopy.Interface.View.Sidebar
  ( ContentPage (..)
  , SidebarView (..)
  , sidebarView
  ) where

import Control.Monad (mfilter)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)

import MediaCopy.Domain.Job (JobId)
import MediaCopy.Model (Model (..), selectedEntry)

-- $setup
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> import MediaCopy.Domain.Job (JobId (..))
-- >>> import MediaCopy.Interface.Theme (PaletteMode (..))
-- >>> import MediaCopy.Model (initialModel)
-- >>> :set -Wno-ambiguous-fields
-- >>> let empty = initialModel (UTCTime (fromGregorian 2026 1 1) 0) LightPalette

data ContentPage = EmptyPage | DetailPage
  deriving stock (Eq, Show)

data SidebarView = SidebarView
  { selected :: Maybe JobId
  , page :: ContentPage
  }
  deriving stock (Eq, Show)

-- |
-- >>> sidebarView empty
-- SidebarView {selected = Nothing, page = EmptyPage}
-- >>> sidebarView empty {selected = Just (JobId 7)}
-- SidebarView {selected = Nothing, page = EmptyPage}
sidebarView :: Model -> SidebarView
sidebarView model =
  SidebarView
    { selected = mfilter (\jobId -> Map.member jobId model.jobs) model.selected
    , page = if isJust (selectedEntry model) then DetailPage else EmptyPage
    }

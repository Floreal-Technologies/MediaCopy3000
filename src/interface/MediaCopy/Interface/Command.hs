module MediaCopy.Interface.Command
  ( Command (..)
  , commands
  , commandId
  , commandLabel
  , Section (..)
  , sectionLabel
  , mainMenuLabel
  , palettePlaceholder
  , paletteNoMatch
  ) where

import Data.List (List)
import Data.Text (Text)

import MediaCopy.Interface.Translation
import MediaCopy.Interface.Translation.Messages qualified as Messages

-- $setup
-- >>> import Data.Text qualified as T
-- >>> import MediaCopy.Interface.Translation (SupportedLanguage (..))
-- >>> import MediaCopy.Interface.Translation.Embedded (embeddedWording)

data Command
  = NewOffload
  | VerifyFolder
  | SealMedia
  | SaveReport
  | CancelJob
  | ReviewJob
  | ClearFinished
  | NextJob
  | PreviousJob
  | CommandPalette
  | Preferences
  | Plugins
  | KeyboardShortcuts
  | About
  | CloseWindow
  | Quit
  deriving stock (Eq, Ord, Show, Enum, Bounded)

commands :: List Command
commands = [minBound .. maxBound]

-- |
-- >>> commandId SaveReport
-- "save-report"
commandId :: Command -> Text
commandId = \case
  NewOffload -> "new-offload"
  VerifyFolder -> "verify"
  SealMedia -> "seal"
  SaveReport -> "save-report"
  CancelJob -> "cancel-job"
  ReviewJob -> "review-job"
  ClearFinished -> "clear-finished"
  NextJob -> "next-job"
  PreviousJob -> "previous-job"
  Preferences -> "preferences"
  Plugins -> "plugins"
  KeyboardShortcuts -> "keyboard-shortcuts"
  About -> "about"
  CloseWindow -> "close-window"
  Quit -> "quit"
  CommandPalette -> "command-palette"

-- |
-- >>> commandLabel (embeddedWording English) NewOffload
-- "New Offload\8230"
-- >>> commandLabel (embeddedWording French) NewOffload
-- "Nouveau d\233chargement\8230"
commandLabel :: Wording -> Command -> Text
commandLabel wording command = getTranslation' wording (reference command) []
  where
    reference = \case
      NewOffload -> Messages.commandNewOffload
      VerifyFolder -> Messages.commandVerify
      SealMedia -> Messages.commandSeal
      SaveReport -> Messages.commandSaveReport
      CancelJob -> Messages.commandCancelJob
      ReviewJob -> Messages.commandReviewJob
      ClearFinished -> Messages.commandClearFinished
      NextJob -> Messages.commandNextJob
      PreviousJob -> Messages.commandPreviousJob
      Preferences -> Messages.commandPreferences
      Plugins -> Messages.commandPlugins
      KeyboardShortcuts -> Messages.commandKeyboardShortcuts
      About -> Messages.commandAbout
      CloseWindow -> Messages.commandCloseWindow
      Quit -> Messages.commandQuit
      CommandPalette -> Messages.commandCommandPalette

data Section = JobsSection | NavigationSection | GeneralSection
  deriving stock (Eq, Show, Enum, Bounded)

-- |
-- >>> sectionLabel (embeddedWording French) JobsSection
-- "T\226ches"
sectionLabel :: Wording -> Section -> Text
sectionLabel wording section = getTranslation' wording (reference section) []
  where
    reference = \case
      JobsSection -> Messages.shortcutsSectionJobs
      NavigationSection -> Messages.shortcutsSectionNavigation
      GeneralSection -> Messages.shortcutsSectionGeneral

mainMenuLabel :: Wording -> Text
mainMenuLabel wording = getTranslation' wording Messages.shortcutsMainMenu []

palettePlaceholder :: Wording -> Text
palettePlaceholder wording = getTranslation' wording Messages.palettePlaceholder []

paletteNoMatch :: Wording -> Text
paletteNoMatch wording = getTranslation' wording Messages.paletteNoMatch []

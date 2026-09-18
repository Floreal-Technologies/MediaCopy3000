module Main (main) where

import Test.Tasty

import MediaCopy.Domain.DirectoryHashTest qualified as DirectoryHashTest
import MediaCopy.Domain.JobFormatTest qualified as JobFormatTest
import MediaCopy.Domain.JobTest qualified as JobTest
import MediaCopy.Domain.PlanTest qualified as PlanTest
import MediaCopy.Effects.FileSystemTest qualified as FileSystemTest
import MediaCopy.EngineTest qualified as EngineTest
import MediaCopy.Interface.ThemeTest qualified as ThemeTest
import MediaCopy.Interface.WordingTest qualified as WordingTest
import MediaCopy.ModelTest qualified as ModelTest

main :: IO ()
main =
  defaultMain
    ( testGroup
        "mediacopy3000"
        [DirectoryHashTest.tests, JobFormatTest.tests, JobTest.tests, PlanTest.tests, FileSystemTest.tests, EngineTest.tests, ModelTest.tests, ThemeTest.tests, WordingTest.tests]
    )

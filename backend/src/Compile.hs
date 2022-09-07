module Compile
  ( compile
  ) where

-- import qualified Control.Monad.IO.Class as IO
import qualified System.Process
import qualified System.IO.Temp
import qualified Data.Text.IO
import qualified System.Directory

import Common.Lib.Prelude
import qualified Common.Api.Figure as Figure
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Error as Error

-- TODO: error handling (IO, problem2tex, emacs)
compile
  :: Text -- ^ Problem contents
  -> Compile.RandomSeed -- Seed for randomized variables
  -> [Figure.BareFigure] -- Figures
  -> IO (Either Error.Error Text)
compile contents randomSeed figures = do
  tmpDir <- System.Directory.getTemporaryDirectory
  -- Do all of the work within a temporary directory (deleted after)
  -- e.g., /tmp/prb-e4bd89e5d00acdee
  System.IO.Temp.withTempDirectory tmpDir "prb" $ \prbDir ->
    System.Directory.withCurrentDirectory prbDir $ do
    -- Create files for problem2tex
    -- Problem: main.prb
    -- Figures: schem1.asc, schem2.asc, ..., fig1.svg, fig2.svg, ...
    createFiles

    -- Run problem2tex on files (creates new files)
    -- New problem: main.org
    -- New figures: schem1NEWasc.svg, schem2NEWasc.svg, ..., fig1NEW.svg, fig2NEW.svg, ...
    runProblem2tex

    -- Run emacs on main.org to export HTML
    -- Creates main.html (with embedded SVGs)
    runEmacs

    -- Read and return main.html contents as text
    Right <$> Data.Text.IO.readFile "main.html"
  where
    createFiles = do
      -- Problem: main.prb
      Data.Text.IO.writeFile "main.prb" contents
      -- Figures: schem1.asc, schem2.asc, ..., fig1.svg, fig2.svg, ...
      void $ mapM_
        (\figure -> do
            let fName = cs $ Figure.bfName figure
            let fContents = cs $ Figure.bfContents figure
            Data.Text.IO.writeFile fName fContents
        )
        figures
        
    runProblem2tex = do
      System.Process.callProcess
        "problem2tex"
        [ "-export=main.org"
        , "-random=" <> show randomSeed
        , "main.prb"
        ]

    runEmacs = do
      -- emacs main.org --batch -f org-html-export-to-html --kill
      System.Process.callProcess
        "emacs"
        [ "main.org"
        , "--batch"
        , "-f", "org-html-export-to-html"
        , "--kill"
        ]

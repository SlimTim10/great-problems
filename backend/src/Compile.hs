module Compile
  ( compile
  ) where

-- import qualified Control.Monad.IO.Class as IO
import qualified System.Process
import qualified System.IO.Temp
import qualified Data.Text.IO
import System.FilePath ((</>))

import Common.Lib.Prelude
import qualified Common.Api.Figure as Figure
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Error as Error

-- This should be updated to handle IO errors.
-- Ideally, any IO error should have its message printed in the console and an ambiguous text returned (e.g., Left "Something went wrong").
compile
  :: Text -- ^ Problem contents
  -> Compile.RandomSeed -- Seed for randomized variables
  -> [Figure.BareFigure] -- Figures
  -> IO (Either Error.Error Text)
compile contents randomSeed figures = do
  -- Do all of the work within a temporary directory (deleted after)
  -- e.g., /tmp/prb-e4bd89e5d00acdee
  -- Note: It may be tempting to use System.Directory.withCurrentDirectory, but it is problematic in this multithreaded environment because the current working directory is a global state shared among all threads of the process.
  -- (see: https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory.html#v:setCurrentDirectory)
  -- It is important to only use absolute paths.
  System.IO.Temp.withSystemTempDirectory "prb" $ \prbDir -> do
    -- Create files for problem2tex
    -- Problem: main.prb
    -- Figures: schem1.asc, schem2.asc, ..., fig1.svg, fig2.svg, ...
    createFiles prbDir

    -- Run problem2tex on files (creates new files)
    -- New problem: main.org
    -- New figures: schem1NEWasc.svg, schem2NEWasc.svg, ..., fig1NEW.svg, fig2NEW.svg, ...
    runProblem2tex prbDir

    -- Run emacs on main.org to export HTML
    -- Creates main.html (with embedded SVGs)
    runEmacs prbDir

    -- Read and return main.html contents as text
    Right <$> Data.Text.IO.readFile (prbDir </> "main.html")
  where
    createFiles dir = do
      -- Problem: main.prb
      Data.Text.IO.writeFile (dir </> "main.prb") contents
      -- Figures: schem1.asc, schem2.asc, ..., fig1.svg, fig2.svg, ...
      void . forM_ figures $ \figure -> do
        let fName = dir </> (cs $ Figure.bfName figure)
        let fContents = cs $ Figure.bfContents figure
        Data.Text.IO.writeFile fName fContents
        
    runProblem2tex dir = do
      System.Process.callProcess
        "problem2tex"
        [ "-export=" <> dir </> "main.org"
        , "-random=" <> show randomSeed
        , dir </> "main.prb"
        ]

    runEmacs dir = do
      -- emacs main.org --batch -f org-html-export-to-html --kill
      System.Process.callProcess
        "emacs"
        [ dir </> "main.org"
        , "--batch"
        , "-f", "org-html-export-to-html"
        , "--kill"
        ]

-- | Running actions with explicit input\/output connected to
-- @`stdin`@\/@`stdout`@.
--
-- This module is inspired by the package <http://hackage.haskell.org/package/silently>.

module System.IO.Fake where



import Control.DeepSeq
import Control.Exception
import GHC.IO.Handle
import System.Directory
import System.IO



-- | Perform an action that with access to a temporary file. The file is removed
-- after the action is completed.
withTempFile
    :: FilePath                     -- ^ Path to directory for temporary file
    -> String                       -- ^ Base name for temporary file
    -> ((FilePath,Handle) -> IO a)  -- ^ Action
    -> IO a
withTempFile tmpDir base k = bracket
    (openTempFile tmpDir base)
    (\(file,h) -> hClose h >> removeFile file)
    k

-- | Perform an action with a redirected handle
withRedirect
    :: Handle  -- ^ Shadowing handle
    -> Handle  -- ^ Shadowed handle
    -> IO a    -- ^ Action in which the redirect takes place
    -> IO a
withRedirect new old act = bracket
    (do buffering <- hGetBuffering old
        dupH      <- hDuplicate old
        hDuplicateTo new old
        return (dupH,buffering)
    )
    (\(dupH,buffering) -> do
        hDuplicateTo dupH old
        hSetBuffering old buffering
        hClose dupH
    )
    (\_ -> act)

-- | Perform an action with explicit input\/output connected to
-- @`stdin`@\/@`stdout`@
fakeIO
    :: IO a       -- ^ Action
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
fakeIO act inp = do
    tmpDir <- getTemporaryDirectory
    withTempFile tmpDir "fakeInput" $ \(inpFile,inpH) ->
      withTempFile tmpDir "fakeOutput" $ \(outFile,outH) -> do
        withRedirect outH stdout $
          withRedirect inpH stdin $ do
            hPutStr inpH inp
            hSeek inpH AbsoluteSeek 0
            act
            hFlush stdout
            hSeek outH AbsoluteSeek 0
            str <- hGetContents outH
            str `deepseq` return str


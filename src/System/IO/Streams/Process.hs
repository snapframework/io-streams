-- | A module adapting the functions from "System.Process" to work with
-- @io-streams@.
module System.IO.Streams.Process
  ( module System.Process
  , runInteractiveCommand
  , runInteractiveProcess
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Char8         (ByteString)
import           System.IO                     (hClose)
import           System.Process                (CmdSpec (..), CreateProcess (CreateProcess, close_fds, cmdspec, create_group, cwd, std_err, std_in, std_out), ProcessHandle, StdStream (..), createProcess, getProcessExitCode, interruptProcessGroupOf, proc, rawSystem, readProcess, readProcessWithExitCode, runCommand, shell, showCommandForUser, system, terminateProcess, waitForProcess)
------------------------------------------------------------------------------
import qualified System.IO.Streams.Combinators as Streams
import qualified System.IO.Streams.Handle      as Streams
import           System.IO.Streams.Internal    (InputStream, OutputStream)
import qualified System.IO.Streams.Internal    as Streams

import qualified System.Process                as P


------------------------------------------------------------------------------
-- | Runs a command using the shell, and returns streams that may be used to
-- communicate with the process via its stdin, stdout, and stderr respectively.
--
-- The streams returned by this command are guarded by locks and are therefore
-- safe to use in multithreaded code.
--
-- /Since: 1.0.2.0/
--
runInteractiveCommand :: String
                      -> IO (OutputStream ByteString,
                             InputStream ByteString,
                             InputStream ByteString,
                             ProcessHandle)
runInteractiveCommand scmd = do
    (hin, hout, herr, ph) <- P.runInteractiveCommand scmd
    sIn  <- Streams.handleToOutputStream hin >>=
            Streams.atEndOfOutput (hClose hin) >>=
            Streams.lockingOutputStream
    sOut <- Streams.handleToInputStream hout >>=
            Streams.atEndOfInput (hClose hout) >>=
            Streams.lockingInputStream
    sErr <- Streams.handleToInputStream herr >>=
            Streams.atEndOfInput (hClose herr) >>=
            Streams.lockingInputStream
    return (sIn, sOut, sErr, ph)


------------------------------------------------------------------------------
-- | Runs a raw command, and returns streams that may be used to communicate
-- with the process via its @stdin@, @stdout@ and @stderr@ respectively.
--
-- For example, to start a process and feed a string to its stdin:
--
-- > (inp,out,err,pid) <- runInteractiveProcess "..."
-- > forkIO (Streams.write (Just str) inp)
--
-- The streams returned by this command are guarded by locks and are therefore
-- safe to use in multithreaded code.
--
-- /Since: 1.0.2.0/
--
runInteractiveProcess
    :: FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ Arguments to pass to the executable
    -> Maybe FilePath           -- ^ Optional path to the working directory
    -> Maybe [(String,String)]  -- ^ Optional environment (otherwise inherit)
    -> IO (OutputStream ByteString,
           InputStream ByteString,
           InputStream ByteString,
           ProcessHandle)
runInteractiveProcess cmd args wd env = do
    (hin, hout, herr, ph) <- P.runInteractiveProcess cmd args wd env
    sIn  <- Streams.handleToOutputStream hin >>=
            Streams.atEndOfOutput (hClose hin) >>=
            Streams.lockingOutputStream
    sOut <- Streams.handleToInputStream hout >>=
            Streams.atEndOfInput (hClose hout) >>=
            Streams.lockingInputStream
    sErr <- Streams.handleToInputStream herr >>=
            Streams.atEndOfInput (hClose herr) >>=
            Streams.lockingInputStream
    return (sIn, sOut, sErr, ph)

module Compiler.Fuse where


-- :import           System.Fuse

{-

data FuseOperations fh = FuseOperations
      { -- | Implements 'System.Posix.Files.getSymbolicLinkStatus' operation
        --   (POSIX @lstat(2)@).
        fuseGetFileStat :: FilePath -> IO (Either Errno FileStat),

        -- | Implements 'System.Posix.Files.readSymbolicLink' operation (POSIX
        --   @readlink(2)@).  The returned 'FilePath' might be truncated
        --   depending on caller buffer size.
        fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath),

        -- | Implements 'System.Posix.Files.createDevice' (POSIX @mknod(2)@).
        --   This function will also be called for regular file creation.
        fuseCreateDevice :: FilePath -> EntryType -> FileMode
                         -> DeviceID -> IO Errno,

        -- | Implements 'System.Posix.Directory.createDirectory' (POSIX
        --   @mkdir(2)@).
        fuseCreateDirectory :: FilePath -> FileMode -> IO Errno,

        -- | Implements 'System.Posix.Files.removeLink' (POSIX @unlink(2)@).
        fuseRemoveLink :: FilePath -> IO Errno,

        -- | Implements 'System.Posix.Directory.removeDirectory' (POSIX
        --   @rmdir(2)@).
        fuseRemoveDirectory :: FilePath -> IO Errno,

        -- | Implements 'System.Posix.Files.createSymbolicLink' (POSIX
        --   @symlink(2)@).
        fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno,

        -- | Implements 'System.Posix.Files.rename' (POSIX @rename(2)@).
        fuseRename :: FilePath -> FilePath -> IO Errno,

        -- | Implements 'System.Posix.Files.createLink' (POSIX @link(2)@).
        fuseCreateLink :: FilePath -> FilePath -> IO Errno,

        -- | Implements 'System.Posix.Files.setFileMode' (POSIX @chmod(2)@).
        fuseSetFileMode :: FilePath -> FileMode -> IO Errno,

        -- | Implements 'System.Posix.Files.setOwnerAndGroup' (POSIX
        --   @chown(2)@).
        fuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno,

        -- | Implements 'System.Posix.Files.setFileSize' (POSIX @truncate(2)@).
        fuseSetFileSize :: FilePath -> FileOffset -> IO Errno,

        -- | Implements 'System.Posix.Files.setFileTimes'
        --   (POSIX @utime(2)@).
        fuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno,

        -- | Implements 'System.Posix.Files.openFd' (POSIX @open(2)@).  On
        --   success, returns 'Right' of a filehandle-like value that will be
        --   passed to future file operations; on failure, returns 'Left' of the
        --   appropriate 'Errno'.
        --
        --   No creation, exclusive access or truncating flags will be passed.
        --   This should check that the operation is permitted for the given
        --   flags.
        fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh),

        -- | Implements Unix98 @pread(2)@. It differs from
        --   'System.Posix.Files.fdRead' by the explicit 'FileOffset' argument.
        --   The @fuse.h@ documentation stipulates that this \"should return
        --   exactly the number of bytes requested except on EOF or error,
        --   otherwise the rest of the data will be substituted with zeroes.\"
        fuseRead :: FilePath -> fh -> ByteCount -> FileOffset
                 -> IO (Either Errno B.ByteString),

        -- | Implements Unix98 @pwrite(2)@. It differs
        --   from 'System.Posix.Files.fdWrite' by the explicit 'FileOffset' argument.
        fuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset
                  -> IO (Either Errno ByteCount),

        -- | Implements @statfs(2)@.
        fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats),

        -- | Called when @close(2)@ has been called on an open file.
        --   Note: this does not mean that the file is released.  This function may be
        --   called more than once for each @open(2)@.  The return value is passed on
        --   to the @close(2)@ system call.
        fuseFlush :: FilePath -> fh -> IO Errno,

        -- | Called when an open file has all file descriptors closed and all
        -- memory mappings unmapped.  For every @open@ call there will be
        -- exactly one @release@ call with the same flags.  It is possible to
        -- have a file opened more than once, in which case only the last
        -- release will mean that no more reads or writes will happen on the
        -- file.
        fuseRelease :: FilePath -> fh -> IO (),

        -- | Implements @fsync(2)@.
        fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno,

        -- | Implements @opendir(3)@.  This method should check if the open
        --   operation is permitted for this directory.
        fuseOpenDirectory :: FilePath -> IO Errno,

        -- | Implements @readdir(3)@.  The entire contents of the directory
        --   should be returned as a list of tuples (corresponding to the first
        --   mode of operation documented in @fuse.h@).
        fuseReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)]),

        -- | Implements @closedir(3)@.
        fuseReleaseDirectory :: FilePath -> IO Errno,

        -- | Synchronize the directory's contents; analogous to
        --   'fuseSynchronizeFile'.
        fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno,

        -- | Check file access permissions; this will be called for the
        --   access() system call.  If the @default_permissions@ mount option
        --   is given, this method is not called.  This method is also not
        --   called under Linux kernel versions 2.4.x
        fuseAccess :: FilePath -> Int -> IO Errno, -- FIXME present a nicer type to Haskell

        -- | Initializes the filesystem.  This is called before all other
        --   operations.
        fuseInit :: IO (),

        -- | Called on filesystem exit to allow cleanup.
        fuseDestroy :: IO ()
      }
-}

-- fuseRunInline     use args to set mount point
-- OnOpen
-- OnLoad
fuseSystem :: FuseOperations fh
fuseSystem = FuseOperations
  { fuseGetFileStat          = \_ -> return (Left eNOSYS)
  , fuseReadSymbolicLink     = \_ -> return (Left eNOSYS)
  , fuseCreateDevice         = \_ _ _ _ -> return eNOSYS
  , fuseCreateDirectory      = \_ _ -> return eNOSYS
  , fuseRemoveLink           = \_ -> return eNOSYS
  , fuseRemoveDirectory      = \_ -> return eNOSYS
  , fuseCreateSymbolicLink   = \_ _ -> return eNOSYS
  , fuseRename               = \_ _ -> return eNOSYS
  , fuseCreateLink           = \_ _ -> return eNOSYS
  , fuseSetFileMode          = \_ _ -> return eNOSYS
  , fuseSetOwnerAndGroup     = \_ _ _ -> return eNOSYS
  , fuseSetFileSize          = \_ _ -> return eNOSYS
  , fuseSetFileTimes         = \_ _ _ -> return eNOSYS
  , fuseOpen                 = \_ _ _ -> return (Left eNOSYS)
  , fuseRead                 = \_ _ _ _ -> return (Left eNOSYS)
  , fuseWrite                = \_ _ _ _ -> return (Left eNOSYS)
  , fuseGetFileSystemStats   = \_ -> return (Left eNOSYS)
  , fuseFlush                = \_ _ -> return eOK
  , fuseRelease              = \_ _ -> return ()
  , fuseSynchronizeFile      = \_ _ -> return eNOSYS
  , fuseOpenDirectory        = \_ -> return eNOSYS
  , fuseReadDirectory        = \_ -> return (Left eNOSYS)
  , fuseReleaseDirectory     = \_ -> return eNOSYS
  , fuseSynchronizeDirectory = \_ _ -> return eNOSYS
  , fuseAccess               = \_ _ -> return eNOSYS
  , fuseInit                 = return ()
  , fuseDestroy              = return ()
  }

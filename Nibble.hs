{-# LANGUAGE ScopedTypeVariables #-}

module Nibble where

import qualified Control.Monad.Trans.Reader as R
import qualified System.IO as IO
import qualified System.IO.Temp as T
import qualified Data.Foldable as F
import           Control.Monad.Trans.Class (lift)
import qualified System.Posix.Files as PF
import qualified System.Process as P
import qualified System.Directory as D

newtype N a = N (R.ReaderT Params IO a)

unN :: N a -> R.ReaderT Params IO a
unN (N a) = a

ask :: N Params
ask = N R.ask

instance Functor N where
  fmap f = N . fmap f . unN

instance Monad N where
  return = N . return
  m >>= f = N (unN m >>= (unN . f))

liftIO :: IO a -> N a
liftIO = N . lift


data Params = Params { pHeapDir :: String }

data Heap
data PreHeap

data Directory s a = Directory String deriving Show

data PackageDB
data PackageDBList
data Raw
data Dynamic

data TypeRep = TypeRep String

class Typeable a where
  typeRep :: proxy a -> TypeRep

toDyn :: Typeable a => Directory Heap a -> N (Directory Heap Dynamic)
toDyn a = do
  d <- emptyDirectoryPreHeap
  createHeapLink d "obj" a
  let dir = directoryFilename d
  let TypeRep typerep = typeRep a
  liftIO (IO.writeFile (dir ++ "/TypeRep") typerep)
  stampDirectory (unsafeCoerce d)

fromDyn :: forall a. Typeable a => Directory Heap Dynamic -> N (Maybe (Directory Heap a))
fromDyn a = do
  let dir = directoryFilename a
      ret = unsafeCoerceHeap a
      TypeRep typerep' = typeRep (ret :: Directory Heap a)
  typerep <- liftIO (IO.readFile (dir ++ "/TypeRep"))
  if typerep == typerep'
    then return (Just ret)
    else return Nothing

unsafeCoerce :: Directory PreHeap a -> Directory PreHeap b
unsafeCoerce (Directory d) = Directory d

unsafeCoerceHeap :: Directory Heap a -> Directory Heap b
unsafeCoerceHeap (Directory d) = Directory d

emptyDirectoryPreHeap :: N (Directory PreHeap Raw)
emptyDirectoryPreHeap = do
  heapDir <- fmap pHeapDir ask
  liftIO $ do
    newDir <- T.createTempDirectory heapDir "heapObject"
    return (Directory newDir)
  
-- Could switch Raw to a
createFile :: Directory PreHeap Raw -> String -> String -> N ()
createFile (Directory dir) filename = liftIO . IO.writeFile (dir ++ "/" ++ filename) 

createEmptyPackageDB :: N (Directory PreHeap PackageDB)
createEmptyPackageDB = do
  d <- emptyDirectoryPreHeap
  createFile d "package.conf" "[]"
  return (unsafeCoerce d)

directoryFilename :: Directory s a -> String
directoryFilename (Directory d) = d

createHeapLink :: Directory PreHeap a -> String -> Directory Heap b -> N ()
createHeapLink (Directory dir) linkName target =
  liftIO (PF.createSymbolicLink (directoryFilename target) (dir ++ "/" ++ linkName))

runN :: Params -> N a -> IO a
runN p (N a) = R.runReaderT a p

runTest :: N a -> IO a
runTest = runN Params { pHeapDir = "/home/tom/tmp-nibble-heap" }

storePackageDBList :: [Directory Heap PackageDB] -> N (Directory Heap PackageDBList)
storePackageDBList packagedbs = do
  dir <- emptyDirectoryPreHeap

  F.forM_ (zip [1 :: Int ..] packagedbs) $ \(i, d) ->
      createHeapLink dir (show i) d

  dirStamped <- stampDirectory (unsafeCoerce dir)
  return dirStamped

stampDirectory :: Directory PreHeap a -> N (Directory Heap a)
stampDirectory (Directory d) = return (Directory d)

packageConf :: Directory a PackageDB -> String
packageConf (Directory d) = d ++ "/" ++ "package.conf"

cabalConfigureBuildInstall :: [Directory Heap PackageDB] -> N (Directory Heap PackageDB)
cabalConfigureBuildInstall packagedbs = do
  let packageConfs = concatMap (\packagedb -> " --package-db " ++ packageConf packagedb) packagedbs
      configure = "cabal configure" ++ packageConfs
      build = "cabal build"

  liftIO $ do
    ignoreFIXME $ P.system configure
    ignoreFIXME $ P.system build

  newpackagedb <- createEmptyPackageDB
  newdeployment <- emptyDirectoryPreHeap
  
  let extrapackageConf = " --package-db " ++ packageConf newpackagedb
      prefix = " --prefix " ++ directoryFilename newdeployment
      install = concat ["cabal install", packageConfs, extrapackageConf, prefix]

  ignoreFIXME (liftIO (P.system install))

  mapM_ (\(i, d) -> createHeapLink newpackagedb (show i) d) (zip [1 :: Int ..] packagedbs)

  stampDirectory newpackagedb

ignoreFIXME :: Functor m => m a -> m ()
ignoreFIXME = fmap (const ())

test :: IO ()
test = do
  D.setCurrentDirectory "/home/tom/tmp-cabal/bystander/1.1"
  bystander <- runTest (cabalConfigureBuildInstall [])
  print bystander
  
  D.setCurrentDirectory "/home/tom/tmp-cabal/conman/1.1"
  conman <- runTest (cabalConfigureBuildInstall [])
  print conman

  D.setCurrentDirectory "/home/tom/tmp-cabal/moneyholder/1.1"
  moneyholder <- runTest (cabalConfigureBuildInstall [bystander, conman])
  print moneyholder

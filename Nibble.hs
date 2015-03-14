module Nibble where

import qualified Control.Monad.Trans.Reader as R
import qualified System.IO as IO
import qualified System.IO.Temp as T
import qualified Data.Foldable as F
import           Control.Monad.Trans.Class (lift)
import qualified System.Posix.Files as PF
import qualified System.Process as P
import qualified System.Directory as D

type N = R.ReaderT Params IO

data Params = Params { pHeapDir :: String }

data Object a = HODirectory (Directory a)
              | HOFile (File a)

data Heap
data PreHeap

data Directory a = Directory String deriving Show
data File a      = File String

data PackageDB a = PackageDB (Directory a) deriving Show
data PackageDBList = PackageDBList (Directory Heap) deriving Show


emptyDirectoryPreHeap :: N (Directory PreHeap)
emptyDirectoryPreHeap = do
  heapDir <- fmap pHeapDir R.ask
  lift $ do
    newDir <- T.createTempDirectory heapDir "heapObject"
    return (Directory newDir)
  
createFile :: Directory PreHeap -> String -> String -> N ()
createFile (Directory dir) filename = lift . IO.writeFile (dir ++ "/" ++ filename) 

createEmptyPackageDB :: N (PackageDB PreHeap)
createEmptyPackageDB = do
  d <- emptyDirectoryPreHeap
  createFile d "package.conf" "[]"
  return (PackageDB d)

objectFilename :: Object a -> String
objectFilename (HODirectory d) = directoryFilename d
objectFilename (HOFile (File n)) = n

directoryFilename :: Directory a -> String
directoryFilename (Directory n) = n

createHeapLink :: Directory PreHeap -> String -> Object Heap -> N ()
createHeapLink (Directory dir) linkName object =
  lift (PF.createSymbolicLink (objectFilename object) (dir ++ "/" ++ linkName))

runN :: Params -> N a -> IO a
runN = flip R.runReaderT

runTest :: N a -> IO a
runTest = runN (Params { pHeapDir = "/home/tom/tmp-nibble-heap" })

storePackageDBList :: [PackageDB Heap] -> N PackageDBList
storePackageDBList packagedbs = do
  dir <- emptyDirectoryPreHeap

  F.forM_ (zip [1 :: Int ..] packagedbs) $ \(i, PackageDB d) ->
      createHeapLink dir (show i) (HODirectory d)

  dirStamped <- stampDirectory dir
  return (PackageDBList dirStamped)

stampDirectory :: Directory PreHeap -> N (Directory Heap)
stampDirectory (Directory d) = return (Directory d)

stampPackageDB :: PackageDB PreHeap -> N (PackageDB Heap)
stampPackageDB (PackageDB d) = do
  s <- stampDirectory d
  return (PackageDB s)

packageConf :: PackageDB a -> String
packageConf (PackageDB (Directory d)) = d ++ "/" ++ "package.conf"

cabalConfigureBuildInstall :: [PackageDB Heap] -> N (PackageDB Heap)
cabalConfigureBuildInstall packagedbs = do
  let packageConfs = concatMap (\packagedb -> " --package-db " ++ packageConf packagedb) packagedbs
      configure = "cabal configure" ++ packageConfs
      build = "cabal build"

  lift $ do
    ignoreFIXME $ P.system configure
    ignoreFIXME $ P.system build

  newpackagedb@(PackageDB newpackagedbdir) <- createEmptyPackageDB
  newdeployment <- emptyDirectoryPreHeap
  
  let extrapackageConf = " --package-db " ++ packageConf newpackagedb
      prefix = " --prefix " ++ directoryFilename newdeployment
      install = concat ["cabal install", packageConfs, extrapackageConf, prefix]

  ignoreFIXME (lift (P.system install))

  mapM_ (\(i, PackageDB d) -> createHeapLink newpackagedbdir (show i) (HODirectory d)) (zip [1 :: Int ..] packagedbs)

  stampPackageDB newpackagedb

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

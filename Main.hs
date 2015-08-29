{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.List as L
import qualified System.Directory as D
import Control.Monad.Trans.Resource
import Data.Conduit.Text as CT
import qualified Data.Text as T
import Control.Monad


glob = "/home/blazej/Programowanie/Haskell/Projects/ConuitLab/ConduitP"


            
hidden = not . L.isPrefixOf "."
      
      
files :: (FilePath -> Bool) -> FilePath -> IO [FilePath]      
files excludeDir dir =  
    do filesL <- D.getDirectoryContents dir
       ls <- forM (filter excludeDir filesL) $ \ oneFile -> do 
            let path = dir ++ "/" ++oneFile
            isDir <- D.doesDirectoryExist $ path
            if isDir then
               files excludeDir $ path
            else
               return [path]
       return $ concat ls      

  
            


                 
fum = do lf <- files hidden glob 
         let filtered = filter (L.isSuffixOf "hs") lf
         ln <- countLineInFiles filtered
         mapM_ print ln
         print " -- "
         let total = L.foldl' (\acc (fn,  x) -> x+acc ) 0 ln
         print total
                 

countLineInFiles :: [FilePath] -> IO ([(FilePath, Int)])                 
countLineInFiles files = traverse ftoIO files
    where ftoIO fn = ((fn,))<$>(lineCounter $ fn)
    
    
    
    
source  :: MonadResource m => FilePath ->  Source m BS.ByteString                 
source file = CB.sourceFile $ file                 



filterC :: MonadResource m => (T.Text -> Bool) -> Conduit T.Text m T.Text
filterC cond = do mLine <- await
                  case mLine of
                    Just line -> filtered line >> (filterC cond)                                 
                    Nothing -> return ()
               where filtered line =  
                          if (cond line) then yield line
                             else return ()
                    
                    

countLines :: MonadResource m => Conduit T.Text m Int
countLines = addLines 0
    where addLines n = 
              do x <- await
                 case x of 
                   Just k -> addLines $ n + 1
                   Nothing -> yield n
 
 
-- TODO return either 
 
sink :: Sink Int (ResourceT IO) Int
sink = do x <- await
          case x of
            Just k -> return k
            _ -> return 0


lineCounter :: FilePath -> IO Int
lineCounter file = runResourceT $ source (file)
                      $= CT.decodeUtf8 
                      =$= CT.lines 
                      -- =$= (filterC $ not . (T.all (==' ')))
                      =$= countLines
                      $$ sink 

main = lineCounter "Main.hs"
    
                   
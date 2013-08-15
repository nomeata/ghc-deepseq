{-|
Module      :  GHC.DeepSeq
Copyright   :  (c) 2013 Joachim Breitner
License     :  BSD3
Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>

A generic version of 'deepseq' that investigates the heap structure.
-}


module GHC.DeepSeq where

import GHC.HeapView
import Control.Monad
import System.IO.Unsafe

evalBox :: Box -> IO ()
evalBox (Box a) = a `seq` return ()

needsEval :: Closure -> IO Bool
needsEval c = do
    case c of
        ThunkClosure {}    -> return True 
        APClosure {}       -> return True
        IndClosure {}      -> getBoxedClosureData (indirectee c) >>= needsEval
        SelectorClosure {} -> return True
        _                  -> return False

isConstructor :: Closure -> Bool
isConstructor (ConsClosure {}) = True
isConstructor _ = False


deepEval :: Box -> IO ()
deepEval b = do
    c <- getBoxedClosureData b
    evalNeeded <- needsEval c
    when evalNeeded $ evalBox b
    when (isConstructor c) $ do
        c' <- getBoxedClosureData b
        mapM_ deepEval (allPtrs c')

deepseq :: a -> b -> b
deepseq v x = unsafePerformIO $ do
    deepEval (asBox v)
    return x
    

module System.CNN.Network where

import Control.Monad(join)
import Control.Monad.State.Lazy
import Data.Foldable(foldl')
import Numeric.LinearAlgebra(cmap,add)
import qualified Numeric.LinearAlgebra as HM
import Numeric.LinearAlgebra.Data(Matrix,R)


import System.CNN.Layer(convolute,pool,export,foldingUnit,map2)

filterDefault :: (Matrix R,Int)
filterDefault = undefined

type Filter = (Matrix R, Int)

--定数項
decrease :: R -> R
decrease x = x - 0.1


activate :: R -> R
activate x
  | x < 0 = 0
  | otherwise = x
  --ReLU関数。シグモイドでも良いかもしれない

poolSize :: Int
poolSize = 3

--CNNの状態 プーリング層は状態を保持しないとする。
data StateCNN =
  StateCNN{
   conv1 :: Filter,
   bConv1 :: R, --定数項
   conv2 :: Filter,
   bConv2 :: R,
   expo1 :: ([Matrix R]),
   bExpo1 :: R
          }

type Layer = State StateCNN (Matrix R)
-- StateCNN -> (StateCNN,Matrix R)

--stateConv1 :: Matrix R -> Layer
--stateConv1 x = state $ \s -> (convolute (conv1 s) x,s)

--statePool :: Matrix R -> Layer
--statePool x = state $ \s -> (pool x poolSize,s)

runCNN :: StateCNN -> [Matrix R]
       -> ([Matrix R],[Matrix R],
            [Matrix R],[Matrix R],[R])
runCNN s x = afterExpo1  $ afterPool2 $
              afterConv2  $ afterPool1 afterConv1
  -- $
  --allActivate (id)
  -- . pool 5
  where
    --流石にもうちょっと修正できるはず
    afterConv1 :: [Matrix R]
    afterConv1 = allActivate (+ (bConv1 s))
      . convolute (conv1 s)
      <$> x

    afterPool1 :: [Matrix R] -> ([Matrix R],[Matrix R])
    afterPool1 ac1 = (ac1,
                      allActivate (id)
                      . pool 5 <$> ac1)

    afterConv2 :: ([Matrix R],[Matrix R])
               -> ([Matrix R],[Matrix R],[Matrix R])
    afterConv2 (ac1,ap1) = (ac1,ap1,
                            allActivate (+ (bConv2 s))
                           . convolute (conv2 s) <$> ap1)

    afterPool2 :: ([Matrix R],[Matrix R],[Matrix R])
               -> ([Matrix R],[Matrix R],
                   [Matrix R],[Matrix R])
    afterPool2 (ac1,ap1,ac2) = (ac1,ap1,ac2,
                      allActivate (id)
                      . pool 5 <$> ac2)

    afterExpo1 :: ([Matrix R],[Matrix R],
                   [Matrix R],[Matrix R])
                  -> ([Matrix R],[Matrix R],
                   [Matrix R],[Matrix R],[R])
    afterExpo1 (ac1,ap1,ac2,ap2) = (ac1,ap1,ac2,ap2,
                                    export (expo1 s) ap2)

    allActivate :: (R -> R) -> Matrix R -> Matrix R
    allActivate d = (cmap $ activate . d)


type Learning = ([Matrix R],[Matrix R],
                   [Matrix R],[Matrix R],[R])

minusMatrix :: Matrix R -> Matrix R -> Matrix R
minusMatrix a b = (a `HM.add`) $ HM.cmap (\x -> -x) b

mapMinusMatrix :: [Matrix R] -> [Matrix R] -> [Matrix R]
mapMinusMatrix as bs = map (\(a,b) -> minusMatrix a b) $
  zip as bs


reshapeCNN :: StateCNN
           -> ([Matrix R],[R])
           -> (StateCNN,[R])
reshapeCNN s (sample,answer) =
  let data0 = trialCNN in
    let newExpo1 = reshapingExport
                        (loss data0) (expo1 s)
                        ((\(_,_,_,a,_) -> a) $ data0 )in
      let newConv2 = reshapingConv
                      ((mapMinusMatrix newExpo1) $

                      (\(_,_,_,a,_)-> a) $ data0) (conv2 s)
                      ((\(_,a,_,_,_) -> a) $ data0)
                      ((\(_,_,a,_,_) -> a) $ data0)  in

        let newConv1 = reshapingConv
                      ((mapMinusMatrix newConv2) $

                      (\(_,_,_,a,_)-> a) $ data0) (conv1 s)
                      ((\(_,a,_,_,_) -> a) $ data0)
                      ((\(_,_,a,_,_) -> a) $ data0)  in
          (StateCNN newConv1 (bConv1 s) newConv2 (bConv2 s)
           newExpo1 (bExpo1 s), -- no learning teisuukou
          loss data0)
  where
    learnCo :: R
    learnCo = 0.01 --学習係数

    trialCNN :: Learning
    trialCNN = runCNN s sample

    --誤差を出力
    loss :: Learning -> [R]
    loss trial0 = case trial0 of
      (_,_,_,_,a0) -> (\(a,b) -> b - a) <$> zip a0 answer

    --aas is pastIm0

    reshapingExport :: [R]
                    -> [Matrix R]
                    -> [Matrix R]
                    -> [Matrix R]
    reshapingExport loss0 import0 pastIm0 =
      map (\(a,b,c) -> resharpingExportUnit a b c) $
      zip3 loss0 import0 pastIm0

    resharpingExportUnit :: R
                         -> Matrix R
                         -> Matrix R
                         -> Matrix R
    resharpingExportUnit dy aas input0 =
      cmap ( \x -> x
      - (learnCo * dy *
          (
            sum $
            map (1 /)
            $ join
            $ HM.toLists aas
          )
        )) input0


    reshapingConv :: [Matrix R]
                  -> Filter
                  -> [Matrix R]
                  -> [Matrix R]
                  -> Filter
    reshapingConv dys filter00 pastIn0 input0 =
      foldl' reshapingConvUnit filter00
      (zip3 dys pastIn0 input0)






    reshapingConvUnit :: Filter
                      -> (Matrix R
                         , Matrix R
                         , Matrix R)
                      -> Filter
    reshapingConvUnit (filt0,size0) (dys, pastIn0, input0) =
      let aas0 = HM.toBlocksEvery size0 size0 pastIn0 in
      (
      (input0 `HM.add`) $  HM.fromLists
      $ ((\x -> -x) `map2` ) $ foldingUnit filt0 `map2` aas0
      ,
      size0
      )

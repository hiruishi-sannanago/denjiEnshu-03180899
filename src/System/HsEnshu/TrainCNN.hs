module System.HsEnshu.TrainCNN where

import Control.Applicative((<*>))
import Control.Monad(join,forM)
import Data.List(repeat)
import Numeric.LinearAlgebra.Data(Matrix,R,Vector,fromLists,toLists)
import qualified Numeric.LinearAlgebra.Data as HM


import Data.Making(TrainData(..),TrainDatasBase(),
                    makeChargeField,Charge,map3,meshSizeOfSide)
import System.CNN.Network(runCNN,reshapeCNN,StateCNN(..),Filter)

data StandardTD =
    StandardTD {
    dataSetTD :: [TrainDatasBase],
    chargeTD :: Charge
    }

-- type TrainDatasBase = [Matrix R] --３成分に分解してリストに

standarizeTrainData :: TrainData -> StandardTD
standarizeTrainData td = StandardTD makeDataList getCharge

    where
        makeDataList :: [TrainDatasBase]
        makeDataList = standarizeDataList dataList

        standarizeDataList :: [TrainDatasBase] -> [TrainDatasBase]
        standarizeDataList = map $ (map fromLists) .
            (map3 $ standarize getMin getMax) .
            (map toLists)

        dataList :: [TrainDatasBase]
        dataList = [dataUp,dataForward,dataRight,dataLeft
                            ,dataBack,dataDown]
                        <*> return td

        getMax = dataMax td

        getMin = dataMin td

        getCharge = dataCharge td

        standarize :: R -> R -> R -> R
        standarize min0 max0 now = (now - min0) / (max0 - min0)

compress :: StandardTD -> ([Matrix R],[R])
compress td0 = (compressDataSet $ dataSetTD td0,
                compressCharge $ chargeTD td0)
  where
    compressDataSet :: [TrainDatasBase] -> [Matrix R]
    compressDataSet = join

    compressCharge :: Charge -> [R]
    compressCharge xxs = join $
      (\(x,y) ->  x : (HM.toList y))
      `map` xxs

-- default settings
-- the list num of expo1 must be 8
-- q x y z q x y z
-- 11 * 11 gyouretsu no hazu
defaultStateCNN :: StateCNN
defaultStateCNN = StateCNN
  defaultConv
  0.1
  defaultConv
  0.1
  defaultExpo1
  0.1
  where
    defaultConv :: Filter
    defaultConv = ((5 HM.>< 5) [0.9,1.0,0.9,0.8,0.9,0.2,0.3,0.5,0.6,0.1,0.2,0.2,0.1,0.9,0.9,0.1,0.9,0,0.1,0.9,0.9,0.3,0.2,0.2,0.3],5)

    defaultExpo1 :: [Matrix R]
    defaultExpo1 = Prelude.take 8 $ repeat $ (meshSizeOfSide + 1) HM.>< (meshSizeOfSide + 1) $ repeat 0.5

learning :: IO ()
learning = do
  charge0 <- forM [1::Int .. 1000] $ \_ -> makeChargeField
  datas0 <- return $ compress . standarizeTrainData <$> charge0
  (forTrain,forTest) <- return $ splitAt 900 datas0
  st0 <- reshaping 0 defaultStateCNN forTrain
  result0 <- return $ runCNN st0 <$> fst <$> forTest
  putStrLn "(expect,really) ="
  print $ zip result0 $ snd <$> forTest
  putStrLn "= (expect,really)"
  where
    reshaping :: Int -> StateCNN
              -> [([Matrix R],[R])]
              -> IO StateCNN
    reshaping i s xxs = case xxs of
      x : xs
       | i `mod` 100 == 0 -> do
           (state0,answer0) <- return $ reshapeCNN s x
           putStrLn "printing the loss ..."
           print answer0
           reshaping (i+1) state0 xs
       | otherwise -> do
           (state0,_) <- return $ reshapeCNN s x
           reshaping (i+1) state0 xs
      [] -> return s

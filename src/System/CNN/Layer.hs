module System.CNN.Layer where

import Control.Applicative((<$>),pure)
import Data.DList(DList(..),toList)
import Data.Monoid((<>))
import Data.Foldable(foldl')
import Numeric.LinearAlgebra()
import qualified Numeric.LinearAlgebra as HM
import Numeric.LinearAlgebra.Data(Matrix,R,Vector,(??)
                                 ,maxElement)
-- Extractor は　行ー＞列の順番で指定
import Data.Semigroup()

--filter :: (Matrix R,Int)
--filter = undefined
 --畳み込み層のフィルターとその大きさ


extract :: Matrix R -> (Int,Int) -> Matrix R
extract a (minRange,maxRange) =
  a HM.?? (HM.Drop (minRange - 1)
          , HM.Drop (minRange - 1))
  ?? (HM.Take (maxRange - minRange + 1)
     , HM.Take (maxRange - minRange + 1))

actEach :: Int
        -> ((Int,Int) -> Matrix R -> R)
        -> Matrix R
        -> Matrix R
actEach filterSize f a = go (0,0) allRange 0 mempty
  where
    allRange :: (Int,Int)
    allRange = HM.size a

    go :: (Int,Int)
       -> (Int,Int)
       -> Int
       -> DList R
       -> Matrix R
    go (x,y) (goalX ,goalY ) col0 accum0
        | x > goalX
      = HM.matrix col0 $ Data.DList.toList accum0
        | x <= goalX && y <= goalY
      = go (x,y + filterSize) (goalX,goalY)
        col0 (accum0 <> (pure $ f (x,y) a))
        | x <= goalX && y > goalY
      = go (x + filterSize,0) (goalX,goalY)
        (col0 + 1) (accum0 <> (pure $ f (x,y) a))
        | otherwise
      = undefined


map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map $ map f

 --畳み込み層
convolute :: (Matrix R,Int)
          -> Matrix R
          -> Matrix R
convolute (filt,filtSize) x =
  HM.fromLists $ (foldingUnit filt) `map2`
  (HM.toBlocksEvery
  blockSize blockSize x)
    where
        blockSize :: Int
        blockSize = (itsSize `div` filtSize)

        itsSize :: Int
        itsSize = fst $ HM.size x

foldingUnit :: Matrix R
            -> Matrix R
            -> R
foldingUnit filter0 input0 =
          foldl' (+) 0 (
          zipNorm <$>
            zip (HM.toRows input0)
            (HM.toRows filter0)
          )
 where
        zipNorm :: (Vector R,Vector R) -> R
        zipNorm = \(x0,y0) -> x0 HM.<.> y0



pool :: Int -> Matrix R -> Matrix R
pool filtSize imput = HM.fromLists $ pooling `map2`
                      (HM.toBlocksEvery
                      blockSize blockSize imput)
  where
    blockSize :: Int
    blockSize = (itsSize `div` filtSize)

    itsSize :: Int
    itsSize = fst $ HM.size imput

    pooling :: Matrix R -> R
    pooling = maxElement



 --出力、全結合層
 --出力の数を引数に取る。
  --はずだったがとらなくてもリストの要素数でわかるかも。
 --今回なら（電荷と座標*3）＊２で8
export :: ([Matrix R]) -> [Matrix R] -> [R]
export (filt) xxs = summing <$> filtering

  where

    summing :: Matrix R -> R
    summing = (sum) . (map sum) . HM.toLists

    filtering :: [Matrix R]
    filtering = filterUnit xxs <$> filt

    filterUnit :: [Matrix R] -> Matrix R -> Matrix R
    filterUnit xxs' fUnit = sum $ (fUnit <>) <$> xxs'

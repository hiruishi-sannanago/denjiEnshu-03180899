module Data.Making where
import Control.Monad(join)
import Data.List(foldl')
import Foreign.Storable(Storable)
import Numeric.LinearAlgebra(scale,(<.>))
import qualified Numeric.LinearAlgebra as HM
import Numeric.LinearAlgebra.Data(Matrix,R,Vector,fromLists)
import qualified Numeric.LinearAlgebra.Data as HM
import Numeric.LinearAlgebra((<.>))
import System.Random.MWC

-- 課題１：初期値を設定
numOfCharges :: Int --電子の個数
numOfCharges = 2

lengthOfSide :: R --立方体の一片の長さ(m)
lengthOfSide = 0.1

maxCoulomb :: R --電荷の最大値
maxCoulomb = 10

meshSizeOfSide :: Int --一辺当たり何メッシュか
meshSizeOfSide = 10
-- maxCoulomb = undefined

pi' :: R -- 円周率
pi' = pi

epsilon :: R --真空の誘電率
epsilon = 8.85418782 *  ( 1.0e-12 )

data TrainData =
  TrainData {
  dataUp :: TrainDatasBase ,
  dataForward :: TrainDatasBase ,
  dataRight :: TrainDatasBase ,
  dataLeft :: TrainDatasBase ,
  dataBack :: TrainDatasBase ,
  dataDown :: TrainDatasBase ,
  dataCharge :: Charge,
  dataMin :: R ,
  dataMax :: R
            }

type Charge = [(R,Vector R)]

type TrainDatasBase = [Matrix R] --３成分に分解してリストに

-- 電場分布と電荷条件をファイルに出力
makeChargeField :: IO TrainData
makeChargeField = do
  charge <- setCharge
  let up0 = convert $ getSurfaceField Up charge
  let forward0 = convert $ getSurfaceField Forward charge
  let right0 = convert $ getSurfaceField Right' charge
  let left0 = convert $ getSurfaceField Left' charge
  let back0 = convert $ getSurfaceField Back charge
  let down0 = convert $ getSurfaceField Down charge
  let min0 = findMinOrMax minimum HM.minElement
             up0 forward0 right0 left0 back0 down0
  let max0 = findMinOrMax maximum HM.maxElement
             up0 forward0 right0 left0 back0 down0
  return $
    TrainData up0 forward0 right0 left0 back0 down0 charge min0 max0

  where
    findMinOrMax :: ([R] -> R)
                 -> (Matrix R -> R)
                 -> TrainDatasBase
                 -> TrainDatasBase
                 -> TrainDatasBase
                 -> TrainDatasBase
                 -> TrainDatasBase
                 -> TrainDatasBase
                 -> R
    findMinOrMax f1 f2 u f r l b d
      = f1 $ map (f2) $ join [u,f,r,l,b,d]

    convert :: [[Vector R]] -> TrainDatasBase
    convert fs =
      [
        (HM.fromLists $ map2 (ex HM.<.>) fs),
        (HM.fromLists $ map2 (ey HM.<.>) fs),
        (HM.fromLists $ map2 (ez HM.<.>) fs)
      ]


ex :: Vector R
ex = HM.vector [1,0,0]

ey :: Vector R
ey = HM.vector [0,1,0]

ez :: Vector R
ez = HM.vector [0,1,0]

coordinateFromInt :: Int -> R
coordinateFromInt x = lengthOfSide *
         toEnum x /
         toEnum meshSizeOfSide


--実数と実ベクトルの組を電荷の個数分生成
--mwc-randomで乱数を噛ませるのでIOがつきます。
--単体での　$　は文末までのカッコ（関数適用演算子）
setCharge :: IO [(R,Vector R)]
setCharge = (take numOfCharges) <$> makeChargeList
  where

    --無限リストを以下で作成し、
    --numOfChargesの数だけsetChargeで引っ張ってくる。
    --電荷は0~maxCoulombの範囲
    --座標は両端を除いてメッシュの辺上
    makeChargeList :: IO [(R,Vector R)]
    makeChargeList = (return repeat) <*>
      do
        q <- random 0 maxCoulomb :: IO Double
        x3 <- (return $ take 3 . repeat) <*> coordinate
        return (q, HM.vector x3)

    --乱数
    random :: Variate a => a -> a -> IO a
    random min0 max0 = withSystemRandom . asGenIO $
      \gen -> do
        uniformR (min0, max0) gen

    --乱数を整数値で出した後、座標に変換
    coordinate :: IO R
    coordinate = do
       x0 <- random 0 meshSizeOfSide :: IO Int
       --メッシュの両端はメッシュ数+1
       return $ coordinateFromInt x0

data Field =
  Field {
   vectorOfField :: Vector R ,
   positionOfField :: Vector R
        }

type Position = Vector R
-- 位置ベクトルとその他のベクトルを区別

appendField :: Field -> Vector R -> Field
appendField x e =
  Field
   (vectorOfField x + e)
    $ positionOfField x

data Surface
  = Up |
    Forward |
    Right' |
    Left' |
    Back |
    Down deriving Eq
-- 直和型で六面を表現

--一辺当たりのメッシュの数と電荷の情報をもとに
--電場を生成してリストのリストに（もっとうまいやり方ありそう）
getSurfaceField :: Surface
                -> [(R,Vector R)]
                -> [[Vector R]]
getSurfaceField sur qs
  | sur == Up = map2 fieldFromCoordinate $ intCoordinate (1,0,0) (0,1,0) (0,0,0)
  | sur == Forward = map2 fieldFromCoordinate $ intCoordinate (0,1,0) (0,0,1) (0,0,0)
  | sur == Right' = map2 fieldFromCoordinate $ intCoordinate (1,0,0) (0,0,1) (0,fieldSize,0)
  | sur == Left' = map2 fieldFromCoordinate $ intCoordinate (1,0,0) (0,0,1) (0,0,0)
  | sur == Back = map2 fieldFromCoordinate $ intCoordinate (0,1,0) (0,0,1) (fieldSize,0,0)
  | sur == Down = map2 fieldFromCoordinate $ intCoordinate (1,0,0) (0,1,0) (0,0,fieldSize)
  where
    intCoordinate :: (Int,Int,Int)
                  -> (Int,Int,Int)
                  -> (Int,Int,Int)
                  -> [[(Int,Int,Int)]]
    intCoordinate (vx,vy,vz) (vx2,vy2,vz2) start =
      take fieldSize $ iterate (map (
                                     \(a,b,c) ->
                                       (a+vx2,b+vy2,c+vz2)
                                      ))
      $ take fieldSize $ iterate (\(a,b,c) -> (a+vx,b+vy,c+vz) ) start


    fieldFromCoordinate :: (Int,Int,Int)
                        -> Vector R
    fieldFromCoordinate (x,y,z) = fieldHere $
      HM.vector [coordinateFromInt x,
                coordinateFromInt y,
                coordinateFromInt z]


    fieldHere :: Position
              -> Vector R
    fieldHere x = vectorOfField $
      foldl' oneField (Field zeroVector x) qs

    --今の電場に新しい電荷q0による影響を加算する
    oneField :: Field
             -> (R,Position)
             -> Field
    oneField pre0 (q0, x0) = appendField pre0 $ field q0

        (x0 - vectorOfField pre0)


    vectorSize :: Vector R -> R
    vectorSize v = sqrt (v <.> v)

    field :: R -> Position -> Vector R
    field q r =
      (q / ( 4 * pi' * epsilon *  vectorSize  (r ^ 3) ))
      `HM.scale`
      r

    zeroVector :: Vector R
    zeroVector = HM.vector [0,0,0]

    fieldSize :: Int
    fieldSize = meshSizeOfSide + 1


map3 :: (a -> b) -> [[[a]]] -> [[[b]]]
map3 f =  map $ map (map f)

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map $ map f

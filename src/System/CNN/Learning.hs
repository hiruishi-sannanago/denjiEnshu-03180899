module System.CNN.Learning where

import Control.Applicative((<$>),pure,empty)
import Data.DList(DList(..),fromList,toList)
import Data.Monoid((<>))
import Numeric.LinearAlgebra(scale,(<.>))
import qualified Numeric.LinearAlgebra as HM
import Numeric.LinearAlgebra.Data(Matrix,R,Vector,diagl,(??)
                                 ,Extractor(..),size,maxElement,
                                 toRows,matrix)
import Numeric.LinearAlgebra hiding ((<>))
import Numeric.LinearAlgebra.Data hiding ((<>))
-- Extractor は　行ー＞列の順番で指定
import qualified Numeric.LinearAlgebra.Data as HM
import Data.Semigroup()

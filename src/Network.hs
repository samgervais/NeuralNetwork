module Network where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data
import Data.Maybe (isJust, fromJust)

data Network = Network { numLayers :: Int
                       , sizes :: [Int]
                       , biases :: Matrix Double
                       , weights :: Matrix Double
                       } deriving (Show)

data TrainingSet = TrainingSet { input :: Matrix Double
                               , output :: Matrix Double
                               } deriving (Show, Eq)

sigmoid :: Floating a => a -> a
sigmoid z = 1/(1 + exp(-1*z))

sigmoid' :: Floating a => a -> a
sigmoid' z = sigmoid(z)*(1-sigmoid(z))

feedForward :: Network -> Matrix Double -> Matrix Double
feedForward net a = reshape (last $ sizes net) vectorOut
  where vectorOut = foldl (\a (b,w) -> sigmoid $ w * a + b) (flatten a) $ zip (toColumns $ biases net) (toColumns $ weights net)

-- sgd :: Network -> [TrainingSet] -> Int -> Int -> Double -> (Maybe [TrainingSet]) -> IO Network
sgd net trainingData epochs miniBatchSize eta testData = 1
  where ntest = if isJust testData then fromJust $ length <$> testData else 0

-- def SGD(self, training_data, epochs, mini_batch_size, eta,
--             test_data=None):
--         if test_data: n_test = len(test_data)
--         n = len(training_data)
--         for j in xrange(epochs):
--             random.shuffle(training_data)
--             mini_batches = [
--                 training_data[k:k+mini_batch_size]
--                 for k in xrange(0, n, mini_batch_size)]
--             for mini_batch in mini_batches:
--                 self.update_mini_batch(mini_batch, eta)
--             if test_data:
--                 print "Epoch {0}: {1} / {2}".format(
--                     j, self.evaluate(test_data), n_test)
--             else:
--                 print "Epoch {0} complete".format(j)

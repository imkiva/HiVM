module Utils.UniqueId
  ( UniqueId
  , makeUniqueId
  ) where

import           System.Random

type UniqueId = Int

-- | Don't ask why. I generated this in GHCi by
-- | > newRand = randomIO :: IO Int
-- | > newRand
-- | 2353995195697428765
magicSeed :: Int
magicSeed = 2353995195697428765

randomOne :: Int -> UniqueId
randomOne = fst . random . mkStdGen

makeUniqueId :: UniqueId
makeUniqueId = randomOne magicSeed

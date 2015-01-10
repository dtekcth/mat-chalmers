-- | Configuration

module Config where

data Config =
  Config {nextDayHour :: Int -- ^ When to show next days menu
         ,updateInterval :: Int -- ^ Time to wait between menu updates
         }

def :: Config
def =
  Config 12
         (1000000 * 60 * 60) -- 1 hour

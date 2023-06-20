module CalculateRelease(main) where

main = do
    read <$> interact "Latest block height: "

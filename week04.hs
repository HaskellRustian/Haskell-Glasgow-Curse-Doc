{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

journeyCost :: Float -> Float -> Float
journeyCost miles fuelcostperlitre = 
 let milespergallon = 35
     litrespergallon = 4.55
     gallons = miles / milespergallon
 in (gallons * litrespergallon * fuelcostperlitre)

sumFee :: Float -> Float
sumFee qty =
    let commision  = 1.0
    in (qty + commision + x + actualPrice)
    where 
          x = 1.0
          y = 2.0

actualPrice :: Float
actualPrice = 35.00

priceFilter :: Float -> Float
priceFilter x
    | x < actualPrice = x - actualPrice
    | otherwise = x

data Symbol = Base  | Quote      deriving (Show, Read)
data Base = BTC | LTC | BNB      deriving (Show, Read)
data Quote  = USDT | BUSD | DAI  deriving (Show, Read)

symbolTicker :: Base -> Quote -> String
symbolTicker base quote =
    case base of
        BTC -> "BTC" ++ quote'
        LTC -> "LTC" ++ quote' 
        _   -> "ASSET IS NOT TRADEABLE"
    where
        quote' = quoteAsset quote

quoteAsset :: Quote -> String
quoteAsset quote =
    case quote of
        USDT -> "USDT"
        DAI  -> "DAI"
        BUSD -> "BUSD"
        _    -> "s"

testMaybe:: () -> String
testMaybe () = let x = (Just Nothing)
               in
                   case x of
                       (Just _) -> "SOMETHING"
                       Nothing -> "NON"

helloMonad :: String -> IO String
helloMonad x =
    do
        putStrLn ("Hello" ++ x)
        putStrLn "what's your name ?"
        name <- getLine
        return name


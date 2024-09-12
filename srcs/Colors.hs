module Colors (Color (..), colored, putColorful) where

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White

colorCode :: Color -> String
colorCode Red = "\ESC[31m"
colorCode Green = "\ESC[32m"
colorCode Yellow = "\ESC[33m"
colorCode Blue = "\ESC[34m"
colorCode Magenta = "\ESC[35m"
colorCode Cyan = "\ESC[36m"
colorCode White = "\ESC[37m"

colored :: Color -> String -> String
colored color text = ansiBold ++ colorCode color ++ text ++ ansiReset
  where
    ansiReset = "\ESC[0m"
    ansiBold = "\ESC[1m"

putColorful :: Color -> String -> IO ()
putColorful color text = putStrLn $ colored color text
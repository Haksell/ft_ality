import Colors (Color (..), putColorful)

main :: IO ()
main = do
  putColorful Red "This text is bold and red!"
  putColorful Green "This text is bold and green!"
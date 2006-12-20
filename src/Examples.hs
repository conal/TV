import Graphics.UI.TV
import Graphics.UI.Phooey (UI)

main = runTV tv2

tv0 :: TV UI String
tv0 = tv (stringDisplay "message") "Hello World!"

tv1 = tv (showDisplay "how much") (3 :: Int)

tv2 = tv (oLambda (islider "foo" 3 (0,10)) (showDisplay "bar")) id


apples   = islider "apples"   3 (0,10)
bananas  = islider "bananas"  7 (0,10)
total    = showDisplay "total"

tv3 :: TV UI (Int -> Int -> Int)
tv3 = tv (oLambda apples $ oLambda bananas $ total) (+)

tv4 :: TV UI ((Int,Int) -> Int)
tv4 = tv (oLambda (iPair apples bananas) total) (uncurry (+))

-- ui1 =  title "Shopping List" $
--        proc () -> do
--          a  <- title "apples"   (islider 3)  -< (0,10)
--          b  <- title "bananas"  (islider 7)  -< (0,10)
--          title "total"  showDisplay          -< a+b


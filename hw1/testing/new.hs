hanoi :: Int -> String -> String -> String -> IO ()
hanoi 1 str1 str2 str3 = putStrLn ("move disc from" ++ str1 ++ " to " ++ str3 ++ "\n")
hanoi n str1 str2 str3 = do
        hanoi (n - 1) str1 str3 str2
        putStrLn ("move disc from " ++ str1 ++ " to " ++ str2 ++ "\n")
        hanoi (n - 1) str3 str2 str1


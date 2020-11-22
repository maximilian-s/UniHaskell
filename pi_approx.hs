-- Various functions for approximating Pi --

pi_approx :: Int -> Double
pi_approx n
    | n < 1 =  error "erst ab 1"
    | otherwise = sqrt(6*sum [1/fromIntegral(x*x) | x <- [1..n]])


pi_approx' :: Int -> Double
pi_approx' 1 = 1
pi_approx' n = sqrt (6*pi_help n)
    where pi_help 1 = 1
          pi_help n = 1/fromIntegral(n*n) + pi_help (n-1)

-- Tail Recursion for significantly improved efficiency --

pi_approx'' :: Int -> Double
pi_approx'' 1 = 1
pi_approx'' n = sqrt (6*pi_help n 1)
    where pi_help 1 akk = akk
          pi_help n akk = pi_help (n-1) (1/fromIntegral(n*n)+akk)

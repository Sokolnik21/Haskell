sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (i, c) = (c, i)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (a, b, c)  | a /= b    = False
                      | b /= c    = False
                      | otherwise = True

triangleAreaW :: (Double, Double, Double) -> Double
triangleAreaW (a, b, c) = sqrt(p * (p - a) * (p - b) * (p - c))
  where p = 1.0/2 * (a + b + c)

triangleAreaL :: (Double, Double, Double) -> Double
triangleAreaL (a, b, c) = let p = 1.0/2 * (a + b + c)
  in sqrt(p * (p - a) * (p - b) * (p - c))

-- Example below shows why "where" and "let...in" is useful
-- Thanks to them code is simpler to understand and looks like spaghetti a little bit less
triangleAreaDirty :: (Double, Double, Double) -> Double
triangleAreaDirty (a, b, c) = sqrt(1.0/2 * (a + b + c) * (1.0/2 * (a + b + c) - a) * (1.0/2 * (a + b + c) - b) * (1.0/2 * (a + b + c) - c))

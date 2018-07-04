roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
  where d = sqrt ( b ^ 2 - 4 * a * c)
        e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = (a / l, b / l)
  where l = sqrt ( a^2 + b^2 )

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) = (a / l, b / l, c / l)
  where l = sqrt ( a^2 + b^2 + c^2 )

triangleAreaW :: (Double, Double, Double) -> Double
triangleAreaW (a, b, c) = sqrt(p * (p - a) * (p - b) * (p - c))
  where p = 1.0/2 * (a + b + c)

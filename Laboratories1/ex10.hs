roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
 let d = sqrt (b * b - 4 * a * c)
     e = 2 * a
 in ( (-b - d) / e, (-b + d) / e )

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) =
  let l = sqrt ( a^2 + b^2 )
  in (a / l, b / l)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) =
  let l = sqrt ( a^2 + b^2 + c^2 )
  in (a / l, b / l, c / l)

triangleAreaW :: (Double, Double, Double) -> Double
triangleAreaW (a, b, c) =
  let p = 1.0/2 * (a + b + c)
  in sqrt(p * (p - a) * (p - b) * (p - c))

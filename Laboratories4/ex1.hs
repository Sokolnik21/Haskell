polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)
-- nothing gained with aliases

newtype CartesianCoord'' a = MkCartesianCoord'' (a, a)
newtype PolarCoord'' a = MkPolarCoord'' (a, a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)
-- "newtype" provides some kind of security in which only exact type can be calculate\

newtype SphericalCoord a = MkSphericalCoord (a, a, a)
newtype CylindricalCoord a = MkCylindricalCoord (a, a, a)
newtype CartesianCoord3D a = MkCartesianCoord3D (a, a, a)

sphericalToCartesian :: Floating a => SphericalCoord a -> CartesianCoord3D a
sphericalToCartesian (
  MkSphericalCoord (
    r,
    geoLength,
    geoWidth)) =
  MkCartesianCoord3D (
    r * sin geoLength * cos geoWidth,
    r * cos geoLength * sin geoWidth,
    r * sin geoLength)

cylindricalToCartesian :: Floating a => CylindricalCoord a -> CartesianCoord3D a
cylindricalToCartesian (
  MkCylindricalCoord (
    axialDistance,
    azimuth,
    height)) =
  MkCartesianCoord3D (
    axialDistance * cos azimuth,
    axialDistance * sin azimuth,
    height)

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
  "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) =
  "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

newtype PersonInfo'' = MkPersonInfo'' (Name'', Surname'', Address'')
newtype Name'' = MkName'' String
newtype Surname'' = MkSurname'' String
newtype Address'' = MkAddress'' String
printName (MkName'' a) = a
printSurname (MkSurname'' a) = a
printAddress (MkAddress'' a) = a

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (
  MkPersonInfo'' (
    nm,
    snm,
    addr)) =
  "name: " ++ printName nm ++ ", surname: " ++ printSurname snm ++ ", addr: " ++ printAddress addr

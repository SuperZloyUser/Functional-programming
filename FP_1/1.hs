linEq a b
  | (a == 0) && (b == 0) = error "Infinity roots"
  | (a == 0) = error "No roots"
  | otherwise = (-b / a)
  
makeList a = [a]

sqrEq a b c
  | (a == 0) && (b == 0) && (c == 0) = error "Infinity roots"
  | (a == 0) && (b == 0) || discriminant < 0 = error "No real roots"
  | (a == 0) && (b /= 0) = makeList (linEq b c)
  | discriminant == 0 = soloRoot
  | otherwise = roots
  where
    discriminant = (b ^ 2 - 4 * a * c)
    soloRoot = [(-b / (2 * a))]
    roots = [((-b - sqrt discriminant) / (2 * a)), ((-b + sqrt discriminant) / (2 * a))]

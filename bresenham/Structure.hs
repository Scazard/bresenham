module Structure where


data Square = Off | On
type Row = [Square]
data Grid = Grid {rows :: [Row]}
type Pos = (Int, Int)
data Action = Negative | Positive


instance Show Square where
    show Off = "_"
    show On  = "O"

showRows :: [Row] -> String
showRows [] = ""
showRows (r:rs) = showSquares r ++ "\n" ++ showRows rs

showSquares :: Row -> String
showSquares [] = ""
showSquares (x:xs) = show x ++ " " ++ showSquares xs
instance Show Grid where
    show (Grid g) = showRows g
import Text.Regex.Posix
import Data.List (nubBy, maximumBy, group, sort)
import Data.Ord (comparing)

data Actor = Actor {
  name :: String,
  movieTitle :: String
} deriving (Show)

data Movie = Movie {
  title :: String,
  genre :: String,
  year :: String
} deriving (Show)

parseActors :: String -> [Actor]
parseActors content = map (\x -> Actor (x !! 1) (x !! 2)) (content =~ actorRegex :: [[String]])
  where actorRegex = "actor\\(([^,]+),([^\\)]+)\\)"

parseMovies :: String -> [Movie]
parseMovies content = map (\x -> Movie (x !! 1) (x !! 2) (x !! 3)) (content =~ movieRegex :: [[String]])
  where movieRegex = "movie\\(([^,]+),([^,]+),([^\\)]+)\\)"

actorsWithMoviesTitle :: [Actor] -> [(Actor, [String])]
actorsWithMoviesTitle actors = uniqueActors
  where uniqueActors = nubBy (\x y -> name (fst x) == name (fst y)) actorsWithMovies
        actorsWithMovies = map (\x -> (x, map movieTitle (filter (\y -> name x == name y) actors))) actors

joinMoviesWithActors :: [Movie] -> [(Actor, [String])] -> [(Actor, [Movie])]
joinMoviesWithActors movies actors = map (\x -> (fst x, map (\y -> head (filter (\z -> title z == y) movies)) (snd x))) actors

mostPopularGenreByActor :: [(Actor, [Movie])] -> [(Actor, String)]
mostPopularGenreByActor actorsWithMovies = map (\x -> (fst x, mostCommonGenre (snd x))) actorsWithMovies
  where mostCommonGenre movies = head $ maximumBy (\x y -> compare (length x) (length y)) $ group $ sort $ map genre movies

main :: IO ()
main = do
  content <- readFile "data.txt"

  let actors = parseActors content
      movies = parseMovies content
      ac = actorsWithMoviesTitle actors
      acWithMovies = joinMoviesWithActors movies ac
      mostPopularGenre = mostPopularGenreByActor acWithMovies

  putStrLn "Most popular genre by actor:"
  mapM_ (\x -> putStrLn $ name (fst x) ++ " - " ++ snd x) mostPopularGenre
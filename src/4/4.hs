-- В файлах хранится информация об актерах и фильмах в следующем
-- виде: актер(<фамилия>,<название_фильма>),
-- фильм(<название_фильма>,<жанр>,<год_выпуска>). Написать
-- программу, формирующую список актеров по жанрам (если актер
-- снимался в фильмах разных жанров, то определять его жанр исходя
-- из максимального количества ролей в фильмах одного жанра).

import Text.Regex.Posix
import Data.List (nubBy, maximumBy, group, sort)
import Data.Ord (comparing)
import System.Directory (getCurrentDirectory)

data Actor = Actor {
  name :: String,
  movieTitle :: String
} deriving (Show)

data Movie = Movie {
  title :: String,
  genre :: String,
  year :: String
} deriving (Show)

--parseActorsTest :: String -> [[String]]
--parseActorsTest content = content =~ actorRegex :: [[String]]
--  where actorRegex = "actor\\(([^,]+),([^\\)]+)\\)"

parseActors :: String -> [Actor]
parseActors content = map (\x -> Actor (x !! 1) (x !! 2)) (content =~ actorRegex :: [[String]])
  where actorRegex = "actor\\(([^,]+),([^\\)]+)\\)"

parseMovies :: String -> [Movie]
parseMovies content = map (\x -> Movie (x !! 1) (x !! 2) (x !! 3)) (content =~ movieRegex :: [[String]])
  where movieRegex = "movie\\(([^,]+),([^,]+),([^\\)]+)\\)"

actorsWithMoviesTitle :: [Actor] -> [(Actor, [String])]
actorsWithMoviesTitle actors =
  [(actor, [title | movie <- movies, let title = movieTitle movie]) |
    actor <- uniqueActors,
    let movies = filter (\movie -> name actor == name movie) actors]
  where uniqueActors = nubBy (\x y -> name x == name y) actors

joinMoviesWithActors :: [Movie] -> [(Actor, [String])] -> [(Actor, [Movie])]
joinMoviesWithActors movies actors =
  [(actor, [movie | movie <- movies, title movie `elem` movieTitles]) | (actor, movieTitles) <- actors]

mostCommonGenre :: [Movie] -> String
mostCommonGenre = head . maximumBy (comparing length) . group . sort . map genre

mostPopularGenreByActor :: [(Actor, [Movie])] -> [(Actor, String)]
mostPopularGenreByActor = map (\(a, ms) -> (a, mostCommonGenre ms))


main :: IO ()
main = do

  content <- readFile "src/4/data.txt"

  --  print $ parseActorsTest content

  let actors = parseActors content
      movies = parseMovies content
      acWithMovieTitles = actorsWithMoviesTitle actors
      acWithMovies = joinMoviesWithActors movies acWithMovieTitles
      mostPopularGenre = mostPopularGenreByActor acWithMovies

  putStrLn "Most popular genre by actor:"
  mapM_ (\x -> putStrLn $ name (fst x) ++ " - " ++ snd x) mostPopularGenre

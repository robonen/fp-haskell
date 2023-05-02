-- Определите тип данных для хранения следующей информации по
-- футбольным командам: название команды; информация по
-- результатам сыгранных игр – [(x1,y1), (x2,y2), …], где каждый
-- кортеж – это информация по отдельной игре и xi – количество
-- забитых голов, yi – количество пропущенных голов. Определить
-- для этого типа функцию сравнения ‘==’ класса Eq, а также функции
-- ‘<’, ‘<=’ класса Ord, сравнивая команды по количеству набранных
-- очков (выигрыш=3, ничья=1), если у двух команд равное
-- количество набранных очков, то дальше сравнивают разницу между
-- количеством забитых и количеством пропущенных голов.

import Data.List (sortBy)

data FootballTeam = FootballTeam {
  teamName :: String,
  gameResults :: [(Int, Int)]
} deriving (Show)

instance Eq FootballTeam where
  team1 == team2 = team1Score == team2Score && team1Diff == team2Diff
    where
      team1Score = calculateScore team1
      team2Score = calculateScore team2
      team1Diff = calculateGoalDifference team1
      team2Diff = calculateGoalDifference team2

instance Ord FootballTeam where
  compare team1 team2
    | team1Score < team2Score = LT
    | team1Score > team2Score = GT
    | team1Diff < team2Diff = LT
    | team1Diff > team2Diff = GT
    | otherwise = EQ
    where
      team1Score = calculateScore team1
      team2Score = calculateScore team2
      team1Diff = calculateGoalDifference team1
      team2Diff = calculateGoalDifference team2

calculateScore :: FootballTeam -> Int
calculateScore team = sum [3 | (x, y) <- gameResults team, x > y] + sum [1 | (x, y) <- gameResults team, x == y]

calculateGoalDifference :: FootballTeam -> Int
calculateGoalDifference team = sum [x - y | (x, y) <- gameResults team]

main :: IO ()
main = do
  let teams = [FootballTeam "Team1" [(2, 1), (1, 1), (0, 1)], -- score: 4, diff: 0
               FootballTeam "Team2" [(1, 2), (3, 1), (2, 2)], -- score: 4, diff: 1
               FootballTeam "Team3" [(1, 1), (2, 2), (3, 3)]] -- score: 3, diff: 0

  putStr "Sorted teams: "
  print $ map teamName $ sortBy compare teams

  let team1 = FootballTeam "Team1" [(2, 1), (1, 1), (0, 1)] -- score: 4, diff: 0
  let team2 = FootballTeam "Team2" [(2, 1), (1, 1), (0, 1)] -- score: 4, diff: 0
  let team3 = FootballTeam "Team3" [(2, 1), (2, 1), (0, 1)] -- score: 4, diff: 1

  putStr "Equal team1 and team2: "
  print $ team1 == team2
  putStr "Equal team1 and team3: "
  print $ team1 == team3

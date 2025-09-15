import Control.DeepSeq
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import Test.HUnit qualified as HUnit

data Part = Part1 | Part2
data Args = RunPart Part FilePath | RunTests

part_parser :: Parser Part
part_parser =
    ( flag' Part1 (
        long "part1"
        <> short '1'
        <> help "Run puzzle part 1" ) )
    <|> ( flag' Part2 (
        long "part2"
        <> short '2'
        <> help "Run puzzle part 2" ) )

arg_parser :: Parser Args
arg_parser = RunPart
    <$> part_parser <*> argument str (
        metavar "PATH"
        <> help "Path to the input data file" ) <|>
    RunTests <$ ( flag' RunTests (
        long "test"
        <> short 't'
        <> help "Run unit tests" ) )

type InputType = Array (Int, Int) Bool

load_input :: FilePath -> IO InputType
load_input path = withFile path ReadMode $ \handle -> do
    input_lines <- fmap lines (hGetContents handle)
    let input = fmap (\line -> fmap (\c -> c == '#') line) input_lines
    let width = length $ head input
    let height = length input

    -- Use $!! to force evaluation while we have the file open.
    -- Otherwise, lazy IO might mean hGetContents is called after the handle is closed.
    return $!! array ((0, 0), (height-1, width-1)) [((r, c), input !! r !! c) | (r, c) <- range ((0, 0), (height-1, width-1))]

type ConwayState = Array (Int, Int, Int) Bool

initial_state :: InputType -> ConwayState
initial_state input = array new_bounds [((i, j, k), if k == 0 then input!(i, j) else False) | (i, j, k) <- range new_bounds]
    where
        ((i_min, j_min), (i_max, j_max)) = bounds input
        new_bounds = ((i_min, j_min, 0), (i_max, j_max, 0))

neighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbours (i, j, k) = filter (\x -> x /= (i, j, k)) [(i+di, j+dj, k+dk)
        | di <- [-1, 0, 1], dj <- [-1, 0, 1], dk <- [-1, 0, 1]]

-- https://stackoverflow.com/questions/573751/using-foldl-to-count-number-of-true-values/573767#573767
neighbour_count :: ConwayState -> (Int, Int, Int) -> Int
neighbour_count state pos = sum $ map fromEnum [if inRange state_bounds neighbour_pos then state!neighbour_pos else False | neighbour_pos <- neighbours pos]
    where state_bounds = bounds state

next_iteration :: ConwayState -> ConwayState
next_iteration prev = array new_bounds [
    (pos, if (if inRange old_bounds pos then prev!pos else False)
        then (inRange (2, 3) (neighbour_count prev pos))
        else ((neighbour_count prev pos) == 3))
    | pos <- range new_bounds]
    where
        old_bounds = bounds prev
        ((i_min, j_min, k_min), (i_max, j_max, k_max)) = old_bounds
        new_bounds = ((i_min-1, j_min-1, k_min-1), (i_max+1, j_max+1, k_max+1))

count_active :: ConwayState -> Int
count_active state = sum $ fmap fromEnum state

part1 :: InputType -> Integer
part1 input = toInteger $ count_active $ (iterate next_iteration (initial_state input)) !! 6

part2 :: InputType -> Integer
part2 input = 0 -- TODO

load_answer :: FilePath -> IO Integer
load_answer path = withFile path ReadMode $ \handle -> do
    contents <- hGetContents handle
    let answer :: Integer = read $ dropWhileEnd isSpace contents
    -- Use $!! to force evaluation while we have the file open.
    return $!! answer

run_tests :: IO ()
run_tests = do
    data_files <- getCurrentDirectory
        >>= \x -> return (x </> "data")
        >>= getDirectoryContents
        >>= sequence . map (\y -> (getCurrentDirectory >>= return . flip combine ("data" </> y)))
        >>= filterM doesFileExist

    let part_tests = \part_str -> \part_fn -> do
            let inputs = map (\x -> (dropExtension x, x)) (filter (\x -> (takeExtension x) == (".answer" ++ part_str)) data_files)
            input_data <- sequence [load_input input_path | (input_path, _) <- inputs]
            let answers = [part_fn input | input <- input_data]
            expected <- sequence [load_answer answer_path | (_, answer_path) <- inputs]
            return $ HUnit.TestLabel ("part " ++ part_str) $ HUnit.TestList [HUnit.TestLabel (takeBaseName i) $ HUnit.TestCase $ HUnit.assertEqual "" e a | ((i, _), a, e) <- zip3 inputs answers expected]

    tests <- sequence $ [part_tests "1" part1, part_tests "2" part2]
    _ <- HUnit.runTestTT $ HUnit.TestList tests
    return ()

run :: Args -> IO ()
run (RunPart Part1 path) = do
    input <- load_input path
    let answer = part1 input
    putStrLn $ show answer
run (RunPart Part2 path) = do
    input <- load_input path
    let answer = part2 input
    putStrLn $ show answer
run RunTests = run_tests

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (arg_parser <**> helper)
            ( fullDesc
            <> progDesc "Advent of Code 2020 Day 17" )

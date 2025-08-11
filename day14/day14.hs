-- Required for `deriving NFData`
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.DeepSeq
import Control.Monad
import Data.Char
import Data.List
import GHC.Generics (Generic)
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import Test.HUnit
import Text.Regex.Applicative

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

-- Input data types must derive Generic and NFData, so we can use $!! from Control.DeepSeq to force
-- their evaluation before the handle to the input file is closed.
data MaskBit = X | Bit Bool deriving (Generic, NFData)
instance Show MaskBit where
    show (X) = "X"
    show (Bit True) = "1"
    show (Bit False) = "0"
type Mask = [MaskBit]
data Instruction = Mask Mask | Write Integer Integer deriving (Generic, NFData)
instance Show Instruction where
    show (Mask m) = "mask = " ++ concat [show b | b <- m]
    show (Write a b) = "mem[" ++ show a ++ "] = " ++ show b
type InputType = [Instruction]

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (arg_parser <**> helper)
            ( fullDesc
            <> progDesc "Advent of Code 2020 Day 14" )

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

line_to_instruction :: String -> Maybe Instruction
line_to_instruction line = do
    let maskbit_regex = X <$ sym 'X' <|> Bit False <$ sym '0' <|> Bit True <$ sym '1'
    let mask_regex = many maskbit_regex
    let num_regex = read <$> many (psym isDigit) :: RE Char Integer
    let instruction_regex = (string "mask = " *> (Mask <$> mask_regex)) <|> (Write <$ string "mem[" <*> num_regex <* string "] = " <*> num_regex)
    line =~ instruction_regex

load_input :: FilePath -> IO InputType
load_input path = withFile path ReadMode $ \handle -> do
    input_lines <- fmap lines (hGetContents handle)
    let input_data = fmap (\line -> case line_to_instruction line of
            Just instruction -> instruction
            Nothing -> error "Invalid input line"
            ) input_lines
    -- Use $!! to force evaluation while we have the file open.
    -- Otherwise, lazy IO might mean hGetContents is called after the handle is closed.
    return $!! input_data

part1 :: InputType -> Integer
part1 input = toInteger $ length input -- TODO

part2 :: InputType -> Integer
part2 input = toInteger $ length input -- TODO

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
            return $ TestLabel ("part " ++ part_str) $ TestList [TestLabel (takeBaseName i) $ TestCase $ assertEqual "" e a | ((i, _), a, e) <- zip3 inputs answers expected]

    tests <- sequence $ [part_tests "1" part1, part_tests "2" part2]
    _ <- runTestTT $ TestList tests
    return ()

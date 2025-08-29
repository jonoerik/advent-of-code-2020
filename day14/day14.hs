-- Required for `deriving NFData`
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- Required for `makeLenses`
{-# LANGUAGE TemplateHaskell #-}

import Control.DeepSeq
import Control.Lens hiding (parts, argument)
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import GHC.Generics (Generic)
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import Test.HUnit qualified as HUnit
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
data MaskBit = X | Bit Bool deriving (Generic, NFData, Eq, Ord)
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

data P1State = P1State
    { _p1mask :: Maybe Mask
    , _p1memory :: Map.Map Integer Integer
    }
makeLenses ''P1State

mask_bit_to_val :: MaskBit -> Integer -> Integer -> Integer -> Integer
mask_bit_to_val X v _ _ = v
mask_bit_to_val (Bit True) _ v _ = v
mask_bit_to_val (Bit False) _ _ v = v

mask_val :: Mask -> Integer -> Integer
mask_val m i = i .&. (foldl (\a b -> a * 2 + (mask_bit_to_val b 1 1 0)) 0 m) .|. (foldl (\a b -> a * 2 + (mask_bit_to_val b 0 1 0)) 0 m)

run_instruction_part1 :: Instruction -> State P1State ()
run_instruction_part1 (Mask m) = p1mask .= Just m
run_instruction_part1 (Write addr val) = do
    current_mask <- use p1mask
    p1memory %= \m -> Map.insert addr (mask_val (fromJust current_mask) val) m

part1 :: InputType -> Integer
part1 input = Map.foldr (+) 0 $ view p1memory final_state
    where final_state = execState (traverse run_instruction_part1 input) (P1State Nothing Map.empty)

data P2State = P2State
    { _p2mask :: Maybe Mask
    , _p2memory :: Map.Map Mask Integer
    }
makeLenses ''P2State

-- Return a list of addresses which together cover all addresses in a1 except those in a2.
addr_without :: Mask -> Mask -> [Mask]
addr_without [] [] = []
addr_without _ [] = error "Mismatched mask sizes"
addr_without [] _ = error "Mismatched mask sizes"
addr_without (a1:a1s) (a2:a2s) = case (a1, a2) of
        (Bit False, Bit False) -> fixed_value $ Bit False
        (Bit False, Bit True) -> no_overlap
        (Bit True, Bit False) -> no_overlap
        (Bit True, Bit True) -> fixed_value $ Bit True
        (X, Bit _) -> (addr_without (Bit True : a1s) (a2:a2s)) ++ (addr_without (Bit False : a1s) (a2:a2s))
        (Bit False, X) -> fixed_value $ Bit False
        (Bit True, X) -> fixed_value $ Bit True
        (X, X) -> fixed_value X
    where
        result_tail = addr_without a1s a2s
        fixed_value = \b -> map ((:) b) result_tail -- Resulting addresses must have value b in the first dimension.
        no_overlap = [a1 : a1s] -- The two input masks don't overlap at all, as they're completely separated in the first dimension.

part2_mask_addr :: Integer -> Mask -> Mask
part2_mask_addr 0 [] = []
part2_mask_addr _ [] = error "Address too large for current mask"
part2_mask_addr a m = (part2_mask_addr (a .>>. 1) (init m)) ++ [case last m of
        Bit False -> if a .&. 1 == 1 then Bit True else Bit False
        Bit True -> Bit True
        X -> X
    ]

run_instruction_part2 :: Instruction -> State P2State ()
run_instruction_part2 (Mask m) = p2mask .= Just m
run_instruction_part2 (Write addr val) = do
    current_mask <- use p2mask
    let modified_addr = part2_mask_addr addr (fromJust current_mask)
    p2memory %= Map.foldrWithKey (\k v m -> foldr (\a -> Map.insert a v) m (addr_without k modified_addr)) Map.empty
    p2memory %= Map.insert modified_addr val

part2_memory_sum :: Map.Map Mask Integer -> Integer
part2_memory_sum = Map.foldrWithKey (\k v r -> r + v * 2 ^ (length . filter (\x -> case x of
        X -> True
        Bit _ -> False
    )) k) 0

part2 :: InputType -> Integer
part2 input = part2_memory_sum $ view p2memory final_state
    where final_state = execState (traverse run_instruction_part2 input) (P2State Nothing Map.empty)

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
            <> progDesc "Advent of Code 2020 Day 14" )

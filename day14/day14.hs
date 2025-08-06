import Options.Applicative

data Part = Part1 | Part2
data Args = Args 
    { part :: Part
    , path :: String }

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
arg_parser = Args
    <$> part_parser <*> argument str (
        metavar "PATH"
        <> help "Path to the input data file" )

data MaskBit = X | Bit Bool
type Mask = [MaskBit]
data Instruction = Mask Mask | Write Integer Integer
type InputType = [Instruction]

main :: IO ()
main = run =<< execParser opts
    where
        opts = info (arg_parser <**> helper)
            ( fullDesc
            <> progDesc "Advent of Code 2020 Day 14" )

run :: Args -> IO ()
run (Args Part1 path) = do
    input <- load_input $ path
    putStrLn . show . part1 $ input
run (Args Part2 path) = do
    input <- load_input $ path
    putStrLn . show . part2 $ input

load_input :: String -> IO InputType
load_input _ = do
    return [] -- TODO

part1 :: InputType -> Integer
part1 input = 1 -- TODO

part2 :: InputType -> Integer
part2 input = 2 -- TODO

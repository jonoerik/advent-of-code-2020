: print-usage ( -- )
    \ Print usage message and quit
    ." Usage: gforth day08.fth --part1|--part2 input-file" cr
    .\" \t--part1|-1 Run puzzle part 1" cr
    .\" \t--part2|-2 Run puzzle part 2" cr
    .\" \tinput-file Path to file from which to load puzzle input" cr
    bye
;

: test-part-arg ( -- n )
    \ Test the --part command line argument, and return the part to run
    next-arg 2dup 0 0 d= if
        print-usage
    then
    2dup 2dup s" --part1" compare 0 = -rot s" -1" compare 0 = or if
        2drop
        1
    else
        2dup s" --part2" compare 0 = -rot s" -2" compare 0 = or if
            2
        else
            print-usage
        then
    then
;

: test-input-path ( -- c-addr u )
    \ Test for the input-file command line argument, and return it as a string
    next-arg 2dup 0 0 d= if
        print-usage
    then
;

0 constant op-nop
1 constant op-acc
2 constant op-jmp

: parse-input-line ( c-addr u -- n1 n2)
    \ Parse a line of input into an opcode and an operand value
    dup 5 <= if
        ." Line too short in input" cr
        bye
    then
    over 3 s" nop" compare 0= if
        op-nop
    else over 3 s" acc" compare 0= if
        op-acc
    else over 3 s" jmp" compare 0= if
        op-jmp
    else
        .\" Unrecognised opcode in line \"" type .\" \" in input" cr
        bye
    then then then
    -rot

    swap 4 + swap 4 -
    over 1 s" +" compare 0= if
        1
    else over 1 s" -" compare 0= if
        -1
    else
        ." Operand value not marked as + or -" cr
        bye
    then then
    -rot

    swap 1 + swap 1 -
    0 0 2swap >number 2drop 0 <> if
        ." Operand value too large for single cell integer" cr
        bye
    then
    *
;

0 value input-fd
64 constant linebufsize
create linebuf linebufsize 2 + allot

: load-input ( c-addr u -- TODO )
    \ Load input from the filename provided
    r/o open-file 0<> if
        ." Could not open input file" cr
        bye
    then
    to input-fd

    begin
        linebuf linebufsize input-fd read-line throw
    while
        linebuf swap parse-input-line
        swap . . cr \ TODO write n1 n2 into array, increment array size
    repeat drop

    input-fd close-file throw
;

test-part-arg
test-input-path
load-input
bye

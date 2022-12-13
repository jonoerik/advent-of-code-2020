: print-usage ( -- )
    \ Print usage message and quit
    ." Usage: gforth day08.fth --part1|--part2 input-file" cr
    ."        gforth day08.fth --test" cr cr
    .\" \t--part1|-1 Run puzzle part 1" cr
    .\" \t--part2|-2 Run puzzle part 2" cr
    .\" \tinput-file Path to file from which to load puzzle input" cr
    .\" \t--test Run unit tests" cr
    2 (bye)
;

: test-part-arg ( -- n )
    \ Test the --part command line argument, and return the part to run
    \ Or 3 if tests should be run
    next-arg 2dup 0 0 d= if
        print-usage
    then
    2dup 2dup s" --part1" compare 0 = -rot s" -1" compare 0 = or if
        2drop
        1
    else
        2dup 2dup s" --part2" compare 0 = -rot s" -2" compare 0 = or if
            2
        else
            s" --test" compare 0 = if
                3
            else
                print-usage
            then
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
        1 (bye)
    then
    over 3 s" nop" compare 0= if
        op-nop
    else over 3 s" acc" compare 0= if
        op-acc
    else over 3 s" jmp" compare 0= if
        op-jmp
    else
        .\" Unrecognised opcode in line \"" type .\" \" in input" cr
        1 (bye)
    then then then
    -rot

    swap 4 + swap 4 -
    over 1 s" +" compare 0= if
        1
    else over 1 s" -" compare 0= if
        -1
    else
        ." Operand value not marked as + or -" cr
        1 (bye)
    then then
    -rot

    swap 1 + swap 1 -
    0 0 2swap >number 2drop 0 <> if
        ." Operand value too large for single cell integer" cr
        1 (bye)
    then
    *
;

0 value input-fd
64 constant linebufsize
create linebuf linebufsize 2 + allot

struct
    cell% field instruction-opcode
    cell% field instruction-operand
    cell% field instruction-run-count
end-struct instruction%

1024 constant instr-buf-size
instruction% instr-buf-size * %allot constant instr-buf
0 value instr-count

: load-input ( c-addr u -- )
    \ Load input from the filename provided
    r/o open-file 0<> if
        ." Could not open input file" cr
        1 (bye)
    then
    to input-fd

    begin
        linebuf linebufsize input-fd read-line throw
    while
        linebuf swap parse-input-line
        instr-buf instr-count instruction% %size * + dup dup
        instruction-run-count 0 swap !
        -rot instruction-operand !
        instruction-opcode !

        instr-count 1+ to instr-count
    repeat drop

    input-fd close-file throw
;

: print-instructions ( -- )
    \ Print list of instructions, in the same format as the input file
    instr-count 0 ?DO
        instr-buf i instruction% %size * + dup
        instruction-opcode @
        case
            op-nop of ." nop" endof
            op-acc of ." acc" endof
            op-jmp of ." jmp" endof
        endcase space
        instruction-operand @ dup
        0>= if ." +" then . cr
    LOOP
;

: run-instructions ( -- n )
    \ Run instructions until a repeated instruction is found,
    \ the index of which is returned as n
    0 0
    begin ( accumulator instruction-index )
        dup instr-buf swap instruction% %size * + dup instruction-run-count @ 0=
    while ( accumulator instruction-index next-instruction )
        dup instruction-run-count dup @ 1+ swap !
        dup instruction-operand @ swap instruction-opcode @
        ( accumulator instruction-index operand opcode )
        case
            op-nop of drop 1+ endof
            op-acc of rot + swap 1+ endof
            op-jmp of + endof
        endcase
    repeat drop drop
;

: load-test-answer ( c-addr u -- n )
    \ From the filename on the stack, load the file, parse a single int
    \ in the contents, and return it on the stack
    r/o open-file 0<> if
        ." Could not open input file" cr
        1 (bye)
    then
    to input-fd

    linebuf linebufsize input-fd read-line throw
    false = if
        ." Failed to open test answer file" cr
        1 (bye)
    then
    linebuf swap
    0 0 2swap >number 2drop 0 <> if
        ." Test answer value too large for single cell integer" cr
        1 (bye)
    then

    input-fd close-file throw
;

: maybe-run-tests ( n -- )
    \ If n is 3, run tests and quit
    3 = if
        s" data/sample1" load-input
        run-instructions
        s" data/sample1.answer1" load-test-answer
        = if
            ." sample1 part1 :: PASSED" cr
            0 (bye)
        else
            ." sample1 part1 :: FAILED" cr
            1 (bye)
        then
    then
;

test-part-arg
dup maybe-run-tests
test-input-path
load-input
run-instructions
. cr
0 (bye)

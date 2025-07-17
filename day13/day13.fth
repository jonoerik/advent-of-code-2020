\ Use gforth-string extension.
require string.fs

: print-usage ( -- )
    \ Print usage message and quit
    ." Usage: gforth day13.fth --part1|--part2 input-file" cr
    ."        gforth day13.fth --test" cr cr
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
            2drop
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

0 value input-fd
300 constant linebufsize
create linebuf linebufsize 2 + allot
variable charbuf

create input-start-time 0 ,
0 value bus-count
300 constant bus-max
create bus-array bus-max cells allot

: load-input ( c-addr u -- )
    \ Load input from the filename provided
    r/o open-file 0<> if
        ." Could not open input file" cr
        1 (bye)
    then
    to input-fd
    linebuf linebufsize input-fd read-line throw drop
    0 0 rot linebuf swap >number
    0<> if
        ." Unexpected character in input." cr
        1 (bye)
    then

    drop
    0<> if
        ." Bus start time is too large." cr
        1 (bye)
    then
    input-start-time !

    0 to bus-count
    true \ Number is valid (set to false if we encounter 'x').
    0 0 \ Current number.
    begin
        charbuf 1 input-fd read-file throw
    while
        charbuf @ [char] , <> if
            charbuf @ [char] x <> if
                charbuf 1 >number 2drop
            else
                rot drop false -rot
            then
        else
            rot if
                bus-count bus-max >= if
                    ." Too many bus values in input." cr
                    1 (bye)
                then
                0<> if
                    ." Bus ID is too large." cr
                    1 (bye)
                then
                bus-array bus-count cells + !
                bus-count 1+ to bus-count
            else
                2drop
            then
            true 0 0
        then
    repeat
    rot if
        bus-count bus-max >= if
            ." Too many bus values in input." cr
            1 (bye)
        then
        0<> if
            ." Bus ID is too large." cr
            1 (bye)
        then
        bus-array bus-count cells + !
        bus-count 1+ to bus-count
    then

    input-fd close-file throw
;

: calculate-bus-time ( i -- n )
    \ For bus index i, return the time to wait for that bus.
    input-start-time @ bus-array rot cells + @ dup -rot mod
    dup 0= if
        \ Bus arrives straight away.
        2drop 0
    else
        \ Non-zero wait for bus.
        -
    then
;

: run-part1 ( -- n )
    \ Run part1 of the puzzle, and return the answer.
    bus-array 0 cells + @ \ Current best bus ID.
    0 calculate-bus-time \ Current best bus time.
    bus-count 1 ?do
        i calculate-bus-time
        2dup > if
            -rot 2drop bus-array i cells + @ swap
        else
            drop
        then
    loop
    *
;

: run-part2 ( -- n )
    \ Run part2 of the puzzle, and return the answer.
    0 \ TODO
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

: .-no-space ( n -- )
    \ Display number n without trailling space.
    0 .r
;

: n-to-string ( n -- c-addr u )
    \ Convert number n to a string representation.
    s>d <# #s #>
;

variable test-string-buf

: run-test ( f n1 n2 -- f )
    \ Run sample n1, part n2, print the result, and update flag f if the test failed.

    s" data/sample" test-string-buf $!
    swap dup n-to-string test-string-buf $+! swap
    test-string-buf $@ load-input

    dup 1 = if
        run-part1
    else
        dup 2 = if
            run-part2
        else
            ." Unexpected test part." cr
            1 (bye)
        then
    then

    s" .answer" test-string-buf $+!
    swap dup n-to-string test-string-buf $+! swap
    test-string-buf $@ load-test-answer

    2swap swap s" sample" test-string-buf $! n-to-string test-string-buf $+!
    s"  part" test-string-buf $+! n-to-string test-string-buf $+!

    2dup = if
        test-string-buf $@ type ."  :: PASSED" cr
        2drop
    else
        test-string-buf $@ type ."  :: FAILED" cr
        swap .\" \tReturned " .-no-space ." , expected " .-no-space ." ." cr
        drop false
    then

    test-string-buf @ free throw 0 test-string-buf !
;

: run-tests ( -- )
    true \ All tests passed.
    1 1 run-test

    if
        ." All tests passed." cr
        0 (bye)
    else
        1 (bye)
    then
;

: run-main ( -- )
    test-part-arg
    dup 3 = if
        run-tests
    then
    test-input-path
    load-input
    1 = if
        run-part1
    else
        run-part2
    then
    . cr
    0 (bye)
;

run-main

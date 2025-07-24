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
struct
    cell% field bus-index
    cell% field bus-id
end-struct bus%
0 value bus-count
300 constant bus-max
bus% bus-max * %allot constant bus-array

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
    0 \ Current index in input.
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
                bus-array bus-count bus% %size * + tuck
                bus-id !
                swap dup rot bus-index !
                bus-count 1+ to bus-count
            else
                2drop
            then
            1 + \ Increment current index in input.
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
        bus-array bus-count bus% %size * + tuck
        bus-id !
        bus-index !
        bus-count 1+ to bus-count
    then

    input-fd close-file throw
;

: calculate-bus-time ( i -- n )
    \ For the ith bus in bus-array, return the time to wait for that bus.
    input-start-time @ bus-array rot bus% %size * + bus-id @ dup -rot mod
    dup 0= if
        \ Bus arrives straight away.
        2drop 0
    else
        \ Non-zero wait for bus.
        -
    then
;

: run-part1 ( -- d )
    \ Run part1 of the puzzle, and return the answer.
    bus-array bus-id @ \ Current best bus ID.
    0 calculate-bus-time \ Current best bus time.
    bus-count 1 ?do
        i calculate-bus-time
        2dup > if
            -rot 2drop bus-array i bus% %size * + bus-id @ swap
        else
            drop
        then
    loop
    *
    s>d
;

: u-width ( u1 -- u2 )
    \ Return number of bits required to hold u1.
    0 swap
    begin
        dup 0<>
    while
        1 rshift swap 1+ swap
    repeat
    drop
;

: cell-width ( -- u )
    true u-width
;

: ud-width ( ud -- u )
    \ Return number of bits required to hold ud.
    dup 0<> if
        u-width cell-width + swap drop
    else
        drop u-width
    then
;

: 2-rot ( d1 d2 d3 -- d3 d1 d2 )
    2rot 2rot
;

: 4dup ( d1 d2 -- d1 d2 d1 d2 )
    2swap 2dup 2rot 2dup 2-rot
;

: d* ( d1 d2 -- d3 )
    \ Double multiplication
    false \ Should the result be negated?
    -rot 2dup
    d0< -rot 2swap if
        invert
        -rot dnegate rot
    then
    >r 2swap r>
    -rot 2dup
    d0< -rot 2swap if
        invert
        -rot dnegate rot
    then
    >r

    \ Stack: ud2 ud1, R: negate?
    \ Calculate number of result bits.
    4dup ud-width -rot ud-width +
    cell-width 2* 1- \ Allow an extra bit's space, if case we need to negate.'
    u> if
        ." Result of d* too large for double width integer."
        1 (bye)
    then

    \ Move the smaller number above the larger one on the stack.
    \ From the bit width check, we know that at least one of the
    \ numbers is small enough to fit in a single cell, so convert it.
    4dup d< if 2swap then
    0<> if
        ." Unexpected large number in d*."
        1 (bye)
    then

    \ Stack: ud u, R: negate?
    tuck 2swap um* >r -rot um*
    0<> if
        ." Unexpected large number in d*."
        1 (bye)
    then
    + r>

    r> if
        dnegate
    then
;

: high-bit-1 ( -- x )
    \ Return an unsigned number with the highest bit in the cell set to 1,
    \ and all other bits 0.
    1 cell-width 1- lshift
;

: udlshift ( ud1 u -- ud2 )
    \ Double width unsigned left shift by u bits.
    0 ?do
        swap dup high-bit-1 and 0<>
        rot 1 lshift swap if 1 or then
        swap 1 lshift swap
    loop
;

: udrshift ( ud1 u -- ud2 )
    \ Double width unsigned right shift by u bits.
    0 ?do
        dup 1 and 0<>
        rot 1 rshift swap if high-bit-1 or then
        swap 1 rshift
    loop
;

: 2and ( ud1 ud2 -- ud3 )
    \ Bitwise and.
    rot and -rot and swap
;

: 2or ( ud1 ud2 -- ud3 )
    \ Bitwise or.
    rot or -rot or swap
;

create ud/mod-full-q 0 s>d , ,
create ud/mod-full-r 0 s>d , ,

: ud/mod-full ( ud1 ud2 -- ud3 ud4 )
    \ Double /mod operator.
    \ Similar to ud/mod, but dealing only in double width integers.
    \ Returns remainder ud3, and quotient ud4.
    \ https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_(unsigned)_with_remainder
    2dup d0= if
        ." Divide by 0 in ud/mod."
        1 (bye)
    then

    0 s>d ud/mod-full-q 2!
    0 s>d ud/mod-full-r 2!

    0 cell-width 2* 1- ?do
        ud/mod-full-r 2@ 1 udlshift
        2rot 2dup 1 s>d i udlshift 2and d0<> if 1 else 0 then
        >r 2-rot r> m+
        4dup du<= if
            2over d-
            ud/mod-full-q 2@ 1 s>d i udlshift 2or ud/mod-full-q 2!
        then
        ud/mod-full-r 2!
    -1 +loop

    2drop 2drop
    ud/mod-full-r 2@ ud/mod-full-q 2@
;

create d/mod-full-d2-abs 0 s>d , ,

: d/mod-full ( d1 d2 -- d3 d4 )
    \ No bounds checking here; maybe don't pass in too large inputs.
    2dup dabs d/mod-full-d2-abs 2!

    4dup d0< dup if >r 2swap dnegate 2swap r> then
    -rot d0< dup if 2rot dnegate 2-rot then
    over <>

    2-rot ud/mod-full 2rot
    if \ (d1 < 0 and d2 >= 0) or (d1 >= 0 and d2 < 0)
        >r
        1 s>d d+ dnegate
        2swap
        d/mod-full-d2-abs 2@ 2swap d-
        2swap
        r>
    then
    if \ (d2 > 0)
        2swap dnegate 2swap
    then
;

create bezout-r-1 0 s>d , ,
create bezout-r0 0 s>d , ,
create bezout-s-1 0 s>d , ,
create bezout-s0 0 s>d , ,
create bezout-t-1 0 s>d , ,
create bezout-t0 0 s>d , ,

: bezout-coefficients ( ud1 ud2 -- d3 d4 )
    \ Calculate Bezout's coefficients of d1 and d2,
    \ using extended euclidean algorithm.
    bezout-r0 2! bezout-r-1 2!
    1 s>d bezout-s-1 2! 0 s>d bezout-s0 2!
    0 s>d bezout-t-1 2! 1 s>d bezout-t0 2!
    begin
        bezout-r-1 2@ bezout-r0 2@ ud/mod-full 2swap
        bezout-r0 2@ bezout-r-1 2!
        bezout-r0 2!

        2dup bezout-s-1 2@ 2swap bezout-s0 2@ d* d-
        bezout-s0 2@ bezout-s-1 2!
        bezout-s0 2!

        bezout-t-1 2@ 2swap bezout-t0 2@ d* d-
        bezout-t0 2@ bezout-t-1 2!
        bezout-t0 2!

        bezout-r0 2@ d0=
    until

    bezout-r-1 2@ 1. d<> if
        ." Pair of inputs are not coprime." cr
        1 (bye)
    then
    bezout-s-1 2@ bezout-t-1 2@
;

create crt-dn1 0 s>d , ,
create crt-da1 0 s>d , ,
create crt-dn2 0 s>d , ,
create crt-da2 0 s>d , ,

: chinese-remainder-theorem ( dn1 da1 dn2 da2 -- dx )
    \ In a system
    \ dx = da1 (mod dn1)
    \ dx = da2 (mod dn2)
    \ Returns dx, (mod (dn1 * dn2))
    crt-da2 2! crt-dn2 2! crt-da1 2! crt-dn1 2!
    crt-dn2 2@ crt-dn1 2@
    bezout-coefficients
    crt-dn1 2@ d* crt-da2 2@ d*
    2swap crt-dn2 2@ d* crt-da1 2@ d*
    d+
    crt-dn1 2@ crt-dn2 2@ d* d/mod-full 2drop
;

create part2-running-mod 0 s>d , ,
create part2-running-remainder 0 s>d , ,

: run-part2 ( -- d )
    \ Run part2 of the puzzle, and return the answer.
    1 s>d part2-running-mod 2! 0 s>d part2-running-remainder 2! \ Prime the running values with x = 0 (mod 1).
    bus-count 0 ?do
        bus-array i bus% %size * + dup bus-id @ swap bus-index @
        \ We're solving the set of congruences:
        \ x + a = 0 mod n
        \ i.e.
        \ x = -a mod n
        \ So we need to negate our bus indices before running the chinese remainder theorem.
        negate swap tuck mod

        swap dup -rot

        part2-running-mod 2@ 2swap part2-running-remainder 2@ 2swap
        s>d rot s>d
        chinese-remainder-theorem

        rot s>d part2-running-mod 2@ d* 2dup part2-running-mod 2!
        ud/mod-full 2drop
        part2-running-remainder 2!
    loop
    part2-running-remainder 2@
;

: load-test-answer ( c-addr u -- d )
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
    0 0 2swap >number 0 <> if
        ." Unable to convert expected test result into a number." cr
        1 (bye)
    then
    drop

    input-fd close-file throw
;

: .-no-space ( n -- )
    \ Display number n without trailling space.
    0 .r
;

: d.-no-space ( d -- )
    \ Display number d without trailling space.
    0 d.r
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
    rot dup n-to-string test-string-buf $+! -rot
    test-string-buf $@ load-test-answer

    2rot swap s" sample" test-string-buf $! n-to-string test-string-buf $+!
    s"  part" test-string-buf $+! n-to-string test-string-buf $+!

    2dup 2rot 2dup 2rot d= if
        test-string-buf $@ type ."  :: PASSED" cr
        2drop 2drop
    else
        test-string-buf $@ type ."  :: FAILED" cr
        2swap .\" \tReturned " d.-no-space ." , expected " d.-no-space ." ." cr
        drop false
    then

    test-string-buf @ free throw 0 test-string-buf !
;

: run-tests ( -- )
    true \ All tests passed.
    1 1 run-test
    1 2 run-test
    2 2 run-test
    3 2 run-test
    4 2 run-test
    5 2 run-test
    6 2 run-test

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
    d. cr
    0 (bye)
;

run-main

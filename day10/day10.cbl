IDENTIFICATION DIVISION.
    PROGRAM-ID. DAY10.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT input-file
            ASSIGN TO DISK input-path
            ORGANIZATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL.
        SELECT test-answer-file
            ASSIGN TO DISK test-answer-path
            ORGANIZATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
    FD input-file.
        01 input-file-line PIC X(32).
    FD test-answer-file.
        01 test-answer-file-line PIC X(32).

WORKING-STORAGE SECTION.
    01 TAB-CHAR PIC X VALUE X'09'.
    01 args.
        02 args-len USAGE BINARY-C-LONG UNSIGNED VALUE 0.
        02 command-name PIC X(32).
        02 input-path PIC X(128).

    01 input-data-length USAGE BINARY-C-LONG VALUE 0.
    01 input-data.
        02 input-entry
            USAGE BINARY-C-LONG UNSIGNED
            OCCURS 0 TO 300 TIMES DEPENDING ON input-data-length
            INDEXED BY input-i input-j.

    01 part1-data.
        02 part1-difference-counts
            USAGE BINARY-C-LONG UNSIGNED
            OCCURS 3 TIMES
            VALUE 0.
        02 part1-current-difference USAGE BINARY-C-LONG UNSIGNED.

    01 part2-data.
        02 part2-paths-to-end
            USAGE BINARY-C-LONG UNSIGNED
            VALUE 0
            OCCURS 0 TO 300 TIMES DEPENDING ON input-data-length.

    01 output-data.
        02 output-result USAGE BINARY-C-LONG UNSIGNED.
        02 output-result-display PIC Z(32).

    01 test-data.
        02 test-all-passed PIC 9 VALUE 1.
        02 test-current-sample PIC X(32).
        02 test-current-part PIC X(32).
        02 test-answer-path PIC X(128).
        02 test-expected-result USAGE BINARY-C-LONG UNSIGNED.
        02 test-expected-result-display PIC Z(32).

PROCEDURE DIVISION.
    ACCEPT args-len FROM ARGUMENT-NUMBER END-ACCEPT
    EVALUATE args-len
        WHEN 1
            DISPLAY 1 UPON ARGUMENT-NUMBER END-DISPLAY
            ACCEPT command-name FROM ARGUMENT-VALUE END-ACCEPT
            IF command-name IS EQUAL TO "test"
                PERFORM tests
            ELSE
                PERFORM print-usage
            END-IF
        WHEN 2
            DISPLAY 1 UPON ARGUMENT-NUMBER END-DISPLAY
            ACCEPT command-name FROM ARGUMENT-VALUE END-ACCEPT
            EVALUATE command-name
                WHEN "part1"
                    DISPLAY 2 UPON ARGUMENT-NUMBER END-DISPLAY
                    ACCEPT input-path FROM ARGUMENT-VALUE END-ACCEPT
                    PERFORM load-input
                    PERFORM part1
                    PERFORM display-result
                WHEN "part2"
                    DISPLAY 2 UPON ARGUMENT-NUMBER END-DISPLAY
                    ACCEPT input-path FROM ARGUMENT-VALUE END-ACCEPT
                    PERFORM load-input
                    PERFORM part2
                    PERFORM display-result
                WHEN OTHER
                    PERFORM print-usage
            END-EVALUATE
        WHEN OTHER
            PERFORM print-usage
    END-EVALUATE
    STOP RUN RETURNING 0
    .

load-input.
    OPEN INPUT input-file
    INITIALIZE input-data-length ALL TO VALUE
    PERFORM UNTIL EXIT
        READ input-file
            AT END EXIT PERFORM
        END-READ
        COMPUTE input-data-length = input-data-length + 1 END-COMPUTE
        COMPUTE input-entry(input-data-length) = FUNCTION NUMVAL(input-file-line) END-COMPUTE
    END-PERFORM
    CLOSE input-file
    .

display-result.
    *> Display result left-justified, with no leading spaces.
    DISPLAY FUNCTION TRIM(output-result-display) END-DISPLAY
    .

part1.
    INITIALIZE part1-data ALL TO VALUE
    SORT input-entry ON ASCENDING KEY input-entry

    *> Initial jump from outlet voltage (0) to the first adapter.
    SET part1-difference-counts(input-entry(1)) UP BY 1
    PERFORM WITH TEST BEFORE VARYING input-i FROM 2 UNTIL input-i > input-data-length
        COMPUTE part1-current-difference = input-entry(input-i) - input-entry(input-i - 1) END-COMPUTE
        SET part1-difference-counts(part1-current-difference) UP BY 1
    END-PERFORM
    *> Final jump from last input adapter to the device's built-in adapter.'
    SET part1-difference-counts(3) UP BY 1

    COMPUTE output-result = part1-difference-counts(1) * part1-difference-counts(3) END-COMPUTE
    MOVE output-result TO output-result-display
    .

part2.
    INITIALIZE part2-data ALL TO VALUE
    SORT input-entry ON ASCENDING KEY input-entry

    MOVE 0 TO output-result
    *> Treat the set of adapters as a DAG.
    *> One node for each adapter, one for the starting outlet voltage, and one for the device's
    *> built-in adapter.
    *> An edge connects n_1 -> n_2 if n_2 can be plugged into n_1, i.e. n_2 is at most 3V higher
    *> than n_1.
    *> A valid configuration of adapters is one for which there is a path through the graph from
    *> the outlet node to the device node.
    *> As voltages are all directly comparable, our sorted input constitutes a lexicographic
    *> ordering of the nodes.
    *> We build a new graph, based on the input graph, where the value of each node is the number of
    *> valid paths from that node to the device node.
    *> The value in this graph of node n_a is the sum of the values of all nodes n_b, where an edge
    *> n_a -> n_b exists.
    PERFORM WITH TEST BEFORE VARYING input-i FROM input-data-length BY -1 UNTIL input-i < 1
        *> Account for edges to the implicit device node.
        IF input-entry(input-i) >= input-entry(input-data-length)
            SET part2-paths-to-end(input-i) UP BY 1
        END-IF
        *> Check other adapter nodes.
        COMPUTE input-j = input-i + 1 END-COMPUTE
        PERFORM WITH TEST BEFORE UNTIL input-j > input-data-length
            IF input-entry(input-j) <= input-entry(input-i) + 3
                SET part2-paths-to-end(input-i) UP BY part2-paths-to-end(input-j)
            ELSE
                EXIT PERFORM
            END-IF
            SET input-j UP BY 1
        END-PERFORM
        *> If an edge to this node from the implicit outlet node exists, add this node's value
        *> to the result.
        IF input-entry(input-i) <= 3
            SET output-result UP BY part2-paths-to-end(input-i)
        END-IF
    END-PERFORM

    MOVE output-result TO output-result-display
    .

tests.
    MOVE "1" TO test-current-sample
    MOVE "1" TO test-current-part
    PERFORM tests-run-one
    MOVE "2" TO test-current-sample
    PERFORM tests-run-one
    MOVE "1" TO test-current-sample
    MOVE "2" TO test-current-part
    PERFORM tests-run-one
    MOVE "2" TO test-current-sample
    PERFORM tests-run-one

    IF test-all-passed IS EQUAL TO 1
        DISPLAY "All tests passed." END-DISPLAY
    END-IF
    .

tests-run-one.
    STRING "data/sample" FUNCTION TRIM(test-current-sample) ".answer" FUNCTION TRIM(test-current-part) INTO test-answer-path END-STRING
    OPEN INPUT test-answer-file
    READ test-answer-file END-READ
    COMPUTE test-expected-result = FUNCTION NUMVAL(test-answer-file-line) END-COMPUTE
    CLOSE test-answer-file
    MOVE test-expected-result TO test-expected-result-display

    STRING "data/sample" FUNCTION TRIM(test-current-sample) INTO input-path END-STRING
    PERFORM load-input
    EVALUATE test-current-part
        WHEN "1"
            PERFORM part1
        WHEN "2"
            PERFORM part2
    END-EVALUATE

    IF output-result IS EQUAL TO test-expected-result
        DISPLAY "Test sample" FUNCTION TRIM(test-current-sample) " part " FUNCTION TRIM(test-current-part) ": PASSED" END-DISPLAY
    ELSE
        DISPLAY "Test sample" FUNCTION TRIM(test-current-sample) " part " FUNCTION TRIM(test-current-part) ": FAILED" END-DISPLAY
        DISPLAY TAB-CHAR "Returned " FUNCTION TRIM(output-result-display) ", expected " FUNCTION TRIM(test-expected-result-display) "." END-DISPLAY
        SET test-all-passed TO 0
    END-IF
    .

print-usage.
    DISPLAY "Invalid arguments." END-DISPLAY
    DISPLAY "Usage:" END-DISPLAY
    DISPLAY TAB-CHAR "./day10 <part1|part2> [input_file]" END-DISPLAY
    DISPLAY TAB-CHAR "./day10 test" END-DISPLAY
    STOP RUN RETURNING 1
    .

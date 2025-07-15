IDENTIFICATION DIVISION.
    PROGRAM-ID. DAY12.

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

    01 input-data.
        02 input-value
            PIC 9(32).

     01 part1-data.
        02 part1-ship-n USAGE BINARY-C-LONG SIGNED VALUE 0.
        02 part1-ship-e USAGE BINARY-C-LONG SIGNED VALUE 0.
        *> Degrees counter-clockwise from east.
        02 part1-ship-direction USAGE BINARY-C-LONG UNSIGNED VALUE 0.

    01 part2-data.
        02 part2-ship-n USAGE BINARY-C-LONG SIGNED VALUE 0.
        02 part2-ship-e USAGE BINARY-C-LONG SIGNED VALUE 0.
        *> Position of the waypoint relative to the ship.
        02 part2-waypoint-n USAGE BINARY-C-LONG SIGNED VALUE 1.
        02 part2-waypoint-e USAGE BINARY-C-LONG SIGNED VALUE 10.
        *> Temp values for calculating ship or waypoint movement.
        02 part2-i USAGE BINARY-C-LONG SIGNED VALUE 0.
        02 part2-j USAGE BINARY-C-LONG SIGNED VALUE 0.

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
                    PERFORM part1
                    PERFORM display-result
                WHEN "part2"
                    DISPLAY 2 UPON ARGUMENT-NUMBER END-DISPLAY
                    ACCEPT input-path FROM ARGUMENT-VALUE END-ACCEPT
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

display-result.
    *> Display result left-justified, with no leading spaces.
    IF output-result IS EQUAL TO 0
        DISPLAY 0 END-DISPLAY
    ELSE
        DISPLAY FUNCTION TRIM(output-result-display) END-DISPLAY
    END-IF
    .

part1.
    INITIALIZE part1-data ALL TO VALUE
    OPEN INPUT input-file

    PERFORM UNTIL EXIT
        READ input-file
            AT END EXIT PERFORM
        END-READ
        MOVE input-file-line(2:) TO input-value
        EVALUATE input-file-line(1:1)
            WHEN "N"
                SET part1-ship-n UP BY input-value
            WHEN "S"
                SET part1-ship-n DOWN BY input-value
            WHEN "E"
                SET part1-ship-e UP BY input-value
            WHEN "W"
                SET part1-ship-e DOWN BY input-value
            WHEN "L"
                SET part1-ship-direction UP BY input-value
                PERFORM WITH TEST BEFORE UNTIL part1-ship-direction < 360
                    SET part1-ship-direction DOWN BY 360
                END-PERFORM
            WHEN "R"
                *> Ensure we avoid unsigned negative overflow.
                SET part1-ship-direction UP BY 360
                SET part1-ship-direction DOWN BY input-value
                PERFORM WITH TEST BEFORE UNTIL part1-ship-direction < 360
                    SET part1-ship-direction DOWN BY 360
                END-PERFORM
            WHEN "F"
                EVALUATE part1-ship-direction
                    WHEN 0
                        SET part1-ship-e UP BY input-value
                    WHEN 90
                        SET part1-ship-n UP BY input-value
                    WHEN 180
                        SET part1-ship-e DOWN BY input-value
                    WHEN 270
                        SET part1-ship-n DOWN BY input-value
                    WHEN OTHER
                        DISPLAY "Unexpected ship direction angle: " part1-ship-direction "." END-DISPLAY
                        CLOSE input-file
                        STOP RUN RETURNING 2
                END-EVALUATE
            WHEN OTHER
                DISPLAY "Invalid initial character '" input-file-line(1:1) "' in input." END-DISPLAY
                CLOSE input-file
                STOP RUN RETURNING 2
        END-EVALUATE
    END-PERFORM

    CLOSE input-file

    COMPUTE output-result = FUNCTION ABS(part1-ship-n) + FUNCTION ABS(part1-ship-e) END-COMPUTE
    MOVE output-result TO output-result-display
    .

part2.
    INITIALIZE part2-data ALL TO VALUE
    OPEN INPUT input-file

    PERFORM UNTIL EXIT
        READ input-file
            AT END EXIT PERFORM
        END-READ
        MOVE input-file-line(2:) TO input-value
        EVALUATE input-file-line(1:1)
            WHEN "N"
                SET part2-waypoint-n UP BY input-value
            WHEN "S"
                SET part2-waypoint-n DOWN BY input-value
            WHEN "E"
                SET part2-waypoint-e UP BY input-value
            WHEN "W"
                SET part2-waypoint-e DOWN BY input-value
            WHEN "L"
                MOVE part2-waypoint-n TO part2-i
                MOVE part2-waypoint-e TO part2-j
                EVALUATE input-value
                    WHEN 0
                        CONTINUE
                    WHEN 90
                        COMPUTE part2-waypoint-n = part2-j END-COMPUTE
                        COMPUTE part2-waypoint-e = -part2-i END-COMPUTE
                    WHEN 180
                        COMPUTE part2-waypoint-n = -part2-i END-COMPUTE
                        COMPUTE part2-waypoint-e = -part2-j END-COMPUTE
                    WHEN 270
                        COMPUTE part2-waypoint-n = -part2-j END-COMPUTE
                        COMPUTE part2-waypoint-e = part2-i END-COMPUTE
                    WHEN OTHER
                        DISPLAY "Unexpected ship direction angle: " input-value "." END-DISPLAY
                        CLOSE input-file
                        STOP RUN RETURNING 2
                END-EVALUATE
            WHEN "R"
                MOVE part2-waypoint-n TO part2-i
                MOVE part2-waypoint-e TO part2-j
                EVALUATE input-value
                    WHEN 0
                        CONTINUE
                    WHEN 90
                        COMPUTE part2-waypoint-n = -part2-j END-COMPUTE
                        COMPUTE part2-waypoint-e = part2-i END-COMPUTE
                    WHEN 180
                        COMPUTE part2-waypoint-n = -part2-i END-COMPUTE
                        COMPUTE part2-waypoint-e = -part2-j END-COMPUTE
                    WHEN 270
                        COMPUTE part2-waypoint-n = part2-j END-COMPUTE
                        COMPUTE part2-waypoint-e = -part2-i END-COMPUTE
                    WHEN OTHER
                        DISPLAY "Unexpected ship direction angle: " input-value "." END-DISPLAY
                        CLOSE input-file
                        STOP RUN RETURNING 2
                END-EVALUATE
            WHEN "F"
                COMPUTE part2-i = part2-waypoint-n * input-value END-COMPUTE
                COMPUTE part2-j = part2-waypoint-e * input-value END-COMPUTE
                SET part2-ship-n UP BY part2-i
                SET part2-ship-e UP BY part2-j
            WHEN OTHER
                DISPLAY "Invalid initial character '" input-file-line(1:1) "' in input." END-DISPLAY
                CLOSE input-file
                STOP RUN RETURNING 2
        END-EVALUATE
    END-PERFORM

    CLOSE input-file

    COMPUTE output-result = FUNCTION ABS(part2-ship-n) + FUNCTION ABS(part2-ship-e) END-COMPUTE
    MOVE output-result TO output-result-display
    .

tests.
    MOVE "1" TO test-current-sample
    MOVE "1" TO test-current-part
    PERFORM tests-run-one
    MOVE "2" TO test-current-part
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
    DISPLAY TAB-CHAR "./day12 <part1|part2> [input_file]" END-DISPLAY
    DISPLAY TAB-CHAR "./day12 test" END-DISPLAY
    STOP RUN RETURNING 1
    .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. aoc_2022-1b.
       AUTHOR. SK.
       DATE-WRITTEN. Oct 7 2023.
       REMARKS. Advent of Code 2022 Day 1 Part 2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ELVES
           ASSIGN TO 'aoc_2022-1.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ELVES.
       01 CALORIES PIC X(10) VALUE ' '.
        
       WORKING-STORAGE SECTION.
       01 WS-EOF                  PIC A(1)  VALUE 'F'.
       01 WS-CALORIES-AS-NUM      PIC 9(10) VALUE 0.
       01 WS-MAX-CALORIES-TABLE.
       05 WS-MAX-CALORIES         PIC 9(12) VALUE 0 OCCURS 4 TIMES.
       01 WS-SUM-3-CALORIES       PIC 9(12).
       01 WS-MAX-CALORIES-TO-DISP PIC Z(11)9(1). *> disp at least 1 zero  

       PROCEDURE DIVISION.
           OPEN INPUT ELVES
           PERFORM UNTIL WS-EOF = 'T'
             READ ELVES
             AT END
               MOVE 'T' TO WS-EOF
 	     NOT AT END
	       IF CALORIES <> ' '
	         MOVE CALORIES TO WS-CALORIES-AS-NUM
	         ADD WS-CALORIES-AS-NUM TO WS-MAX-CALORIES(4) *> use as a buffer
      *	         END-ADD
	       ELSE *> end of this elf's calorie count
    		 SORT WS-MAX-CALORIES ON DESCENDING KEY WS-MAX-CALORIES
	         MOVE 0 TO WS-MAX-CALORIES(4) *> Reset
	       END-IF
 	     END-READ
	   END-PERFORM
           CLOSE ELVES
   	   COMPUTE WS-SUM-3-CALORIES = FUNCTION SUM(WS-MAX-CALORIES(1),
  	     WS-MAX-CALORIES(2), WS-MAX-CALORIES(3))
   	   MOVE WS-SUM-3-CALORIES TO WS-MAX-CALORIES-TO-DISP
  	   DISPLAY  "Total calories of top 3 elves: "
  	     WS-MAX-CALORIES-TO-DISP
           STOP RUN.

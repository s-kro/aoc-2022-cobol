       IDENTIFICATION DIVISION.
       PROGRAM-ID. aoc_2022-1.
       AUTHOR. Stephen Krochenski.
       DATE-WRITTEN. Sep 17 2023.
       REMARKS. Advent of Code 2022 Day 1 Part 1.

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
       01 WS-TOTAL-CALORIES       PIC 9(12) VALUE 0.
       01 WS-MAX-CALORIES         PIC 9(12) VALUE 0.
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
	         ADD WS-CALORIES-AS-NUM TO WS-TOTAL-CALORIES
      *	         END-ADD
      *	         DISPLAY WS-TOTAL-CALORIES
 		 IF WS-TOTAL-CALORIES > WS-MAX-CALORIES
  		   MOVE WS-TOTAL-CALORIES TO WS-MAX-CALORIES
		 END-IF
	       ELSE
	         MOVE 0 TO WS-TOTAL-CALORIES *> Reset
      *		 DISPLAY " "
	       END-IF
 	     END-READ
	   END-PERFORM
           CLOSE ELVES
      *    Format MAX_CALORIES to suppress leading zeros
   	   MOVE WS-MAX-CALORIES TO WS-MAX-CALORIES-TO-DISP
  	   DISPLAY  "Max Calories: " WS-MAX-CALORIES-TO-DISP
           STOP RUN.

>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                         V J U S T L E F T                      *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VJUSTLEFT.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Y    PIC S9(4) COMP-5.
 01 X    PIC S9(4) COMP-5.
 01 NSTRING PIC X(80) VALUE SPACES.
 LINKAGE SECTION.
 01 JSTRING.
     02 JSTR PIC X OCCURS 1 TO 80 DEPENDING ON JLEN.
 01 JLEN    PIC S9(4) COMP-5.
 PROCEDURE DIVISION USING JSTRING JLEN.
 0000-BEGIN.
     MOVE 0 TO X Y.
     MOVE SPACES TO NSTRING.
     PERFORM VARYING X FROM 1 BY 1
       UNTIL X > JLEN
      IF JSTR(X) > SPACES OR Y > 0
       ADD 1 TO Y
       MOVE JSTRING(X:1) TO NSTRING(Y:1)
      END-IF
     END-PERFORM.
     MOVE NSTRING TO JSTRING.
     Goback.

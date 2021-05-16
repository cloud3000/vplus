>>source format free.
*>******************************************************************
*>*                      V J U S T R I G H T                       *
*>******************************************************************
*>* EXAMPLE: CALL "VJUSTRIGHT" USING JSTRING JLEN.
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VJUSTRIGHT.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Z       PIC S9(4) COMP-5.
 01 Y       PIC S9(4) COMP-5.
 01 X       PIC S9(4) COMP-5.
 01 JPTR    PIC S9(4) COMP-5.
 01 NSTRING PIC X(80) VALUE SPACES.
 LINKAGE SECTION.
 01 JSTRING.
     02 JSTR PIC X OCCURS 1 TO 80 DEPENDING ON JLEN.
 01 JLEN    PIC S9(4) COMP-5.
 PROCEDURE DIVISION USING JSTRING JLEN.
 0000-BEGIN.
     MOVE 0 TO X Y Z.
     MOVE SPACES TO NSTRING.
     PERFORM VARYING X FROM JLEN BY -1
       UNTIL X < 1
      IF JSTR(X) > SPACES OR Y > 0
       ADD 1 TO Z
       COMPUTE Y = ((JLEN - Z) + 1) END-COMPUTE
       MOVE JSTRING(X:1) TO NSTRING(Y:1)
      END-IF
     END-PERFORM.
     MOVE NSTRING TO JSTRING.
     Goback.

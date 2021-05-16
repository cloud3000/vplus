>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                 V F I L L T R A I L I N G                      *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VFILLTRAILING.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Y         PIC S9(4) COMP-5.
 01 X         PIC S9(4) COMP-5.
 01 LAST-CHAR PIC X VALUE SPACE.
 01 NSTRING   PIC X(80) VALUE SPACES.
 01 BEGFLD-SW PIC X VALUE "0".
    88 BEGIN-OF-FIELD VALUE "1".

 LINKAGE SECTION.
 01 JCHAR     PIC X.
 01 JSTRING.
     02 JSTR  PIC X OCCURS 1 TO 80 DEPENDING ON JLEN.
 01 JLEN      PIC S9(4) COMP-5.
 PROCEDURE DIVISION USING JCHAR JSTRING JLEN.
 0000-BEGIN.
     MOVE 0 TO X Y.
     MOVE "0" TO BEGFLD-SW.
     MOVE SPACES TO LAST-CHAR.
     PERFORM VARYING X FROM JLEN BY -1
       UNTIL X < 0
      ADD 1 TO Y
      IF BEGIN-OF-FIELD
       OR ((JSTR(X) NOT = JCHAR) AND
           (JSTR(X) NOT = " "))
       MOVE "1" TO BEGFLD-SW
       MOVE JSTR(X) TO NSTRING(Y:1)
      ELSE
       MOVE " " TO NSTRING(Y:1)
      END-IF
     END-PERFORM.
     MOVE NSTRING TO JSTRING
     Goback.
 END PROGRAM VFILLTRAILING.

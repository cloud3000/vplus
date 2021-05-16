>>source format free.
*>*****************************************************************
*>                       V X D B L S P A C E                      *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VXDBLSPACE.
*> CALL "VXDBLSPACE" USING JSTRING JLEN JSPACES.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Y         PIC S9(4) COMP-5.
 01 X         PIC S9(4) COMP-5.
 01 LAST-CHAR PIC X VALUE SPACE.
 01 NSTRING   PIC X(80) VALUE SPACES.
 LINKAGE SECTION.
 01 JSTRING.
     02 JSTR     PIC X OCCURS 1 TO 80 DEPENDING ON JLEN.
 01 JLEN         PIC S9(4) COMP-5.
 01 JSPACES      PIC S9(4) COMP-5.
 PROCEDURE DIVISION USING JSTRING JLEN JSPACES.
 0000-BEGIN.
     MOVE 0 TO X Y.
     MOVE SPACES TO LAST-CHAR.
     PERFORM VARYING X FROM 1 BY 1
       UNTIL X > JLEN
      IF ((JSTR(X) > " ") OR (LAST-CHAR NOT = " "))
       ADD 1 TO Y
       MOVE JSTR(X) TO NSTRING(Y:1)
       MOVE JSTR(X) TO LAST-CHAR
       IF JSTR(X) = " "
        ADD 1 TO JSPACES
       END-IF
      END-IF
     END-PERFORM.
     IF Y > 0 AND Y < JLEN
      MOVE NSTRING TO JSTRING
     END-IF.
     Goback.

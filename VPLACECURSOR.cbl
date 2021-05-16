>>source format free.
*>******************************************************************
*>*                  V P L A C E C U R S O R                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VPLACECURSOR.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 disp-err pic ----9.
 01 X PIC S9(4) COMP-5 VALUE 0.
 01 FLD-FOUND-SW PIC X VALUE "0".
    88 FIELD-FOUND VALUE "1".
 01 My-FieldNum PIC 9(4) COMP-5.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 FIELDNUM PIC S9(4) COMP-5.
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA FIELDNUM.
 0000-BEGIN-VPLACECURSOR.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO INTR-CALLNAME
     string "VPLACECURSOR "
      delimited by "  " into INTR-CALLNAME.
     Move FIELDNUM TO disp-err.


*>
*> Set default values.
     MOVE "0" TO FLD-FOUND-SW.
     MOVE 0 TO CSTATUS.
     MOVE "VPLACECURSOR" TO INTR-ERRNAME.
     MOVE IFLD-NUMBER(1) TO CURSOR-POS-FIELD.
*>
*> Check for negative FIELDNUM
     IF FIELDNUM < 0
      Move FIELDNUM To My-FieldNum
      IF My-FieldNum > 0 and My-FieldNum <= AF-AppFld-Cnt
        MOVE HFLD-NUMBER(My-FieldNum) TO CURSOR-POS-FIELD
        MOVE "1" TO FLD-FOUND-SW
      Else
         MOVE 801 TO CSTATUS
      End-If
     Else
*> FIELDNUM is positive, it must match a field number on the form!
      PERFORM VARYING X FROM 1 BY 1 UNTIL FIELD-FOUND OR  X > AF-AppFld-Cnt
       IF FIELDNUM = HFLD-NUMBER(X)
        MOVE "1" TO FLD-FOUND-SW
        IF HFLD-TYPE(X) NOT = "D "
         MOVE HFLD-NUMBER(X) TO CURSOR-POS-FIELD
        ELSE
         MOVE 800 TO CSTATUS
        END-IF
       END-IF
      END-PERFORM
     End-IF.

     IF NOT FIELD-FOUND
      MOVE 501 TO CSTATUS.
     IF CSTATUS NOT = 0
      MOVE 0 TO CURSOR-POS-FIELD
     End-If.

     Goback.


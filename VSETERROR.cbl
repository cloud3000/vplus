>>source format free.
*>******************************************************************
*>*                         V S E T E R R O R                      *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSETERROR.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01 MYFLDNUM PIC S9(4) COMP-5 VALUE 0.
 01 xfldnum pic ----9.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
 01 DebugBUF     pic x(256)  VALUE SPACES.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01   FLDNUM PIC S9(4) COMP-5.
 01   ERRMSG PIC X(80).
 01   MSGLEN PIC S9(4) COMP-5.
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA FLDNUM ERRMSG MSGLEN.
*>*---------------------------------------------------------*
 0000-BEGIN-VSETERROR.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO INTR-CALLNAME
     string "VSETERROR "
      delimited by "  " into INTR-CALLNAME.

     Move fldnum to xfldnum.
     MOVE FLDNUM TO MYFLDNUM.
     IF FLDNUM < 0
      COMPUTE MYFLDNUM = FLDNUM * -1
      PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
       IF MYFLDNUM = HFLD-SCRNORDER(IFLD-IDX)
        ADD 1         TO NUMERRS
        MOVE ERRMSG   TO HFLD-ERRMSG(IFLD-IDX)
        MOVE MYFLDNUM TO HFLD-ERRFLG(IFLD-IDX)
       END-IF
      END-PERFORM
     ELSE
      MOVE FLDNUM TO MYFLDNUM
      PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
       IF MYFLDNUM = HFLD-NUMBER(IFLD-IDX)
        ADD 1         TO NUMERRS
        MOVE ERRMSG   TO HFLD-ERRMSG(IFLD-IDX)
        MOVE MYFLDNUM TO HFLD-ERRFLG(IFLD-IDX)
       END-IF
      END-PERFORM.
     MOVE 80 TO MSGLEN.
     Goback.

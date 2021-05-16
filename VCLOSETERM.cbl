>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                      V C L O S E T E R M                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VCLOSETERM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01 JCW-PARMS.
    15 JCW-NAME                 PIC X(16) VALUE SPACES.
    15 OPEN-SW                  PIC S9(4) COMP VALUE 0.
       88 TERM-CLOSED             VALUE ZERO.
       88 TERM-OPEN               VALUE 1.
    15 RETURN-STATUS            PIC S9(4) COMP VALUE 0.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1   USAGE POINTER.

*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01  MYTERM-FILE PIC X(36).
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA MYTERM-FILE.
 0000-BEGIN-VCLOSETERM.

     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES TO INTR-CALLNAME
     string "VCLOSETERM "
      delimited by "  " into INTR-CALLNAME.

     move LOW-VALUES to DebugBUF.
     string "VCLOSETERM: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     IF VOPENTERM-SW NOT = "N"
      CALL "SP2" USING SP2-CLEAR-WINDOW SP2-NULL-PARM.
     MOVE SP2-MOUSE-WAIT TO SP2-NP-RET-CODE.
     CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.

     MOVE "N" TO VOPENTERM-SW.
     Goback.

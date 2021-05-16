>>source format free.
*>******************************************************************
*>*                        V T U R N O N                           *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VTURNON.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01 VIEW-FILE-PATH     PIC X(132) VALUE SPACES.
 01 DebugBUF     pic x(256)  VALUE SPACES.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA.
*>*---------------------------------------------------------*
 0000-BEGIN-VTURNON.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

*>*-------------- SETUP SP2-OPEN-FILE PARMS.

*>*>
*>*> debug start
     move LOW-VALUES TO INTR-CALLNAME
     string "VTURNON "
      delimited by " " into INTR-CALLNAME.
     move LOW-VALUES to DebugBUF.
     string "VTURNON: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

*>*>
*>*-------------- SETUP SP2-OPEN-FILE PARMS.
     MOVE LOW-VALUES         TO SP2-FI-DATA.
     ACCEPT VIEW-FILE-PATH FROM ENVIRONMENT "SP2DIR".
     STRING MPE-FORMS-FILE delimited by " "
            ".pan"
      DELIMITED BY SIZE  INTO SP2-FI-NAME.
     Move +1  To  SP2-OPEN-FILE.
     Move +10 To  SP2-FI-LEN-LEN.
     Move +0  To  SP2-FI-NUM-LEN.
     Move +2  To  SP2-FI-CHAR-LEN.
     Move +80 To  SP2-FI-VAR-LEN.
     Move +80 To  SP2-FI-NAME-LEN.

*>*-------------- SP2-OPEN-FILE CALL.
     CALL "SP2" USING SP2-OPEN-FILE SP2-FILE-DEF.

     MOVE LOW-VALUES            TO SP2-WD-DATA.
     MOVE "HPTERM"              TO SP2-WD-NAME.
     MOVE HPAN-COMMENT          TO SP2-WD-TITLE.
     MOVE "m"                   TO SP2-WD-BOR-TYPE.
     COMPUTE SP2-WD-WIDTH = 92 * 8  END-COMPUTE.
     COMPUTE SP2-WD-HEIGHT = 40 * 8 END-COMPUTE.
     MOVE -9999                     TO SP2-WD-ROW.
     MOVE -9999                     TO SP2-WD-COL.
     MOVE 1                     TO SP2-WD-CELL-WIDTH.
     MOVE 2                     TO SP2-WD-CELL-HEIGHT.
     MOVE X"01"                 TO SP2-WD-MORE-OPTIONS.
     MOVE SP2-MOUSE-ARROW TO SP2-NP-RET-CODE.
     MOVE CFNAME TO NFNAME.
     CALL "VGETNEXTFORM" USING COMAREA.
     CALL "VINITFORM" USING COMAREA.

     Goback.

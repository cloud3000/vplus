>>source format free.
*>******************************************************************
*>*                        V O P E N T E R M                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VOPENTERM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
*>*>>>>>>>>>>> 01 JCW-PARMS.
 01 JCW-NAME                 PIC X(16) VALUE SPACES.
 01 OPEN-SW                  PIC S9(4) COMP VALUE 0.
       88 TERM-CLOSED             VALUE ZERO.
       88 TERM-OPEN               VALUE 1.
 01 RETURN-STATUS            PIC S9(4) COMP VALUE 0.

    COPY "vplus/SP250".
 01 GLOBAL-ADDRESS1   USAGE POINTER.
 01 GLOBAL-ADDRESS2   USAGE POINTER.
 01 DebugBUF     pic x(256)  VALUE SPACES.

*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01  LINK-TERM-FILE PIC X(36).
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA LINK-TERM-FILE.
 0000-VOPENTERM.

     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     IF NOT OPENFORMF-CALLED
      INITIALIZE VPLUSMEM.

     IF VOPENTERM-SW = "Y"
        Move LOW-VALUES to DebugBUF
        String
         "VOPENTERM: Called once already." DELIMITED BY size into DebugBUF
        End-String
        CALL "LogDebug" USING DebugBUF
        GO TO END-OF-PROG.

     move LOW-VALUES to DebugBUF.
     string "VOPENTERM: " LINK-TERM-FILE DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     MOVE LOW-VALUES TO JCW-NAME.
     Move 0 To OPEN-SW RETURN-STATUS.
     STRING "TERMSW" DELIMITED BY SIZE INTO JCW-NAME.
     CALL "MPE_FINDJCW" USING JCW-NAME OPEN-SW RETURN-STATUS.
     IF RETURN-STATUS NOT = 0
      MOVE 0 TO OPEN-SW.

     IF TERM-OPEN
      MOVE 0 TO CSTATUS
      MOVE 2 TO FILEN
      MOVE "Y" TO VOPENTERM-SW
      GO TO END-OF-PROG.

     IF LANGUAGE NOT = 0
      MOVE 46 TO CSTATUS
      GO TO END-OF-PROG.

     MOVE LOW-VALUES TO SP2-WD-DATA.
     MOVE "HPTERM"              TO SP2-WD-NAME.
     MOVE HPAN-COMMENT          TO SP2-WD-TITLE.
     MOVE "m"                   TO SP2-WD-BOR-TYPE.
     COMPUTE  SP2-WD-WIDTH = 92 * 8  END-COMPUTE.
     COMPUTE SP2-WD-HEIGHT = 40 * 8  END-COMPUTE.
     MOVE 1                     TO SP2-WD-CELL-WIDTH.
     MOVE 2                     TO SP2-WD-CELL-HEIGHT.
     MOVE -9999                 TO SP2-WD-ROW.
     MOVE -9999                 TO SP2-WD-COL.
     MOVE X"01"                 TO SP2-WD-MORE-OPTIONS.
     CALL "SP2" USING SP2-OPEN-WINDOW SP2-WINDOW-DEF.
     IF SP2-WD-RET-CODE NOT = 0
      MOVE 9 TO CSTATUS
      MOVE 0 TO FILEN
      MOVE "F" TO VOPENTERM-SW
     ELSE
      MOVE 0 TO CSTATUS
      MOVE 2 TO FILEN
      MOVE "Y" TO VOPENTERM-SW.

     MOVE LOW-VALUES TO JCW-NAME.
     Move 0 To RETURN-STATUS.
     MOVE 1 TO OPEN-SW.
     STRING "TERMSW" DELIMITED BY SIZE INTO JCW-NAME.
     CALL "MPE_PUTJCW" USING JCW-NAME OPEN-SW RETURN-STATUS.

 END-OF-PROG.
     MOVE CSTATUS TO INTR-ERRNUM.
     Goback.

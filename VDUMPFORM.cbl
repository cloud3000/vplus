>>source format free.
*>*_________________________________________________________________
*>******************************************************************
*>*                     V D U M P F O R M                      *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VDUMPFORM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA.
*>*---------------------------------------------------------*
 0000-BEGIN-VFINISHFORM.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     MOVE LOW-VALUES         TO SP2-FI-DATA.
     MOVE "w" TO SP2-FI-MODE.
     STRING "DUMPFORM" DELIMITED BY SIZE
      ".pan" DELIMITED BY SIZE INTO SP2-FI-NAME.
     CALL "SP2" USING SP2-CREATE-FILE SP2-FILE-DEF.
     CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM.
     MOVE LOW-VALUES TO SP2-FI-DATA.
     MOVE "w" TO SP2-FI-MODE.
     STRING "DUMPFORM" DELIMITED BY SIZE
      ".pan" DELIMITED BY SIZE INTO SP2-FI-NAME.
     CALL "SP2" USING SP2-OPEN-FILE SP2-FILE-DEF.
     CALL "SP2" USING SP2-WRITE-PANEL SP2-NULL-PARM.
     CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM.
     CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM.
     CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM.
     STOP RUN.
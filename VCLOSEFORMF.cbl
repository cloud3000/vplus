>>source format free.
*>*****************************************************************
*>                   V C L O S E F O R M F                        *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VCLOSEFORMF.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01 DebugBUF     pic x(256)  VALUE SPACES.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
 0000-BEGIN-VCLOSEFORMF.
*>-------------- INITIALIZATION AND PARM-EDITS.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES to DebugBUF.
     string "VCLOSEFORMF: " MPE-FORMS-FILE DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     IF VOPENFORMF-SW NOT = "N"
      CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM.
     MOVE "N"             To VOPENFORMF-SW.
     Goback.

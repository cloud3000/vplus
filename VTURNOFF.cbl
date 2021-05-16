>>source format free.
*>******************************************************************
*>*                          V T U R N O F F                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VTURNOFF.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01 DebugBUF     pic x(256)  VALUE SPACES.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA.
*>*---------------------------------------------------------*
 0000-BEGIN-VTURNOFF.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES TO INTR-CALLNAME
     string "VTURNOFF "
      delimited by " " into INTR-CALLNAME.

     MOVE SP2-MOUSE-WAIT TO SP2-NP-RET-CODE.
     CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.
     Goback.


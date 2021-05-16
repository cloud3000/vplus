>>source format free.
*>******************************************************************
*>*                    V S T E R M I N A T E                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSTERMINATE.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.

 WORKING-STORAGE SECTION.
 01  TMP-PTR USAGE IS POINTER.
   COPY "vplus/SP250".
 01 DISP-ERR        PIC ----9.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 STATUS-CODE PIC S9(4) COMP-5 VALUE 0.
 01 STATUS-FLAG PIC S9(9) COMP-5 VALUE 0.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
 01 TEMP-CALLNAME       PIC X(32)        VALUE SPACES.
 01 TEMP-ERRNAME        PIC X(32)        VALUE SPACES.
 01 TEMP-ERRNUM         PIC S9(4) COMP-5 VALUE 0.
 01 TEMP-ERRLEN         PIC S9(4) COMP-5 VALUE 0.
 01 TEMP-ERRMSG         PIC X(160)       VALUE SPACES.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA.
*>*---------------------------------------------------------*
 0000-VSTERMINATE.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO TEMP-CALLNAME
     string "VSTERMINATE "
      delimited by " " into TEMP-CALLNAME.

     MOVE COMAREA         TO MM-COMAREA
     CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM.
     GOBACK.

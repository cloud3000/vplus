>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                  V G E T K E Y L A B E L S                     *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETKEYLABELS.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 xforg            PIC ----9.
 01 xnumoflabels     PIC ----9.
 01 SP2-PR-ID   PIC S9(4) COMP-5 VALUE 0.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
 01 DebugBUF     pic x(256)  VALUE SPACES.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".

 01 forg         PIC S9(4) COMP-5.
 01 numoflabels  PIC S9(4) COMP-5.
 01 labels.
    02 LINK-LABEL OCCURS 8.
       05 LL-LINE1        PIC X(8).
       05 LL-LINE2        PIC X(8).
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA forg numoflabels labels.
*>---------------------------------------------------------*
 0000-BEGIN-VGETKEYLABELS.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO INTR-CALLNAME
     string "VGETKEYLABELS "
      delimited by "  " into INTR-CALLNAME.

     Move forg     To xforg.
     MOVE SPACES TO LABELS.
     Move numoflabels To xnumoflabels.
     If numoflabels > 8 Move 8 To numoflabels.
     Perform Varying retries from 1 by 1 until retries > numoflabels
       MOVE HOT-KEY-LABEL(retries) TO LINK-LABEL(retries)
     End-perform.
     Goback.

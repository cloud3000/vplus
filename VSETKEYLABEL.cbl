>>source format free.
*>******************************************************************
*>*                    V S E T K E Y L A B E L                     *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSETKEYLABEL.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01 xforg         PIC -9.
 01 xKEYLABEL     PIC -9.
 01 KEYLABEL-STRING.
    05 KL-LINE1 PIC X(8) VALUE SPACES.
    05 KL-CRLF  PIC XX   VALUE X"0D0A".
    05 KL-LINE2 PIC X(8) VALUE SPACES.
 01 SP2-PR-ID   PIC S9(4) COMP-5 VALUE 0.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
 01 DebugBUF     pic x(256)  VALUE SPACES.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 forg          PIC S9(4) COMP-5.
 01 KEYLABEL      PIC S9(4) COMP-5.
 01 LABELX        PIC X(16).
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA forg KEYLABEL LABELX.
*>*---------------------------------------------------------*
 0000-BEGIN-VSETKEYLABEL.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     Move KEYLABEL To XKEYLABEL.

     Move forg     To xforg.
     Move KEYLABEL To XKEYLABEL.
     If KEYLABEL > 8 Move 8 To KEYLABEL.
     If KEYLABEL < 1 Move 1 To KEYLABEL.
      MOVE LABELX TO HOT-KEY-LABEL(KEYLABEL)
     Goback.

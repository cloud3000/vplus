>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                  V G E T F I L E I N F O                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETFILEINFO.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 disp-err pic ----9.
 01 disp-DBL pic ----9.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
 01 DebugBUF     pic x(256)  VALUE SPACES.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 FILE-linkage.
    05 fl-BUFFENTRIES    PIC S9(4)  COMP-5.
    05 fl-ENTRYLEN       PIC S9(4)  COMP-5.
    05 fl-ENTRYTABLE OCCURS 1 TIMES.
       10 fl-VERSION     PIC S9(8)  COMP-5.
       10 fl-NUM-OF-FORMS     PIC S9(4)  COMP-5.
       10 fl-MAX-FIELDS       PIC S9(4)  COMP-5.
       10 fl-MAX-BUFFSIZE     PIC S9(4)  COMP-5.
       10 fl-SAVE-FIELDS      PIC S9(4)  COMP-5.
       10 fl-HEAD-FORM        PIC X(15).
       10 FILLER           PIC X.
       10 fl-ERROR-ENH        PIC X(4).
       10 fl-WINDOW-ENH       PIC X(4).
       10 fl-WINDOW-POSITION  PIC S9(4)  COMP-5.
 01 infobuflen   PIC S9(4) COMP-5.
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA FILE-linkage infobuflen.
*>---------------------------------------------------------*
 0000-BEGIN-VGETFILEINFO.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO INTR-CALLNAME
     string "VGETFILEINFO "
      delimited by "  " into INTR-CALLNAME.

     If VOPENFORMF-SW NOT = "Y"
      MOVE "VGETFILEINFO" TO INTR-ERRNAME
      Move 45             to INTR-ERRNUM
      Move 45 to CSTATUS
      GO TO VGETFILEINFO-RETURN.

     COMPUTE fl-ENTRYLEN = FUNCTION LENGTH(FILE-linkage)
     MOVE  1                       TO fl-BUFFENTRIES.
     Move  IFILE-NBR-OF-FORMS  TO fl-NUM-OF-FORMS(1).
     Move  IFILE-VERSION       TO fl-VERSION(1).
     Move  IFILE-SAVE-FLDS     TO fl-SAVE-FIELDS(1).
     MOVE  IFILE-HEAD-FRM      TO fl-HEAD-FORM(1).
     MOVE  IFILE-ERR-ENH       TO fl-ERROR-ENH(1).
     MOVE  IFILE-WIN-ENH       TO fl-WINDOW-ENH(1).
     MOVE  IFILE-WIN-POS       TO fl-WINDOW-POSITION(1).
     MOVE  IFILE-MAX-FLDS      TO fl-MAX-FIELDS(1).
     MOVE  IFILE-MAX-BUF       TO fl-MAX-BUFFSIZE(1).
 VGETFILEINFO-RETURN.
     Goback.

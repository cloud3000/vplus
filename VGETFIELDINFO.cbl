>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                V G E T F I E L D I N F O                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETFIELDINFO.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  MY-FLDLINK-NUMOFENTRIES  PIC S9(4) COMP-5.
 01  MY-FLDLINK-ENTRYLENGTH   PIC S9(4) COMP-5.
 01  FIELD-NUMBER          PIC S9(4) COMP-5.
 01  NUM-WORK              PIC 9999.
 01  FILLER REDEFINES NUM-WORK.
     02  FILLER            PIC XXX.
     02  LAST-DIGIT        PIC 9.
 01 ENTRYBYTES       pic 9(4) COMP-5.
 01 TBLX             pic 9(4) COMP-5.
 01 NUMIDX           pic 9(4) COMP-5.
 01 NUMCNT           pic 9(4) COMP-5.
 01 NUMERR           pic 9(4) COMP-5.
 01 NUMDEC           pic 9(4) COMP-5.
 01 NUM1 PIC X(14).
 01 NUM2 PIC 9(14).

 01 disp-n1          pic ----9.
 01 disp-n2          pic ----9.
 01 disp-err         pic ----9.
 01 disp-dbl         pic 9(9).
 01 MESSAGE-LENGTH   pic 9(9) COMP-5.
 01 MY-FLDLINK-LEN  pic 9(9) COMP-5.
 01 MY-LEN           pic 9(9) COMP-5.
 01 MY-START         pic 9(9) COMP-5.
 01 X                pic 9(9) COMP-5.
 01 COMP5-dbl        pic 9(9) COMP-5.
 01 TBLIDX           pic 9(9) COMP-5.
 01 TBLCNT           pic 9(9) COMP-5.
 01 MY-DISP-dbl      pic X(9).
 01 X32              pic X(32).
 01 X-dbl           pic X(9).
 01 MY-FLDNUM REDEFINES X-DBL PIC 9(9).
 01  I2             PIC S9(9) COMP-5.

 01 RETURN-DATA.
    02 RD-ENTRY OCCURS 256 TIMES.
         05  RD-FIELDNAME PIC X(16).
         05  RD-FIELD     PIC S9(4) COMP-5.
         05  RD-ORDER     PIC S9(4) COMP-5.
         05  RD-FIELD-LEN PIC S9(4) COMP-5.
         05  RD-OFFSET    PIC S9(4) COMP-5.
         05  RD-ENH       PIC X(4).
         05  RD-DATA-TYPE PIC X(4).
         05  RD-TYPE      PIC XX.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01  FLDLINK.
     02  FLDLINK-NUMOFENTRIES  PIC S9(4) COMP-5.
     02  FLDLINK-ENTRYLENGTH   PIC S9(4) COMP-5.
     02  FLDLINK-FORM          PIC X(16).
     02  FLDLINK-TABLE.
       03 FLDLINK-ENTRIES occurs 1 to 1024
         depending on FLDLINK-NUMOFENTRIES.
          05 FLDLINK-ENTRY.
             10 RETURN-SPACE PIC X(34).
 01  FLDLINK-LEN           PIC S9(4) COMP-5.
      COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA FLDLINK FLDLINK-LEN.
 0000-BEGIN-VGETFIELDINFO.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     MOVE "VGETFIELDINFO"  TO INTR-ERRNAME.

     move LOW-VALUES TO INTR-CALLNAME
     string "VGETFIELDINFO "
      delimited by "  " into INTR-CALLNAME.

     IF NOT GETNEXTFORM-CALLED
        MOVE 132 TO INTR-ERRNUM
        GO TO VGETFIELDINFO-RETURN.

     Perform varying IFLD-IDX from 1 by 1 until IFLD-IDX > AF-AppFld-Cnt
      MOVE HFLD-NAME(IFLD-IDX)      TO RD-FIELDNAME(IFLD-IDX)
      MOVE HFLD-NUMBER(IFLD-IDX)    TO RD-FIELD(IFLD-IDX)
      MOVE HFLD-SCRNORDER(IFLD-IDX) TO RD-ORDER(IFLD-IDX)
      MOVE HFLD-BUFSTART(IFLD-IDX)  TO RD-OFFSET(IFLD-IDX)
      MOVE HFLD-LENGTH(IFLD-IDX)    TO RD-FIELD-LEN(IFLD-IDX)
      MOVE HFLD-ENH(IFLD-IDX)       TO RD-ENH(IFLD-IDX)
      MOVE HFLD-DTYPE(IFLD-IDX)     TO RD-DATA-TYPE(IFLD-IDX)
      MOVE HFLD-TYPE(IFLD-IDX)      TO RD-TYPE(IFLD-IDX)
     end-perform.
*>---
*>---  return data!
     COMPUTE ENTRYBYTES = FLDLINK-ENTRYLENGTH * 2.
     MOVE 0 TO TBLIDX.
     PERFORM VARYING TBLIDX FROM 1 BY 1 UNTIL TBLIDX > AF-AppFld-Cnt
      COMPUTE TBLX
       = ((TBLIDX * ENTRYBYTES) - (ENTRYBYTES - 1))
      END-COMPUTE
      MOVE RD-ENTRY(TBLIDX) TO FLDLINK-TABLE(TBLX:ENTRYBYTES)
     END-PERFORM.
     MOVE AF-AppFld-Cnt TO FLDLINK-NUMOFENTRIES.
     GO TO VGETFIELDINFO-RETURN.
*>---------------------------------------------------------------
 VGETFIELDINFO-RETURN.
     Goback.
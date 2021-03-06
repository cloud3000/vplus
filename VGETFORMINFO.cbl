>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                  V G E T F O R M I N F O                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETFORMINFO.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 LINKLEN   PIC S9(09) COMP-5 VALUE 0.
 01 TABLELEN  PIC S9(09) COMP-5 VALUE 0.
 01 TBLIDX    PIC S9(09) COMP-5 VALUE 0.
 01 TBLX      PIC S9(09) COMP-5 VALUE 0.

 01 MYSP2DIR  PIC X(256) VALUE SPACES.
 01 MYCOMLEN  PIC S9(9) COMP-5 VALUE 0.


 01 fl-ENTRYBYTES    PIC S9(4)  COMP-5.
 01 MY-DATA.
   05 F1-ENTRIES OCCURS 512 TIMES.
       10 F1-NAME          PIC X(15).
       10 FILLER           PIC X.
       10 F1-NUMBER        PIC S9(4)  COMP-5.
       10 F1-NUM-OF-FIELDS PIC S9(4)  COMP-5.
       10 F1-BUF-LENGTH    PIC S9(4)  COMP-5.
       10 F1-NEXT-FORM     PIC X(15).
       10 FILLER           PIC X.
       10 F1-REPEAT-OPTION PIC X.
       10 F1-NFORM-OPTION  PIC X.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 FORM-linkage.
    05 fl-BUFFENTRIES    PIC S9(4)  COMP-5.
    05 fl-ENTRYLEN       PIC S9(4)  COMP-5.
    05 fl-ENTRYTABLE.
     06 F1-ENTRY OCCURS 2 TO 512
     DEPENDING ON  fl-BUFFENTRIES.
      08 F1-DATA  PIC X(40).

 01 infobuflen   PIC S9(4) COMP-5.

    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA FORM-linkage infobuflen.
*>---------------------------------------------------------*
 0000-BEGIN-VGETFORMINFO.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO INTR-CALLNAME
     string "VGETFORMINFO "
      delimited by "  " into INTR-CALLNAME.

     INITIALIZE MY-DATA.
     PERFORM PARMCHECK THRU PARMCHECK-EXIT.
     IF CSTATUS NOT = 0
      Move CSTATUS        to INTR-ERRNUM
      GO TO VGETFORMINFO-RETURN.
     COMPUTE LINKLEN = FUNCTION LENGTH(FORM-linkage).
     COMPUTE TABLELEN = FUNCTION LENGTH(fl-ENTRYTABLE).
     COMPUTE fl-ENTRYBYTES = fl-ENTRYLEN * 2.
     PERFORM VARYING TBLIDX FROM 1 BY 1 UNTIL (TBLIDX > fl-BUFFENTRIES) OR (TBLIDX > IPAN-CNT)
      COMPUTE TBLX = ((TBLIDX * fl-ENTRYBYTES)  - (fl-ENTRYBYTES - 1)) END-COMPUTE
      MOVE fl-ENTRYTABLE(TBLX:fl-ENTRYBYTES) TO F1-ENTRIES(TBLIDX)
*      display "F1-NAME(" TBLIDX ") [" F1-NAME(TBLIDX) "]"
*      display "F1-NUMBER(" TBLIDX ") [" F1-NUMBER(TBLIDX) "]"
      IF ((F1-NAME(TBLIDX) NOT = SPACES) OR (F1-NUMBER(TBLIDX) > 0 AND F1-NUMBER(TBLIDX) <= IFILE-NBR-OF-FORMS))
       PERFORM VARYING IPAN-IDX FROM 1 BY 1
         UNTIL IPAN-IDX > IPAN-CNT
        IF F1-NAME(TBLIDX) = IPAN-NAME(IPAN-IDX) OR F1-NUMBER(TBLIDX) = IPAN-NUMBER(IPAN-IDX)
*         display "a IPAN-NAME(" IPAN-IDX ") [" IPAN-NAME(IPAN-IDX) "]"
*         display "a IPAN-NUMBER(" IPAN-IDX ") [" IPAN-NUMBER(IPAN-IDX) "]"
         MOVE IPAN-NAME(IPAN-IDX)         TO F1-NAME(TBLIDX)
         MOVE IPAN-NUMBER(IPAN-IDX)       TO F1-NUMBER(TBLIDX)
         MOVE IPAN-FLDCNT(IPAN-IDX)       TO F1-NUM-OF-FIELDS(TBLIDX)
         MOVE IPAN-BUF-LEN(IPAN-IDX)      TO F1-BUF-LENGTH(TBLIDX)
         MOVE IPAN-NEXT-FRM(IPAN-IDX)     TO F1-NEXT-FORM(TBLIDX)
         MOVE IPAN-REPEAT-OPT(IPAN-IDX)   TO F1-REPEAT-OPTION(TBLIDX)
         MOVE IPAN-NFORM-OPT(IPAN-IDX)    TO F1-NFORM-OPTION(TBLIDX)
        END-IF
       END-PERFORM
      ELSE
       MOVE IPAN-NAME(TBLIDX)        TO F1-NAME(TBLIDX)
       MOVE IPAN-NUMBER(TBLIDX)      TO F1-NUMBER(TBLIDX)
       MOVE IPAN-FLDCNT(TBLIDX)      TO F1-NUM-OF-FIELDS(TBLIDX)
       MOVE IPAN-BUF-LEN(TBLIDX)     TO F1-BUF-LENGTH(TBLIDX)
       MOVE IPAN-NEXT-FRM(TBLIDX)    TO F1-NEXT-FORM(TBLIDX)
       MOVE IPAN-REPEAT-OPT(TBLIDX)  TO F1-REPEAT-OPTION(TBLIDX)
       MOVE IPAN-NFORM-OPT(TBLIDX)   TO F1-NFORM-OPTION(TBLIDX)
      END-IF
      MOVE F1-ENTRIES(TBLIDX)       TO fl-ENTRYTABLE(TBLX:fl-ENTRYBYTES)
     END-PERFORM.
 VGETFORMINFO-RETURN.
     Goback.
*>----------------------------------------------------------------
 PARMCHECK.
     MOVE "VGETFORMINFO" TO INTR-ERRNAME.
     MOVE LENGTH OF COMAREA TO MYCOMLEN.
     DIVIDE MYCOMLEN BY 2 GIVING MYCOMLEN.
     IF MYCOMLEN < 60
      Move 43             to INTR-ERRNUM
      GO TO PARMCHECK-EXIT.

     IF LANGUAGE NOT = 0
      Move 46             to INTR-ERRNUM
      GO TO PARMCHECK-EXIT.

     If VOPENFORMF-SW NOT = "Y"
      Move 45             to INTR-ERRNUM.
 PARMCHECK-EXIT. EXIT.

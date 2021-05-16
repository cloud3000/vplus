>>source format free.
*>*****************************************************************
*>                 V C H A N G E F I E L D                        *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VCHANGEFIELD.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.

 INPUT-OUTPUT SECTION.
 FILE-CONTROL.

     SELECT TEXT-FILE
        ASSIGN TO TXT-FILENAME
     ORGANIZATION IS LINE SEQUENTIAL.


 DATA DIVISION.
 FILE SECTION.
*>
*>--------- F I L E   D A T A   R E C O R D
 FD  TEXT-FILE.
 01  TEXT-RECORD                 PIC  X(132).

 WORKING-STORAGE SECTION.
 01  DASH-SW         PIC X Value "0".
     88  DASH-FOUND        VALUE "1".

 01 BYTE-IDX        PIC S9(4) COMP-5 VALUE 0.
 01 TEMPLATE-STRING.
    02 TEMPLATE-BYTE   pic x OCCURS 8.

 01 ASCIIVALUES.
    02 ASCIIBYTE1 PIC X VALUE X"00".
    02 ASCIIBYTE2 PIC X VALUE " ".
 01 ASCIIBYTEVAL REDEFINES ASCIIVALUES.
       05 ASCIICHAR pic s9(4) comp-5.

 01 TEMPLATE-VALUE Pic S9(9) COMP-5 value 0.

    COPY "vplus/SP250".
 01  Field-Template             Pic S9(9) Comp-5.
 01  Field-Template-Name        Pic x(8).
 01  HOT-Template-Name          Pic x(8).
 01  TXT-RECORD                  PIC  X(132).
 01 disp-n           pic ---,---,--9.99.
 01 disp-n1          pic ----9.
 01 disp-n2          pic ----9.
 01 disp-err         pic ----9.
 01 disp-dbl         pic 9(9).

 01 WINDOW-DATA.
    05 CNT PIC S9(4) COMP-5 VALUE 0.
    05 IDX PIC S9(4) COMP-5 VALUE 0.
    05 WINDATA-TABLE.
       10 WD-ENTRY OCCURS 10 TIMES.
          15 WD-FLDSUNIQUE PIC S9(9) COMP-5.
          15 WD-FLDNAME    PIC X(16).
          15 WD-Row        PIC 99.
          15 WD-Col        PIC 99.
          15 WD-TYPE       PIC X(4).
 01 MYROW PIC 99 VALUE ZERO.
 01 MYCOL PIC 99 VALUE ZERO.
 01 MY-SD-ID PIC S9(4) COMP-5.
 01 MY-SRNORD PIC S9(4) COMP-5.


 01 UNDERLINE-SW PIC X VALUE "0".
    88 IS-UNDERLINE VALUE "1".
 01 INVERSE-SW   PIC X VALUE "0".
    88 INVERSE   VALUE "1".

 01 CERR PIC S9(4) COMP-5.
 01 NEWPTR PIC S9(4) COMP-5.
 01 THISLEN PIC S9(4) COMP-5.
 01 LASTLEN PIC S9(4) COMP-5.
 01 NEXTLEN PIC S9(4) COMP-5.
 01 sbidx             pic s9(4) comp-5 value 0.
 01 mycomlen          pic s9(4) comp-5 value 0.
 01 my-sblen          pic s9(4) comp-5 value 0.
 01 my-sbcount        pic s9(4) comp-5 value 0.
 01 MY-SBENTLEN        pic s9(4) comp-5 value 0.

 01  MYLEN            PIC S9(4) COMP-5.
 01  MYBUFLEN         PIC S9(4) COMP-5.
 01  MYBUF            PIC X(102) value Spaces.
 01  SAVE-ENH        PIC X(4) Value Spaces.
 01  SAVE-TYPE       PIC X(4) Value Spaces.
 01  SAVE-DATA-TYPE  PIC X(4) Value Spaces.
 01  MY-FIELD-TYPE   PIC X(4) Value Spaces.
     88 Valid-Type
      Values are "CHAR" "DIG" "IMP0" "NUM0" "DMY" "MDY" "YMD"
                 "IMP " "IMP1" "IMP2" "IMP3" "IMP4" "IMP5" "IMP6"
                 "IMP7" "IMP8" "IMP9" "NUM " "NUM0" "NUM1" "NUM2"
                 "NUM3" "NUM4" "NUM5" "NUM6" "NUM7" "NUM8" "NUM9".

 01 TXT-FILENAME pic x(18) value "VCHANGEFIELD.TXT  ".
 01 fd-what      pic x(24) value spaces.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
     COPY "comlib/USERAREA".
     COPY "comlib/DRIVAREA".
 01 SLDEV         PIC 9(4)  value 0.
 01 CONNTYPE      PIC X     value space.
 01 RETURN-ERROR  PIC S9(4) COMP value 0.
 01 Session-Type  Pic XX Value Spaces.
    88 Web-Client     Value "WC".
    88 Thin-Client    Value "TC".
*>########################################################
 LINKAGE SECTION.
*>
    COPY "vplus/COMAREA".
 01  SPECBUF.
     02  SPECBUF-ENTRY OCCURS 384.
         03  SPECBUF-FIELD    PIC S9(4) COMP-5.
         03  SPECBUF-TYPE     PIC S9(4) COMP-5.
         03  SPECBUF-SPEC     PIC X(4).

 01  SPECBUF-ENTRIES       PIC S9(4) COMP-5.
*>
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA SPECBUF SPECBUF-ENTRIES.
 0000-BEGIN-CHANGEFIELD.
*>-------------- INITIALIZATION AND PARM-EDITS.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     If Not INITFORM-CALLED
        move LOW-VALUES to DebugBUF
        String "VCHANGEFIELD Warning: "
           "VINITFORM was not called since the last VGETNEXTFORM"
           Delimited by size into DebugBUF
        End-String
        MOVE 1         TO INTR-ERRNUM
        Move DebugBUF  TO INTR-ERRMSG
        CALL "LogDebug" USING DebugBUF
     End-If.

     Perform PARMCHECK Thru PARMCHECK-EXIT.
     If INTR-ERRNUM Not = 0
      Go To BAD-ENDING.

     Move 0 To IFLD-PTR.
     Perform Varying sbidx from 1 by 1 Until (sbidx > SPECBUF-ENTRIES)

      Perform Varying IFLD-IDX from 1 by 1 Until (IFLD-IDX > AF-AppFld-Cnt)
       IF HFLD-TYPE(IFLD-IDX) NOT = "FK"
        IF HFLD-SCRNORDER(IFLD-IDX) > 0
         COMPUTE MY-SRNORD = HFLD-SCRNORDER(IFLD-IDX) * -1 END-COMPUTE
        ELSE
         MOVE HFLD-SCRNORDER(IFLD-IDX) TO MY-SRNORD
        END-IF
        If ((SPECBUF-FIELD(sbidx) > 0) And (SPECBUF-FIELD(sbidx) = HFLD-NUMBER(IFLD-IDX)))
        Or ((SPECBUF-FIELD(sbidx) < 0) And (SPECBUF-FIELD(sbidx) = MY-SRNORD))
         Move IFLD-IDX to IFLD-PTR
          Perform CHANGEFIELD Thru CHANGEFIELD-Exit
          Move AF-AppFld-Cnt To IFLD-IDX
        End-If
       End-If
      End-Perform
     End-Perform.
     Go To VCHANGEFIELD-RETURN.
*>-----------------------------------------------------------
 PARMCHECK.
     If cstatus Not = 0
      Move 0 To cstatus.

     MOVE "VCHANGEFIELD" TO INTR-CALLNAME.
     INITIALIZE INTR-ERRNUM INTR-ERRMSG.
     MOVE LENGTH OF COMAREA TO MYCOMLEN.
     DIVIDE MYCOMLEN BY 2 GIVING MYCOMLEN.
     MOVE "VCHANGEFIELD"  TO INTR-ERRNAME.

     IF MYCOMLEN < 60
      String "Comarea length may not be less than 60."
       Delimited by size into INTR-ERRMSG
      MOVE 43              TO INTR-ERRNUM
      GO TO PARMCHECK-EXIT.

     IF LANGUAGE NOT = 0
      String "Unrecognized Comarea Language code passed."
       Delimited by size into INTR-ERRMSG
      MOVE 46              TO INTR-ERRNUM
      GO TO PARMCHECK-EXIT.

     If VOPENFORMF-SW NOT = "Y"
      String "FORMSFILE has not yet been opened."
       Delimited by size into INTR-ERRMSG
      Move 45              to INTR-ERRNUM
      GO TO PARMCHECK-EXIT.

     Compute My-sbentlen = Function Length(SPECBUF-ENTRY(1)).
     If My-sbentlen > 0
      Compute My-sblen
       = My-sbentlen * SPECBUF-ENTRIES
      END-COMPUTE
      Compute My-sbcount = My-sblen / My-sbentlen.

     If My-sbcount < 1 or My-Sbcount > 384
      Move 856             to INTR-ERRNUM
      GO TO PARMCHECK-EXIT.
*>
*> CHECK THE CONTENTS OF THE SPECBUF PASSED FROM THE APPLICATION.
*> SPECBUF LOOP
     Perform Varying sbidx from 1 by 1 Until (sbidx > My-sbcount) or (INTR-ERRNUM Not = 0)
      If SPECBUF-TYPE(sbidx) < 1 or SPECBUF-TYPE(sbidx) > 6
       Move 856             to INTR-ERRNUM
      End-If
      Move 0 To IFLD-PTR
*> MATCH FIELD NUMBER LOOP
      Perform Varying IFLD-IDX from 1 by 1
        Until (IFLD-IDX > AF-AppFld-Cnt) OR (IFLD-PTR Not = 0) OR (INTR-ERRNUM Not = 0)
       IF HFLD-TYPE(IFLD-IDX) NOT = "FK"
        IF HFLD-SCRNORDER(IFLD-IDX) > 0
         COMPUTE MY-SRNORD
          = HFLD-SCRNORDER(IFLD-IDX) * -1
         END-COMPUTE
        ELSE
         MOVE HFLD-SCRNORDER(IFLD-IDX) TO MY-SRNORD
        END-IF
        If ((SPECBUF-FIELD(sbidx) > 0) And (SPECBUF-FIELD(sbidx) = HFLD-NUMBER(IFLD-IDX)))
        Or ((SPECBUF-FIELD(sbidx) < 0) And (SPECBUF-FIELD(sbidx) = MY-SRNORD))
         Move IFLD-IDX to IFLD-PTR
        End-If
       End-If
      End-Perform
      if ifld-idx <= AF-AppFld-Cnt
       If IFLD-PTR = 0
        Move 500             to INTR-ERRNUM
       End-If
       If SPECBUF-TYPE(sbidx) = 1 or 4
        MOVE SPACES TO HOT-TEMPLATE-NAME
        STRING SPECBUF-SPEC(SBIDX) DELIMITED BY SPACE
                "-"                   DELIMITED BY SIZE
               HFLD-TYPE(IFLD-PTR) DELIMITED BY SPACE
          INTO HOT-TEMPLATE-NAME
        END-STRING
        Perform HOT-Template
        If Field-Template  = 0
         Move 852             to INTR-ERRNUM
        End-If
       End-If
       If SPECBUF-TYPE(sbidx) = 2 or 5
        IF SPECBUF-SPEC(sbidx)(1:1) NOT = "O"
                       AND "D" and "P" and "R"
         Move 850             to INTR-ERRNUM
        End-If
       End-If
       If SPECBUF-TYPE(sbidx) = 3 or 6
        Move SPECBUF-SPEC(sbidx) To My-Field-Type
        If NOT Valid-Type
         Move 851             to INTR-ERRNUM
        End-If
       End-If
      end-if
     End-Perform.

 PARMCHECK-EXIT.
     EXIT.

*>----------------------------------------------------------
 BAD-ENDING.
     MOVE INTR-ERRNUM TO CSTATUS
     MOVE COMAREA TO MM-COMAREA.
*>-----------------------------------------------------------
 VCHANGEFIELD-RETURN.
     Goback.
*>----------------------------------------------------------------
 CHANGEFIELD.
     MOVE HFLD-SCRNORDER(IFLD-PTR) TO PREVIOUS-SCRNORDER(IFLD-PTR).

     MOVE HFLD-NUMBER(IFLD-PTR) TO PREVIOUS-NUMBER(IFLD-PTR).

     If SPECBUF-TYPE(sbidx) = 1 or 4
      Perform Change-Enhancement
      GO TO CHANGEFIELD-EXIT.
     If SPECBUF-TYPE(sbidx) = 2 or 5
      Perform Change-Type
      GO TO CHANGEFIELD-EXIT.
     If SPECBUF-TYPE(sbidx) = 3 or 6
      Perform Change-Datatype.
 CHANGEFIELD-EXIT.
     Exit.
*>
*>----------------------------------------------------------------
 Change-Enhancement.
     move LOW-VALUES to END-MSG.

     MOVE HFLD-ENH(IFLD-PTR)  TO PREVIOUS-ENH(IFLD-PTR).
     Move SPECBUF-SPEC(sbidx) to HFLD-ENH(IFLD-PTR).
     If SPECBUF-TYPE(sbidx) = 1
      Move PREVIOUS-ENH(IFLD-PTR) TO SPECBUF-SPEC(sbidx).
*>----------------------------------------------------------------
 Change-Type.
     move LOW-VALUES to END-MSG.
     MOVE HFLD-TYPE(IFLD-PTR) TO PREVIOUS-TYPE(IFLD-PTR).
     Move SPECBUF-SPEC(sbidx) to HFLD-TYPE(IFLD-PTR).
     If SPECBUF-TYPE(sbidx) = 2
      MOVE PREVIOUS-TYPE(IFLD-PTR) to SPECBUF-SPEC(sbidx).
*>---------------------------------------------------------------
 Change-Datatype.
     move LOW-VALUES to END-MSG.
     MOVE HFLD-DTYPE(IFLD-PTR) TO PREVIOUS-DTYPE(IFLD-PTR).
     Move SPECBUF-SPEC(sbidx)  to HFLD-DTYPE(IFLD-PTR).
     If SPECBUF-TYPE(sbidx) = 3
      MOVE PREVIOUS-DTYPE(IFLD-PTR) to SPECBUF-SPEC(sbidx).
*>-----------------------------------------------
 HOT-Template.
     Move HOT-TEMPLATE-NAME To TEMPLATE-STRING.
     MOVE SPACES TO Field-Template-Name.
     Move 0 To IFD-IDX Field-Template.
     Perform GET-TEMPLATE-VALUE.

     If TEMPLATE-VALUE > 0
      Perform Varying IFD-IDX from 1 by 1 Until IFD-IDX > IFD-CNT Or Field-Template-Name > Spaces
       If IFD-TEMPLATE-ID(IFD-IDX) = TEMPLATE-VALUE
        Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
        Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       End-If
      End-Perform.

     If Field-Template  = 0
      DISPLAY HOT-TEMPLATE-NAME " NOT FOUND"
      Move "NONED" To Field-Template-Name.
*>-----------------------------------------------------------
 GET-TEMPLATE-VALUE.
     INITIALIZE BYTE-IDX TEMPLATE-VALUE.
     MOVE "0" TO DASH-SW.
     PERFORM VARYING BYTE-IDX FROM 1 BY 1 UNTIL BYTE-IDX > 8

      IF TEMPLATE-BYTE(BYTE-IDX) = "-"
       MOVE "1" TO DASH-SW
      END-IF

      IF TEMPLATE-BYTE(BYTE-IDX) > " " AND DASH-SW = "0"
       MOVE TEMPLATE-BYTE(BYTE-IDX) TO ASCIIBYTE2
       Add ASCIICHAR To TEMPLATE-VALUE
      END-IF

      IF TEMPLATE-BYTE(BYTE-IDX) > " " AND DASH-SW = "1"
       MOVE TEMPLATE-BYTE(BYTE-IDX) TO ASCIIBYTE2
       COMPUTE TEMPLATE-VALUE = TEMPLATE-VALUE * ASCIICHAR END-COMPUTE
      END-IF

     END-PERFORM.


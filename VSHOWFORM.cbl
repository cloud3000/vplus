>>source format free.
*>*****************************************************************
*>                        V S H O W F O R M                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSHOWFORM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
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
    COPY "vplus/TCLDBMEM".
 01  NEXT-CURSOR-POS            PIC S9(4) COMP-5 VALUE 0.
 01  BUFLEN                     PIC S9(4) COMP-5 VALUE 150.
 01  ENHIDX2                    PIC S9(4) COMP-5 VALUE 150.
 01  ENHPTR2                    PIC S9(4) COMP-5 VALUE 150.
 01  TXT2                       PIC X(80).
 01  CNT                        PIC S9(4) COMP-5 VALUE 150.
 01  Field-Template             Pic S9(9) Comp-5 VALUE 0.
 01  Field-Template-Name        Pic x(8).
 01  HOT-Template-Name          Pic x(8).
 01  MYROW                      PIC 99 VALUE ZERO.
 01  DISP-9                     PIC ----9.
 01  MYCOL                      PIC 99 VALUE ZERO.
 01  MY-SD-ID                   PIC S9(4) COMP-5 VALUE 0.
 01  NEWPTR                     PIC S9(4) COMP-5 VALUE 0.
 01  THISLEN                    PIC S9(4) COMP-5 VALUE 0.
 01  LASTLEN                    PIC S9(4) COMP-5 VALUE 0.
 01  NEXTLEN                    PIC S9(4) COMP-5 VALUE 0.
 01  PutBufNumCnt               PIC S9(4) COMP-5 VALUE 0.
 01  PutBufAlphaCnt             PIC S9(4) COMP-5 VALUE 0.
 01  PutBufSpaceCnt             PIC S9(4) COMP-5 VALUE 0.

 01  ESCAPE-TABLE.
     05 BUFCNT                  PIC S9(4) COMP-5.
     05 BUFIDX                  PIC S9(4) COMP-5.
     05 ESCCNT                  PIC S9(4) COMP-5.
     05 ESCIDX                  PIC S9(4) COMP-5.
     05 ESCPTR                  PIC S9(4) COMP-5.
     05 ESCAPE-ENTRY OCCURS 20 TIMES.
        10 ESC                  PIC X(4).
        10 ESCCOL               PIC 9(3).
        10 ENH                  PIC X.
        10 TXT                  PIC X(80).
        10 TXLEN                PIC S9(4) COMP-5.
        10 TXSTRT               PIC S9(4) COMP-5.
 01  MYLEN                      PIC S9(4) COMP-5.
 01  MYBUFLEN                   PIC S9(4) COMP-5.
 01  MYBUF                      PIC X(150) value Spaces.
 01  MY-WINDOWENH               PIC XX VALUE SPACES.
 01 TCLFLD-FM-UNIQUE    PIC ZZZZZZZZ9 VALUE ZERO.
 01 TCLFLD-NAME         PIC X(16)     VALUE SPACES.
 01 TCLFLD-TYPE         PIC X(2)      VALUE SPACES.
 01 TCLFLD-DTYPE        PIC X(4)      VALUE SPACES.
 01 TCLFLD-LENGTH       PIC ZZZ9      VALUE ZERO.

 01 TCL-EVAL-STR pic x(2048) VALUE LOW-VALUES.
 01 TCL-VARVALUE pic x(80)   VALUE SPACES.


 01  NIBBLE-WORK.
     02 NIBBLE-1                PIC X.
     02 NIBBLE-2                PIC X.
 01  NIBBLE-WORK-I REDEFINES NIBBLE-WORK.
     02 NIBBLE-WORK-NUMBER PIC S9(4) COMP-5.
 01  ENHCHAR                    PIC X VALUE SPACES.
 01  CHARACTER-Testing.
     05 CharTest                pic x.
 01  ENH-CONVERSION-TABLE.
     05 FILLER PIC X VALUE "@".
     05 FILLER PIC X(5) VALUE "NONE ".
     05 FILLER PIC X VALUE "A".
     05 FILLER PIC X(5) VALUE "B    ".
     05 FILLER PIC X VALUE "B".
     05 FILLER PIC X(5) VALUE "I    ".
     05 FILLER PIC X VALUE "C".
     05 FILLER PIC X(5) VALUE "IB   ".
     05 FILLER PIC X VALUE "D".
     05 FILLER PIC X(5) VALUE "U    ".
     05 FILLER PIC X VALUE "E".
     05 FILLER PIC X(5) VALUE "UB   ".
     05 FILLER PIC X VALUE "F".
     05 FILLER PIC X(5) VALUE "UI   ".
     05 FILLER PIC X VALUE "G".
     05 FILLER PIC X(5) VALUE "UIB  ".
     05 FILLER PIC X VALUE "H".
     05 FILLER PIC X(5) VALUE "H    ".
     05 FILLER PIC X VALUE "I".
     05 FILLER PIC X(5) VALUE "HB   ".
     05 FILLER PIC X VALUE "J".
     05 FILLER PIC X(5) VALUE "HI   ".
     05 FILLER PIC X VALUE "K".
     05 FILLER PIC X(5) VALUE "HIB  ".
     05 FILLER PIC X VALUE "L".
     05 FILLER PIC X(5) VALUE "HU   ".
     05 FILLER PIC X VALUE "M".
     05 FILLER PIC X(5) VALUE "HUB  ".
     05 FILLER PIC X VALUE "N".
     05 FILLER PIC X(5) VALUE "HUI  ".
     05 FILLER PIC X VALUE "O".
     05 FILLER PIC X(5) VALUE "HUIB ".
 01  ENH-CONVERSION-LIST REDEFINES  ENH-CONVERSION-TABLE.
     05 ECL-ENTRY OCCURS 16.
        10 ECL-CHAR PIC X.
        10 ECL-ENH  PIC X(5).
 01  MY-PROPERTY.
     05  MY-PR-RET-CODE         PIC S9(4) COMP-5.
     05  MY-PR-LENS.
         10  MY-PR-LEN-LEN      PIC S9(4) COMP-5 VALUE +18.
         10  MY-PR-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
         10  MY-PR-CHAR-LEN     PIC S9(4) COMP-5 VALUE +20.
         10  MY-PR-VAR-LEN      PIC S9(4) COMP-5 VALUE +2000.
         10  FILLER              PIC S9(4) COMP-5 VALUE +0.
         10  MY-PR-LENL-LEN     PIC S9(4) COMP-5 VALUE -1.
         10  MY-PR-NUML-LEN     PIC S9(4) COMP-5 VALUE +0.
         10  MY-PR-VAR-LEN-L    PIC S9(8) COMP-5 VALUE +0.
     05  MY-PR-DATA.
*>******** MY-PR-NUM-DATA ********
         10  MY-PR-ID           PIC S9(4) COMP-5.
         10  MY-PR-ROW          PIC S9(4) COMP-5.
         10  MY-PR-COL          PIC S9(4) COMP-5.
*>******** MY-PR-CHAR-DATA *******
         10  MY-PR-KEY.
             15  MY-PR-OBJECT-TYPE
                                 PIC X.
                 88  MY-PR-WINDOW   VALUE "W".
                 88  MY-PR-PANEL    VALUE "P".
                 88  MY-PR-STATIC   VALUE "S".
                 88  MY-PR-FIELD    VALUE "F".
                 88  MY-PR-GROUP    VALUE "G".
                 88  MY-PR-REPEAT   VALUE "R".
             15  MY-PR-TYPE     PIC X.
                 88  MY-PR-LEN-T    VALUE "L".
                 88  MY-PR-NUM-T    VALUE "N".
                 88  MY-PR-CHAR-T   VALUE "C".
                 88  MY-PR-VAR-T    VALUE "V".
             15  MY-PR-VAR-TYPE PIC X.
                 88  MY-PR-VAR-1    VALUE "A".
                 88  MY-PR-VAR-2    VALUE "B".
                 88  MY-PR-VAR-3    VALUE "C".
                 88  MY-PR-VAR-4    VALUE "D".
                 88  MY-PR-VAR-5    VALUE "E".
                 88  MY-PR-VAR-6    VALUE "F".
                 88  MY-PR-VAR-7    VALUE "G".
                 88  MY-PR-VAR-8    VALUE "H".
                 88  MY-PR-VAR-9    VALUE "I".
                 88  MY-PR-VAR-10   VALUE "J".
             15  MY-PR-OFFSET   PIC 9(5).
             15  MY-PR-LEN      PIC 9(5).
             15  MY-PR-FORMAT   PIC X.
                 88  MY-PR-NUMBER   VALUE "N".
                 88  MY-PR-BINARY   VALUE "B".
                 88  MY-PR-DECIMAL  VALUE "D".
                 88  MY-PR-LONG     VALUE "L".
             15  MY-PR-ACTION   PIC X.
                 88  MY-PR-REDRAW   VALUE "R".
                 88  MY-PR-RECREATE VALUE "C".
             15  MY-PR-VAR-ACT  PIC X.
                 88  MY-PR-RESET-LEN
                                     VALUE "L".
         10  FILLER              PIC X(4).
*>******** MY-PR-VAR-DATA ********
         10  MY-PR-VALUE        PIC X(2000).
         10  MY-PR-NUM-VALUE REDEFINES MY-PR-VALUE
                                 PIC 9(5).
         10  MY-PR-BIN-VALUE REDEFINES MY-PR-VALUE.
             15  MY-PR-BIT-VALUE OCCURS 8
                                 PIC X.
         10  MY-PR-LONG-VALUE REDEFINES MY-PR-VALUE
                                 PIC 9(8).
*>******** MY-PR-VAR-DATA-L ******
*>******** 10  MY-PR-VALUE-L *****

 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
 0000-BEGIN-SHOWFORM.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.
     If Not INITFORM-CALLED
        Move SPACES To INTR-ERRMSG
        String "SHOWFORM Error: "
           "VINITFORM was not called since the last VGETNEXTFORM"
           Delimited by size into INTR-ERRMSG
        End-String
        MOVE 1              TO INTR-ERRNUM
        Go TO SHOWFORM-RETURN.

     Move 0 To INTR-ERRNUM.

     move LOW-VALUES TO INTR-CALLNAME
     string "VSHOWFORM "
      delimited by " " into INTR-CALLNAME.


*>   Display "VSHOWFORM: Adding template data to Field user-data".
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
      perform SET-FIELD-USERDATA
     END-PERFORM.
*>   Display "VSHOWFORM: FINISHED Adding template data to Field user-data".

     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
      IF (CURRENT-ENH(IFLD-IDX)  NOT = HFLD-ENH(IFLD-IDX))
      OR (CURRENT-TYPE(IFLD-IDX) NOT = HFLD-TYPE(IFLD-IDX))
       MOVE HFLD-ENH(IFLD-IDX)   TO CURRENT-ENH(IFLD-IDX)
       MOVE HFLD-TYPE(IFLD-IDX)  TO CURRENT-TYPE(IFLD-IDX)
       PERFORM Set-Field-Template
       PERFORM CHANGE-Data-Field
      END-IF
      IF CURRENT-DTYPE(IFLD-IDX) NOT = HFLD-DTYPE(IFLD-IDX)
       MOVE HFLD-DTYPE(IFLD-IDX) TO CURRENT-DTYPE(IFLD-IDX)
      END-IF
     END-PERFORM.

*>
*> Apply any function key label changes
     PERFORM VARYING FKEY-IDX FROM 1 BY 1
       UNTIL FKEY-IDX > FKEY-CNT
       MOVE HOT-KEY-ENTRY(FKEY-IDX) TO CUR-KEY-ENTRY(FKEY-IDX)
       PERFORM SET-FKEY-LABEL
     END-PERFORM.
*>
*> Remove message window fields
     move low-values to sp2-sd-data.
     move 700 to sp2-sd-id.
     perform until sp2-sd-id > 706
      call "SP2" using sp2-delete-static sp2-static-def
      add 1 to sp2-sd-id
     end-perform.
*>
*> Mark any fields with VFIELDEDIT errors.
*> This is done from the bottom up so that the error msg set in
*> the PUTWINDOW-BUFFER ends up to be the first field in an
*> error state from top down.
*>
     PERFORM VARYING IFLD-IDX FROM AF-AppFld-Cnt BY -1 UNTIL IFLD-IDX < 1
      IF HFLD-ERRFLG(IFLD-IDX) > 0
       MOVE HFLD-ERRMSG(IFLD-IDX) TO PUTWINDOW-BUFFER
       MOVE HFLD-NUMBER(IFLD-IDX) TO CURSOR-POS-FIELD
       IF MARKED-FIELD(IFLD-IDX) NOT = "1"
        MOVE "1" TO MARKED-FIELD(IFLD-IDX)
        MOVE LOW-VALUES              TO SP2-FD-DATA
        MOVE HFLD-NUMBER(IFLD-IDX)   TO SP2-FD-ID
        CALL "SP2" USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
        MOVE X"21"                   TO SP2-FD-COLR
        CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
        Perform Set-WebClient-Errorflag
       END-IF
      ELSE
       IF MARKED-FIELD(IFLD-IDX) = "1"
        MOVE "0" TO MARKED-FIELD(IFLD-IDX)
        MOVE LOW-VALUES              TO SP2-FD-DATA
        MOVE HFLD-NUMBER(IFLD-IDX)   TO SP2-FD-ID
        CALL "SP2" USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
        PERFORM Set-Field-Template
        MOVE IFD-COLR(IFD-PTR)       TO SP2-FD-COLR
        CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
       END-IF
      END-IF
     END-PERFORM.
*>
*> APPLY message window (IF PUTWINDOW-BUFFER NOT = SPACES)
     IF PUTWINDOW-BUFFER = SPACES
      GO TO SHOWFORM-RETURN.

     MOVE PUTWINDOW-BUFFER  TO MYBUF.
     MOVE SPACES TO PUTWINDOW-BUFFER.
     PERFORM GET-VWINDOW-DATA.
     perform Banner-Extract.
     Display "VSHOWFORM Esc=" ESCCNT
             " Num=" PutBufNumCnt
             " Alph=" PutBufAlphaCnt
             " Space=" PutBufSpaceCnt.


     IF ESCCNT > 0
      PERFORM SCREEN-BANNER
     ELSE
      Display "VSHOWFORM PUTWINDOW(1:6) [" MYBUF(1:6) "]"
      PERFORM VSP2-PUTWINDOW
     END-IF.
     MOVE "Y" TO VSHOWFORM-SW.

*>---------------------------------------------------------------
 SHOWFORM-RETURN.

     MOVE INTR-ERRNUM TO CSTATUS
     MOVE COMAREA TO MM-COMAREA.
     IF CSTATUS = 0
        COMPUTE TERMOPTIONS = MM-TERMOPTIONS + 2
        MOVE CURSOR-POS-FIELD TO NEXT-CURSOR-POS
        Call "VREADFIELDS" USING COMAREA
        MOVE NEXT-CURSOR-POS  TO CURSOR-POS-FIELD
        COMPUTE TERMOPTIONS = MM-TERMOPTIONS - 2
     End-IF.

     GOBACK.

*>---------------------------------------------------------------
 Set-WINDOW-Template.
     EVALUATE MY-WINDOWENH
      WHEN "64" MOVE "@ " TO NIBBLE-WORK
      WHEN "65" MOVE "A " TO NIBBLE-WORK
      WHEN "66" MOVE "B " TO NIBBLE-WORK
      WHEN "67" MOVE "C " TO NIBBLE-WORK
      WHEN "68" MOVE "D " TO NIBBLE-WORK
      WHEN "69" MOVE "E " TO NIBBLE-WORK
      WHEN "70" MOVE "F " TO NIBBLE-WORK
      WHEN "71" MOVE "G " TO NIBBLE-WORK
      WHEN "72" MOVE "H " TO NIBBLE-WORK
      WHEN "73" MOVE "I " TO NIBBLE-WORK
      WHEN "74" MOVE "J " TO NIBBLE-WORK
      WHEN "75" MOVE "K " TO NIBBLE-WORK
      WHEN "76" MOVE "L " TO NIBBLE-WORK
      WHEN "77" MOVE "M " TO NIBBLE-WORK
      WHEN "78" MOVE "N " TO NIBBLE-WORK
      WHEN "79" MOVE "O " TO NIBBLE-WORK
      WHEN OTHER
        MOVE MY-WINDOWENH TO NIBBLE-WORK.
     MOVE NIBBLE-2 TO ENHCHAR.
     IF NIBBLE-1 > NIBBLE-2
      MOVE NIBBLE-1 TO ENHCHAR.
     MOVE 0 TO ENHPTR2.
     PERFORM VARYING ENHIDX2 FROM 1 BY 1
       UNTIL ENHIDX2 > 16 OR ENHPTR2 > 0
      IF ECL-CHAR(ENHIDX2) = ENHCHAR
       MOVE ENHIDX2 TO ENHPTR2
      END-IF
     END-PERFORM.
     INITIALIZE HOT-TEMPLATE-NAME.
     STRING ECL-ENH(ENHPTR2) DELIMITED BY " "
            "-M"             DELIMITED BY SIZE
      INTO HOT-TEMPLATE-NAME.
     MOVE 0 TO IFD-PTR.
     Move 0 To Field-Template.
     Perform Varying IFD-IDX from 1 by 1
       Until IFD-IDX > IFD-CNT OR IFD-PTR > 0
      If IFD-TEMPLATE-NAME(IFD-IDX) = HOT-TEMPLATE-NAME
       Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
       Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       MOVE IFD-IDX TO IFD-PTR
      End-If
     End-Perform.
     If Field-Template  = 0
      Move "?" To Field-Template-Name.
     MOVE Field-Template-Name TO HOT-TEMPLATE-NAME.

*>---------------------------------------------------------------
 Set-FIELD-Template.
     INITIALIZE HOT-TEMPLATE-NAME.
     STRING  HFLD-ENH(IFLD-IDX) DELIMITED BY " "
             "-" DELIMITED BY SIZE
             HFLD-TYPE(IFLD-IDX) DELIMITED BY " "
      INTO HOT-TEMPLATE-NAME.

     MOVE 0 TO IFD-PTR.
     Move 0 To Field-Template.
     Perform Varying IFD-IDX from 1 by 1
       Until IFD-IDX > IFD-CNT OR IFD-PTR > 0
      If IFD-TEMPLATE-NAME(IFD-IDX) = HOT-TEMPLATE-NAME
       Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
       Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       MOVE IFD-IDX TO IFD-PTR
      End-If
     End-Perform.
     If Field-Template  = 0
      Move "?" To Field-Template-Name.
     MOVE HOT-TEMPLATE-NAME TO Field-Template-Name.
*>---------------------------------------------------------------
 CHANGE-Data-Field.
     MOVE LOW-VALUES TO SP2-FD-DATA.
     Move HFLD-NUMBER(IFLD-IDX)     To SP2-FD-ID.
     CALL "SP2" USING SP2-GET-FIELD-DEF SP2-FIELD-DEF.
     If SP2-FD-RET-CODE = 0
      MOVE IFD-BOR-TYPE(IFD-PTR)     TO SP2-FD-BOR-TYPE
      MOVE IFD-OUTPUT(IFD-PTR)       TO SP2-FD-OUTPUT
      MOVE IFD-FONT-ID(IFD-PTR)      TO SP2-FD-FONT-ID
      MOVE IFD-COLR(IFD-PTR)         TO SP2-FD-COLR
      COMPUTE SP2-FD-HEIGHT = IFD-HEIGHT(IFD-PTR) * 8 END-COMPUTE
      MOVE "_" TO SP2-FD-USER-DATA
      MOVE HOT-TEMPLATE-NAME TO SP2-FD-USER-DATA(2:7)
*> /\/\/\/\/\/\/\/\
      CALL "SP2" USING SP2-DELETE-FIELD SP2-FIELD-DEF
*> \/\/\/\/\/\/\/\/
      CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.
*>----------------------------------------------------------------
 SET-FKEY-LABEL.
     MOVE LOW-VALUES TO SP2-FD-DATA.
     MOVE HOT-KEY-SP2-ID(FKEY-IDX)   TO SP2-FD-ID.
     CALL "SP2" USING SP2-GET-FIELD-DEF SP2-FIELD-DEF.

     If SP2-FD-RET-CODE = 0
      Move HOT-KEY-LABEL(FKEY-IDX) To SP2-FD-VAR-DATA
      CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.

     If SP2-FD-RET-CODE NOT = 0
      MOVE FKEY-IDX TO DISP-9
      move LOW-VALUES to END-MSG
      string "ERROR CALLING SET-FIELD-DEF FKEY " DISP-9
             " [" HOT-KEY-LABEL(FKEY-IDX) "]"
        DELIMITED BY SIZE into END-MSG END-STRING.
*>----------------------------------------------------------------
 GET-VWINDOW-DATA.
     MOVE IFILE-WIN-POS TO MYROW.
     IF MYROW IS LESS THAN 1 OR MYROW GREATER THAN 25
        MOVE 1 TO MYROW.
     IF MYROW GREATER THAN 1
        MOVE 24 TO MYROW.
     MOVE 1 TO MYCOL.
*>----------------------------------------------------------------
 VSP2-PUTWINDOW.
     MOVE LOW-VALUES          TO  SP2-STATIC-DEF.
     MOVE 700                 TO  SP2-SD-ID.
     Move +10                 TO  SP2-SD-LEN-LEN.
     Move +12                 TO  SP2-SD-NUM-LEN.
     Move +4                  TO  SP2-SD-CHAR-LEN.
     Move +80                 TO  SP2-SD-VAR-LEN.
     Move +80                 TO  SP2-SD-TEXT-LEN.
     MOVE MYROW               TO  SP2-SD-ROW.
     COMPUTE SP2-SD-ROW = MYROW * 11.
     COMPUTE SP2-SD-COL = MYCOL * 9.
     MOVE MYBUF               TO SP2-SD-TEXT.
     COMPUTE SP2-SD-HEIGHT = 10 * 8 END-COMPUTE.
     COMPUTE SP2-SD-WIDTH = 800 * 10 END-COMPUTE.
     MOVE WINDOWENH           TO MY-WINDOWENH.
     PERFORM SET-WINDOW-TEMPLATE.
     Move IFD-FONT-ID(IFD-PTR)         To  SP2-SD-FONT-ID.
     Move IFD-COLR(IFD-PTR)            To  SP2-SD-COLR.
     Move 'l'                To  SP2-SD-JUSTIFY.
     Move LOW-VALUE          To  SP2-SD-MISC-OPTIONS.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.
*>----------------------------------------------------------------
 SCREEN-BANNER.
     MOVE LOW-VALUES          TO SP2-STATIC-DEF.
     MOVE 1                   TO SP2-SD-ID.
     Move +10                 TO  SP2-SD-LEN-LEN.
     Move +12                 TO  SP2-SD-NUM-LEN.
     Move +4                  TO  SP2-SD-CHAR-LEN.
     Move +80                 TO  SP2-SD-VAR-LEN.
     Move +80                 TO  SP2-SD-TEXT-LEN.
     MOVE 700                 TO MY-SD-ID.
     PERFORM VARYING ESCIDX FROM 1 BY 1
      UNTIL ESCIDX > ESCCNT
*      MOVE MYROW              TO SP2-SD-ROW
      COMPUTE SP2-SD-ROW = MYROW * 11 END-COMPUTE
*      MOVE ESCCOL(ESCIDX)     TO SP2-SD-COL
      COMPUTE SP2-SD-COL = ESCCOL(ESCIDX) * 9 END-COMPUTE
      ADD 1 TO MY-SD-ID
      MOVE MY-SD-ID           TO SP2-SD-ID
      MOVE TXT(ESCIDX)        TO SP2-SD-TEXT
      MOVE TXLEN(ESCIDX)      TO SP2-SD-VAR-LEN
      MOVE TXLEN(ESCIDX)      TO SP2-SD-TEXT-LEN
      MOVE ENH(ESCIDX) TO MY-WINDOWENH
      PERFORM SET-WINDOW-TEMPLATE

      Move IFD-FONT-ID(IFD-PTR)         To  SP2-SD-FONT-ID
      Move IFD-COLR(IFD-PTR)            To  SP2-SD-COLR
      COMPUTE SP2-SD-HEIGHT = 10 * 8 END-COMPUTE
      COMPUTE sp2-sd-width = (TXLEN(ESCIDX) * 10) * 10 END-COMPUTE
      CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF
     END-PERFORM.
*>-----------------------------------------------------------
 Banner-Extract.
     MOVE 0 TO THISLEN LASTLEN NEXTLEN.
     MOVE 0 TO PutBufNumCnt PutBufAlphaCnt PutBufSpaceCnt.
     INITIALIZE ESCAPE-TABLE.
     move 150 to mybuflen.
     PERFORM VARYING BUFIDX FROM 1 BY 1 UNTIL (BUFIDX >= MYBUFLEN)

        Move mybuf(BUFIDX:1) To CharTest
        If CharTest is Numeric
           Add 1 To PutBufNumCnt
        End-If
        If CharTest is ALPHABETIC
           Add 1 To PutBufAlphaCnt
        End-If
        If CharTest = " "
           Add 1 To PutBufSpaceCnt
        End-If

        if CharTest Not = X"1B"
           Move 0 to NEXTLEN
           Perform VARYING NEWPTR From 1 by 1 Until NEWPTR > EscIdx
              Add TxLen(NEWPTR) to NEXTLEN
           End-Perform
        Else
           Perform Escape-Table-Add
        END-IF

     END-PERFORM.
     Perform Escape-Table-Add
     MOVE ESCIDX TO ESCCNT.
*>------------------------------------------------------------
 Escape-Table-Add.
     Move " " to CharTest.
     Compute BufIdx = BufIdx + 2.
     Perform VARYING cnt from BUFIDX by 1
       until (cnt >= mybuflen)
          or (CharTest = X"1B")
      Move mybuf(cnt:1) to CharTest
     End-perform
     if cnt <= mybuflen
      ADD 1 TO ESCIDX
      Add 1 TO NEXTLEN
      MOVE NEXTLEN           TO ESCCOL(ESCIDX)
      Compute BufIdx = BufIdx - 2 END-COMPUTE
      IF BUFIDX > 0
       MOVE mybuf(BUFIDX:4)  TO ESC(ESCIDX)
       MOVE ESC(ESCIDX)(4:1)  TO ENH(ESCIDX)
       Add 4 to BufIdx
      END-IF
      MOVE BUFIDX            TO TXSTRT(ESCIDX)
      COMPUTE
        TXLEN(ESCIDX) = cnt - (bufidx + 1)
      END-COMPUTE
      MOVE THISLEN           TO LASTLEN
      MOVE TXLEN(ESCIDX)     TO THISLEN
      IF TXSTRT(ESCIDX) > 0
       MOVE mybuf(TXSTRT(ESCIDX):TXLEN(ESCIDX) + 2)
         TO TXT(ESCIDX).
*>--------------------------------------------------------------------
 Send-Field-Properties.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
      IF (CURRENT-TYPE(IFLD-IDX) NOT = HFLD-TYPE(IFLD-IDX)) OR
         (CURRENT-DTYPE(IFLD-IDX) NOT = HFLD-DTYPE(IFLD-IDX))
       MOVE HFLD-FM-UNIQUE(IFLD-IDX) TO TCLFLD-FM-UNIQUE
       MOVE HFLD-NAME(IFLD-IDX)      TO TCLFLD-NAME
       MOVE HFLD-TYPE(IFLD-IDX)      TO TCLFLD-TYPE
       MOVE HFLD-DTYPE(IFLD-IDX)     TO TCLFLD-DTYPE
       MOVE HFLD-LENGTH(IFLD-IDX)    TO TCLFLD-LENGTH
       PERFORM UPDATE-TEMP-TBL
      END-IF
     END-PERFORM.
*>-----------------------------------------------------------
 UPDATE-TEMP-TBL.
     MOVE LOW-VALUES TO TCL-EVAL-STR.
     MOVE 0 TO TCL-RESULT.
     STRING "::viewplus::UPDATEFIELD "
      TCLFLD-FM-UNIQUE " " TCLFLD-NAME " " TCLFLD-TYPE " " TCLFLD-DTYPE " " TCLFLD-LENGTH
      DELIMITED BY SIZE INTO TCL-EVAL-STR.
     CALL "TCLEVAL" USING My-Tcl TCL-EVAL-STR TCL-RESULT
     IF TCL-RESULT NOT = 0
      MOVE "ERROR TCL calling ::viewplus::UPDATEFIELD " TO END-MSG.
*>--------------------------------------------------------------
 SET-FIELD-USERDATA.
*>
*> Field USER-DATA is being used to STORE the Vue3 Template name of the field.
*> Example: NONE-D, or HI-O
*> This data will be used by the WEB Client to place each field in it's
*> own class. This class will then be referenced in the CSS file.
*>
*> This USER-DATA will be read by the SP2.CBX script using the SP2CGI.exe
*> It will be represented in the CBX file as "$026VG0(8)"
*>
     MOVE SPACES TO HOT-TEMPLATE-NAME Field-Template-Name.
     Move 0 To IFD-IDX Field-Template.

     STRING HFLD-ENH(IFLD-IDX)  DELIMITED BY SPACE
            "-"                 DELIMITED BY SIZE
            HFLD-TYPE(IFLD-IDX) DELIMITED BY SPACE
       INTO HOT-TEMPLATE-NAME.

     Move HOT-TEMPLATE-NAME To TEMPLATE-STRING.
     Perform GET-TEMPLATE-VALUE.

     If TEMPLATE-VALUE > 0
      Perform Varying IFD-IDX from 1 by 1 Until IFD-IDX > IFD-CNT Or Field-Template-Name > Spaces
       If IFD-TEMPLATE-ID(IFD-IDX) = TEMPLATE-VALUE
        Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
        Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       End-If
      End-Perform.

     If Field-Template = 0
      Move "NONED" TO HOT-TEMPLATE-NAME
     Else
      Move Field-Template-Name TO HOT-TEMPLATE-NAME.

     Inspect HOT-TEMPLATE-NAME REPLACING all "-" By "_".
     MOVE LOW-VALUES TO MY-PR-DATA.
     MOVE HFLD-NUMBER(IFLD-IDX) TO MY-PR-ID.
     MOVE "F" TO MY-PR-OBJECT-TYPE.
     MOVE "V" TO MY-PR-TYPE.
     MOVE "H" TO MY-PR-VAR-TYPE.
     MOVE 0   TO MY-PR-OFFSET.
     COMPUTE MY-PR-LEN = FUNCTION LENGTH(HOT-TEMPLATE-NAME) + 1.
     MOVE "-" TO MY-PR-FORMAT.
     MOVE "-" TO MY-PR-ACTION.
     MOVE "L" TO MY-PR-VAR-ACT.
     MOVE SPACES TO MY-PR-VALUE.
     CALL "SP2" USING SP2-GET-PROPERTY MY-PROPERTY.

     MOVE LOW-VALUES TO MY-PR-DATA.
     MOVE HFLD-NUMBER(IFLD-IDX) TO MY-PR-ID.
     MOVE "F" TO MY-PR-OBJECT-TYPE.
     MOVE "V" TO MY-PR-TYPE.
     MOVE "H" TO MY-PR-VAR-TYPE.
     MOVE 0   TO MY-PR-OFFSET.
     COMPUTE MY-PR-LEN = FUNCTION LENGTH(HOT-TEMPLATE-NAME) + 1.
     MOVE "-" TO MY-PR-FORMAT.
     MOVE "-" TO MY-PR-ACTION.
     MOVE "L" TO MY-PR-VAR-ACT.
     MOVE "_" TO MY-PR-VALUE.
     MOVE HOT-TEMPLATE-NAME TO MY-PR-VALUE(2:8).
     CALL "SP2" USING SP2-SET-PROPERTY MY-PROPERTY.

     MOVE LOW-VALUES TO MY-PR-DATA.
     MOVE HFLD-NUMBER(IFLD-IDX) TO MY-PR-ID.
     MOVE "F" TO MY-PR-OBJECT-TYPE.
     MOVE "V" TO MY-PR-TYPE.
     MOVE "H" TO MY-PR-VAR-TYPE.
     MOVE 0   TO MY-PR-OFFSET.
     COMPUTE MY-PR-LEN = FUNCTION LENGTH(HOT-TEMPLATE-NAME) + 1.
     MOVE "-" TO MY-PR-FORMAT.
     MOVE "-" TO MY-PR-ACTION.
     MOVE "L" TO MY-PR-VAR-ACT.
     MOVE SPACES TO MY-PR-VALUE.
     CALL "SP2" USING SP2-GET-PROPERTY MY-PROPERTY.
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
*>-----------------------------------------------------------
 Set-WebClient-Errorflag.
     MOVE "ERRFLG" TO HOT-TEMPLATE-NAME.

     MOVE LOW-VALUES TO MY-PR-DATA.
     MOVE HFLD-NUMBER(IFLD-IDX) TO MY-PR-ID.
     MOVE "F" TO MY-PR-OBJECT-TYPE.
     MOVE "V" TO MY-PR-TYPE.
     MOVE "H" TO MY-PR-VAR-TYPE.
     MOVE 0   TO MY-PR-OFFSET.
     COMPUTE MY-PR-LEN = FUNCTION LENGTH(HOT-TEMPLATE-NAME).
     MOVE "-" TO MY-PR-FORMAT.
     MOVE "-" TO MY-PR-ACTION.
     MOVE "L" TO MY-PR-VAR-ACT.
     MOVE "_EDITERR" TO MY-PR-VALUE.
     CALL "SP2" USING SP2-SET-PROPERTY MY-PROPERTY.

>>source format free.
*>vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
*> vvvvv vvvvv vvvvv vvvvv vvvvv vvvvv vvvvv vvvvv vvvvv vvvvv vvvv
*>  vvv   vvv   vvv   vvv   vvv   vvv   vvv   vvv   vvv   vvv   vvv
*>   v     v     v     v     v     v     v     v     v     v     v
*>   .     .     .     .     .     .     .     .     .     .     .
*>*****************************************************************
*>                  V G E T N E X T F O R M                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETNEXTFORM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 Input-Output Section.
 File-Control.
     Select Phaseout
        ORGANIZATION IS LINE SEQUENTIAL
        Assign       To PhaseoutName
        File Status  Is Phaseout-Status.

 Data Division.
 File Section.

 Fd Phaseout.
 01 Phaseout-Record.
    05 Phaseout-Name            Pic x(16).
    05 Phaseout-Link            Pic X(469).

 WORKING-STORAGE SECTION.
 01 PhaseoutName           Pic X(64) Value "/volume1/panels/phaseout.conf".
 01 Phaseout-Status        Pic XX Value "00".
 01 Phased-Out-Sw          Pic 9 Value 0.
    88 Phased-Out          Value 1, False 0.
 01 End-Of-Phaseout-Sw     Pic 9 Value 0.
    88 End-Of-Phaseout     Value 1, False 0.
 01 pridx                  PIC S9(4) COMP-5 VALUE 0.
 01 StrIdx                 PIC S9(4) COMP-5 VALUE 0.

 01  MY-MESSAGE-DATA.
     05  MY-MS-RET-CODE         PIC S9(4) COMP-5.
     05  MY-MS-LENS.
         10  MY-MS-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
         10  MY-MS-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
         10  MY-MS-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
         10  MY-MS-VAR-LEN      PIC S9(4) COMP-5 VALUE +592.
         10  MY-MS-TITLE-LEN    PIC S9(4) COMP-5 VALUE +80.
         10  MY-MS-LINE-LEN     PIC S9(4) COMP-5 VALUE +512.
     05  MY-MS-DATA.
*>******* MY-MS-NUM-DATA ********
         10  MY-MS-LINE-CNT     PIC S9(4) COMP-5.
*>******* MY-MS-CHAR-DATA *******
         10  MY-MS-ICON         PIC X.
             88 vp-bang  value "b". *> = bang (exclamation mark)
             88 vp-stop  value "s". *> = stop sign
             88 vp-info  value "i". *> = information
             88 vp-quest value "q". *> = question
         10  MY-MS-BUTTON       PIC X.
             88 vp-ok    value "o". *> = ok
             88 vp-yn    value "y". *> = yes/no
             88 vp-ny    value "n". *> = no/yes
             88 vp-retry value "r". *> = retry/cancel
         10  MY-MS-CANCEL       PIC X.
             88 vp-canc  value "y". *> = display cancel button
         10  MY-MS-REPLY        PIC X.
             88 vpr-ok   value "o". *> = ok
             88 vpr-yes  value "y". *> = yes
             88 vpr-no   value "n". *> = no
             88 vpr-rtry value "r". *> = retry
             88 vpr-canc value "c". *> = cancel
*>******* MY-MS-VAR-DATA *******
         10  MY-MS-TITLE        PIC X(80).
         10  MY-MS-TEXT         PIC X(512).
 01  TEMP-TEXT         PIC X(512).
    COPY "vplus/TCLDBMEM".
    COPY "vplus/SP250".
 01 x pic s9(4) comp-5.
 01 y pic s9(4) comp-5.
 01 z pic s9(4) comp-5.
 01 DISP-Z4 PIC ZZZ9.
 01 Field-Template             Pic S9(9) Comp-5.
 01 Field-Template-Name           Pic x(8).
 01 HOT-Template-Name           Pic x(8).
 01 MY-SP2-DATA.
    05 My-Prog-Off       Pic s9(4) comp-5 Value 0.
    05 My-Prog-Num       Pic s9(4) comp-5 Value 0.
    05 My-FD-ID          Pic s9(4) comp-5 Value 0.

 01 NIBBLE-WORK.
    02 NIBBLE-1 PIC X.
    02 NIBBLE-2 PIC X.
 01 NIBBLE-NUM REDEFINES NIBBLE-WORK.
    02 NIBBLE-NUM PIC S9(4) COMP-5.

 01 FillerSIze      PIC S9(4) COMP-5 VALUE 0.
 01 my-total-length PIC S9(4) COMP-5 VALUE 0.
 01 MYCOMLEN        PIC S9(9) COMP-5 VALUE 0.
 01 disp-err        pic ----9.
 01 disp-dbl        pic 9(9).
 01 COMP5-dbl       pic 9(9) COMP-5.
 01 X-DBL           pic X(9).
 01 MY-FLDNUM REDEFINES X-DBL PIC 9(9).
 01  I2             PIC S9(9) COMP-5.
 01 MY-PANEL-ID.
    02 MY-FILLER    PIC X.
    02 MY-PANEL-NUM PIC 9(7).
 01 NUMIDX           pic 9(4) COMP-5.
 01 NUMCNT           pic 9(4) COMP-5.
 01 NUMERR           pic 9(4) COMP-5.
 01 NUMDEC           pic 9(4) COMP-5.
 01 NUM1 PIC X(14).
 01 NUM2 PIC 9(14).
 01 DebugBUF               PIC X(256) Value Spaces.

 01 PANEL-SW PIC X VALUE "0".
       88 NO-MORE-PANELS VALUE "1".
 01 WRK-STRING PIC X(32).
 01 VSLOG-REC  PIC X(80).
     COPY "comlib/USERAREA".
     COPY "comlib/DRIVAREA".
 01 SLDEV         PIC 9(4)  value 0.
 01 CONNTYPE      PIC X     value space.
 01 RETURN-ERROR  PIC S9(4) COMP value 0.
 01 LINK1             PIC S9(4) Comp-5 value 0.
 01 LINK2             PIC S9(4) Comp-5 value 0.
 01 Init-Auth-ID                Pic X(48) Value Spaces.
 01 Init-IPADDR                 Pic X(16) Value Spaces.


 01 FLDBUFFLEN            PIC S9(4)  COMP-5.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
*>
    COPY "COMAREA" of vplus.
*>
    COPY "VPLUSSQL" of vplus.
    COPY "VPLUSMEM" of vplus.
 01 My-Tcl             USAGE POINTER.
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
 0000-BEGIN-GETNEXTFORM.
*-------------- INITIALIZATION AND PARM-EDITS.
       CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
       SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
       SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.

       Perform GET-DSEG.
       MOVE "N" TO VINITFORM-SW.
       MOVE "N" TO VSHOWFORM-SW.
       Perform PARMCHECK THRU PARMCHECK-EXIT.
       If Cstatus Not = 0
          Go To Return-To-Caller.

*> All SP2 screens are being phased out, one by one.
*> To phase out a screen,
*>    add the NFNAME screen name to the phaseout.conf file
*>    in the /volume1/panels/ directory.
*>
*> Code to Check if the NFNAME screen has been phased-out
       Set Phased-Out       To False.
       Set End-Of-Phaseout  To False
       Open Input Phaseout.
       If Phaseout-Status <> "00"
          Display "Phaseout configuration file not found"
       Else
*>
*> Read the phasout file to see if NFNAME can be found.
*> # Pound sign (Hash Tag) in column 1 is a comment.
          Perform Varying pridx from 1 by 1 until End-Of-Phaseout
             Initialize Phaseout-Record
             Read Phaseout
                At End
                   Set End-Of-Phaseout To True
                Not At End
                   If Phaseout-Record(1:1) Not = "#"
                      If NFNAME = Phaseout-Name
                         Set Phased-Out To True
                      End-IF
                   End-IF
             End-Read
             If Phased-Out
                Exit Perform
             End-If
          End-Perform.

       Close Phaseout.

       If Phased-Out
          Go To Phaseout-Shutdown.

       ADD 1 TO VGNF-CNT.
       IF VGNF-CNT < 6
          MOVE NFNAME TO VGNF-FORM(VGNF-CNT)
       ELSE
          PERFORM VARYING VGNF-IDX FROM 2 BY 1 UNTIL VGNF-IDX > 5
             COMPUTE VGNF-CNT = VGNF-IDX - 1 END-COMPUTE
             MOVE VGNF-FORM(VGNF-IDX) TO VGNF-FORM(VGNF-CNT)
          END-PERFORM
          MOVE 5 TO VGNF-CNT
          MOVE NFNAME TO VGNF-FORM(VGNF-CNT).

       COMPUTE VGNF-IDX = VGNF-CNT - 1.
       MOVE  IFILE-HEAD-FRM          TO VGNF-HEAD
       MOVE  HPAN-NEXT-FRM           TO VGNF-NEXT
       MOVE  VGNF-FORM(VGNF-IDX)     TO VGNF-RETURN
       MOVE  "$END"                  TO VGNF-END
       MOVE  NFNAME                  TO CFNAME.
       MOVE  NFNAME                  TO VGNF-REFRESH
       PERFORM LOAD-FIELDS.
       move  AF-AppBuf-Len           TO DBUFLEN.
       MOVE "N" TO VINITFORM-SW.
       MOVE "Y" TO VGETNEXTFORM-SW.

*>---------------------------------------------------------------
 Return-To-Caller.
     IF CSTATUS NOT = 0
      MOVE CSTATUS    TO INTR-ERRNUM
      MOVE CSTATUS    TO DISP-ERR
      CALL "VGETERRMSG" USING COMAREA
      MOVE LOW-VALUES TO MY-MS-DATA
      MOVE 1          TO MY-MS-LINE-CNT
      MOVE "s"        TO MY-MS-ICON
      MOVE "r"        TO MY-MS-BUTTON
      MOVE "Vue3 ERROR" to MY-MS-TITLE
      STRING "*** Vue3 ERROR " DISP-ERR " OCCURED in VGETNEXTFORM ***" X"0D0A"
            INTR-ERRMSG   X"0D0A"
            "FORM NAME="  NFNAME X"0D0A"
            "PRESS CANCEL TO ABORT"
       DELIMITED BY SIZE INTO MY-MS-TEXT
      END-STRING
      Move 1 to link1
      Move My-MS-TEXT To TEMP-TEXT
      Compute link2 = Function Length(MY-MS-TEXT) END-COMPUTE
      Call "TrimString" USING TEMP-TEXT link1 link2 END-CALL
      Move link2 to MY-MS-LINE-LEN
      CALL "SP2" USING SP2-DISPLAY-MESSAGE MY-MESSAGE-DATA
      If MY-MS-REPLY = "c"
        CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM
        CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM
        MOVE SP2-MOUSE-WAIT TO SP2-NP-RET-CODE
        CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM
        CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM
        STOP RUN
      END-IF
      MOVE "VGETNEXTFORM" TO INTR-CALLNAME
      MOVE CSTATUS        TO  INTR-ERRNUM
     END-IF.

     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
       MOVE -1 TO HFLD-ERRFLG(IFLD-IDX)
     END-PERFORM.
     INITIALIZE MM-CD-DATABUFFER.

     MOVE MM-CD-DATABUFFER(1:AF-AppBuf-Len) TO LAST-BUFFER SVT-BUFFER.
     Goback.
*>----------------------------------------------------------------
 PARMCHECK.
*>  VGETNEXTFORM  Error Messages
*>    Number  Message
*>    100     Can't find the next form. (FSERR !)
*>    101     Form $HEAD does not apply, no prior call to VGETNEXTFORM.
*>    102     An invalid COM'REPEAT value was supplied programmatically.
*>    103     An invalid COM'NFOPT value was supplied programatically.
*>    104     Formsfile not compiled for this run-time terminal.
*>    105     HP3075/6 terminal with numeric only display is not supported.
*>    106     HP3077 terminal is not supported except for open and close.
*>    107     Form $REFRESH does not apply, no prior call to VGETNEXTFORM.
*>    108     Form $NEXT does not apply, no prior call to VGETNEXTFORM.
*>    109     Form $RETURN does not apply, no prior call to VGETNEXTFORM.
*>
     MOVE "VGETNEXTFORM" TO INTR-CALLNAME.
     MOVE LENGTH OF COMAREA TO MYCOMLEN.
     DIVIDE MYCOMLEN BY 2 GIVING MYCOMLEN.
     IF (VOPENTERM-SW NOT = "Y") or (VOPENFORMF-SW NOT = "Y")
        String "TERMINAL or FORMSFILE has not yet been opened."
           Delimited by size into vslog-REC
        END-STRING
        Move 45             to INTR-ERRNUM
        MOVE 45 TO CSTATUS
        GO TO PARMCHECK-EXIT.


     IF MYCOMLEN < 60
        String "Comarea length may not be less than 60."
           Delimited by size into vslog-REC
        END-STRING
        Move 43             to INTR-ERRNUM
        MOVE 43 TO CSTATUS
        GO TO PARMCHECK-EXIT.

     IF LANGUAGE NOT = 0
        String "Unrecognized Comarea Language code passed."
           Delimited by size into vslog-REC
        END-STRING
        Move 46             to INTR-ERRNUM
        MOVE 46 TO CSTATUS
        GO TO PARMCHECK-EXIT.

     If NOT OPENFORMF-CALLED
        String "FORMSFILE has not yet been opened."
           Delimited by size into vslog-REC
        END-STRING
        Move 45             to INTR-ERRNUM
        Move 45 to CSTATUS
        GO TO PARMCHECK-EXIT.

     IF REPEATAPP NOT = 0
        String "An invalid COM'REPEAT value was "
               "supplied programmatically."
           Delimited by size into vslog-REC
        END-STRING
        Move 102             to INTR-ERRNUM
        Move 102 to CSTATUS
        GO TO PARMCHECK-EXIT.

     IF FREEZAPP NOT = 0
        String "An invalid COM'NFOPT value was "
               "supplied programmatically."
           Delimited by size into vslog-REC
        END-STRING
        Move 102             to INTR-ERRNUM
        Move 102 to CSTATUS
        GO TO PARMCHECK-EXIT.

     IF NFNAME = "$HEAD"
        IF NOT GETNEXTFORM-CALLED
           String "Form $HEAD does not apply, "
                  "no prior call to VGETNEXTFORM."
              Delimited by size into vslog-REC
           END-STRING
           Move 101             to INTR-ERRNUM
           MOVE 101 TO CSTATUS
           GO TO PARMCHECK-EXIT
        ELSE
           MOVE VGNF-HEAD TO NFNAME
        END-IF
     END-IF.

     IF NFNAME = "$REFRESH"
        IF NOT GETNEXTFORM-CALLED
           String "Form $REFRESH does not apply, "
                  "no prior call to VGETNEXTFORM."
              Delimited by size into vslog-REC
           END-STRING
           Move 107             to INTR-ERRNUM
           MOVE 107 TO CSTATUS
           GO TO PARMCHECK-EXIT
        ELSE
           MOVE VGNF-REFRESH TO NFNAME
        END-IF
     END-IF.

     IF NFNAME = "$RETURN"
        IF ( NOT GETNEXTFORM-CALLED ) OR
           ( VGNF-CNT < 2 )
           String "Form $RETURN does not apply, "
                  "displayed forms less than two."
              Delimited by size into vslog-REC
           END-STRING
           Move 109             to INTR-ERRNUM
           MOVE 109 TO CSTATUS
           GO TO PARMCHECK-EXIT
        ELSE
           MOVE VGNF-REFRESH TO NFNAME
        END-IF
     END-IF.

     IF NFNAME = "$NEXT"
        IF NOT GETNEXTFORM-CALLED
           String "Form $NEXT does not apply, "
                  "no prior call to VGETNEXTFORM."
              Delimited by size into vslog-REC
           END-STRING
           Move 108             to INTR-ERRNUM
           MOVE 108 TO CSTATUS
           GO TO PARMCHECK-EXIT
        ELSE
           MOVE VGNF-NEXT TO NFNAME
        END-IF
     END-IF.

*> Try to find the next form in the internal vplus table.
     PERFORM VARYING IPAN-IDX FROM 1 BY 1
       UNTIL IPAN-IDX > IPAN-CNT
          OR NFNAME = IPAN-NAME(IPAN-IDX)
      CONTINUE
     END-PERFORM.

*> Did we find it?
     If IPAN-IDX > IPAN-CNT
        String
           "Can't find the next form " DELIMITED BY SIZE
           QUOTE                        DELIMITED BY SIZE
           NFNAME                       DELIMITED BY SPACE
           QUOTE                        DELIMITED BY SIZE
              into vslog-REC
        END-STRING
        Move 100             to INTR-ERRNUM
        Move 100 to CSTATUS
        GO TO Return-To-Caller.


*> Did we REALLY find it? If test fails maybe for is not compiled.
     IF NFNAME = IPAN-NAME(IPAN-IDX)
        MOVE IPAN-ENTRY(IPAN-IDX) TO HOT-PANEL
     ELSE
        Move 53             to INTR-ERRNUM
        MOVE 53 TO CSTATUS
        GO TO Return-To-Caller.



 PARMCHECK-EXIT. EXIT.
*>---------------------------------------------------------------
 LOAD-FIELDS.
*> GET ALL DATA FIELDS FOR EACH FORM FOUND ABOVE
      INITIALIZE FKEY-CNT IFLD-CNT ISTAT-CNT my-total-length.
      PERFORM FIND-VFIELD-BYFMUNIQUE
      IF TCL-ROWCOUNT > 0
       PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > TCL-ROWCOUNT
        PERFORM GET-VFIELD
        IF IFLD-LEN > 0
         IF FLD-TYPE = "FK" AND FKEY-CNT < 8
          ADD 1 TO FKEY-CNT
          MOVE FLD-FD-TEMPLATE-ID TO PANEL-KEY-TEMPLATE-ID(FKEY-CNT)
          MOVE FLD-INITVAL        TO PANEL-KEY-LABEL(FKEY-CNT)
          MOVE FLD-ROW            TO PANEL-KEY-ROW(FKEY-CNT)
          MOVE FLD-COLUMN         TO PANEL-KEY-COL(FKEY-CNT)
          MOVE FLD-SCRNORDER      TO PANEL-KEY-HID(FKEY-CNT)
         ELSE
          ADD 1 TO IFLD-CNT
          MOVE FLD-VFIELD TO IFLD-ENTRY(IFLD-CNT)
          Add FLD-LENGTH TO my-total-length
         END-IF
        END-IF
       END-PERFORM.
      Move my-total-length To AF-AppBuf-Len.
      Move IFLD-CNT  To AF-AppFld-Cnt.

*>--------------------
*> VPLUSAUTH FLD-VFIELD.
*>--------------------
      Move 999999001  TO FLD-FM-UNIQUE.
      Move 930240     To FLD-FD-TEMPLATE-ID.
      Move "SECAUTH"  To FLD-NAME.
      Move "NONE"     To FLD-ENH.
      Move "D"        To FLD-TYPE.
      Move "CHAR"     To FLD-DTYPE.
      Compute FLD-BUFSTART = my-total-length + 1 End-Compute.
      Compute FLD-BUFEND   = FLD-BUFSTART + 48 End-Compute.
      Move 1          To FLD-COLUMN.
      Move 30         To FLD-ROW.
      Move 401        To FLD-SCRNORDER.
      Move 401        To FLD-NUMBER.
      Move 48         To FLD-LENGTH.

      ACCEPT Init-Auth-ID FROM ENVIRONMENT "HPAUTHCODE".
      If Init-Auth-ID = "NONE"
         Move Spaces       To FLD-INITVAL
      Else
         Move Init-Auth-ID To FLD-INITVAL.

      Move Spaces     To FLD-PROCSPEC.
      ADD 1 TO IFLD-CNT.
      MOVE FLD-VFIELD TO IFLD-ENTRY(IFLD-CNT).
      add 48 To My-Total-Length.
*>--------------------
*> CLIENTIP FLD-VFIELD.
*> Example: nnn.nnn.nnn.nnn
      Move 999999002  TO FLD-FM-UNIQUE.
      Move 930240     To FLD-FD-TEMPLATE-ID.
      Move "CLIENTIP" To FLD-NAME.
      Move "NONE"     To FLD-ENH.
      Move "D"        To FLD-TYPE.
      Move "CHAR"     To FLD-DTYPE.
      Compute FLD-BUFSTART = my-total-length + 1 End-Compute.
      Compute FLD-BUFEND   = FLD-BUFSTART + 16 End-Compute.
      Move 1          To FLD-COLUMN.
      Move 30         To FLD-ROW.
      Move 402        To FLD-SCRNORDER.
      Move 402        To FLD-NUMBER.
      Move 16         To FLD-LENGTH.

      If Init-Auth-ID = "NONE"
         Move Spaces      To FLD-INITVAL
      Else
         ACCEPT Init-IPADDR FROM ENVIRONMENT "HPREMIPADDR"
         Move Init-IPADDR To FLD-INITVAL.

      Move Spaces     To FLD-PROCSPEC.
      ADD 1 TO IFLD-CNT.
      MOVE FLD-VFIELD TO IFLD-ENTRY(IFLD-CNT).
      Add 16 To My-Total-Length.
      Move My-Total-Length To HPAN-BUF-LEN.
      Move IFLD-CNT To HPAN-FLDCNT.
*>--------------------
*> SECAUTH SECURL1.
*> Example: http://hostname/program.cgi?auth=xxxx
      Move 999999003  TO FLD-FM-UNIQUE.
      Move 930240     To FLD-FD-TEMPLATE-ID.
      Move "SECURL1"  To FLD-NAME.
      Move "NONE"     To FLD-ENH.
      Move "D"        To FLD-TYPE.
      Move "CHAR"     To FLD-DTYPE.
      Compute FLD-BUFSTART = my-total-length + 1 End-Compute.
      Compute FLD-BUFEND   = FLD-BUFSTART + 79 End-Compute.
      Move 1          To FLD-COLUMN.
      Move 30         To FLD-ROW.
      Move 403        To FLD-SCRNORDER.
      Move 403        To FLD-NUMBER.
      Move 79         To FLD-LENGTH.
      Move Spaces     To FLD-INITVAL.
      Move Spaces     To FLD-PROCSPEC.
      ADD 1 TO IFLD-CNT.
      MOVE FLD-VFIELD TO IFLD-ENTRY(IFLD-CNT).
      add 79 To My-Total-Length.

*>--------------------
*> SECAUTH SECURL2.
*> Example: http://hostname/program.cgi?auth=xxxx
      Move 999999004  TO FLD-FM-UNIQUE.
      Move 930240     To FLD-FD-TEMPLATE-ID.
      Move "SECURL2"  To FLD-NAME.
      Move "NONE"     To FLD-ENH.
      Move "D"        To FLD-TYPE.
      Move "CHAR"     To FLD-DTYPE.
      Compute FLD-BUFSTART = my-total-length + 1 End-Compute.
      Compute FLD-BUFEND   = FLD-BUFSTART + 79 End-Compute.
      Move 1          To FLD-COLUMN.
      Move 30         To FLD-ROW.
      Move 404        To FLD-SCRNORDER.
      Move 404        To FLD-NUMBER.
      Move 79         To FLD-LENGTH.
      Move Spaces     To FLD-INITVAL.
      Move Spaces     To FLD-PROCSPEC.
      ADD 1 TO IFLD-CNT.
      MOVE FLD-VFIELD TO IFLD-ENTRY(IFLD-CNT).
      add 79 To My-Total-Length.
*>--------------------
*> KEYVAL FLD-VFIELD. Offset (Ap-Bufflen + 223:16)
*> Example: zzzzzzzzzzzzzn
      Move 999999002  TO FLD-FM-UNIQUE.
      Move 930240     To FLD-FD-TEMPLATE-ID.
      Move "KEYVAL"   To FLD-NAME.
      Move "NONE"     To FLD-ENH.
      Move "D"        To FLD-TYPE.
      Move "CHAR"     To FLD-DTYPE.
      Compute FLD-BUFSTART = my-total-length + 1 End-Compute.
      Compute FLD-BUFEND   = FLD-BUFSTART + 14 End-Compute.
      Move 1          To FLD-COLUMN.
      Move 30         To FLD-ROW.
      Move 405        To FLD-SCRNORDER.
      Move 405        To FLD-NUMBER.
      Move 14         To FLD-LENGTH.
      Move Spaces     To FLD-INITVAL
      Move Spaces     To FLD-PROCSPEC.
      ADD 1 TO IFLD-CNT.
      MOVE FLD-VFIELD TO IFLD-ENTRY(IFLD-CNT).
      Add 16 To My-Total-Length.
      Move My-Total-Length To HPAN-BUF-LEN.
      Move IFLD-CNT To HPAN-FLDCNT.

*>
*> GET ALL STATIC FIELDS FOR A FORM
      INITIALIZE ISTAT-TABLE
      PERFORM FIND-VSTATIC-BYFMUNIQUE
      IF TCL-ROWCOUNT > 0
       PERFORM VARYING ISTAT-IDX FROM 1 BY 1
         UNTIL ISTAT-IDX > TCL-ROWCOUNT
        PERFORM GET-VSTATIC
        IF ISTAT-LEN > 0
         ADD 1 TO ISTAT-CNT
         MOVE STATIC-VFIELD TO ISTAT-ENTRY(ISTAT-CNT)
        END-IF
       END-PERFORM.
*>----------------------------------------------------------------
 FIND-VFIELD-BYFMUNIQUE.
     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "FM_UNIQUE"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.
     MOVE IPAN-UNIQUE(Ipan-Idx) TO DISPL-99.
     STRING DISPL-99 DELIMITED BY SIZE INTO SQL-SEARCH-KEY.
     CALL "TCLSETVAR" USING My-Tcl, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "VFIELD:BYFMUNIQUE"
      DELIMITED BY SIZE INTO SQL-TABLE.
     CALL "SQLGetResultSet" USING My-Tcl, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-VFIELD.
      CALL "SQLGetNext" USING My-Tcl, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
      END-CALL.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.

      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO IFLD-LEN
       SET ADDRESS OF FLD-VFIELD TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO IFLD-LEN
      END-IF.
*>----------------------------------------------------------------
 FIND-VSTATIC-BYFMUNIQUE.
     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "FM_UNIQUE"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.
     MOVE IPAN-UNIQUE(Ipan-Idx) TO DISPL-99.
     STRING DISPL-99 DELIMITED BY SIZE INTO SQL-SEARCH-KEY.
     CALL "TCLSETVAR" USING My-Tcl, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "VSTATIC:BYFMUNIQUE"
      DELIMITED BY SIZE INTO SQL-TABLE.
     CALL "SQLGetResultSet" USING My-Tcl, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-VSTATIC.
      CALL "SQLGetNext" USING My-Tcl, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
      END-CALL.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.

      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO ISTAT-LEN
       SET ADDRESS OF STATIC-VFIELD TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO ISTAT-LEN
      END-IF.
*>----------------------------------------------------------------
 GET-DSEG.
     CALL "VPLUSSDEV" USING SLDEV CONNTYPE RETURN-ERROR.
     MOVE SLDEV TO SD-TERMINAL-NUMBER.
     CALL "GETXDS-HEADER" USING DRIVER-AREA, USER-AREA.
     Move XDS-AUTH-CODE To AF-Auth-ID.
     Move XDS-AUTH-CLIENT To AF-IPADDR.

*>----------------------------------------------------------------
 Phaseout-Shutdown.
*>
*> Note: Shut Down this session, but we are not notifying TC-Master.
*>       If TC-Master must be notified, see VREADFIELDS.cob
*>
      Initialize MY-MS-TITLE MY-MS-TEXT.
      MOVE CSTATUS    TO INTR-ERRNUM.
      MOVE CSTATUS    TO DISP-ERR.
      MOVE LOW-VALUES TO MY-MS-DATA.
      MOVE 1          TO MY-MS-LINE-CNT.
      MOVE "s"        TO MY-MS-ICON.
      MOVE "o"        TO MY-MS-BUTTON.
      MOVE "Vue3 Phased Out Screen" to MY-MS-TITLE.
      Move 1 to StrIdx.
      Move 1 to link1.
      Move 512 To link2.
      Move Phaseout-Link To TEMP-TEXT.
      Call "TrimString" USING TEMP-TEXT link1 link2.
      Move TEMP-TEXT(link1:link2) To Phaseout-Link.
*> Add link1 to link2.

      STRING X"0D0A"         Delimited By Size
             Phaseout-Name   Delimited By space
             " has been phased out, additional info to follow:"  Delimited By Size
             INTO MY-MS-TEXT With pointer StrIdx.

      String X"0D0A" Phaseout-Link(1:link2)
         Delimited By size InTo MY-MS-TEXT With pointer StrIdx.

      String X"0D0A" "Your Session will be Aborted."
             Delimited By Size InTo MY-MS-TEXT With pointer StrIdx.

      Move x"0D0A" To MY-MS-TEXT(StrIdx:2).
      Add 2 To StrIdx.
*> Display message, wait for user click..
      Move 1 to link1.
      Move My-MS-TEXT To TEMP-TEXT.
      Compute link2 = Function Length(MY-MS-TEXT).
      Call "TrimString" USING TEMP-TEXT link1 link2.
      Move link2 to MY-MS-LINE-LEN.
      If MY-MS-LINE-LEN > 512
         Move 512 To MY-MS-LINE-LEN.

*> Waiting.
      CALL "SP2" USING SP2-DISPLAY-MESSAGE MY-MESSAGE-DATA.

*> Abrupt, non-graceful, method to shut down a session.
      MOVE SP2-MOUSE-WAIT TO SP2-NP-RET-CODE.
      CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.
      CALL "SP2" USING SP2-CLOSE-FILE SP2-NULL-PARM.
      CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM.
      CALL "SP2" USING SP2-END-SESSION SP2-NULL-PARM.
      STOP RUN.

>>source format free.
*>******************************************************************
*>*                      V O P E N F O R M F                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VOPENFORMF.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
*>*****************************************************************

 01  New-AUTHKEY                Pic X(32)  Value Spaces.
 01  MY-BUFFER.
     05  MY-BF-RET-CODE         PIC S9(4) COMP-5.
     05  MY-BF-LEN              PIC S9(4) COMP-5 VALUE +128.
     05  MY-BF-DATA             PIC X(128).

 01  SP2-THIN-CLIENT-CODES.
     05  SP2-EXECUTE-PROGRAM     PIC S9(4) COMP-5 VALUE +97.
     05  SP2-EXECUTE-CLIENT-PROG PIC S9(4) COMP-5 VALUE +107.
     05  SP2-COPY-FILE-TO-CLIENT PIC S9(4) COMP-5 VALUE +108.
     05  SP2-COPY-FILE-TO-SERVER PIC S9(4) COMP-5 VALUE +112.
     05  SP2-GET-COMMAND-LINE    PIC S9(4) COMP-5 VALUE +113.
     05  SP2-GET-CLIENT-IP       PIC S9(4) COMP-5 VALUE +119.
     05  QPR-END-SESSION         PIC S9(4) COMP-5 VALUE +16.

 01 Command-Line-Data.
    05 ARG-IDX                   Pic S9(4) Comp-5 Value 0.
    05 ARG-CNT                   Pic S9(4) Comp-5 Value 0.
    05 ARG-PTR                   Pic S9(4) Comp-5 Value 0.
    05 ARG-MAX                   Pic S9(4) Comp-5 Value 0.
    05 ARG-Entries.
       10 ARG-ITEM OCCURS 10 Times Pic X(64).
    05 ARG-LIST REDEFINES ARG-Entries.
       10 ARG-ITEM01            Pic X(64).
       10 ARG-ITEM02            Pic X(64).
       10 ARG-ITEM03            Pic X(64).
       10 ARG-ITEM04            Pic X(64).
       10 ARG-ITEM05            Pic X(64).
       10 ARG-ITEM06            Pic X(64).
       10 ARG-ITEM07            Pic X(64).
       10 ARG-ITEM08            Pic X(64).
       10 ARG-ITEM09            Pic X(64).
       10 ARG-ITEM10            Pic X(64).
 01 LINK1             PIC S9(4) Comp-5 value 0.
 01 LINK2             PIC S9(4) Comp-5 value 0.
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
*>******** MY-MS-NUM-DATA ********
         10  MY-MS-LINE-CNT     PIC S9(4) COMP-5.
*>******** MY-MS-CHAR-DATA *******
         10  MY-MS-ICON         PIC X.
         10  MY-MS-BUTTON       PIC X.
         10  MY-MS-CANCEL       PIC X.
         10  MY-MS-REPLY        PIC X.
*>******** MY-MS-VAR-DATA *******
         10  MY-MS-TITLE        PIC X(80).
         10  MY-MS-TEXT         PIC X(512).
 01  TEMP-TEXT         PIC X(512).
    COPY "vplus/TCLDBMEM".
    COPY "vplus/SP250".
  01 DISP-NUMBER.
     02 DISP-ERR        PIC ----9.
     02 DISP-ERR-X REDEFINES DISP-ERR PIC X(5).

  01 VIEW-FILE-PATH     PIC X(132) VALUE SPACES.
  01 NUM1               PIC X(14)  VALUE SPACES.
  01 NUM2               PIC 9(14)  VALUE ZERO.
  01 NUMDEC             PIC S9(4) COMP-5 VALUE 0.
  01 NUMERR             PIC S9(4) COMP-5 VALUE 0.
  01 LBLIDX             PIC S9(4) COMP-5 VALUE 0.
  01 SFLDCNT            PIC S9(4) COMP-5 VALUE 0.
  01 VSLOG-REC          PIC X(80) VALUE SPACES.
  01 DISP-DBL           PIC 9(10).
  01 FFLEN              PIC S9(9) COMP-5.
  01 MYCOMLEN           PIC S9(9) COMP-5 VALUE 0.
  01 PANEL-SW           PIC X VALUE "0".
       88 NO-MORE-PANELS VALUE "1".
 01 GLOBAL-ADDRESS1   USAGE POINTER.
 01 GLOBAL-ADDRESS2   USAGE POINTER.
 01 errno             PIC S9(9) COMP-5 EXTERNAL.
 01  THE-PROGRAM-NAME       PIC X(36).
 01 DebugBUF     pic x(256)  VALUE SPACES.

*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01  FORM-FILE PIC X(36).
    COPY "vplus/VPLUSSQL".
    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.
*>*########################################################
 PROCEDURE DIVISION USING COMAREA FORM-FILE.
 0000-BEGIN.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.
     IF NOT OPENTERM-CALLED
        INITIALIZE VPLUSMEM.

     IF VOPENFORMF-SW = "Y"
        Move LOW-VALUES to DebugBUF
        String
         "VOPENFORMF: Called once already." DELIMITED BY size into DebugBUF
        End-String
        CALL "LogDebug" USING DebugBUF
        GO TO Return-To-Caller.

*>*-------------- INITIALIZATION AND PARM-EDITS.
     move LOW-VALUES to DebugBUF.
     string "VOPENFORMF: " FORM-FILE DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     COMPUTE FFLEN = FUNCTION LENGTH(FORM-FILE).
     MOVE 0            TO INTR-ERRNUM.
     PERFORM PARMCHECK THRU PARMCHECK-EXIT.
     IF CSTATUS NOT = 0
      GO TO RETURN-TO-CALLER.


     MOVE LOW-VALUES         TO SP2-FI-DATA.
     INITIALIZE SP2-FI-NAME.
     STRING FORM-FILE delimited by space
            ".pan" DELIMITED BY size INTO SP2-FI-NAME.
     MOVE "w" TO SP2-FI-MODE.
     CALL "SP2" USING SP2-CREATE-FILE SP2-FILE-DEF.

*>*-------------- SETUP SP2-OPEN-FILE PARMS.
     MOVE LOW-VALUES         TO SP2-FI-DATA.
     ACCEPT VIEW-FILE-PATH FROM ENVIRONMENT "SP2DIR".
     STRING FORM-FILE delimited by space
            ".pan" DELIMITED BY size INTO SP2-FI-NAME.
     Move +1  To  SP2-OPEN-FILE.
     Move +10 To  SP2-FI-LEN-LEN.
     Move +0  To  SP2-FI-NUM-LEN.
     Move +2  To  SP2-FI-CHAR-LEN.
     Move +80 To  SP2-FI-VAR-LEN.
     Move +80 To  SP2-FI-NAME-LEN.

*>*-------------- SP2-OPEN-FILE CALL.
     CALL "SP2" USING SP2-OPEN-FILE SP2-FILE-DEF.
     MOVE SP2-FI-RET-CODE TO CSTATUS.

     PERFORM TCL-INIT.
     PERFORM LOAD-FILE-DATA.

*>*-------------- SUCCESS RETURN TO CALLER
*>* VOPENFORMF sets the following comarea items:
     Move 0               To CSTATUS.
     MOVE 0               TO lastkey.
     MOVE 0               TO numerrs.
     MOVE 0               TO recnum.
     MOVE 0               TO dbuflen.
     MOVE 0               TO cmode.
     MOVE 0               TO repeatapp.
     MOVE 0               TO freezapp.
     MOVE 0               TO printfilnum.
     MOVE 0               TO deleteflag.
     MOVE SPACES          TO cfname.
     MOVE IFILE-HEAD-FRM  TO nfname.
     MOVE 0               TO filerrnum.
     MOVE IFILE-WIN-ENH   TO windowenh.
     MOVE FORM-FILE       TO MPE-FORMS-FILE.
     MOVE "Y"             To VOPENFORMF-SW.
     INITIALIZE THE-PROGRAM-NAME Auth-Fields.
     CALL "PROCNAME" USING THE-PROGRAM-NAME.
     Move THE-PROGRAM-NAME To AF-PROGNAME.
     GO TO Return-To-Caller.
*>*-----------------------------------------------------------
 Return-To-Caller.
     MOVE CSTATUS TO INTR-ERRNUM.
     IF CSTATUS NOT = 0
      MOVE CSTATUS    TO INTR-ERRNUM
      MOVE CSTATUS    TO DISP-ERR
      CALL "VGETERRMSG" USING COMAREA
      MOVE LOW-VALUES TO MY-MS-DATA
      MOVE 1          TO MY-MS-LINE-CNT
      MOVE "s"        TO MY-MS-ICON
      MOVE "r"        TO MY-MS-BUTTON
      MOVE "Vue3 ERROR" to MY-MS-TITLE
      STRING "*** Vue3 ERROR " DISP-ERR " OCCURED in VOPENFORMF ***" X"0D0A"
            INTR-ERRMSG   X"0D0A"
            "FORMS FILE="  FORM-FILE X"0D0A"
            "PRESS CANCEL TO ABORT"
       DELIMITED BY SIZE INTO MY-MS-TEXT
      END-STRING
      Move 1 to link1
      Compute link2 = Function Length(MY-MS-TEXT) END-COMPUTE
      MOVE MY-MS-TEXT TO TEMP-TEXT
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
      MOVE "F"             To VOPENFORMF-SW
      End-IF.

     Goback.

*>*---------------------------------------------------------
 PARMCHECK.
*>*$ VOPENFORMF and VCLOSEFORMF messages
*>*$
*>040 Formsfile not found.
*>042 Formsfile name not passed to VOPENFORMF.
*>043 Comarea length may not be less than 60.
*>044 Comarea length is too large.
*>045 TERMINAL or FORMSFILE has not yet been opened.
*>046 Unrecognized Comarea Language code passed.
*>050 Forms File open failed. (FSERR !)
*>051 The file is not a VPLUS Forms File.
*>052 Forms File FGETINFO failed. (FSERR !)
*>053 Forms File probably hasn't been compiled. (FSERR !)
*>060 The program supplied COMAREA extension is too small.
*>061 Failure to obtain required PASCAL Heap area.
*>062 Failure to return PASCAL Heap area.
*>$

     COMPUTE MYCOMLEN = FUNCTION LENGTH(COMAREA).
     IF MYCOMLEN < 120
      String "Comarea length may not be less than 120."
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

     IF FORM-FILE LESS THAN OR EQUAL TO SPACES
      String "Formsfile name not passed to VOPENFORMF"
       Delimited by size into vslog-REC
      END-STRING
      Move 42             to INTR-ERRNUM
      MOVE 42 TO CSTATUS
      GO TO PARMCHECK-EXIT.


 PARMCHECK-EXIT. EXIT.
*>---------------------------------------------------------------
 LOAD-FILE-DATA.
     PERFORM RUN-VFILE-QUERY.
     IF IFILE-LEN > 0
      MOVE VFILE TO IFILE
     ELSE
      GO TO Return-To-Caller.

*> GET PANEL TEMPLATE ASSOCIATED TO THE FILE TYPE
     PERFORM FIND-PD-TEMPLATE-BYNAME.
     IF TCL-ROWCOUNT > 0
      PERFORM GET-PD-TEMPLATE
      IF IPD-LEN > 0
       MOVE PD-TEMPLATE TO IPD-TEMPLATE
      ELSE
       Move "Failed to retrieve the panel template." to END-MSG
       GO TO Return-To-Caller
      END-IF.

*> GET ALL SAVE FIELDS (AKA GLOBAL FIELDS)
     INITIALIZE ISF-GLOBALS.
     PERFORM FIND-VSAVFLD-SERIAL.
     IF TCL-ROWCOUNT > 0
      PERFORM VARYING ISF-IDX FROM 1 BY 1 UNTIL ISF-IDX > TCL-ROWCOUNT
       PERFORM GET-VSAVFLD
       IF ISF-LEN > 0
        ADD 1 TO ISF-CNT
        MOVE VSAVFLD TO ISF-ENTRY(ISF-CNT)
       END-IF
      END-PERFORM
     END-IF.

*> GET ALL FIELD TEMPLATES
     INITIALIZE IFD-TABLE.
     PERFORM FIND-FD-TEMPLATE-SERIAL.
     IF TCL-ROWCOUNT > 0
      PERFORM VARYING IFD-IDX FROM 1 BY 1 UNTIL IFD-IDX > TCL-ROWCOUNT
       PERFORM GET-FD-TEMPLATE
       IF IFD-LEN > 0
        ADD 1 TO IFD-CNT
        MOVE FD-TEMPLATE TO IFD-ENTRY(IFD-CNT)
       END-IF
      END-PERFORM
     ELSE
      MOVE 1 TO MY-MS-LINE-CNT
      MOVE "s" TO MY-MS-ICON
      MOVE "o" TO MY-MS-BUTTON
      MOVE "Vue3 INTERNAL ERROR" to MY-MS-TITLE
      STRING "*** FIND-FD-TEMPLATE-SERIAL ***" X"0D0A" DELIMITED BY SIZE
             "*** NO TEMPLATES FOUND      ***" X"0D0A" DELIMITED BY SIZE
             "FORMS FILE="    DELIMITED BY SIZE
                  FORM-FILE   DELIMITED BY " "
             X"0D0A"   DELIMITED BY SIZE INTO MY-MS-TEXT
      END-STRING
      CALL "SP2" USING SP2-DISPLAY-MESSAGE MY-MESSAGE-DATA END-CALL
     END-IF.

*> GET ALL FORMS (AKA PANELS)
     INITIALIZE IPAN-TABLE.
     PERFORM FIND-VPANEL-BYTEMPLATE.
     IF TCL-ROWCOUNT > 0
      PERFORM VARYING IPAN-IDX FROM 1 BY 1
        UNTIL IPAN-IDX > TCL-ROWCOUNT
       PERFORM GET-VPANEL
       IF IPAN-LEN > 0
        ADD 1 TO IPAN-CNT
        MOVE "0" TO IPAN-SW(IPAN-CNT)
        MOVE FM-VPANEL TO IPAN-ENTRY(IPAN-CNT)
       END-IF
      END-PERFORM
     END-IF.
*>----------------------------------------------------------------
 FIND-PD-TEMPLATE-BYNAME.
     MOVE LOW-VALUES TO VPLUS-DB.
     STRING "GLOBAL.SECURE.SYS.vform" DELIMITED BY SIZE INTO VPLUS-DB.
     MOVE LOW-VALUES TO  VPLUS-DB-VAR.
     STRING "VPLUSDB" DELIMITED BY SIZE INTO VPLUS-DB-VAR.
     CALL "TCLSETVAR" USING MY-TCL, VPLUS-DB-VAR, VPLUS-DB.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "PD_TEMPLATE_NAME"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.

     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     STRING IFILE-TYPE
      DELIMITED BY SIZE INTO SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "PD_TEMPLATE:BYNAME"
      DELIMITED BY SIZE INTO SQL-TABLE.

     CALL "TCLSETVAR" USING MY-TCL, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     CALL "SQLGetResultSet" USING MY-TCL, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-PD-TEMPLATE.
      IF TCL-ROWCOUNT > 0
       CALL "SQLGetNext" USING MY-TCL, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
       END-CALL
      END-IF.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.
      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO IPD-LEN
       SET ADDRESS OF PD-TEMPLATE TO TCL-BUFFER-PTR.
*>---------------------------------------------------------------
 RUN-VFILE-QUERY.
     PERFORM FIND-VFILE-BYUNIQUE.
     IF TCL-ROWCOUNT > 0 and TCL-ROWCOUNT < 10
      PERFORM GET-VFILE
      MOVE VFILE TO IFILE
     ELSE
      MOVE 52 TO CSTATUS
      MOVE 0 TO IFILE-LEN
      INITIALIZE IFILE
     END-IF.
*>----------------------------------------------------------------
 FIND-VFILE-BYUNIQUE.
     MOVE LOW-VALUES TO VPLUS-DB.
     STRING FORM-FILE DELIMITED BY " "
      ".vform" DELIMITED BY SIZE INTO VPLUS-DB.
     MOVE LOW-VALUES TO  VPLUS-DB-VAR.
     STRING "VPLUSDB" DELIMITED BY SIZE INTO VPLUS-DB-VAR.
     CALL "TCLSETVAR" USING MY-TCL, VPLUS-DB-VAR, VPLUS-DB.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "VFIL_UNIQUE"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.

     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     STRING "1" DELIMITED BY SIZE INTO SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "VFILE:BYUNIQUE"
      DELIMITED BY SIZE INTO SQL-TABLE.

     CALL "TCLSETVAR" USING MY-TCL, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     CALL "SQLGetResultSet" USING MY-TCL, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-VFILE.
*>
*> After Gettting the Resultset you can loop through
*> one result at a time using SQLGetNext
*>
      IF TCL-ROWCOUNT > 0
       CALL "SQLGetNext" USING MY-TCL, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
       END-CALL
      END-IF.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.
      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO IFILE-LEN
       SET ADDRESS OF VFILE TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO IFILE-LEN
      END-IF.
*>----------------------------------------------------------------
 FIND-VSAVFLD-SERIAL.
     MOVE LOW-VALUES TO VPLUS-DB.
     STRING FORM-FILE DELIMITED BY " "
      ".vform" DELIMITED BY SIZE INTO VPLUS-DB.
     MOVE LOW-VALUES TO  VPLUS-DB-VAR.
     STRING "VPLUSDB" DELIMITED BY SIZE INTO VPLUS-DB-VAR.
     CALL "TCLSETVAR" USING MY-TCL, VPLUS-DB-VAR, VPLUS-DB.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "SLENGTH"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.

     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     STRING "0" DELIMITED BY SIZE INTO SQL-SEARCH-KEY.
     CALL "TCLSETVAR" USING MY-TCL, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "VSAVFLD:SERIAL"
      DELIMITED BY SIZE INTO SQL-TABLE.
     CALL "SQLGetResultSet" USING MY-TCL, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-VSAVFLD.
      CALL "SQLGetNext" USING MY-TCL, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
      END-CALL.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.
      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO ISF-LEN
       SET ADDRESS OF VSAVFLD TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO ISF-LEN
      END-IF.
*>----------------------------------------------------------------
 FIND-VPANEL-BYTEMPLATE.
     MOVE LOW-VALUES TO VPLUS-DB.
     STRING FORM-FILE DELIMITED BY " "
      ".vform" DELIMITED BY SIZE INTO VPLUS-DB.
     MOVE LOW-VALUES TO  VPLUS-DB-VAR.
     STRING "VPLUSDB" DELIMITED BY SIZE INTO VPLUS-DB-VAR.
     CALL "TCLSETVAR" USING MY-TCL, VPLUS-DB-VAR, VPLUS-DB.
     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "PD_TEMPLATE_ID"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.
     MOVE IPD-TEMPLATE-ID TO DISPL-99.
     STRING DISPL-99 DELIMITED BY SIZE INTO SQL-SEARCH-KEY.
     CALL "TCLSETVAR" USING MY-TCL, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "VPANEL:BYTEMPLATE"
      DELIMITED BY SIZE INTO SQL-TABLE.
     CALL "SQLGetResultSet" USING MY-TCL, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-VPANEL.
      CALL "SQLGetNext" USING MY-TCL, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
      END-CALL.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.

      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO IPAN-LEN
       SET ADDRESS OF FM-VPANEL TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO IPAN-LEN
      END-IF.
*>---------------------------------------------------------
 TCL-INIT.
     CALL "STARTTCL" USING MY-TCL TCL-RESULT.
     IF TCL-RESULT NOT = 0
      MOVE "FAILED TO INITIALIZE TCL" TO END-MSG
      GO TO Return-To-Caller.

     MOVE 0 TO IDX.
     MOVE LOW-VALUES TO VPLUS-DB.
     STRING FORM-FILE DELIMITED BY " "
      ".vform" DELIMITED BY SIZE INTO VPLUS-DB.
     MOVE LOW-VALUES TO  VPLUS-DB-VAR.
     STRING "VPLUSDB" DELIMITED BY SIZE INTO VPLUS-DB-VAR.
     CALL "TCLSETVAR" USING MY-TCL, VPLUS-DB-VAR, VPLUS-DB.
*>----------------------------------------------------------------
 FIND-FD-TEMPLATE-SERIAL.
     MOVE LOW-VALUES TO VPLUS-DB.
     STRING "GLOBAL.SECURE.SYS.vform" DELIMITED BY SIZE INTO VPLUS-DB.
     MOVE LOW-VALUES TO  VPLUS-DB-VAR.
     STRING "VPLUSDB" DELIMITED BY SIZE INTO VPLUS-DB-VAR.
     CALL "TCLSETVAR" USING MY-TCL, VPLUS-DB-VAR, VPLUS-DB.
     INITIALIZE SQL-TABLE TCL-ROWCOUNT TCL-RESULT TCL-ERROR.
     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "FD_TEMPLATE_ID"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.
     STRING "0" DELIMITED BY SIZE INTO SQL-SEARCH-KEY.
     CALL "TCLSETVAR" USING MY-TCL, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "FD_TEMPLATE:SERIAL"
      DELIMITED BY SIZE INTO SQL-TABLE.
     CALL "SQLGetResultSet" USING MY-TCL, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-FD-TEMPLATE.
      CALL "SQLGetNext" USING MY-TCL, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
      END-CALL.
      IF TCL-RESULT NOT = 0
       MOVE "SQLGetNext FAILED!" TO END-MSG
      END-IF.

      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO IFD-LEN
       SET ADDRESS OF FD-TEMPLATE TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO IFD-LEN
      END-IF.

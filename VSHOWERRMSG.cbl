>>source format free.
*>******************************************************************
*>*                     V S H O W E R R M S G                      *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSHOWERRMSG.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    COPY "vplus/SP250".
 01  MESSAGE-BUFFER             PIC X(150) VALUE SPACES.
 01  MESSAGE-LENGTH             PIC S9(4) COMP-5 VALUE 0.
 01  MY-CONVERSE-DATA.
     05  MY-CD-RET-CODE         PIC S9(4) COMP-5.
     05  MY-CD-LENS.
         10  MY-CD-LEN-LEN      PIC S9(4) COMP-5.
         10  MY-CD-IP-NUM-LEN   PIC S9(4) COMP-5.
         10  MY-CD-IP-CHAR-LEN  PIC S9(4) COMP-5.
         10  MY-CD-OP-NUM-LEN   PIC S9(4) COMP-5.
         10  MY-CD-OP-CHAR-LEN  PIC S9(4) COMP-5.
         10  MY-CD-FIELD-LEN    PIC S9(4) COMP-5.
         10  MY-CD-COLR-LEN     PIC S9(4) COMP-5.
         10  MY-CD-TYPE-LEN     PIC S9(4) COMP-5.
         10  FILLER             PIC S9(4) COMP-5.
         10  FILLER             PIC S9(4) COMP-5.
     05  MY-CD-DATA.
       06  MY-CD-IP-NUM-DATA.
         10  MY-CD-KEY          PIC S9(4) COMP-5.
         10  MY-CD-NEXT-FLD-ID  PIC S9(4) COMP-5.
         10  MY-CD-NEXT-FLD-NUM PIC S9(4) COMP-5.
         10  MY-CD-NEXT-TAB-NUM PIC S9(4) COMP-5.
         10  MY-CD-NEXT-OCCURS  PIC S9(4) COMP-5.
         10  MY-CD-LAST-FLD-ID  PIC S9(4) COMP-5.
         10  MY-CD-LAST-FLD-NUM PIC S9(4) COMP-5.
         10  MY-CD-LAST-TAB-NUM PIC S9(4) COMP-5.
         10  MY-CD-LAST-OCCURS  PIC S9(4) COMP-5.
         10  MY-CD-MENU-ID      PIC S9(4) COMP-5.
         10  MY-CD-CTRL-FIELD-KEY REDEFINES MY-CD-MENU-ID
                                 PIC S9(4) COMP-5.
         10  MY-CD-BUTTON-ID REDEFINES MY-CD-MENU-ID
                                 PIC S9(4) COMP-5.
         10  MY-CD-ROW-COL-SW   PIC S9(4) COMP-5.
         10  MY-CD-CURSOR-ROW   PIC S9(4) COMP-5.
         10  MY-CD-CURSOR-COL   PIC S9(4) COMP-5.
         10  MY-CD-LAST-ROW     PIC S9(4) COMP-5.
         10  MY-CD-LAST-COL     PIC S9(4) COMP-5.
         10  MY-CD-DISP-SW      PIC S9(4) COMP-5.
         10  MY-CD-NEXT-VERT    PIC S9(4) COMP-5.
         10  MY-CD-LAST-VERT    PIC S9(4) COMP-5.
         10  MY-CD-NEXT-HOR     PIC S9(4) COMP-5.
         10  MY-CD-LAST-HOR     PIC S9(4) COMP-5.
       06  MY-CD-IP-CHAR-DATA.
         10  MY-CD-NEXT-PANEL   PIC X(8).
         10  MY-CD-NEXT-FIELD   PIC X(30).
         10  MY-CD-LAST-FIELD   PIC X(30).
         10  MY-CD-MENU-OPTION  PIC X(30).
         10  MY-CD-SWITCH-SW    PIC X.
         10  MY-CD-SIZE-SW      PIC X.
         10  MY-CD-MOUSE-SW     PIC X.
         10  MY-CD-CAPTURE-SW   PIC X.
         10  MY-CD-WAIT-SW      PIC X.
         10  MY-CD-CURS-SW      PIC X.
         10  MY-CD-CHG-SW       PIC X.
         10  MY-CD-TIMEOUT      PIC X.
       06  MY-CD-OP-NUM-DATA.
         10  MY-CD-PAN-POS-SW   PIC S9(4) COMP-5.
         10  MY-CD-PAN-ROW      PIC S9(4) COMP-5.
         10  MY-CD-PAN-COL      PIC S9(4) COMP-5.
       06  MY-CD-OP-CHAR-DATA.
         10  MY-CD-NEW-WINDOW   PIC X.
         10  MY-CD-DISPLAY-SW   PIC X.
       06  MY-CD-DATABUFFER   PIC X(2000).
       06  MY-CD-COLR-DATA    PIC X(512).
       06  MY-CD-TYPE-DATA    PIC X(512).
 01 X               PIC S9(4) COMP-5 VALUE 0.
 01 DISP-N1         PIC ZZZZ9 VALUE ZERO.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA.
*>*---------------------------------------------------------*
 0000-BEGIN-VSHOWERRMSG.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES TO INTR-CALLNAME
     string "VSHOWERRMSG "
      delimited by "  " into INTR-CALLNAME.


     MOVE CSTATUS TO DISP-N1.
     MOVE LOW-VALUES TO SP2-WD-DATA.
     MOVE "VUE3err"              TO SP2-WD-NAME.
     MOVE "m"                   TO SP2-WD-BOR-TYPE.
     MOVE 64                    TO SP2-WD-WIDTH.
     MOVE 20                    TO SP2-WD-HEIGHT.
     MOVE 29                    TO SP2-WD-ROW.
     MOVE 48                    TO SP2-WD-COL.
     MOVE 8                     TO SP2-WD-CELL-WIDTH.
     MOVE 16                    TO SP2-WD-CELL-HEIGHT.
     MOVE X"01"                 TO SP2-WD-MORE-OPTIONS.
     CALL "SP2" USING SP2-OPEN-WINDOW SP2-WINDOW-DEF.
     MOVE LOW-VALUES TO SP2-PD-DATA.
     MOVE 84                    TO SP2-PD-WIDTH.
     MOVE 28                    TO SP2-PD-HEIGHT.
     MOVE 19                    TO SP2-PD-ROW.
     MOVE 40                    TO SP2-PD-COL.
     MOVE -1                    TO SP2-PD-FONT-ID.
     MOVE -1                    TO SP2-PD-TITLE-ROWS.
     MOVE -1                    TO SP2-PD-MENU-ROWS.
     MOVE "VUE3ERR"             TO SP2-PD-NAME.
     MOVE LOW-VALUES            TO SP2-PD-MENU-NAME.
     MOVE "VIEWSUB/VUE3 ERROR"  TO SP2-PD-TITLE.
     Move 80 to sp2-pd-title-len.
     MOVE 8                     TO SP2-PD-CELL-WIDTH.
     MOVE 16                    TO SP2-PD-CELL-HEIGHT.
     MOVE X"01"                 TO SP2-PD-MORE-OPTIONS.
     MOVE 415            TO SP2-PD-CTRL-KEY (1).
     MOVE 416            TO SP2-PD-CTRL-KEY (2).
     MOVE 417            TO SP2-PD-CTRL-KEY (3).
     MOVE 418            TO SP2-PD-CTRL-KEY (4).
     MOVE 419            TO SP2-PD-CTRL-KEY (5).
     MOVE 420            TO SP2-PD-CTRL-KEY (6).
     MOVE 421            TO SP2-PD-CTRL-KEY (7).
     MOVE 422            TO SP2-PD-CTRL-KEY (8).
     MOVE 13         TO SP2-PD-CTRL-KEY (9).
      Compute SP2-PD-DESC-LEN
          = function length(SP2-PD-DESCRIPTION).
      Compute SP2-PD-TITLE-LEN
          = function length(SP2-PD-TITLE).
      Compute SP2-PD-CURS-KEY-LEN
          = function length(SP2-PD-CURS-KEYS).
      Compute SP2-PD-CTRL-KEY-LEN
          = function length(SP2-PD-CTRL-KEYS).
      Compute SP2-PD-MSG-TEXT-LEN
          = function length(SP2-PD-MSG-TEXT).
      Compute SP2-PD-USER-LEN
          = function length(SP2-PD-USER-DATA).
      Compute SP2-PD-HELP-LEN
          = function length(SP2-PD-HELP-KEYWORD).
      MOVE 0                  TO SP2-PD-VAR-LEN.
      ADD SP2-PD-DESC-LEN     TO SP2-PD-VAR-LEN.
      ADD SP2-PD-TITLE-LEN    TO SP2-PD-VAR-LEN.
      ADD SP2-PD-CURS-KEY-LEN TO SP2-PD-VAR-LEN.
      ADD SP2-PD-CTRL-KEY-LEN TO SP2-PD-VAR-LEN.
      ADD SP2-PD-SYNS-LEN     TO SP2-PD-VAR-LEN.
      ADD SP2-PD-MSG-TEXT-LEN TO SP2-PD-VAR-LEN.
      ADD SP2-PD-USER-LEN     TO SP2-PD-VAR-LEN.
      ADD SP2-PD-HELP-LEN     TO SP2-PD-VAR-LEN.
     CALL "SP2" USING SP2-SET-PANEL-DEF SP2-PANEL-DEF.
*>
*> ----  LINE OF TEXT
     MOVE LOW-VALUES           TO SP2-SD-DATA.
     MOVE 1        TO SP2-FD-ID.
     MOVE 1        TO SP2-SD-ROW.
     MOVE 1        TO SP2-SD-COL.
     MOVE 600      TO SP2-SD-WIDTH.
     Move 60       TO SP2-SD-VAR-LEN.
     Move 60       TO SP2-SD-TEXT-LEN.
     MOVE 10       TO SP2-SD-HEIGHT.
     STRING "*** VIEW ERROR " DISP-N1 " OCCURED ***"
      DELIMITED BY SIZE INTO SP2-SD-TEXT.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.
*>
*> ----  LINE OF TEXT
     MOVE LOW-VALUES           TO SP2-SD-DATA.
     MOVE 2        TO SP2-FD-ID.
     MOVE 2        TO SP2-SD-ROW.
     MOVE 1        TO SP2-SD-COL.
     MOVE 800      TO SP2-SD-WIDTH.
     Move 80       TO SP2-SD-VAR-LEN.
     Move 80       TO SP2-SD-TEXT-LEN.
     MOVE 10       TO SP2-SD-HEIGHT.
     MOVE SPACES TO MESSAGE-BUFFER.
     MOVE 79 TO MESSAGE-LENGTH.
     CALL "VERRMSG" USING COMAREA MESSAGE-BUFFER
                          MESSAGE-LENGTH X.
     MOVE MESSAGE-BUFFER TO SP2-SD-TEXT.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.
*>
*> ----  LINE OF TEXT
     MOVE LOW-VALUES           TO SP2-SD-DATA.
     MOVE 2        TO SP2-FD-ID.
     MOVE 4        TO SP2-SD-ROW.
     MOVE 1        TO SP2-SD-COL.
     MOVE 600      TO SP2-SD-WIDTH.
     Move 60       TO SP2-SD-VAR-LEN.
     Move 60       TO SP2-SD-TEXT-LEN.
     MOVE 10                   TO SP2-SD-HEIGHT.
     STRING "FORMS FILE=" MPE-FORMS-FILE
      DELIMITED BY SIZE INTO SP2-SD-TEXT.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.
*>
*> ----  LINE OF TEXT
     MOVE LOW-VALUES           TO SP2-SD-DATA.
     MOVE 3        TO SP2-FD-ID.
     MOVE 5        TO SP2-SD-ROW.
     MOVE 1        TO SP2-SD-COL.
     MOVE 600      TO SP2-SD-WIDTH.
     Move 60       TO SP2-SD-VAR-LEN.
     Move 60       TO SP2-SD-TEXT-LEN.
     MOVE 10                   TO SP2-SD-HEIGHT.
     STRING "FORM=" MM-NFNAME
      DELIMITED BY SIZE INTO SP2-SD-TEXT.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.

*> ADD BUTTON
     MOVE LOW-VALUES TO SP2-FD-DATA.
     MOVE "OK" To SP2-FD-NAME.
     MOVE 15                TO SP2-FD-ROW.
     MOVE 27                TO SP2-FD-COL.
     MOVE 90                          TO SP2-FD-WIDTH.
     MOVE 21                          TO SP2-FD-HEIGHT.
     MOVE "r"                         TO SP2-FD-BOR-TYPE.
     Move -3                          To SP2-FD-FONT-ID.
     MOVE 415                         TO SP2-FD-HELP-KEY.
     MOVE 2                           TO SP2-FD-MAX-LEN.
     MOVE 2                           TO SP2-FD-PROG-NUM.
     MOVE "p"                         TO SP2-FD-CTRL-TYPE.
     Move "   OK   "    To SP2-FD-VAR-DATA.
*     Move X"0D0A"                     To SP2-FD-VAR-DATA(9:2).
*     Move FFFLDS-VFLD-INITVAL(9:8)    To SP2-FD-VAR-DATA(11:8).
     MOVE LOW-VALUES                  TO SP2-FD-VAR-LENS.
     MOVE 18                          TO SP2-FD-INITIAL-LEN.
     MOVE 18                          TO SP2-FD-VAR-LEN.
     move 16                          to SP2-FD-OPTIONS-3.
     MOVE 0    TO SP2-FD-TAB-NUM.
     MOVE "y"  TO SP2-FD-OUTPUT.
     COMPUTE SP2-FD-ID = SP2-FD-HELP-KEY.
     CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.

*> read panel into window
     move "VUE3ERR" to sp2-nd-name.
     call "SP2" using sp2-read-panel sp2-name-def.

     MOVE LOW-VALUES TO MY-CD-DATA.
     COMPUTE MY-CD-LEN-LEN
      = FUNCTION LENGTH(MY-CD-LENS).
     COMPUTE MY-CD-IP-NUM-LEN
      = FUNCTION LENGTH(MY-CD-IP-NUM-DATA).
     COMPUTE MY-CD-IP-CHAR-LEN
      = FUNCTION LENGTH(MY-CD-IP-CHAR-DATA).
     COMPUTE MY-CD-OP-NUM-LEN
      = FUNCTION LENGTH(MY-CD-OP-NUM-DATA).
     COMPUTE MY-CD-OP-CHAR-LEN
      = FUNCTION LENGTH(MY-CD-OP-CHAR-DATA).
     COMPUTE MY-CD-FIELD-LEN
      = FUNCTION LENGTH(MY-CD-DATABUFFER).
     COMPUTE MY-CD-COLR-LEN
      = FUNCTION LENGTH(MY-CD-COLR-DATA).
     COMPUTE MY-CD-TYPE-LEN
      = FUNCTION LENGTH(MY-CD-TYPE-DATA).
     MOVE "VUE3ERR" TO MY-CD-NEXT-PANEL.
     INITIALIZE MY-CD-DATABUFFER.
     MOVE "x" TO MY-CD-NEW-WINDOW.
     CALL "SP2" USING SP2-CONVERSE-PANEL MY-CONVERSE-DATA.
     GOBACK.


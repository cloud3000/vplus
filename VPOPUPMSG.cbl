>>source format free.
*>******************************************************************
*>*                          V P O P U P                           *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VPOPUPMSG.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Valid-Response-Sw            Pic 9 Value 0.
    88 Valid-Response value 1.
 01 MY-CONVERSE-DATA.
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
          10  MY-CD-BUTTON-ID      REDEFINES MY-CD-MENU-ID
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
          10  MY-CD-LAST-HOR     PIC S9(4) COMP-5 SYNC.
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
        06  MY-CD-DATABUFFER     PIC X(3000).
        06  MY-CD-COLR-DATA      PIC X(512).
        06  MY-CD-TYPE-DATA      PIC X(512).

 01 Default-Key                      Pic 9(4) Comp Value 0.
 01 MSGIDX                           Pic 9(4) Comp Value 0.
 01 BTNIDX                           Pic 9(4) Comp Value 0.
 01 Meta-Data.
    05 MD-LineCount                  Pic 9(4) Comp Value 0.
    05 MD-BtnCount                   Pic 9(4) Comp Value 0.
    05 MD-Buttons.
     07 Button-Entries OCCURS 7.
       10 BtnNum                     Pic 9(4) Comp Value 0.
       10 BtnName                    Pic X(6) Value Spaces.
       10 BtnLabel                   Pic X(16) Value Spaces.


 01  BTN-FIELD-DEF.
     05  BTN-FD-RET-CODE         PIC S9(4) COMP-5.
     05  BTN-FD-LENS.
         10  BTN-FD-LEN-LEN      PIC S9(4) COMP-5 VALUE +30.
         10  BTN-FD-NUM-LEN      PIC S9(4) COMP-5 VALUE +52.
         10  BTN-FD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +74.
         10  BTN-FD-VAR-LEN      PIC S9(4) COMP-5 VALUE +22.
         10  BTN-FD-VAR-LENS.
             15  BTN-FD-FORMAT-LEN   PIC S9(4) COMP-5 VALUE +6.
             15  BTN-FD-CAPTION-LEN  PIC S9(4) COMP-5 VALUE +0.
             15  BTN-FD-INITIAL-LEN  PIC S9(4) COMP-5 VALUE +16.
             15  FILLER              PIC S9(4) COMP-5 VALUE +0.
             15  BTN-FD-RANGE-LEN    PIC S9(4) COMP-5 VALUE +0.
             15  BTN-FD-DISCRETE-LEN PIC S9(4) COMP-5 VALUE +0.
             15  BTN-FD-MSG-TEXT-LEN PIC S9(4) COMP-5 VALUE +0.
             15  BTN-FD-USER-LEN     PIC S9(4) COMP-5 VALUE +0.
             15  BTN-FD-HELP-LEN     PIC S9(4) COMP-5 VALUE +0.
         10  BTN-FD-LENL-LEN         PIC S9(4) COMP-5 VALUE -1.
         10  BTN-FD-NUML-LEN         PIC S9(4) COMP-5 VALUE +44.
     05  BTN-FD-DATA.
*>******** BTN-FD-NUM-DATA ********
         10  BTN-FD-ID           PIC S9(4) COMP-5.
         10  BTN-FD-GUI-ID       PIC S9(4) COMP-5.
         10  BTN-FD-GUI-ID-2     PIC S9(4) COMP-5.
         10  BTN-FD-OCCURRENCE   PIC S9(4) COMP-5.
         10  BTN-FD-BASE-ID      PIC S9(4) COMP-5.
         10  BTN-FD-ROW          PIC S9(4) COMP-5.
         10  BTN-FD-COL          PIC S9(4) COMP-5.
         10  BTN-FD-PROG-OFF     PIC S9(4) COMP-5.
         10  BTN-FD-FLD-NUM      PIC S9(4) COMP-5.
         10  BTN-FD-TAB-NUM      PIC S9(4) COMP-5.
         10  BTN-FD-PROG-NUM     PIC S9(4) COMP-5.
         10  BTN-FD-WIDTH        PIC S9(4) COMP-5.
         10  BTN-FD-HEIGHT       PIC S9(4) COMP-5.
         10  BTN-FD-MAX-LEN      PIC S9(4) COMP-5.
         10  BTN-FD-PROG-LEN     PIC S9(4) COMP-5.
         10  BTN-FD-ITEM-LEN     PIC S9(4) COMP-5.
         10  FILLER              PIC S9(4) COMP-5.
         10  BTN-FD-HELP-KEY     PIC S9(4) COMP-5.
         10  FILLER              PIC S9(4) COMP-5.
         10  BTN-FD-GROUP-ID     PIC S9(4) COMP-5.
         10  BTN-FD-REPEAT-ID    PIC S9(4) COMP-5.
         10  BTN-FD-FONT-ID      PIC S9(4) COMP-5.
         10  BTN-FD-PROG-OFF-L   PIC S9(8) COMP-5.
         10  BTN-FD-PROG-NUM-L   PIC S9(8) COMP-5.
*>******** BTN-FD-CHAR-DATA *******
         10  BTN-FD-NAME         PIC X(30).
         10  BTN-FD-TYPE         PIC X.
         10  BTN-FD-OUTPUT       PIC X Value "y".
         10  BTN-FD-PROG-DEC     PIC X.
         10  FILLER              PIC X(4).
         10  BTN-FD-INIT-NUMS    PIC X.
         10  BTN-FD-MISC-OPTIONS PIC X.
         10  FILLER              PIC X.
         10  BTN-FD-HELP         PIC X(8).
         10  BTN-FD-MORE-OPTIONS PIC X.
         10  BTN-FD-BOR-COLR     PIC X.
         10  BTN-FD-ANCHOR       PIC X.
         10  BTN-FD-OPTIONS-3    PIC X.
         10  FILLER              PIC X(4).
         10  BTN-FD-REQUIRED     PIC X.
         10  BTN-FD-PROG-CTRL    PIC X.
         10  BTN-FD-JUSTIFY      PIC X.
         10  BTN-FD-FILL         PIC X.
         10  BTN-FD-ASSUME-DEC   PIC X.
         10  BTN-FD-SPEC-FMT     PIC X.
         10  BTN-FD-CASE         PIC X.
         10  BTN-FD-IMBED-BLANKS PIC X.
         10  BTN-FD-CUR-COLR     PIC X.
         10  BTN-FD-CURS-SKIP    PIC X.
         10  BTN-FD-CURS-SHOW    PIC X.
         10  BTN-FD-BLANK-FIRST  PIC X.
         10  BTN-FD-BLANK-ZERO   PIC X.
         10  BTN-FD-CTRL-TYPE    PIC X Value "p".
         10  BTN-FD-COLR         PIC X.
         10  BTN-FD-MNEMONIC     PIC X.
         10  BTN-FD-BOR-TYPE     PIC X.
         10  BTN-FD-PROG-SPEC    PIC X.
*>******** BTN-FD-VAR-DATA ********
         10  BTN-FD-VAR-DATA     PIC X(3000).
         10  BTN-FD-USER-DATA    PIC X(8).
*>******** BTN-FD-FMT *************
*>******** BTN-FD-CAPTION *********
*>******** BTN-FD-INITIAL-VAL *****
*>******** BTN-FD-RANGE-VALS ******
*>******** BTN-FD-DISC-VALS *******
*>******** BTN-FD-MSG-TEXT ********
*>******** BTN-FD-USER-DATA *******
*>******** BTN-FD-HELP-KEYWORD ****
    COPY "vplus/SP250".


 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 VPOPUP.
    02 VP-Name     Pic X(8).
    02 VP-Message.
       05 VPM-Text Pic X(80) OCCURS 4.
    02 VP-Type     Pic x.
       88 vp-bang  value "b". *> = bang (exclamation mark)
       88 vp-stop  value "s". *> = stop sign
       88 vp-info  value "i". *> = information
       88 vp-quest value "q". *> = question
    02 VP-Button   Pic x.
       88 vp-ok    value "o". *> = ok
       88 vp-yn    value "y". *> = yes/no
       88 vp-ny    value "n". *> = no/yes
       88 vp-retry value "r". *> = retry/cancel
    02 VP-Cancel   Pic X.
       88 vp-canc  value "y". *> = display cancel button
    02 VP-Reply    Pic X.
       88 vpr-ok   value "o". *> = ok
       88 vpr-yes  value "y". *> = yes
       88 vpr-no   value "n". *> = no
       88 vpr-rtry value "r". *> = retry
       88 vpr-canc value "c". *> = cancel
    02 VP-Timeout  Pic s9(4) Comp.

    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING COMAREA VPOPUP.
 BEGIN-VPOPUP.

     Perform ParmCheck-MetaData.
     Perform Open-VPOPUP-Window.
     Perform Create-VPOPUP-Panel.
     Perform Add-VPOPUP-Message.
     Perform Add-VPOPUP-Buttons.
     Perform VPOPUP-Conversation.
     CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM.

 END-VPOPUP.
     GOBACK.


*>----------------------------------------------------
 ParmCheck-MetaData.

     Evaluate True
        When vp-ok
           Move 1 To MD-BtnCount
           If vp-canc
              Add 1 To MD-BtnCount
              Move "CANCEL" To BtnLabel(2)
              Move 7        To BtnNum(2)
              Move "FKEY"   To BtnName(2)
              Move "OK"   To BtnLabel(1)
              Move 1      To BtnNum(1) Default-Key

              Move "FKEY" To BtnName(1)
           Else
              Move "OK"   To BtnLabel(1)
              Move 4      To BtnNum(1) Default-Key
              Move "FKEY" To BtnName(1)

        When vp-yn
           Move 2 To MD-BtnCount
           Move "YES"  To BtnLabel(1)
           Move 1      To BtnNum(1) Default-Key
           Move "FKEY" To BtnName(1)
           Move "NO"   To BtnLabel(2)
           Move 7      To BtnNum(2)
           Move "FKEY" To BtnName(2)
           If vp-canc
              Add 1 To MD-BtnCount
              Move "CANCEL" To BtnLabel(3)
              Move 6        To BtnNum(3)
              Move "FKEY"   To BtnName(3)

        When vp-ny
           Move 2 To MD-BtnCount
           Move "NO"   To BtnLabel(1)
           Move 1      To BtnNum(1) Default-Key
           Move "FKEY" To BtnName(1)
           Move "YES"  To BtnLabel(2)
           Move 7      To BtnNum(2)
           Move "FKEY" To BtnName(2)
           If vp-canc
              Add 1 To MD-BtnCount
              Move "CANCEL" To BtnLabel(3)
              Move 2        To BtnNum(3)
              Move "FKEY"   To BtnName(3)

        When vp-retry
           Move 2 To MD-BtnCount
           Move "RETRY"   To BtnLabel(1)
           Move 1         To BtnNum(1) Default-Key
           Move "FKEY"    To BtnName(1)
           Move "CANCEL"  To BtnLabel(2)
           Move 7         To BtnNum(2)
           Move "FKEY"    To BtnName(2)
     End-Evaluate.

     Perform Varying Msgidx From 1 By 1 Until Msgidx > 4

        If VPM-Text(MsgIdx) > Spaces
           Move MsgIdx To MD-LineCount
        End-If

     End-Perform.

*>----------------------------------------------------
 Open-VPOPUP-Window.
     MOVE LOW-VALUES TO SP2-WD-DATA.
     MOVE 800             TO SP2-WD-WIDTH.
     Compute SP2-WD-HEIGHT = MD-LineCount + 19.
     MOVE -9999           TO SP2-WD-ROW.
     MOVE -9999           TO SP2-WD-COL.
     MOVE -1              TO SP2-WD-TITLE-ROWS.
     MOVE -1              TO SP2-WD-MENU-ROWS.
     MOVE "VPOPUP"       TO SP2-WD-NAME.
     String "Message for " VP-Name  Delimited by size inTO SP2-WD-TITLE.
     CALL "SP2" USING SP2-OPEN-WINDOW SP2-WINDOW-DEF.

*>----------------------------------------------------
 Create-VPOPUP-Panel.
     MOVE LOW-VALUES TO SP2-PD-DATA.
     MOVE 800                TO SP2-PD-WIDTH.
     Compute SP2-PD-HEIGHT = MD-LineCount + 19.
     MOVE 0                 TO SP2-PD-ROW.
     MOVE 0                 TO SP2-PD-COL.
     MOVE "VPOPUP"         TO SP2-PD-NAME.
     MOVE SP2-KEY-LEFT      TO SP2-PD-LEFT.
     MOVE SP2-KEY-RIGHT     TO SP2-PD-RIGHT.
     MOVE SP2-KEY-UP        TO SP2-PD-UP.
     MOVE SP2-KEY-DOWN      TO SP2-PD-DOWN.
     MOVE SP2-KEY-TAB       TO SP2-PD-TAB.
     MOVE SP2-KEY-BACKTAB   TO SP2-PD-BACKTAB.
     MOVE SP2-KEY-BACKSPAC  TO SP2-PD-BACKSPAC.
     MOVE SP2-KEY-DELETE    TO SP2-PD-DELETE.
     MOVE SP2-KEY-INSERT    TO SP2-PD-INSERT.
     MOVE SP2-KEY-HOME      TO SP2-PD-HOME.
     MOVE SP2-KEY-END       TO SP2-PD-END.
     MOVE SP2-KEY-CTRL-HOME TO SP2-PD-HOME-PAN.
     MOVE SP2-KEY-CTRL-END  TO SP2-PD-END-PAN.
     MOVE SP2-KEY-ENTER     TO SP2-PD-CTRL-KEY (1).
     MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (2).
     MOVE SP2-KEY-F1        TO SP2-PD-CTRL-KEY (3).
     MOVE SP2-KEY-F2        TO SP2-PD-CTRL-KEY (4).
     MOVE SP2-KEY-F4        TO SP2-PD-CTRL-KEY (5).
     MOVE SP2-KEY-F6        TO SP2-PD-CTRL-KEY (6).
     MOVE SP2-KEY-F7        TO SP2-PD-CTRL-KEY (7).
     MOVE 500               TO SP2-PD-CTRL-KEY (8).
     CALL "SP2" USING SP2-SET-PANEL-DEF SP2-PANEL-DEF.

*>----------------------------------------------------
 Add-VPOPUP-Message.

     Perform Varying Msgidx From 1 By 1 Until Msgidx > 4

        MOVE LOW-VALUES TO SP2-SD-DATA
        MOVE Msgidx TO SP2-SD-ROW
        MOVE 5 TO SP2-SD-COL
        MOVE 200 TO SP2-SD-WIDTH
        MOVE 10 TO SP2-SD-HEIGHT
        MOVE VPM-Text(MsgIdx) TO SP2-SD-TEXT
        CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF

     End-Perform.
*>----------------------------------------------------
 Add-VPOPUP-Buttons.
     Perform Varying BTNIDX From 1 by 1 Until BTNIDX > MD-BtnCount
        Perform Add-VPOPUP-Button
     End-Perform.
*>----------------------------------------------------
 Add-VPOPUP-Button.
     MOVE LOW-VALUES TO BTN-FD-DATA.
     MOVE BtnNum(BTNIDX)   TO BTN-FD-ID.
     MOVE BtnName(BTNIDX)  To BTN-FD-NAME.
     Move 15                        To BTN-FD-ROW.
     Compute BTN-FD-COL = BtnNum(BTNIDX) * 9 END-COMPUTE.
     MOVE 720                       TO BTN-FD-WIDTH.
     MOVE 168                       TO BTN-FD-HEIGHT.
     MOVE "r"                       TO BTN-FD-BOR-TYPE.
     Move 1                         To BTN-FD-FONT-ID.
     MOVE 17                        TO BTN-FD-MAX-LEN.
     Compute BTN-FD-HELP-KEY = BtnNum(BTNIDX) + 314.
     Move BtnNum(BTNIDX)                    To BTN-FD-PROG-NUM.
     MOVE "p"                       TO BTN-FD-CTRL-TYPE.
     Move BtnLabel(BTNIDX)                  To BTN-FD-VAR-DATA.
     MOVE LOW-VALUES                TO BTN-FD-VAR-LENS.
     MOVE 16                        TO BTN-FD-INITIAL-LEN.
     MOVE 22                        TO BTN-FD-VAR-LEN.
     move 49                        TO BTN-FD-OPTIONS-3.
     MOVE BtnNum(BTNIDX)                    TO BTN-FD-TAB-NUM.
     Move "Y"                       TO BTN-FD-OUTPUT.
     COMPUTE BTN-FD-ID = BTN-FD-HELP-KEY + 100.
     CALL "SP2" USING SP2-SET-FIELD-DEF BTN-FIELD-DEF.

*>----------------------------------------------------
 VPOPUP-Conversation.
     MOVE LOW-VALUES TO MY-CD-DATA.
     COMPUTE MY-CD-LEN-LEN      = FUNCTION LENGTH(MY-CD-LENS).
     COMPUTE MY-CD-IP-NUM-LEN   = FUNCTION LENGTH(MY-CD-IP-NUM-DATA).
     COMPUTE MY-CD-IP-CHAR-LEN  = FUNCTION LENGTH(MY-CD-IP-CHAR-DATA).
     COMPUTE MY-CD-OP-NUM-LEN   = FUNCTION LENGTH(MY-CD-OP-NUM-DATA).
     COMPUTE MY-CD-OP-CHAR-LEN  = FUNCTION LENGTH(MY-CD-OP-CHAR-DATA).
     COMPUTE MY-CD-FIELD-LEN    = FUNCTION LENGTH(MY-CD-DATABUFFER).
     COMPUTE MY-CD-COLR-LEN     = FUNCTION LENGTH(MY-CD-COLR-DATA).
     COMPUTE MY-CD-TYPE-LEN     = FUNCTION LENGTH(MY-CD-TYPE-DATA).

     INITIALIZE MY-CD-DATABUFFER.
     MOVE "x"               TO MY-CD-NEW-WINDOW.
     MOVE "VPOPUP"          TO MY-CD-NEXT-PANEL.
     Move 0 To Valid-Response-Sw.

     Perform Until Valid-Response
        CALL "SP2" USING SP2-CONVERSE-PANEL MY-CONVERSE-DATA
        If  My-Cd-Key = -1
           Move 1 To Valid-Response-Sw
           Exit Perform
        End-If

        EVALUATE MY-CD-KEY
          WHEN 13  MOVE  Default-Key TO LASTKEY
          WHEN 315 MOVE  1 TO LASTKEY
          WHEN 316 MOVE  2 TO LASTKEY
          WHEN 317 MOVE  3 TO LASTKEY
          WHEN 318 MOVE  4 TO LASTKEY
          WHEN 319 MOVE  5 TO LASTKEY
          WHEN 320 MOVE  6 TO LASTKEY
          WHEN 321 MOVE  7 TO LASTKEY
          WHEN 322 MOVE  8 TO LASTKEY
          WHEN  -1 MOVE  9 TO LASTKEY
          WHEN   5 MOVE 10 TO LASTKEY
          WHEN OTHER
           Exit Perform Cycle

        END-EVALUATE
        Move 1 To Valid-Response-Sw
     End-Perform.
*>---------------------------------------------------------
 Viewsub-Style.
     MOVE LOW-VALUES TO SP2-WD-DATA.
     MOVE "vpopup"              TO SP2-WD-NAME.
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
     MOVE "vpopup"             TO SP2-PD-NAME.
     MOVE LOW-VALUES            TO SP2-PD-MENU-NAME.
     MOVE "PopUp Msg"  TO SP2-PD-TITLE.
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
      Compute SP2-PD-VAR-LEN =
              SP2-PD-DESC-LEN +
              SP2-PD-TITLE-LEN +
              SP2-PD-CURS-KEY-LEN +
              SP2-PD-CTRL-KEY-LEN +
              SP2-PD-SYNS-LEN +
              SP2-PD-MSG-TEXT-LEN +
              SP2-PD-USER-LEN +
              SP2-PD-HELP-LEN.
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
     STRING "*** Message ***"
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
     MOVE VPM-Text(1) TO SP2-SD-TEXT.
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
     MOVE VPM-Text(2) TO SP2-SD-TEXT.
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
     MOVE VPM-Text(3) TO SP2-SD-TEXT.
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
     MOVE LOW-VALUES                  TO SP2-FD-VAR-LENS.
     MOVE 18                          TO SP2-FD-INITIAL-LEN.
     MOVE 18                          TO SP2-FD-VAR-LEN.
     move 16                          to SP2-FD-OPTIONS-3.
     MOVE 0    TO SP2-FD-TAB-NUM.
     MOVE "y"  TO SP2-FD-OUTPUT.
     COMPUTE SP2-FD-ID = SP2-FD-HELP-KEY.
     CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.

*> read panel into window
     move "vpopup" to sp2-nd-name.
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
     MOVE "VSP2ERR" TO MY-CD-NEXT-PANEL.
     INITIALIZE MY-CD-DATABUFFER.
     MOVE "x" TO MY-CD-NEW-WINDOW.
     CALL "SP2" USING SP2-CONVERSE-PANEL MY-CONVERSE-DATA.
     CALL "SP2" USING SP2-CLOSE-WINDOW SP2-NULL-PARM.
     MOVE SP2-MOUSE-WAIT TO SP2-NP-RET-CODE.
     CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.

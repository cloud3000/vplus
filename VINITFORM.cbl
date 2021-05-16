>>source format free.
*>*_________________________________________________________________
*>*****************************************************************
*>                        V I N I T F O R M                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VINITFORM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Init-Auth-ID                Pic X(48) Value Spaces.
 01 Init-IPADDR                 Pic X(16) Value Spaces.
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


   COPY "vplus/TCLDBMEM".
 01 TCL-NEW-BUFFER PIC X(2048).
 01 TCL-BYTE-VALUE PIC X.
    88 VALID-BYTE VALUES ARE X"20" THROUGH X"7E".
 01 TCL-EVAL-STR pic x(4096) VALUE LOW-VALUES.
 01 TCL-BUFVALUE pic x(2048)   VALUE SPACES.
 01 TCL-VARVALUE pic x(80)   VALUE SPACES.
 01 STRIDX       PIC S9(4) COMP-5 VALUE 0.
 01 STRCNT       PIC S9(4) COMP-5 VALUE 0.

    COPY "vplus/SP250".
 01 THE-HEIGHT             pic s9(4) comp-5 value 0.
 01 MY-FONT-WIDTH          PIC S9(4) COMP-5.
 01 MY-FONT-HEIGHT         PIC S9(4) COMP-5.
 01 FONTS-LEN              PIC S9(4) COMP-5.

 01 DebugBUF               PIC X(256) Value Spaces.
 01 Trimstr                PIC X(512) Value Spaces.
 01 ts1                    PIC S9(4) Comp-5 Value 0.
 01 ts2                    PIC S9(4) Comp-5 Value 0.

 01 COMPROW                pic s9(4) comp-5 value 0.
 01 KeyCnt                 pic s9(4) comp-5 value 0.
 01 KeyPtr                 pic s9(4) comp-5 value 0.
 01 KeyIdx                 pic s9(4) comp-5 value 0.
 01 Print-Key-1            pic s9(4) comp-5 value 0.
 01 Print-Key-2            pic s9(4) comp-5 value 0.
 01 x                      pic s9(4) comp-5 value 0.
 01 y                      pic s9(4) comp-5 value 0.
 01 z                      pic s9(4) comp-5 value 0.
 01 Field-Template         Pic S9(9) Comp-5.
 01 Field-Template-Name    Pic x(8) VALUE SPACES.
 01 HOT-Template-Name      Pic x(8) VALUE SPACES.
 01 MY-SP2-DATA.
    05 My-Prog-Off         Pic s9(4) comp-5 Value 0.
    05 My-Prog-Num         Pic s9(4) comp-5 Value 0.
    05 My-FD-ID            Pic s9(4) comp-5 Value 0.

 01 NIBBLE-WORK.
    02 NIBBLE-1            PIC X.
    02 NIBBLE-2            PIC X.
 01 NIBBLE-NUM REDEFINES NIBBLE-WORK.
    02 NIBBLE-NUM          PIC S9(4) COMP-5.
 01 DISP-Z4                PIC ZZZ9.
 01 template-class.
    02 tprefix pic x value "T".
    02 tnumber PIC 9(9) value zero.
 01 GLOBAL-ADDRESS1   USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.

 01 TEMPLATE-FONTS.
    02 FONT-ID                 PIC S9(4) COMP-5.
    02 FONT-WIDTH              PIC S9(4) COMP-5.
    02 FONT-HEIGHT             PIC S9(4) COMP-5.
    02 FONT-GUI-ID             PIC S9(4) COMP-5.
    02 FONT-GUI-ID-2           PIC S9(4) COMP-5.
    02 FONT-DECIPOINTS         PIC S9(4) COMP-5.
    02 FONT-ROTATION           PIC S9(4) COMP-5.
    02 FONT-PITCH              PIC X(1).
    02 FONT-WEIGHT             PIC X(1).
    02 FONT-ITALIC             PIC X(1).
    02 FONT-STRIKE-OUT         PIC X(1).
    02 FONT-UNDERLINE          PIC X(1).
    02 FONT-CHAR-SET           PIC X(1).
    02 FONT-NAME               PIC X(30).

*>########################################################
 PROCEDURE DIVISION USING COMAREA.
 0000-BEGIN-INITFORM.

     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.
     move LOW-VALUES TO INTR-CALLNAME
     string "VINITFORM "
      delimited by SIZE into INTR-CALLNAME.
     move LOW-VALUES to DebugBUF.
     string "VINITFORM: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     IF CFNAME Not = NFNAME
        If NOT GETNEXTFORM-CALLED
        move LOW-VALUES to DebugBUF
        String "VINITFORM Error: "
           "Invalid screen pointer (VGETNEXTFORM not done?)"
           Delimited by size into DebugBUF
        End-String
        MOVE 132        TO INTR-ERRNUM
        Move DebugBUF   TO INTR-ERRMSG
        CALL "LogDebug" USING DebugBUF
        MOVE 132 TO CSTATUS
        GO TO INITFORM-ABEND.

*>> SP2 processing
*>**************************************
*>> CHANGE: 06/20/2012 --
*>> If calling "VSINIT" twice using the same formname, then
*>> DON'T DO ANYTHING ELSE!!!! Only initialize the databuffer!
     IF NOT INITFORM-CALLED
        PERFORM SET-Window
        PERFORM SET-PANEL
        Perform Set-Fields
     END-IF.
*>***************************************
*> End of SP2 processing
*>
*> SET DATA BUFFER TO INITIAL VALUES FROM FORMSPEC.
*>
     INITIALIZE MM-CD-DATABUFFER X Z.
     MOVE IFLD-CNT TO DISP-Z4.
     move LOW-VALUES to END-MSG.
     string  " FIELD COUNT = " DELIMITED BY SIZE
             DISP-Z4           DELIMITED BY SIZE
        into END-MSG END-STRING.

     MOVE 1 TO X.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > IFLD-CNT

        MOVE HFLD-LENGTH(IFLD-IDX) TO Y
        MOVE HFLD-LENGTH(IFLD-IDX) TO DISP-Z4
        IF X > 0 AND Y > 0
           MOVE SPACES TO MM-CD-DATABUFFER(X:Y)
           MOVE -1     TO HFLD-ERRFLG(IFLD-IDX)
           IF Y > 80
             MOVE HFLD-INITVAL(IFLD-IDX)(1:80) TO MM-CD-DATABUFFER(X:80)
           ELSE
             MOVE HFLD-INITVAL(IFLD-IDX)(1:Y) TO MM-CD-DATABUFFER(X:Y)
           END-IF
        END-IF
        COMPUTE X = X + Y END-COMPUTE
        MOVE X TO DISP-Z4
        ADD 1 TO Z
     END-PERFORM.

     MOVE MM-CD-DATABUFFER(1:AF-AppBuf-Len) TO LAST-BUFFER.
     PERFORM TCL-INITIALEDITS.
     If TCL-RESULT = 0
        MOVE MM-CD-DATABUFFER(1:AF-AppBuf-Len) TO LAST-BUFFER SVT-BUFFER
     Else
        MOVE LAST-BUFFER TO MM-CD-DATABUFFER(1:AF-AppBuf-Len).
*>
*> The databuffer for the current screen has just been set to
*> the initial values as defined in the vform file.
*>
*> CHANGE: 06/20/2012 --
*> If calling "VSINIT" twice using the same formname, then
*> DON'T DO ANYTHING ELSE!!!! Only initialize the databuffer!
     If INITFORM-CALLED
        MOVE 0 TO CSTATUS
        GO TO INITFORM-RETURN.

*>
*> MOVE HOT FIELD TABLE TO INITIAL AND CURRENT TABLES.
*>
     INITIALIZE CHANGE-FIELDS.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1
       UNTIL IFLD-IDX > IFLD-CNT
      MOVE HFLD-ENH(IFLD-IDX)       TO INITIAL-ENH(IFLD-IDX)
      MOVE HFLD-TYPE(IFLD-IDX)      TO INITIAL-TYPE(IFLD-IDX)
      MOVE HFLD-DTYPE(IFLD-IDX)     TO INITIAL-DTYPE(IFLD-IDX)
      MOVE HFLD-SCRNORDER(IFLD-IDX) TO INITIAL-SCRNORDER(IFLD-IDX)
      MOVE HFLD-NUMBER(IFLD-IDX)    TO INITIAL-NUMBER(IFLD-IDX)
      MOVE INITIAL-ENTRY(IFLD-IDX)  TO CURRENT-ENTRY(IFLD-IDX)
      INITIALIZE PREVIOUS-ENTRY(IFLD-IDX)
     END-PERFORM.
     Move 0 To Cstatus.
     go to INITFORM-RETURN.
*>------------------------------------------------------------
 INITFORM-ABEND.
     MOVE "F"            TO VINITFORM-SW.
     MOVE "N"            TO VSHOWFORM-SW.
     MOVE "VINITFORM"    TO INTR-CALLNAME.
     MOVE CSTATUS        TO INTR-ERRNUM.
     goback.
*>------------------------------------------------------------
 INITFORM-RETURN.
     MOVE "Y" TO VINITFORM-SW.
     MOVE "N" TO VSHOWFORM-SW.
     goback.
*>-----------------------------------------------------------
 Set-Fields.

*> SET HOT FIELD TABLE AND CHANGE FIELD TABLE.
     INITIALIZE HOT-FIELDS CHANGE-FIELDS MARKED-FIELDS.
     move 1 to x.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1
       UNTIL IFLD-IDX > IFLD-CNT
      MOVE IFLD-ENTRY(IFLD-IDX)     TO HFLD-ENTRY(IFLD-IDX)
      MOVE HFLD-ENH(IFLD-IDX)       TO INITIAL-ENH(IFLD-IDX)
      MOVE HFLD-TYPE(IFLD-IDX)      TO INITIAL-TYPE(IFLD-IDX)
      MOVE HFLD-DTYPE(IFLD-IDX)     TO INITIAL-DTYPE(IFLD-IDX)
      MOVE HFLD-SCRNORDER(IFLD-IDX) TO INITIAL-SCRNORDER(IFLD-IDX)
      MOVE HFLD-NUMBER(IFLD-IDX)    TO INITIAL-NUMBER(IFLD-IDX)
      MOVE INITIAL-ENTRY(IFLD-IDX)   TO CURRENT-ENTRY(IFLD-IDX)
      INITIALIZE PREVIOUS-ENTRY(IFLD-IDX)
      PERFORM CREATE-ENTRY-FIELDS
     END-PERFORM.

*>    Display "VINITFORM: Adding template data to Field user-data".
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
      perform SET-FIELD-USERDATA
     END-PERFORM.
*>    Display "VINITFORM: FINISHED Adding template data to Field user-data".

*> SET HOT function key TABLE.
     PERFORM VARYING FKEY-IDX FROM 1 BY 1
       UNTIL FKEY-IDX > FKEY-CNT
      MOVE PANEL-KEY-ENTRY(FKEY-IDX) TO HOT-KEY-ENTRY(FKEY-IDX)
      MOVE HOT-KEY-ENTRY(FKEY-IDX) TO CUR-KEY-ENTRY(FKEY-IDX)
      PERFORM Set-PANEL-KEY-Template
      PERFORM CREATE-FKEY
      MOVE SP2-FD-ID TO PANEL-KEY-SP2-ID(FKEY-IDX)
      MOVE SP2-FD-ID TO HOT-KEY-SP2-ID(FKEY-IDX)
      MOVE SP2-FD-ID TO CUR-KEY-SP2-ID(FKEY-IDX)
     END-PERFORM.

*> SET STATIC FIELDS.
     PERFORM CREATE-STATIC-FIELDS.
     MOVE MM-CD-DATABUFFER(1:AF-AppBuf-Len) TO LAST-BUFFER.
*>---------------------------------------------------------------
 SET-PANEL.
*>*******************************
*> panel definition             *
*> parameter for GET-PANEL-DEF  *
*> also used with SET-PANEL-DEF *
*>*******************************
      CALL "SP2" USING SP2-CLEAR-PANEL SP2-NULL-PARM.
      MOVE SP2-MOUSE-WAIT TO SP2-NP-RET-CODE.
      CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.

     MOVE LOW-VALUES TO SP2-PANEL-DEF.
*>
*> SP2 LENGTH OF VARIABLE PARAMETERS.
*>
      MOVE ipd-LEN-LEN      TO  SP2-PD-LEN-LEN.
      MOVE ipd-NUM-LEN      TO  SP2-PD-NUM-LEN.
      MOVE ipd-CHAR-LEN     TO  SP2-PD-CHAR-LEN.
      MOVE ipd-VAR-LEN      TO  SP2-PD-VAR-LEN.
      MOVE ipd-DESC-LEN     TO  SP2-PD-DESC-LEN.
      MOVE ipd-TITLE-LEN    TO  SP2-PD-TITLE-LEN.
      MOVE ipd-CURS-KEY-LEN TO  SP2-PD-CURS-KEY-LEN.
      MOVE ipd-CTRL-KEY-LEN TO  SP2-PD-CTRL-KEY-LEN.
      MOVE ipd-SYNS-LEN     TO  SP2-PD-SYNS-LEN.
      MOVE ipd-MSG-TEXT-LEN TO  SP2-PD-MSG-TEXT-LEN.
      MOVE ipd-USER-LEN     TO  SP2-PD-USER-LEN.
      MOVE ipd-HELP-LEN     TO  SP2-PD-HELP-LEN.
      MOVE ipd-LENL-LEN     TO  SP2-PD-LENL-LEN.
      MOVE ipd-NUML-LEN     TO  SP2-PD-NUML-LEN.
*>
*> SP2-PD-DATA.
*> SP2-PD-NUM-DATA
*>
     COMPUTE SP2-PD-WIDTH = 92 * 8  END-COMPUTE.
     COMPUTE SP2-PD-HEIGHT = 40 * 8 END-COMPUTE.
      MOVE ipd-WIDTH        TO  SP2-PD-WIDTH.
      MOVE ipd-HEIGHT       TO  SP2-PD-HEIGHT.
      MOVE ipd-ROW          TO  SP2-PD-ROW.
      MOVE ipd-COL          TO  SP2-PD-COL.
      MOVE HPAN-FLDCNT      TO  SP2-PD-FLD-CNT.
      MOVE HPAN-FLDCNT      TO  SP2-PD-PROG-CNT.
      MOVE HPAN-BUF-LEN     TO  SP2-PD-PROG-LEN.
      MOVE ipd-HELP-KEY     TO  SP2-PD-HELP-KEY.
      MOVE ipd-EDIT-OV-KEY  TO  SP2-PD-EDIT-OV-KEY.
      MOVE ipd-MSG-REFRESH  TO  SP2-PD-MSG-REFRESH.
      MOVE ipd-TITLE-ROWS   TO  SP2-PD-TITLE-ROWS.
      MOVE ipd-DEF-PB       TO  SP2-PD-DEF-PB.
      MOVE ipd-MENU-ROWS    TO  SP2-PD-MENU-ROWS.
      MOVE ipd-TOT-WIDTH    TO  SP2-PD-TOT-WIDTH.
      MOVE ipd-TOT-HEIGHT   TO  SP2-PD-TOT-HEIGHT.
      MOVE ipd-MSG-LEN      TO  SP2-PD-MSG-LEN.
      MOVE ipd-CELL-WIDTH   TO  SP2-PD-CELL-WIDTH.
      MOVE ipd-CELL-HEIGHT  TO  SP2-PD-CELL-HEIGHT.
      MOVE ipd-FONT-ID      TO  SP2-PD-FONT-ID.
      MOVE ipd-PROG-LEN-L   TO  SP2-PD-PROG-LEN-L.
      MOVE ipd-PROG-CNT-L   TO  SP2-PD-PROG-CNT-L.
*>
*> SP2-PD-CHAR-DATA
*>
      MOVE HPAN-PAN-NAME   TO  SP2-PD-NAME.

      MOVE ipd-MENU-NAME    TO  SP2-PD-MENU-NAME.
      MOVE ipd-CUR-FLD-COLR TO  SP2-PD-CUR-FLD-COLR.
      MOVE ipd-CURS-SKIP    TO  SP2-PD-CURS-SKIP.
      MOVE ipd-CURS-SHOW    TO  SP2-PD-CURS-SHOW.
      MOVE ipd-CURS-IN-FLD  TO  SP2-PD-CURS-IN-FLD.
      MOVE ipd-SHIFT-NUMS   TO  SP2-PD-SHIFT-NUMS.
      MOVE ipd-BLANK-NUMS   TO  SP2-PD-BLANK-NUMS.
      MOVE ipd-ASSUME-DEC   TO  SP2-PD-ASSUME-DEC.
      MOVE ipd-FORMAT-NUMS  TO  SP2-PD-FORMAT-NUMS.
      MOVE ipd-CURS-WRAP    TO  SP2-PD-CURS-WRAP.
      MOVE ipd-INIT-NUMS    TO  SP2-PD-INIT-NUMS.
      MOVE ipd-OVERRIDE-REQ TO  SP2-PD-OVERRIDE-REQ.
      MOVE ipd-CELL-SIZE    TO  SP2-PD-CELL-SIZE.
      MOVE ipd-MISC-OPTIONS TO  SP2-PD-MISC-OPTIONS.
      MOVE ipd-DIV-WIDTH    TO  SP2-PD-DIV-WIDTH.
      MOVE ipd-DIV-HEIGHT   TO  SP2-PD-DIV-HEIGHT.
      MOVE ipd-COLR         TO  SP2-PD-COLR.
      MOVE ipd-PROG-DATE    TO  SP2-PD-PROG-DATE.
      MOVE ipd-HELP         TO  SP2-PD-HELP.
      MOVE ipd-TEXT-OPTIONS TO  SP2-PD-TEXT-OPTIONS.
      MOVE ipd-TOOLBAR-NAME TO  SP2-PD-TOOLBAR-NAME.
      MOVE ipd-MSG-COLR     TO  SP2-PD-MSG-COLR.
      MOVE X"80"    TO  SP2-PD-MORE-OPTIONS.
      MOVE x"01"    TO  SP2-PD-OPTIONS-3.
      MOVE X"48"    TO  SP2-PD-OPTIONS-4.
      MOVE ipd-OPTIONS-5    TO  SP2-PD-OPTIONS-5.
      MOVE ipd-TAB-OPTIONS  TO  SP2-PD-TAB-OPTIONS.
      MOVE ipd-WIN-BOR-TYPE TO  SP2-PD-WIN-BOR-TYPE.
      MOVE ipd-INITIAL-SW   TO  SP2-PD-INITIAL-SW.
*>
*> SP2-PD-VAR-DATA
*>
      MOVE HPAN-NAME       TO  SP2-PD-DESCRIPTION.
      MOVE HPAN-COMMENT    TO  SP2-PD-TITLE.
      MOVE ipd-LEFT         TO  SP2-PD-LEFT.
      MOVE ipd-RIGHT        TO  SP2-PD-RIGHT.
      MOVE ipd-UP           TO  SP2-PD-UP.
      MOVE ipd-DOWN         TO  SP2-PD-DOWN.
      MOVE ipd-TAB          TO  SP2-PD-TAB.
      MOVE ipd-BACKTAB      TO  SP2-PD-BACKTAB.
      MOVE ipd-TB-ERASE     TO  SP2-PD-TB-ERASE.
      MOVE ipd-BT-ERASE     TO  SP2-PD-BT-ERASE.
      MOVE ipd-DELETE       TO  SP2-PD-DELETE.
      MOVE ipd-BACKSPAC     TO  SP2-PD-BACKSPAC.
      MOVE ipd-ERASE        TO  SP2-PD-ERASE.
      MOVE ipd-INSERT       TO  SP2-PD-INSERT.
      MOVE ipd-HOME         TO  SP2-PD-HOME.
      MOVE ipd-END          TO  SP2-PD-END.
      MOVE ipd-SCRL-UP      TO  SP2-PD-SCRL-UP.
      MOVE ipd-SCRL-DN      TO  SP2-PD-SCRL-DN.
      MOVE ipd-SCRL-LT      TO  SP2-PD-SCRL-LT.
      MOVE ipd-SCRL-RT      TO  SP2-PD-SCRL-RT.
      MOVE ipd-HOME-PAN     TO  SP2-PD-HOME-PAN.
      MOVE ipd-END-PAN      TO  SP2-PD-END-PAN.
      MOVE ipd-CTRL-KEYS    TO  SP2-PD-CTRL-KEYS.

*> Add F12 to list of keys that will return control to application.
      Move 0 To KeyCnt.
      Move SP2-KEY-F12 To Print-Key-1.
      Compute Print-Key-2 = Print-Key-1 * -1.

      Perform Varying KeyIdx From 1 By 1 Until KeyIdx > 20
         If SP2-PD-CTRL-KEY(KeyIdx) = Print-Key-1
            Move Print-Key-2 To SP2-PD-CTRL-KEY(KeyIdx)
            Add 1 To KeyCnt
            Exit Perform Cycle
         End-If
         If SP2-PD-CTRL-KEY(KeyIdx) = Print-Key-2
            Add 1 To KeyCnt
            Exit Perform
         End-If
      End-Perform.

      If KeyCnt = 0
         Perform Varying KeyIdx From 1 by 1 Until KeyIdx > 20
            If SP2-PD-CTRL-KEY(KeyIdx) = 0
               Move Print-Key-2 To SP2-PD-CTRL-KEY(KeyIdx)
               Exit Perform
            End-If
         End-Perform.

*> DONE Adding F12 to list of keys

*> Add ESC to list of keys that will return control to application.
      Move 0 To KeyCnt.
      Move SP2-KEY-ESCAPE To Print-Key-1.
      Compute Print-Key-2 = Print-Key-1 * -1.

      Perform Varying KeyIdx From 1 By 1 Until KeyIdx > 20
         If SP2-PD-CTRL-KEY(KeyIdx) = Print-Key-1
            Move Print-Key-2 To SP2-PD-CTRL-KEY(KeyIdx)
            Add 1 To KeyCnt
            Exit Perform Cycle
         End-If
         If SP2-PD-CTRL-KEY(KeyIdx) = Print-Key-2
            Add 1 To KeyCnt
            Exit Perform
         End-If
      End-Perform.

      If KeyCnt = 0
         Perform Varying KeyIdx From 1 by 1 Until KeyIdx > 20
            If SP2-PD-CTRL-KEY(KeyIdx) = 0
               Move Print-Key-2 To SP2-PD-CTRL-KEY(KeyIdx)
               Exit Perform
            End-If
         End-Perform.

*> DONE Adding ESC to list of keys


      MOVE ipd-MSG-TEXT     TO  SP2-PD-MSG-TEXT.
      MOVE ipd-USER-DATA    TO  SP2-PD-USER-DATA.
      MOVE ipd-HELP-KEYWORD TO  SP2-PD-HELP-KEYWORD.
      CALL "SP2" USING SP2-SET-PANEL-DEF SP2-PANEL-DEF.
*>* SET PANEL AT TOP LEFT CORNER OF WINDOW (SET ROW/COL TO ZERO)
*>     MOVE LOW-VALUES TO SP2-PR-DATA.
*>     MOVE "PN-0000400004" TO SP2-PR-KEY.
*>     CALL "SP2" USING SP2-SET-PROPERTY SP2-PROPERTY.
*>     MOVE LOW-VALUES TO SP2-PR-DATA.
*>     MOVE "PN-0000600004" TO SP2-PR-KEY.
*>     CALL "SP2" USING SP2-SET-PROPERTY SP2-PROPERTY.
*>     MOVE LOW-VALUES TO SP2-PR-DATA.
*>     MOVE "PC-0001700001D" TO SP2-PR-KEY.
*>     CALL "SP2" USING SP2-SET-PROPERTY SP2-PROPERTY.
*>----------------------------------------
 Set-Window.
     INITIALIZE MY-SP2-DATA.
*>  INITIALIZE WINDOW
     MOVE LOW-VALUES            TO SP2-WD-DATA.
     CALL "SP2" USING SP2-CLEAR-WINDOW SP2-NULL-PARM.
     MOVE SP2-MOUSE-ARROW TO SP2-NP-RET-CODE.
     CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.
*>---------------------------------------------------------------
 CREATE-ENTRY-FIELDS.
     PERFORM Set-HOT-Template.
     PERFORM Create-Data-Fields.
*>---------------------------------------------------------------
 CREATE-STATIC-FIELDS.
     PERFORM VARYING ISTAT-IDX FROM 1 BY 1
       UNTIL ISTAT-IDX > ISTAT-CNT
      Move ISTAT-TEMPLATE-ID(ISTAT-IDX) To Field-Template
      Perform Set-STATIC-Template
      PERFORM DB-TO-STATIC
     End-Perform.
*>---------------------------------------------------------------
 Create-Data-Fields.
     MOVE LOW-VALUES          TO SP2-FIELD-DEF.
     Add 1 to My-Prog-Num.
     MOVE +30    to  SP2-FD-LEN-LEN.
     MOVE +52    to  SP2-FD-NUM-LEN.
     MOVE +74    to  SP2-FD-CHAR-LEN.
     move HFLD-LENGTH(IFLD-IDX) to SP2-FD-VAR-LEN.
     MOVE  -1    to  SP2-FD-LENL-LEN.
     MOVE +44    to  SP2-FD-NUML-LEN.

*>------ Create field.
     Move HFLD-NUMBER(IFLD-IDX)         To SP2-FD-ID.
     move HFLD-FD-TEMPLATE-ID(IFLD-IDX) To tnumber.
     MOVE SPACES                        TO SP2-FD-NAME.
     MOVE IFD-CTRL-TYPE(IFD-PTR)        TO SP2-FD-CTRL-TYPE.
     MOVE IFD-BOR-TYPE(IFD-PTR)         TO SP2-FD-BOR-TYPE.

*>------ Lookout for reserved field names, where output is always 'h'.
     If (HFLD-NAME(IFLD-IDX) = "VPLUSURL1" or "VPLUSURL2" or
                               "VPLUSAUTH" or "CLIENTIP" or "KEYVAL")
                             AND (HFLD-ROW(IFLD-IDX) = 30)
         move HFLD-NAME(IFLD-IDX) TO SP2-FD-NAME
         MOVE "h" To SP2-FD-OUTPUT
     Else
         move HFLD-NAME(IFLD-IDX) TO SP2-FD-NAME
         MOVE IFD-OUTPUT(IFD-PTR)       TO SP2-FD-OUTPUT.

     MOVE IFD-FONT-ID(IFD-PTR)          TO SP2-FD-FONT-ID.
     PERFORM FIND-FONTS-BYFONTID.
     IF TCL-RESULT = 0 AND TCL-ROWCOUNT = 1
      PERFORM GET-FONTS
      MOVE FONT-WIDTH                   TO MY-FONT-WIDTH
      MOVE FONT-HEIGHT                  TO MY-FONT-HEIGHT
     ELSE
*>      DID NOT FIND FONT  IFD-FONT-ID(IFD-PTR)
      move 8                            to MY-FONT-WIDTH
      MOVE 17                           TO MY-FONT-HEIGHT
     END-IF
     MOVE IFD-COLR(IFD-PTR)             TO SP2-FD-COLR.
     MOVE HFLD-ROW(IFLD-IDX)            TO COMPROW.
     COMPUTE SP2-FD-ROW = (COMPROW * 11) end-compute.
     COMPUTE SP2-FD-COL = HFLD-COLUMN(IFLD-IDX) * 9 end-compute.
     MOVE 0 TO SP2-FD-HEIGHT.
     MOVE "y"                           TO SP2-FD-CURS-SKIP.
*>
*> Field width is based on character length and the font points
*> per character. Here we assume that we are using a font that
*> uses 10 points per character. Later we will use the actual
*> points per character found in the font record.
*>
     IF HFLD-LENGTH(IFLD-IDX) < 6
       Compute SP2-FD-WIDTH = ((HFLD-LENGTH(IFLD-IDX) * MY-FONT-WIDTH) * 10) END-COMPUTE
     END-IF
*>
     EVALUATE HFLD-LENGTH(IFLD-IDX)
     WHEN 1
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 20))
     WHEN 2
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 16.9))
     WHEN 3
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 15.8))
     WHEN 4
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 14.2))
     WHEN 5
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 12.9))
     WHEN 6
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 12.6))
     WHEN 7
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 12.5))
     WHEN 8
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 11.9))
     WHEN 9
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 11.9))
     WHEN 10
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 11.9))
     WHEN 11
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 11.2))
     WHEN 12
      Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 11.1))
     WHEN OTHER
      IF HFLD-LENGTH(IFLD-IDX) < 81
       Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 10.2))
      END-IF
      IF HFLD-LENGTH(IFLD-IDX) < 61
       Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 10.2))
      END-IF
      IF HFLD-LENGTH(IFLD-IDX) < 41
       Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 10.5))
      END-IF
      IF HFLD-LENGTH(IFLD-IDX) < 31
       Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 10.7))
      END-IF
      IF HFLD-LENGTH(IFLD-IDX) < 21
       Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * ( MY-FONT-WIDTH * 10.7))
      END-IF
     END-EVALUATE.

*> BEGIN support for terminal wrap,
*> fields longer than 80 characters.
     If HFLD-LENGTH(IFLD-IDX) > 80
      MOVE 0                   TO THE-WIDTH
      MOVE IFD-HEIGHT(IFD-PTR) TO THE-HEIGHT
      COMPUTE THE-HEIGHT = ( MY-FONT-HEIGHT * 4.8) END-COMPUTE
      MOVE 800                 TO SP2-FD-WIDTH
      COMPUTE SP2-FD-WIDTH = (80 * MY-FONT-WIDTH) * 9.5 END-COMPUTE
      Perform Varying THE-WIDTH
        From HFLD-LENGTH(IFLD-IDX) by -80
        Until THE-WIDTH < 81
       Add THE-HEIGHT TO SP2-FD-HEIGHT
      End-Perform
      MOVE "m"        TO SP2-FD-CURS-SKIP
     end-if.
*> END support for terminal wrap
*>
     MOVE HFLD-LENGTH(IFLD-IDX)    TO SP2-FD-MAX-LEN
     MOVE HFLD-LENGTH(IFLD-IDX)    TO SP2-FD-PROG-LEN.

     MOVE IFD-CURS-SHOW(IFD-PTR)   TO SP2-FD-CURS-SHOW.
     MOVE HFLD-NUMBER(IFLD-IDX)    TO SP2-FD-FLD-NUM.
     MOVE HFLD-SCRNORDER(IFLD-IDX) TO SP2-FD-TAB-NUM.
     MOVE My-Prog-Off              TO SP2-FD-PROG-OFF.
     MOVE My-Prog-Num              TO SP2-FD-PROG-NUM.
     MOVE IFD-PROG-SPEC(IFD-PTR)   TO SP2-FD-PROG-SPEC.
     MOVE HFLD-INITVAL(IFLD-IDX)   TO SP2-FD-VAR-DATA.

     If (HFLD-NAME(IFLD-IDX) = "VPLUSAUTH" )
      ACCEPT Init-Auth-ID FROM ENVIRONMENT "HPAUTHCODE".
      If Init-Auth-ID = "not-set-yet"
         Move Spaces       To SP2-FD-VAR-DATA
      Else
         Move Init-Auth-ID To SP2-FD-VAR-DATA.

     If (HFLD-NAME(IFLD-IDX) = "CLIENTIP")
      If Init-Auth-ID = "not-set-yet"
         Move Spaces      To SP2-FD-VAR-DATA
      Else
         ACCEPT Init-IPADDR FROM ENVIRONMENT "HPREMIPADDR"
         Move Init-IPADDR To SP2-FD-VAR-DATA.

     If (HFLD-NAME(IFLD-IDX) = "VPLUSURL1" or "VPLUSURL2" or "KEYVAL")
         Move Spaces       To SP2-FD-VAR-DATA.

     MOVE IFD-MORE-OPTIONS(IFD-PTR) TO SP2-FD-MORE-OPTIONS.
     MOVE 0                         TO SP2-FD-VAR-LEN.
     Move Spaces                    To SP2-FD-USER-DATA.
     String "_" Delimited By Size
          HFLD-ENH(IFLD-IDX) Delimited By " "
          "_" Delimited By Size
          HFLD-TYPE(IFLD-IDX) Delimited By " "
          INTO SP2-FD-USER-DATA.

     CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.
     If SP2-FD-RET-CODE not = 0
      MOVE LOW-VALUES to END-MSG NIBBLE-WORK
      String "ERROR calling SP2-SET-FIELD-DEF err=%hd "
         DELIMITED by size into END-MSG
       END-STRING.

     Add HFLD-LENGTH(IFLD-IDX) To My-Prog-Off.
*>----------------------------------------------------------------
 CREATE-FKEY.
     MOVE LOW-VALUES          TO SP2-FIELD-DEF.
     MOVE +30    to  SP2-FD-LEN-LEN.
     MOVE +52    to  SP2-FD-NUM-LEN.
     MOVE +74    to  SP2-FD-CHAR-LEN.
     MOVE  -1    to  SP2-FD-LENL-LEN.
     MOVE +44    to  SP2-FD-NUML-LEN.
     MOVE Field-Template-Name      To SP2-FD-NAME.

     COMPUTE SP2-FD-ROW = HOT-KEY-ROW(FKEY-IDX) * 11 END-COMPUTE.
     COMPUTE SP2-FD-COL = HOT-KEY-COL(FKEY-IDX) * 9 END-COMPUTE.
     MOVE IFD-WIDTH(FKEY-PTR)       TO SP2-FD-WIDTH.
     MOVE IFD-HEIGHT(FKEY-PTR)      TO SP2-FD-HEIGHT.

     MOVE "r"                      TO SP2-FD-BOR-TYPE.
     Move 1     To SP2-FD-FONT-ID.
     MOVE IFD-HELP-KEY(FKEY-PTR)    TO SP2-FD-HELP-KEY.
     MOVE IFD-MAX-LEN(FKEY-PTR)     TO SP2-FD-MAX-LEN.
     MOVE IFD-PROG-NUM(FKEY-PTR)    TO SP2-FD-PROG-NUM.
     MOVE IFD-CTRL-TYPE(FKEY-PTR)   TO SP2-FD-CTRL-TYPE.
     Move HOT-KEY-LABEL(FKEY-IDX)   To SP2-FD-VAR-DATA.
     MOVE LOW-VALUES               TO SP2-FD-VAR-LENS.
     MOVE IFD-INITIAL-LEN(FKEY-PTR) TO SP2-FD-INITIAL-LEN.
     MOVE IFD-VAR-LEN(FKEY-PTR)     TO SP2-FD-VAR-LEN.
     move IFD-OPTIONS-3(FKEY-PTR)   to SP2-FD-OPTIONS-3.
     MOVE IFD-TAB-NUM(FKEY-PTR)     TO SP2-FD-TAB-NUM.
     MOVE IFD-OUTPUT(FKEY-PTR)      TO SP2-FD-OUTPUT.
     COMPUTE SP2-FD-ID = SP2-FD-HELP-KEY + 100.
     IF HOT-KEY-HID(FKEY-IDX) NOT = 0
      MOVE "h" TO SP2-FD-OUTPUT
     END-IF
     CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.
*>---------------------------------------------------------------
 Set-HOT-Template.
     MOVE SPACES TO HOT-TEMPLATE-NAME
     MOVE 0 TO IFD-PTR FIELD-TEMPLATE.
     move SPACES TO Field-Template-Name.
     Perform Varying IFD-IDX from 1 by 1
       Until IFD-IDX > IFD-CNT Or IFD-PTR > 0
      If IFD-TEMPLATE-ID(IFD-IDX) = HFLD-FD-TEMPLATE-ID(IFLD-IDX)
       Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
       Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       Move IFD-IDX to IFD-PTR
      End-If
     End-Perform.
     If Field-Template  = 0
      Move "?" To Field-Template-Name.
     MOVE Field-Template-Name TO HOT-TEMPLATE-NAME.
*>---------------------------------------------------------------
 Set-STATIC-Template.
     Move 0 To IFD-PTR Field-Template.
     move SPACES TO Field-Template-Name.
     Perform Varying IFD-IDX from 1 by 1
       Until IFD-IDX > IFD-CNT or IFD-PTR > 0
          Or Field-Template-Name > Spaces
      If IFD-TEMPLATE-ID(IFD-IDX) = ISTAT-TEMPLATE-ID(ISTAT-IDX)
       Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
       Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       move IFD-IDX to IFD-PTR
      End-If
     End-Perform.
     If Field-Template  = 0
      Move "?" To Field-Template-Name.
*>---------------------------------------------------------------
 Set-PANEL-KEY-Template.
     Move 0 To FKEY-PTR Field-Template.
     move SPACES TO Field-Template-Name.
     Perform Varying IFD-IDX from 1 by 1
       Until IFD-IDX > IFD-CNT or FKEY-PTR > 0
      If IFD-TEMPLATE-ID(IFD-IDX)
          = PANEL-KEY-TEMPLATE-ID(fkey-idx)
       Move IFD-TEMPLATE-NAME(IFD-IDX) To Field-Template-Name
       Move IFD-TEMPLATE-ID(IFD-IDX)   To Field-Template
       MOVE IFD-IDX to FKEY-PTR
      End-If
     End-Perform.
     If Field-Template  = 0
      Move "?" To Field-Template-Name.
*>----------------------------------------------------------------
 DB-TO-STATIC.
*>********************************
*> static definition             *
*> parameter for GET-STATIC-DEF  *
*> also used with SET-STATIC-DEF *
*>********************************

     MOVE LOW-VALUES          TO  SP2-STATIC-DEF.
     Move +10                 To  SP2-SD-LEN-LEN.
     Move +12                 To  SP2-SD-NUM-LEN.
     Move +4                  To  SP2-SD-CHAR-LEN.
     Move 80                  To  SP2-SD-VAR-LEN.
     Move 80                  To  SP2-SD-TEXT-LEN.
*> SP2-SD-NUM-DATA
     Move 0                   To  SP2-SD-ID.
     Move ISTAT-ROW(ISTAT-IDX) To  SP2-SD-ROW.
     COMPUTE SP2-SD-ROW = ISTAT-ROW(ISTAT-IDX) * 11.
     Move ISTAT-COL(ISTAT-IDX) To  SP2-SD-COL.
     COMPUTE SP2-SD-COL = ISTAT-COL(ISTAT-IDX) * 9.
     Move 0                   To  SP2-SD-WIDTH.
     Move 0                   To  SP2-SD-HEIGHT.
     Move IFD-FONT-ID(IFD-PTR) To  SP2-SD-FONT-ID.
*>******* SP2-SD-CHAR-DATA *******
     Move IFD-COLR(IFD-PTR)   To  SP2-SD-COLR.
     Move 'l'                 To  SP2-SD-JUSTIFY.
     Move LOW-VALUE           To  SP2-SD-MISC-OPTIONS.
*>******* SP2-SD-VAR-DATA ********
     Move ISTAT-TEXT(ISTAT-IDX) To  SP2-SD-TEXT.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.
*>-----------------------------------------------------------
 TCL-INITIALEDITS.
     MOVE LOW-VALUES TO TCL-VARNAME
     STRING "VPLUSBUFFER"
       DELIMITED BY SIZE INTO TCL-VARNAME
     end-string.

     MOVE LOW-VALUES TO TCL-BUFVALUE
     STRING MM-CD-DATABUFFER(1:dbuflen)
      DELIMITED BY SIZE INTO TCL-BUFVALUE
     END-STRING
     CALL "TCLSETVAR" USING MY-TCL TCL-VARNAME TCL-BUFVALUE.

     MOVE LOW-VALUES TO TCL-EVAL-STR.
     MOVE 0 TO TCL-RESULT.

     STRING "::viewplus::PROCESSEDITSPECS " delimited by size
            MPE-FORMS-FILE                  delimited by space
            ".vform "                       delimited by size
            HPAN-NAME                       delimited by space
            " INIT $VPLUSBUFFER"            delimited by size
            INTO TCL-EVAL-STR.
     CALL "LogDebug" USING TCL-EVAL-STR end-call
     CALL "TCLEVAL" USING MY-TCL TCL-EVAL-STR TCL-RESULT.

     IF TCL-RESULT NOT = 0
      MOVE "VINITFORM error calling ::viewplus::PROCESSEDITSPECS " TO END-MSG
      CALL "LogDebug" USING TCL-EVAL-STR end-call.

     MOVE LOW-VALUES TO TCL-VARNAME.
     MOVE SPACES TO TCL-NEW-BUFFER.
     STRING "VPLUSBUFFER" DELIMITED BY SIZE INTO TCL-VARNAME.
     MOVE SPACES TO LAST-BUFFER.
     CALL "TCLGETVAR" USING MY-TCL TCL-VARNAME TCL-NEW-BUFFER.

     IF TCL-NEW-BUFFER(1:10) = "can't read"
      Move Spaces To TCL-NEW-BUFFER
     Else
      PERFORM VARYING STRIDX FROM 1 BY 1
        UNTIL STRIDX > DBUFLEN
       IF (MM-CD-DATABUFFER(STRIDX:1) NOT =
           TCL-NEW-BUFFER(STRIDX:1))
        MOVE TCL-NEW-BUFFER(STRIDX:1) TO TCL-BYTE-VALUE
        IF VALID-BYTE
         MOVE TCL-NEW-BUFFER(STRIDX:1)
           TO MM-CD-DATABUFFER(STRIDX:1)
        ELSE
         MOVE " " TO MM-CD-DATABUFFER(STRIDX:1)
        END-IF
       END-IF
      END-PERFORM
     END-IF.
*>----------------------------------------------------------------
 FIND-FONTS-BYFONTID.
     MOVE LOW-VALUES TO SQL-SEARCH-KEY.
     MOVE LOW-VALUES TO SQL-SEARCH-ITEM.
     STRING "FONT_ID"
      DELIMITED BY SIZE INTO SQL-SEARCH-ITEM.
     MOVE IFD-FONT-ID(IFD-PTR) TO DISP-Z4.
     Move DISP-Z4 To Trimstr.
     Move FUNCTION LENGTH(DISP-Z4) To ts2.
     Call "TrimString" using Trimstr ts1 ts2.
     Move Trimstr(ts1:ts2) To SQL-SEARCH-KEY.
     CALL "TCLSETVAR" USING MY-TCL, SQL-SEARCH-ITEM, SQL-SEARCH-KEY.

     MOVE LOW-VALUES TO  SQL-TABLE.
     STRING "FONTS:BYFONTID"
      DELIMITED BY SIZE INTO SQL-TABLE.
     CALL "SQLGetResultSet" USING MY-TCL, TCL-RESULTSET-PTR,
                                  SQL-TABLE, TCL-ROWCOUNT,
                                  TCL-RESULT, TCL-ERROR.
*>----------------------------------------------------------------
 GET-FONTS.
      CALL "SQLGetNext" USING MY-TCL, TCL-RESULTSET-PTR,
                              TCL-BUFFER-PTR, TCL-BUFFERLEN,
                              TCL-RESULT, TCL-ERROR
      END-CALL.

      IF TCL-BUFFERLEN > 0
       MOVE TCL-BUFFERLEN TO FONTS-LEN
       SET ADDRESS OF TEMPLATE-FONTS TO TCL-BUFFER-PTR
      ELSE
       MOVE 0 TO FONTS-LEN
      END-IF.
*>---------------------------------------------------------------
 Create-old-Data-Fields.
     MOVE LOW-VALUES          TO SP2-FIELD-DEF.
     Add 1 to My-Prog-Num.
     MOVE +30    to  SP2-FD-LEN-LEN.
     MOVE +52    to  SP2-FD-NUM-LEN.
     MOVE +74    to  SP2-FD-CHAR-LEN.
     move HFLD-LENGTH(IFLD-IDX) to SP2-FD-VAR-LEN.
     MOVE  -1    to  SP2-FD-LENL-LEN.
     MOVE +44    to  SP2-FD-NUML-LEN.

*>------ Create field.
     Move HFLD-NUMBER(IFLD-IDX)     To SP2-FD-ID.
     MOVE HFLD-NAME(IFLD-IDX)       TO SP2-FD-NAME.
     MOVE IFD-CTRL-TYPE(IFD-PTR)    TO SP2-FD-CTRL-TYPE.
     MOVE IFD-BOR-TYPE(IFD-PTR)     TO SP2-FD-BOR-TYPE.
     MOVE IFD-OUTPUT(IFD-PTR)       TO SP2-FD-OUTPUT.
     MOVE IFD-FONT-ID(IFD-PTR)      TO SP2-FD-FONT-ID.
     MOVE IFD-COLR(IFD-PTR)         TO SP2-FD-COLR.
     MOVE HFLD-ROW(IFLD-IDX)        TO SP2-FD-ROW.
     MOVE HFLD-COLUMN(IFLD-IDX)     TO SP2-FD-COL.
     MOVE IFD-HEIGHT(IFD-PTR)       TO SP2-FD-HEIGHT.
     MOVE "y"                       TO SP2-FD-CURS-SKIP.
*>
*> Field width is based on character length and the font points
*> per character. Here we assume that we are using a font that
*> uses 10 points per character. Later we will use the actual
*> points per character found in the font record.
*>
     Compute SP2-FD-WIDTH = (HFLD-LENGTH(IFLD-IDX) * 10).

*> BEGIN support for terminal wrap,
*> fields longer than 80 characters.
     If HFLD-LENGTH(IFLD-IDX) > 80
      MOVE 0                   TO THE-WIDTH
      MOVE IFD-HEIGHT(IFD-PTR) TO THE-HEIGHT
      MOVE 800                 TO SP2-FD-WIDTH
      Perform Varying THE-WIDTH
        From HFLD-LENGTH(IFLD-IDX) by -80
        Until THE-WIDTH < 81
       Add THE-HEIGHT TO SP2-FD-HEIGHT
      End-Perform
      MOVE "m"        TO SP2-FD-CURS-SKIP
     end-if.
*> END support for terminal wrap
*>
     MOVE HFLD-LENGTH(IFLD-IDX)    TO SP2-FD-MAX-LEN
     MOVE HFLD-LENGTH(IFLD-IDX)    TO SP2-FD-PROG-LEN.

     MOVE IFD-CURS-SHOW(IFD-PTR)   TO SP2-FD-CURS-SHOW.
     MOVE HFLD-NUMBER(IFLD-IDX)    TO SP2-FD-FLD-NUM.
     MOVE HFLD-SCRNORDER(IFLD-IDX) TO SP2-FD-TAB-NUM.
     MOVE My-Prog-Off              TO SP2-FD-PROG-OFF.
     MOVE My-Prog-Num              TO SP2-FD-PROG-NUM.
     MOVE IFD-PROG-SPEC(IFD-PTR)   TO SP2-FD-PROG-SPEC.
     MOVE HFLD-INITVAL(IFLD-IDX)   TO SP2-FD-VAR-DATA.
     MOVE IFD-MORE-OPTIONS(IFD-PTR) TO SP2-FD-MORE-OPTIONS.
     MOVE 0                        TO SP2-FD-VAR-LEN.
     CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.
     If SP2-FD-RET-CODE not = 0
      MOVE LOW-VALUES to END-MSG NIBBLE-WORK
      String "ERROR calling SP2-SET-FIELD-DEF err=%hd "
         DELIMITED by size into END-MSG
       END-STRING.

     Add HFLD-LENGTH(IFLD-IDX) To My-Prog-Off.
*>----------------------------------------------------------------
 old-DB-TO-STATIC.
*>********************************
*> static definition             *
*> parameter for GET-STATIC-DEF  *
*> also used with SET-STATIC-DEF *
*>********************************

     MOVE LOW-VALUES          TO  SP2-STATIC-DEF.
     Move +10                 To  SP2-SD-LEN-LEN.
     Move +12                 To  SP2-SD-NUM-LEN.
     Move +4                  To  SP2-SD-CHAR-LEN.
     Move 80                  To  SP2-SD-VAR-LEN.
     Move 80                  To  SP2-SD-TEXT-LEN.
*> SP2-SD-NUM-DATA
     Move 0                   To  SP2-SD-ID.
     Move ISTAT-ROW(ISTAT-IDX) To  SP2-SD-ROW.
     Move ISTAT-COL(ISTAT-IDX) To  SP2-SD-COL.
     Move 0                   To  SP2-SD-WIDTH.
     Move 0                   To  SP2-SD-HEIGHT.
     Move IFD-FONT-ID(IFD-PTR) To  SP2-SD-FONT-ID.
*>******* SP2-SD-CHAR-DATA *******
     Move IFD-COLR(IFD-PTR)   To  SP2-SD-COLR.
     Move 'l'                 To  SP2-SD-JUSTIFY.
     Move LOW-VALUE           To  SP2-SD-MISC-OPTIONS.
*>******** SP2-SD-VAR-DATA ********
     Move ISTAT-TEXT(ISTAT-IDX) To  SP2-SD-TEXT.
     CALL "SP2" USING SP2-SET-STATIC-DEF SP2-STATIC-DEF.
*>----------------------------------------------------------------
 CREATE-old-FKEY.
     MOVE LOW-VALUES          TO SP2-FIELD-DEF.
     MOVE +30    to  SP2-FD-LEN-LEN.
     MOVE +52    to  SP2-FD-NUM-LEN.
     MOVE +74    to  SP2-FD-CHAR-LEN.
     MOVE  -1    to  SP2-FD-LENL-LEN.
     MOVE +44    to  SP2-FD-NUML-LEN.
     MOVE Field-Template-Name      To SP2-FD-NAME.
     MOVE HOT-KEY-ROW(FKEY-IDX)    TO SP2-FD-ROW.
     MOVE IFD-COL(FKEY-PTR)         TO SP2-FD-COL.
     MOVE IFD-WIDTH(FKEY-PTR)       TO SP2-FD-WIDTH.
     MOVE IFD-HEIGHT(FKEY-PTR)      TO SP2-FD-HEIGHT.
     MOVE "r"                      TO SP2-FD-BOR-TYPE.
     Move IFD-FONT-ID(FKEY-PTR)     To SP2-FD-FONT-ID.
     MOVE IFD-HELP-KEY(FKEY-PTR)    TO SP2-FD-HELP-KEY.
     MOVE IFD-MAX-LEN(FKEY-PTR)     TO SP2-FD-MAX-LEN.
     MOVE IFD-PROG-NUM(FKEY-PTR)    TO SP2-FD-PROG-NUM.
     MOVE IFD-CTRL-TYPE(FKEY-PTR)   TO SP2-FD-CTRL-TYPE.
     Move HOT-KEY-LABEL(FKEY-IDX)   To SP2-FD-VAR-DATA.
     MOVE LOW-VALUES               TO SP2-FD-VAR-LENS.
     MOVE IFD-INITIAL-LEN(FKEY-PTR) TO SP2-FD-INITIAL-LEN.
     MOVE IFD-VAR-LEN(FKEY-PTR)     TO SP2-FD-VAR-LEN.
     move IFD-OPTIONS-3(FKEY-PTR)   to SP2-FD-OPTIONS-3.
     MOVE IFD-TAB-NUM(FKEY-PTR)     TO SP2-FD-TAB-NUM.
     MOVE IFD-OUTPUT(FKEY-PTR)      TO SP2-FD-OUTPUT.
     COMPUTE SP2-FD-ID = SP2-FD-HELP-KEY + 100.

     CALL "SP2" USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.
*>--------------------------------------------------------------
 SET-FIELD-USERDATA.
*>
*>Field USER-DATA is being used to STORE the Vue3 Template name of the field.
*>Example: NONE-D, or HI-O
*>This data will be used by the WEB Client to place each field in it's
*>own class. This class will then be referenced in the CSS file.
*>
*>This USER-DATA will be read by the SP2.CBX script using the SP2CGI.exe
*>It will be represented in the CBX file as "$026VG0(8)"
*>
     MOVE SPACES TO HOT-TEMPLATE-NAME Field-Template-Name.
     Move 0 To IFD-IDX Field-Template.

     STRING HFLD-ENH(IFLD-IDX)  DELIMITED BY SPACE
            "-"                 DELIMITED BY SIZE
            HFLD-TYPE(IFLD-IDX) DELIMITED BY SPACE
       INTO HOT-TEMPLATE-NAME.

     Move HOT-TEMPLATE-NAME To TEMPLATE-STRING.
     Perform GET-TEMPLATE-VALUE.
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


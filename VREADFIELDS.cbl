>>source format free.
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VREADFIELDS.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
*>*****************************************************************
*>                    V R E A D F I E L D S                       *
*>*****************************************************************
*>   .10/30/2017   CM016891 Michael Anderson                      *
*>                 NO LOGIC CHANGED. Removed commented-out code.  *
*>                 Fixed comments to begin in col 7, most COBOL   *
*>   .     .     . editors require comments to begin in column 7. *
*>   .10/30/2017   CM016891 Michael Anderson                      *
*>                 SP2 form and report logging feature.           *
*>                 This change is to support the Logging of FORMS *
*>                 used by Users, at the VREADFIELDS Key-Press.   *
*>   .10/31/2017   CM016891 Michael Anderson                      *
*>                 SP2 form and report logging feature.           *
*>                 Had trouble locating the correct copylib's     *
*>                 Updated VPLUSMEM in the VPLUS copylib.         *
*>                 Then change the COPY statements as follows:    *
*>                    COPY "vplus/TCLDBMEM" of .                  *
*>                    COPY "vplus/SP250".                         *
*>                    COPY "SECDBS".                              *
*>                    COPY "comlib/USERAREA".                     *
*>                    COPY "comlib/DRIVAREA".                     *
*>                    COPY "vplus/COMAREA".                       *
*>                    COPY "vplus/VPLUSMEM".                      *
*>                                                                *
*>              Why not use the original format?                 *
*>              ea: COPY "USERAREA" OF COMLIB                     *
*>                                                                *
*>              Although AcuCobol supports this original format,  *
*>              it also supports this "comlib/USERAREA" format.   *
*>              My editor does not support the original format.  *
*>                                                                *
*>              All other vplus routines must also be changed.    *
*>                                                                *
*>*****************************************************************
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 TEMP-TEXT                     Pic X(80) Value Spaces.
 01 CharCount                         PIC S9(4) Comp-5 value 0.
 01 LINK2                         PIC S9(4) Comp-5 value 0.
     COPY "vplus/TCLDBMEM".
     COPY "vplus/SP250".
 01  Secdb-Database.
     10  Sec-IMAGE-WORD           PIC X(02) VALUE SPACES.
     10  Sec-DBNAME               PIC X(26) VALUE SPACES.
 01  Jobdb-Database.
     10  Job-IMAGE-WORD           PIC X(02) VALUE SPACES.
     10  Job-DBNAME               PIC X(26) VALUE SPACES.

     COPY "comlib/SECDBS".
 01 Search-Key-ULong  Usage is UnSigned-Long.
 01 Dbvariable.
    02 Dbv-bytes         Pic X(2)  Value spaces.
    02 Dbv-Name          Pic X(34) Value spaces.
 01 Dbvarpass            Pic X(8)  Value spaces.


 01 shutmsg                       PIC X(80) VALUE SPACES.
 01 Shutdown-Reason               Pic XX Value Spaces.
        88 Reason-OK             VALUE "OK". *> OK executing
        88 Reason-Expired        VALUE "EX". *> Expired
        88 Reason-Violation      VALUE "AX". *> Access Exception
        88 Reason-Overtime       VALUE "OT". *> Overtime
        88 Reason-DB-Error       VALUE "DE". *> Database Error
        88 Reason-Error          VALUE "ER". *> Error, non-specific
        88 Reason-ScreenLock     VALUE "SL". *> Secure lock, password prompt
        88 Reason-Aborted        VALUE "AB". *> ABORTED by System Management
        88 Reason-NormalEnd      VALUE "NE". *> No Error, Normal End
        88 Valid-Reason Values are "OK" "EX" "AX" "OT" "DE" "ER" "SL" "NE".
        88 Error-Reason Values are "EX" "AX" "DE" "ER".

 01 JOBNUM         Pic X(16) Value "JOBNUMS".
 01 JOBNUM-Buffer.
    02 JOBNUM-OS-UNIQUE-NUM              Usage is UnSigned-Long Value 0.
    02 JOBNUM-JSNUM                      Pic S9(09) Comp Value 0.
    02 JOBNUM-JOBSESS                    Pic X(002) Value Spaces.
    02 JOBNUM-JOBNAME                    Pic X(032) Value Spaces.
    02 JOBNUM-START-TIME                 Pic S9(18) Comp Value 0.
    02 JOBNUM-END-TIME                   Pic S9(18) Comp Value 0.
    02 JOBNUM-ELAPSED-TIME               Pic S9(18) Comp Value 0.
    02 JOBNUM-CPU-TIME                   Pic S9(18) Comp Value 0.
    02 JOBNUM-JOBSTATUS                  Pic X(004) Value Spaces.
    02 JOBNUM-MESSAGE                    Pic X(80)  Value Spaces.


 01  STATUS-AREA.
     03  CONDITION-WORD    PIC S9(4) COMP.
         88  NO-IMAGE-ERRORS         VALUE 0.
         88  IMAGE-ERRORS  VALUES ARE -9999 THRU -1, 1 THRU  9999.
         88  END-OF-FILE             VALUE 11.
         88  BEG-OF-FILE             VALUE 12.
         88  BEG-OF-CHAIN            VALUE 14.
         88  END-OF-CHAIN            VALUE 15.
         88  NO-ENTRY                VALUE 17.
     03  RECORD-LENGTH     PIC S9(4) COMP.
     03  RECORD-NUMBER     PIC S9(9) COMP.
     03  ENTRIES-IN-CHAIN  PIC S9(9) COMP.
     03  PREV-RECORD-NO    PIC S9(9) COMP.
     03  NEXT-RECORD-NO    PIC S9(9) COMP.

 01  Direct-Rec        PIC S9(9) COMP Value 0.


 01  RE-READ           PIC S9(4) COMP SYNC VALUE 1.
 01  SERIAL            PIC S9(4) COMP SYNC VALUE 2.
 01  BACKWD            PIC S9(4) COMP SYNC VALUE 3.
 01  DIRECT            PIC S9(4) COMP SYNC VALUE 4.
 01  CHAINED           PIC S9(4) COMP SYNC VALUE 5.
 01  BACK-CHAINED      PIC S9(4) COMP SYNC VALUE 6.
 01  KEYED             PIC S9(4) COMP SYNC VALUE 7.
 01  MODE-1            PIC S9(4) COMP SYNC VALUE 1.
 01  MODE-3            PIC S9(4) COMP SYNC VALUE 3.
 01  MODE-4            PIC S9(4) COMP SYNC VALUE 4.
 01  MODE-5            PIC S9(4) COMP SYNC VALUE 5.
 01  MODE-N            PIC S9(4) COMP SYNC.
 01  NULL-ITEM         PIC XX VALUE "; ".
 01  ALL-ITEMS         PIC XX VALUE "@;".
 01  SEARCH-ITEM       PIC X(18).
 01 SEARCH-KEY        PIC X(32).
 01 Search-Key-I2     REDEFINES Search-Key.
    02 SKey16         Pic 9(4) Comp.
    02 Filler         Pic X(30).
 01 Search-Key-I4     REDEFINES Search-Key.
    02 SKey32         Pic 9(9) Comp.
    02 Filler         Pic X(28).
 01 Search-Key-I8     REDEFINES Search-Key.
    02 SKey64         Pic s9(18) Comp.
    02 Filler         Pic X(24).

 01  ClockSeconds                PIC S9(18) Comp-5 Value 0.
 01  Expire-Seconds              PIC S9(18) Comp-5 Value 0.
 01  Seconds-Of-No-Activity      PIC S9(18) Comp-5 Value 0.
 01  TO-Minutes      PIC S9(4) Comp-5 Value 0.
 01  TO-Seconds      PIC S9(18) Comp-5 Value 0.

 01  NUM1                        PIC X(14)  Value Spaces.
 01  NUM2                        PIC S9(14) Value Zero.
 01  NUMDEC                      PIC S9(4)  Comp-5 Value 0.
 01  NUMERR                      PIC S9(4)  Comp-5 Value 0.
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
 01 BUFLEN                       PIC S9(4) COMP-5 SYNC VALUE 150.
 01 DISP-DBL                     PIC 9(10).
 01 FLDIDX                       PIC S9(4) COMP-5 VALUE 0.
 01 FLDPTR                       PIC S9(4) COMP-5 VALUE 0.
 01 MY-DATA-TYPE                 PIC X(4)  VALUE SPACES.
 01 Scratch                      PIC X(24) VALUE SPACES.

 01 MYERR                        PIC X(80) VALUE SPACES.
 01 MYOFFSET                     PIC S9(4) COMP-5 VALUE 0.
 01 MYLENGTH                     PIC S9(4) COMP-5 VALUE 0.
 01 MYBUFFER                     PIC X(3000) VALUE SPACES.
 01 MYDATA                       PIC X(80) VALUE SPACES.
 01 Authstart                    PIC S9(4) COMP-5 VALUE 0.
 01 Ipstart                      PIC S9(4) COMP-5 VALUE 0.
 01 NOWAIT-READ-SW               PIC X(1)  Value "X".
    88 NOWAIT-TRUE               VALUE "1".
    88 NOWAIT-FALSE              VALUE "0".
    88 NOWAIT-NOTSET             VALUE "X".

 01 Disp-z4                      Pic zzz9 value zero.
 01 Disp-Key                     Pic ----9 value zero.
 01 numparm1                     Pic S9(4) Comp-5 Value 0.
 01 numparm2                     Pic S9(4) Comp-5 Value 0.
 01 Print-Form-Sw                Pic 9 Value 0.
    88 Print-Key-Pressed  Value 1.
    88 Other-Key-Pressed  Value 0.

 01 Timeout-Read-Sw                Pic 9 Value 0.
    88 Time-Out-Read      Value 1.

 01 Application-Screen-Sw          Pic 9 Value 1.
    88 Is-Appl-Screen     Value 1.

 01 AUTH-READ-BUFFER.
    05 Read-AUTH    Pic X(48) Value Spaces.
    05 Read-IP      Pic X(16) Value Spaces.

     COPY "comlib/USERAREA".
     COPY "comlib/DRIVAREA".
 01 MpeJobNum usage is pointer.
 01 OS-Unique Usage is UnSigned-Long Value 0.

 01 SLDEV         PIC 9(4)  value 0.
 01 JSNUM         PIC 9(4)  value 0.
 01 CONNTYPE      PIC X     value space.
 01 RETURN-ERROR  PIC S9(4) COMP value 0.

 01 Save-Lastkey Pic s9(4) comp-5 Value 0.
 01 ERROR-SW       PIC X     VALUE "0".
    88 ERRORS      VALUE "1".
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
         10  MY-MS-BUTTON       PIC X.
         10  MY-MS-CANCEL       PIC X.
         10  MY-MS-REPLY        PIC X.
*>******* MY-MS-VAR-DATA *******
         10  MY-MS-TITLE        PIC X(80).
         10  MY-MS-TEXT         PIC X(512).
 01  MYTERM-FILE PIC X(36) value spaces.
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
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.

 01 TEMP-BUFFER    PIC X(2000).
 01 STRIDX1        PIC S9(4) COMP-5 VALUE 0.
 01 STRIDX2        PIC S9(4) COMP-5 VALUE 0.
*>########################################################
 LINKAGE SECTION.
    COPY "COMAREA" of vplus.
    COPY "VPLUSMEM" of vplus.
 01 My-Tcl             USAGE POINTER.
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
 0000-BEGIN.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.
*>
*> debug start
     move LOW-VALUES TO INTR-CALLNAME
     string "VREADFIELDS "
      delimited by " " into INTR-CALLNAME.

     move LOW-VALUES to DebugBUF.
     string "VREADFIELDS: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.


*> debug end
*>
*>   Before the read occurs, the "hidden AUTH" fields value must be set in the DATA BUFFER.
*>
*>   VPUTBUFFER/VGETBUFFER are NOT able to access these AUTH fields.
*>   Only VREADFIELDS is allowed to access these AUTH fields.
*>
*>   Here we will append the AUTH fields to the end of the DATA BUFFER.
*>
     Compute Authstart = AF-AppBuf-Len + 1.
     Compute Ipstart = AF-AppBuf-Len + 49.

     Move AF-Auth-ID TO MM-CD-DATABUFFER(Authstart:48).
     MOVE Af-IPADDR  tO MM-CD-DATABUFFER(Ipstart:16).

     Perform MM-REMOVE-INVALID-BYTE.
     Move MM-CD-DATABUFFER To MY-CD-DATABUFFER.
     Move MM-CD-DATABUFFER To MYBUFFER.

     Initialize AUTH-READ-BUFFER.

     CALL "SP2" USING SP2-SET-PANEL-FIELDS MYBUFFER.

     MOVE LOW-VALUES        TO SP2-CD-DATA.
     MOVE LOW-VALUES TO MY-CD-DATA.
     COMPUTE MY-CD-LEN-LEN      = FUNCTION LENGTH(MY-CD-LENS).
     COMPUTE MY-CD-IP-NUM-LEN   = FUNCTION LENGTH(MY-CD-IP-NUM-DATA).
     COMPUTE MY-CD-IP-CHAR-LEN  = FUNCTION LENGTH(MY-CD-IP-CHAR-DATA).
     COMPUTE MY-CD-OP-NUM-LEN   = FUNCTION LENGTH(MY-CD-OP-NUM-DATA).
     COMPUTE MY-CD-OP-CHAR-LEN  = FUNCTION LENGTH(MY-CD-OP-CHAR-DATA).
     COMPUTE MY-CD-FIELD-LEN    = FUNCTION LENGTH(MY-CD-DATABUFFER).
     COMPUTE MY-CD-COLR-LEN     = FUNCTION LENGTH(MY-CD-COLR-DATA).
     COMPUTE MY-CD-TYPE-LEN     = FUNCTION LENGTH(MY-CD-TYPE-DATA).
     Move "x"               To MY-CD-NEW-WINDOW.
     IF (TERMOPTIONS - 2 ) = MM-TERMOPTIONS *> NOWAIT RE-READ BUFFER
      move LOW-VALUES to END-MSG
      Move LASTKEY To Save-Lastkey
      Move "1" TO NOWAIT-READ-SW
      Move "n" To MY-CD-WAIT-SW
     Else
      MOVE SP2-MOUSE-ARROW TO SP2-NP-RET-CODE
      CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM
      Move "a" To MY-CD-WAIT-SW
      Move "0" TO NOWAIT-READ-SW
      MOve X"1E" TO MY-CD-TIMEOUT.

     move LOW-VALUES        TO SP2-NAME-DEF.
     MOVE HPAN-PAN-NAME     TO SP2-ND-NAME.
     INITIALIZE MY-CD-DATABUFFER.
     Move MM-CD-DATABUFFER  To MY-CD-DATABUFFER.
     MOVE "x"               TO MY-CD-NEW-WINDOW.
     MOVE CURSOR-POS-FIELD  TO MY-CD-NEXT-FLD-ID. *> HFLD-NUMBER
     MOVE HPAN-PAN-NAME     TO MY-CD-NEXT-PANEL.
     Move 0 To Timeout-Read-Sw.
     Move 0 To Direct-Rec.
     Move 1 To Application-Screen-Sw.
     If HPAN-NAME = "VPLUS_SECURITY    "
        Move 0 To Application-Screen-Sw.

     If HPAN-NAME = "GETKEY          "
        Move 0 To Application-Screen-Sw.

     IF NOWAIT-FALSE
        CALL "VPLUSSDEV" USING SLDEV CONNTYPE RETURN-ERROR
        MOVE "  SECDB.SECURE.SYS;" TO Secdb-Database
        CALL "DBOPEN" USING Secdb-Database PASSWORD Mode-1 Status-Area
        IF NOT NO-IMAGE-ERRORS
           Display "Shutting down session due to Database Error"
           Perform Database-Error
           Move "DE" To Shutdown-Reason
           Perform Shutdown-Session
        Else
           Perform Get-LDEV-Timeout
           Perform Process-Authdata
        End-If
     End-If.
*>------------------------------------------------------
*> This process will be BLOCKED waiting FOR I/O by SP2-CONVERSE-PANEL
*> Unless (TERMOPTIONS - 2 ) = MM-TERMOPTIONS
*>
*>------------------------------------------------------
*> B E G I N    O F    M A I N   R E A D   L O O P
*>------------------------------------------------------

     set Print-Key-Pressed to True.
     Perform Until Not Print-Key-Pressed
          Move 0 To Timeout-read-sw
          CALL "SP2" USING SP2-CONVERSE-PANEL MY-CONVERSE-DATA

          PERFORM MY-REMOVE-INVALID-BYTE

          If NOWAIT-FALSE And My-Cd-Key = -1
             Display time-of-day " VREADFIELDS Heartbeat Event"
             move LOW-VALUES to DebugBUF
             Move My-Cd-Key To Disp-Key
             string
              "VREADFIELDS Blocked I/O Timeout: Heartbeat Event: " CFNAME
               DELIMITED BY size into DebugBUF
             End-String
             CALL
              "LogDebug" USING DebugBUF
             End-Call
             Move 1 To Timeout-read-sw
             Move MY-CD-DATABUFFER To MM-CD-DATABUFFER

             Perform Process-Authdata
             If AUTHSESS-OK
                Move 2 To MY-CD-ROW-COL-SW
                Perform Set-Vue-Cursor
                EXIT PERFORM CYCLE
             Else
                If Is-Appl-Screen
                   Perform Shutdown-Session
                End-If
             End-If
          End-If

          If ( MY-CD-RET-CODE Not = 0) Or (My-Cd-Key = SP2-KEY-ESCAPE )
             If My-Cd-Key = SP2-KEY-ESCAPE
                Perform Ask-User-First
                IF MY-MS-REPLY = "y"
                   Move "NE" To Shutdown-Reason
                   Perform ShutDown-Session
                Else
                   Move 2 To MY-CD-ROW-COL-SW
                   Perform Set-Vue-Cursor
                   EXIT PERFORM CYCLE
                End-If
             Else
                Move "ER" To Shutdown-Reason
                Perform ShutDown-Session
             End-If
          End-If
          move LOW-VALUES to DebugBUF
          Move My-Cd-Key To Disp-Key
          string
           "VREADFIELDS: " CFNAME
           " KeyPressed: " Disp-Key
           " Print-key-pressed: " Print-Form-Sw
            DELIMITED BY size into DebugBUF
          End-String
          CALL "LogDebug" USING DebugBUF End-Call
          Perform Log-Usage
*>
*> When the following condition is False,
*> we will EXIT the read loop!
*>
          If My-Cd-Key = SP2-KEY-F12
             Call "VPRINTFORM" Using Comarea numparm1 numparm2
          Else
             set Other-Key-Pressed to True
          End-If
          Move 2 To MY-CD-ROW-COL-SW
          Perform Set-Vue-Cursor

     End-Perform.
*>------------------------------------------------------
*> E N D  O F    M A I N   R E A D   L O O P
*>------------------------------------------------------
*>
*> When BLOCKED I/O Completes we check on the events that occured.
     IF NOWAIT-FALSE
        EVALUATE MY-CD-KEY
          WHEN 13  MOVE  0 TO LASTKEY
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
           Move Save-Lastkey TO LASTKEY
        END-EVALUATE
        move LOW-VALUES to DebugBUF
        Move My-Cd-Key To Disp-Key
        string
         "VREADFIELDS Blocked I/O complete: " CFNAME
         " KeyPressed: " Disp-Key
         " Print-key-pressed: " Print-Form-Sw
          DELIMITED BY size into DebugBUF
        End-String
        CALL "LogDebug" USING DebugBUF End-Call
     End-If.


     PERFORM MY-REMOVE-INVALID-BYTE.

*> The following four MOVE's must occur, No matter what.
     MOVE 0                 TO CURSOR-POS-FIELD.
     MOVE HIGH-VALUES       TO SVT-BUFFER.
     MOVE COMAREA           TO MM-COMAREA.
     MOVE MY-CONVERSE-DATA  TO MM-CONVERSE-DATA.

*> Authdata is processed only when doing a BLOCKED read.
     IF NOWAIT-FALSE
        Perform Process-Authdata.

     Go To END-OF-READ.
*>------------------------------------------------------
*> E N D  O F    M A I N
*> Remainning code is sub-paragraphs.
*>------------------------------------------------------------------
 Process-Authdata.

     If NOT Time-Out-Read
        Move MM-CD-DATABUFFER(Authstart:48) to Read-Auth
        Move MM-CD-DATABUFFER(Ipstart:16)   to Read-IP
     End-If.
*>
*> WebClient adds x"ODOA" to the end of the data buffer.
*> x"ODOA" will cause problems in Read-IP,
*> cr/lf must be changed to spaces.
*>
     Move Spaces To Scratch.
     Move 0 To FLDPTR.
     Perform Varying FLDIDX From 1 By 1 UNTIL FLDIDX > 16
        If Read-IP(FLDIDX:1) > " "
           Add 1 To FLDPTR
           Move Read-IP(FLDIDX:1) To Scratch(FLDPTR:1)
        End-If
     End-Perform.
     Move Scratch To Read-IP.

     If NOT Time-Out-Read
        Move Spaces To MM-CD-DATABUFFER(Authstart:64)
     End-If.
     Move Read-IP TO AF-IPADDR.
     If HPAN-NAME NOT = "VPLUS_SECURITY    "
        Perform Check-Update-AuthKey.

*>------------------------------------------------------------------
 Check-Update-AuthKey.
*> Now check and update the AUTHKEY in the database.
*> AUTHKEY record created when the Launcher started.
     Perform Get-AuthSess-Record.

     If Not NO-ENTRY and NOT End-OF-CHAIN
        Move AUTHSESS-AUTHKEY TO AF-Auth-ID
        MOVE AUTHSESS-IPADDR  TO Af-IPADDR
        Perform Get-ClockSeconds
        Move AUTHSESS-LOCK TO Shutdown-Reason
        If Reason-Aborted
           If Is-Appl-Screen
              Perform Shutdown-Session
           End-If
        End-If

        Compute
         Seconds-Of-No-Activity = (ClockSeconds - AUTHSESS-READ-TIME)
        End-Compute

        IF Time-Out-Read
           If ClockSeconds > AUTHSESS-EXPIRE
              Move "OT"             To AUTHSESS-LOCK
           End-If
           If Seconds-Of-No-Activity > 3600 and ClockSeconds > AUTHSESS-EXPIRE
              Move "EX"             To AUTHSESS-LOCK
           End-If
        Else
           If ClockSeconds > AUTHSESS-EXPIRE
              Compute AUTHSESS-EXPIRE = ClockSeconds + 3600 End-Compute
           End-If
           If AUTHSESS-IPADDR Not = Read-IP
               Move all "0" To AUTHSESS-EXPIRE
               Move "AX"    To AUTHSESS-LOCK
           End-IF
           Move ClockSeconds     To  AUTHSESS-READ-TIME
        End-If

        Move HPAN-NAME        TO  AUTHSESS-SCREEN
        Move AF-PROGNAME      TO  AUTHSESS-PROGNAME
        Perform LOCK-AUTHSES
        Perform UPDATE-AUTHSES
        Perform UNLOCK-AUTHSES
     Else
*> If we're here, then we were unable to read the auth record.
        If Is-Appl-Screen
           Move "ER" To AUTHSESS-LOCK Shutdown-Reason
           Perform Shutdown-Session
        End-If
     End-If.
     Move AUTHSESS-LOCK    TO  Shutdown-Reason.
*>-------------------------------------------------------------
 Get-ClockSeconds.
     Move LOW-VALUES TO TCL-BUFFER.
     Move 0 To TCL-RESULT.
     Move 0 To ClockSeconds.
     String "set STIME [clock seconds]" delimited by size into TCL-BUFFER.
     CALL "TCLEVAL" USING My-Tcl TCL-BUFFER TCL-RESULT.

     MOVE LOW-VALUES TO TCL-VARNAME.
     MOVE SPACES TO TCL-BUFFER.
     Move Spaces To ClockSeconds.
     STRING "STIME" DELIMITED BY SIZE INTO TCL-VARNAME.
     CALL "TCLGETVAR" USING My-Tcl TCL-VARNAME TCL-BUFFER.
     Initialize num1 num2 numdec numerr.
     String TCL-BUFFER delimited by x"00" into num1
     Call "NUMGET" Using num1 num2 numdec numerr
     If Numdec = 0 and Numerr = 0
        Move num2 To ClockSeconds
     Else
        Move 0 To ClockSeconds.
*>--------------------------------------------------------------
 ShutDown-Session.
*>
*> Gather data to call VPLUSIPC.
     MOVE SLDEV TO SD-TERMINAL-NUMBER.
     CALL "GETXDS-HEADER" USING DRIVER-AREA, USER-AREA.
     CALL "XDSGET" USING DRIVER-AREA USER-AREA.
*>
*> Call to VPLUSIPC relay to all other processes within this session to shutdown.
     Move "09" To C-Type Drive-Type.
     Move "TERMINATE" To C-screen.
     Call "VPLUSIPC" Using USER-AREA.
*>
*> Close/end the SP2 session
     Call "VCLOSEFORMF" Using COMAREA.
     Call "VCLOSETERM"  Using COMAREA MYTERM-FILE.
     If Is-Appl-Screen
      Perform Get-AuthSess-Record
      If Not NO-ENTRY and NOT End-OF-CHAIN
         Move Shutdown-Reason  To AUTHSESS-LOCK
         Move ClockSeconds     To AUTHSESS-EXPIRE
         Perform LOCK-AUTHSES
         Perform UPDATE-AUTHSES
         Perform UNLOCK-AUTHSES.

     CALL "DBCLOSE" USING Secdb-Database DUMMY MODE-1 STATUS-AREA.
     Perform Get-Session-Number.
*>
*> Log reason for shutdown
     EVALUATE True
         When Reason-Expired
              Move "Session expired, due to lack of activity" To shutmsg
         When Reason-Violation
              Move "Authorization Key mismatch, Security Violation" To shutmsg
         When Reason-DB-Error
              Move "Database Error in VREADFIELDS" To shutmsg
         When Reason-Error
              Move "Error occured in VREADFIELD" To shutmsg
         When Reason-Aborted
              Move "Aborted by System Management" To shutmsg
         When Other
              String "Invalid shutdown reason " Shutdown-Reason
                     Delimited by size into shutmsg
              End-String
     END-EVALUATE.
     Display "VREADFIELDS session shutdown: " shutmsg.

     Move Spaces To Temp-Text.
     String "#S" JSNUM " " shutmsg
        Delimited By Size Into Temp-Text.
     Call "LogConsole" Using Temp-Text.

     INITIALIZE VPOPUP.
     Move AuthSess-User To VP-Name.
     Move shutmsg To VP-Message.
     Move "s" To VP-Type.
     Move "o" To VP-Button.
     Move "o" To VP-Reply.
     Move 10  To VP-Timeout.
*>
*> Do everything needed to release resources in this process.
     Call "TERMINATE" Using User-Area.
     STOP RUN.
*>----------------------------
 Ask-User-First.
     MOVE LOW-VALUES TO MY-MS-DATA.
     MOVE 1 TO MY-MS-LINE-CNT.
     MOVE "s" TO MY-MS-ICON.
     MOVE "y" TO MY-MS-BUTTON.
     MOVE "Escape Pressed" to MY-MS-TITLE.
     STRING "*** YOU PRESSED THE ESCAPE KEY ***" DELIMITED BY SIZE
            X"0D0A"                 DELIMITED BY SIZE
            " This will terminate your session" DELIMITED BY "  "
            X"0D0A"                 DELIMITED BY SIZE
            "          Are you sure?"      DELIMITED BY SIZE
            X"0D0A"                 DELIMITED BY SIZE INTO MY-MS-TEXT
     END-STRING.
     CALL "SP2" USING SP2-DISPLAY-MESSAGE MY-MESSAGE-DATA.
     MOVE 0 TO SP2-NP-RET-CODE.
     CALL "SP2" USING SP2-SET-MOUSE-SHAPE SP2-NULL-PARM.
*>--------------------------------------------------------------
 Get-LDEV-Timeout.
     Initialize TO-Seconds TO-Minutes num1 num2 numdec numerr.
     Move SLDEV To Search-Key.
     CALL "DBGET" USING Secdb-Database DS-DEVICES KEYED
         STATUS-AREA ALL-ITEMS DB-DEVICES SEARCH-KEY.

     IF IMAGE-ERRORS AND NOT NO-ENTRY
        Perform Database-Error.

     If Not No-Entry
        Move DEV-TIMEOUT To Num1
        Call "NUMGET" Using num1 num2 numdec numerr
        If Numdec = 0 and Numerr = 0
           Move num2 To TO-Minutes
        Else
           Move 5 To TO-Minutes.

     If TO-Minutes > 0
        Compute
         TO-Seconds = TO-Minutes * 60
        End-Compute
     Else
        Move 300 To TO-Seconds.
*>--------------------------------------------------------------
 Set-Vue-Cursor.
*> This basically set the cursor to where is was before the
*> CONVERSE-PANEL timed-out.
*> It loops thru a table containning the current panel fields.
*>
*> When a field table entry matches the Last-FLD-ID, this is the field
*> where the cursor was. This will be placed in "My-Cd-Next-Fld-Id".
*>
*> But we're not done yet. Next we get a count of the characters entered
*> into the field by the user, tallyed in CharCount.
*> The value of CharCount will be placed in "My-Cd-Cursor-Col".
*>
*> Now when the time-out occurs, the user is unaware, and this is good.
*>
     Move Spaces To Temp-Text.
     Move 0 To CharCount.

     Perform Varying Ifld-Idx From 1 By 1 Until Ifld-Idx > Af-Appfld-Cnt
        If Hfld-Number(Ifld-Idx) = My-Cd-Last-Fld-Id
           Move My-Cd-Databuffer(Hfld-Bufstart(Ifld-Idx):Hfld-Length(Ifld-Idx)) To Temp-Text
           Perform Varying CharCount From Hfld-Length(Ifld-Idx) By -1 Until CharCount < 1
              If Temp-Text(CharCount:1) > " "
                 Exit Perform
              End-If
           End-Perform
           Move CharCount             To My-Cd-Cursor-Col
           Move Hfld-Number(Ifld-Idx) To My-Cd-Next-Fld-Id
           Exit Perform
        End-If
     End-Perform.
*>--------------------------------------------------------------
 Get-AuthSess-Record.
     IF Direct-Rec = 0
        Perform FIND-AUTHSES
        If Not NO-ENTRY
           Perform Get-AUTHSES-CHAINED
           Move Record-Number To Direct-Rec
        End-If
     Else
        Perform GET-AUTHSES-DIRECT.
*>--------------------------------------------------------------
 Database-Error.
        CALL "DBEXPLAIN" USING Status-Area.
*>-----------------------------*
 LOCK-AUTHSES.
     CALL "DBLOCK" USING Secdb-Database DS-AUTH-SESSION
         MODE-3 STATUS-AREA.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 UNLOCK-AUTHSES.
     CALL "DBUNLOCK" USING Secdb-Database DS-AUTH-SESSION
         MODE-1 STATUS-AREA.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 FIND-AUTHSES.
     Call "MPE_JOBNUM" Returning OS-Unique.
     If Is-Appl-Screen
        Move "TC-MASTER-PIN;" To SEARCH-ITEM
        CALL "DBFIND" USING Secdb-Database DS-AUTH-SESSION
         MODE-1 STATUS-AREA SEARCH-ITEM OS-Unique
        End-Call
     Else
        Move "AUTHKEY;" To SEARCH-ITEM
        Move Read-Auth To Search-Key
        CALL "DBFIND" USING Secdb-Database DS-AUTH-SESSION
         MODE-1 STATUS-AREA SEARCH-ITEM Search-Key
        End-Call
     End-If.
     IF IMAGE-ERRORS AND NOT NO-ENTRY Perform Database-Error.
     IF ENTRIES-IN-CHAIN = ZERO MOVE 17 TO CONDITION-WORD.
*>-----------------------------*
 GET-AUTHSES-CHAINED.
     CALL "DBGET" USING Secdb-Database DS-AUTH-SESSION CHAINED
         STATUS-AREA ALL-ITEMS DB-AUTH-SESSION NULL-ITEM.
     IF IMAGE-ERRORS AND NOT END-OF-CHAIN
         Perform Database-Error.
*>-----------------------------*
 GET-AUTHSES-DIRECT.
     CALL "DBGET" USING Secdb-Database DS-AUTH-SESSION DIRECT
         STATUS-AREA ALL-ITEMS DB-AUTH-SESSION Direct-Rec.
     IF IMAGE-ERRORS AND NOT END-OF-CHAIN
         Perform Database-Error.
*>-----------------------------*
 PUT-AUTHSES.
     CALL "DBPUT" USING Secdb-Database DS-AUTH-SESSION
         MODE-1 STATUS-AREA ALL-ITEMS DB-AUTH-SESSION.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 UPDATE-AUTHSES.
     CALL "DBUPDATE" USING Secdb-Database DS-AUTH-SESSION
         MODE-1 STATUS-AREA ALL-ITEMS DB-AUTH-SESSION.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 DELETE-AUTHSES.
     CALL "DBDELETE" USING Secdb-Database DS-AUTH-SESSION
          MODE-1 STATUS-AREA.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 REWIND-AUTHSES.
     CALL "DBCLOSE" USING Secdb-Database DS-AUTH-SESSION
         MODE-3 STATUS-AREA.
     IF IMAGE-ERRORS Perform Database-Error.

*>-----------------------------*
 LOCK-Usage.
     CALL "DBLOCK" USING Secdb-Database DS-USER-USAGE
         MODE-3 STATUS-AREA.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 UNLOCK-Usage.
     CALL "DBUNLOCK" USING Secdb-Database DS-USER-USAGE
         MODE-1 STATUS-AREA.
     IF IMAGE-ERRORS Perform Database-Error.
*>-----------------------------*
 PUT-Usage.
     CALL "DBPUT" USING Secdb-Database DS-USER-USAGE
         MODE-1 STATUS-AREA ALL-ITEMS DB-USER-USAGE.
     IF IMAGE-ERRORS Perform Database-Error.

*>----------------------------------------------------
 Get-Session-Number.
     Move 0 TO JSNUM.
     Move "JOBDB.PUB.SYS" To Dbv-Name.
     Move ";"             To Dbvarpass.
     Call "DBOPEN" Using Dbvariable Dbvarpass Mode-1 Status-Area.
     If Image-Errors
        Go To Database-Error.

     Move "OS-UNIQUE-NUM;" To Search-Item.
     Move Low-Values       To Search-Key.
     Move AUTHSESS-PIN     To Search-Key-ULong.
     Perform Find-JOBNUM.
     If Not No-Entry
        Perform Get-JOBNUM-Chained
        Move JOBNUM-JSNUM TO JSNUM.

     CALL "DBCLOSE" USING Dbvariable DUMMY MODE-1 STATUS-AREA.
*>******************************
 Find-JOBNUM.
     Call "DBFIND" Using Dbvariable JOBNUM
         Mode-1 Status-Area Search-Item Search-Key-ULong.
     If Image-Errors And Not No-Entry
        Perform Database-Error.
     If Entries-In-Chain = Zero
        Move 17 To Condition-Word.

*>******************************
 Get-JOBNUM-Chained.
     Call "DBGET" Using Dbvariable JOBNUM Chained
         Status-Area All-Items JOBNUM-Buffer Null-Item.
     If Image-Errors And Not End-Of-Chain
         Perform Database-Error.

*>----------------------------------------------------
 MY-REMOVE-INVALID-BYTE.
     MOVE SPACES TO TEMP-BUFFER.
     MOVE 0 TO STRIDX1 STRIDX2.

     PERFORM VARYING STRIDX1 FROM 1 BY 1 UNTIL STRIDX1 > AF-AppBuf-Len
        IF MY-CD-DATABUFFER(STRIDX1:1) NOT = x"C2"
           ADD 1 TO STRIDX2
           MOVE MY-CD-DATABUFFER(STRIDX1:1) TO TEMP-BUFFER(STRIDX2:1)
        END-IF
     END-PERFORM.

     MOVE TEMP-BUFFER(1:AF-AppBuf-Len) TO MY-CD-DATABUFFER(1:AF-AppBuf-Len).

*>----------------------------------------------------
 MM-REMOVE-INVALID-BYTE.
     MOVE SPACES TO TEMP-BUFFER.
     MOVE 0 TO STRIDX1 STRIDX2.

     PERFORM VARYING STRIDX1 FROM 1 BY 1 UNTIL STRIDX1 > AF-AppBuf-Len
        IF MM-CD-DATABUFFER(STRIDX1:1) NOT = x"C2"
           ADD 1 TO STRIDX2
           MOVE MM-CD-DATABUFFER(STRIDX1:1) TO TEMP-BUFFER(STRIDX2:1)
        END-IF
     END-PERFORM.

     MOVE TEMP-BUFFER(1:AF-AppBuf-Len) TO MM-CD-DATABUFFER(1:AF-AppBuf-Len).

*>----------------------------------------------------
 END-OF-READ.
     IF NOWAIT-FALSE
        CALL "DBCLOSE" USING Secdb-Database DUMMY MODE-1 STATUS-AREA.

     move LOW-VALUES        to END-MSG.
     Move LASTKEY           To Disp-z4.
     string "LASTKEY=" Disp-z4 delimited by size into END-MSG.
     move LOW-VALUES to DebugBUF.
     string "VREADFIELDS exit: " END-MSG DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     move HIGH-VALUES       TO LAST-BUFFER(1:AF-AppBuf-Len).
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
       MOVE -1 TO HFLD-ERRFLG(IFLD-IDX)
     END-PERFORM.
     MOVE SP2-MOUSE-WAIT    TO SP2-NP-RET-CODE.
     MOVE COMAREA           TO MM-COMAREA.
     Goback.

*>----------------------------------------------------
 Log-Usage.
     Perform Get-AuthSess-Record.
     Initialize DB-USER-USAGE.
     ACCEPT TEMP-TEXT FROM ENVIRONMENT "FATHER_NAME".
     Move AUTHSESS-USER      To Usage-Username.
     Move AUTHSESS-READ-TIME To Usage-Start-Time.
     Move CFNAME             To Usage-Last-Screen.
     Move AUTHSESS-PROGNAME  To Usage-Last-Progname.
     Move AUTHSESS-PIN       To Usage-Tc-Master-Pin.
     Move TEMP-TEXT          To Usage-Calling-Progname.
     Move MY-CD-KEY          To Usage-Key-Pressed.
     IF NOWAIT-FALSE
        If MY-CD-KEY not < 0
           Perform LOCK-Usage
           Perform PUT-Usage
           Perform UNLOCK-Usage.

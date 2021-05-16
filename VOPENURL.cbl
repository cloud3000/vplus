>>source format free.
*>******************************************************************
*>*                       V O P E N U R L                          *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VOPENURL.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
*>>>>>>>>>>>>>>>> 01  SP2-THIN-CLIENT-CODES.
 01  SP2-EXECUTE-PROGRAM     PIC S9(4) COMP-5 VALUE +97.
 01  SP2-EXECUTE-CLIENT-PROG PIC S9(4) COMP-5 VALUE +107.
 01  SP2-COPY-FILE-TO-CLIENT PIC S9(4) COMP-5 VALUE +108.
 01  SP2-COPY-FILE-TO-SERVER PIC S9(4) COMP-5 VALUE +112.
 01  SP2-GET-COMMAND-LINE    PIC S9(4) COMP-5 VALUE +113.
 01  SP2-GET-CLIENT-IP       PIC S9(4) COMP-5 VALUE +119.
 01  QPR-END-SESSION         PIC S9(4) COMP-5 VALUE +16.

 01  TCL-COMMAND                 PIC X(2048)      value LOW-VALUES.
 01  TCL-buffer                  PIC X(2048)      value LOW-VALUES.
 01  TCL-RESULT                  PIC S9(9) COMP-5 Value 0.
 01  TCL-ERROR                   PIC X(2048)      VALUE SPACES.
 01  URL-Name                    Pic X(24)        Value Spaces.
 01  URL-ROW                     Pic ZZZ9         Value Zero.
 01  URL-COL                     Pic ZZZ9         Value Zero.
 01  URL-TEXT                    Pic X(128)       Value Spaces.
 01  URL-ENH                     Pic X(4)         Value Spaces.
 01  URL-Width                   Pic s9(4) Comp-5 Value 0.
 01  StrIdx                      PIC S9(4) COMP-5 Value 0.
 01  url-Offset                  PIC S9(4) COMP-5 Value 0.
 01  hostname                    Pic X(20) Value Spaces.
 01  jobnum                      Pic X(14) Value Spaces.
 01  jobnum-disp                 Pic 9(8) Value Zero.

 01  TheURL-String               Pic X(160) Value Spaces.

 01  NUM1.
     02  NUMX1                   PIC X  OCCURS 14 TIMES.

 01  NUM2.
     02  NUMX2                   PIC X  OCCURS 14 TIMES.
 01  FILLER REDEFINES NUM2.
     02  NUM2N                   PIC S9(14).

 01  NUMDEC                      PIC 9(4) COMP-5.
 01  NUMERR                      PIC 9(4) COMP-5.

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
     COPY "vplus/SP250".
     COPY "comlib/USERAREA".
     COPY "comlib/DRIVAREA".
 01 SLDEV         PIC 9(4)  value 0.
 01 CONNTYPE      PIC X     value space.
 01 RETURN-ERROR  PIC S9(4) COMP value 0.
 01 Session-Type  Pic XX Value Spaces.
    88 Web-Client     Value "WC".
    88 Thin-Client    Value "TC".
 01 DebugBUF     pic x(256)  VALUE SPACES.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 VURL  PIC X(150).

    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA VURL.
 0000-BEGIN-VOPENURL.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     Perform GET-XDS-CONNTYPE.

     If Web-Client
        Perform URL-FROM-WEB
     Else
        Perform URL-FROM-THINCLIENT.

     Goback.
*>----------------------------------------------------------------
 GET-XDS-CONNTYPE.
     CALL "VPLUSSDEV" USING SLDEV CONNTYPE RETURN-ERROR.
     MOVE SLDEV TO SD-TERMINAL-NUMBER.
     CALL "GETXDS-HEADER" USING DRIVER-AREA, USER-AREA.
     Move XDS-CONNTYPE To Session-Type.

*>-----------------------------------------------------------
 URL-FROM-THINCLIENT.
     move LOW-VALUES to DebugBUF.
     string "VOPENURL:  from ThinClient" VURL DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     If Function Upper-case(VURL(1:4)) Not = "HTTP"
        Move LOW-VALUES To SP2-BUFFER
        Move  VURL  To SP2-BF-DATA
        Move  VURL  To SP2-BF-DATA(41:24)
        Move 80 To SP2-BF-LEN
        Call "SP2" Using SP2-COPY-FILE-TO-CLIENT SP2-BUFFER.

     Move LOW-VALUES To SP2-BUFFER.
     String
        "CMD.exe /C start " Delimited by Size
        VURL      Delimited by Space
      InTo SP2-BF-DATA.

     Move 80 To SP2-BF-LEN.
     Call "SP2" Using SP2-EXECUTE-CLIENT-PROG SP2-BUFFER.


*>-------------------------------------------------------------
 URL-FROM-WEB.
*>      newWindow = window.open(VURL, VPLUSAPPMENU.WindowFeatures);
     move LOW-VALUES to DebugBUF.
     string "VOPENURL:  from WebClient" VURL DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     Accept hostname from ENVIRONMENT "HPHOSTNAME".
     Accept jobnum from ENVIRONMENT "VPLUS_MASTER".
     Move Spaces To TheURL-String.
     Move 1 To StrIdx.
     Compute url-offset = AF-AppBuf-Len + 65.
     String VURL Delimited by Space
      InTo TheURL-String With pointer StrIdx.
     Move TheURL-String To MM-CD-DATABUFFER(url-offset:StrIdx).
     Call "VREADFIELDS" USING COMAREA.
     Move Spaces To MM-CD-DATABUFFER(url-offset:StrIdx).


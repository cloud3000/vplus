>>source format free.
*>******************************************************************
*>*                       V P R I N T F O R M                      *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VPRINTFORM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
*>*>>>>>>>>>>>>>>> 01  SP2-THIN-CLIENT-CODES.
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
 01  Pdf-Name                    Pic X(24)        Value Spaces.
 01  PDF-ROW                     Pic ZZZ9         Value Zero.
 01  PDF-COL                     Pic ZZZ9         Value Zero.
 01  PDF-TEXT                    Pic X(128)       Value Spaces.
 01  PDF-ENH                     Pic X(4)         Value Spaces.
 01  Pdf-Width                   Pic s9(4) Comp-5 Value 0.
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
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 numparm1 Pic S9(4) Comp-5.
 01 numparm2 Pic S9(4) Comp-5.

    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.
*>*########################################################
 PROCEDURE DIVISION USING COMAREA numparm1 numparm2.
 0000-BEGIN-VPRINTFORM.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.
     move LOW-VALUES to DebugBUF.
     string "VPRINTFORM: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     Move Zero To Tcl-Result.
     Move LOW-VALUES To Tcl-Command.

     String "LP2pdf_open " Delimited By Size
            MM-CFNAME      Delimited By Space
            " {"           Delimited By Size
            "letter"       Delimited By Space
            "} "           Delimited By Size
            "l"            Delimited By Size
      Into Tcl-Command.
     CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result.

*> The characters in the string statement
*>     "1 colorname 1 1 1 1 1 1 1"
*>
*> Have the following meanning:
*>            Bars        1=Print bars (as in GREENBAR), 0=No bars
*>            Bars-Color  The color of the bars.
*>            Logo        1=Include logo, 0=No logo
*>            Reqinfo     1=Print username, 0=No userame.
*>            Procinfo    1=Print Server & process info, 0=No process info.
*>            Locinfo     1=Print PDF file name, 0=No file name.
*>            Slogon      1=Print Company matto in footer, 0=No matto.
*>            CopyRight   1=Print Coptright in footer, 0=No copyright.
*>            URL         1=Print Company URL, 0=No URL.

*> FORMFEED is first!
     String "LP2pdf_ff  "      Delimited By Size
            MM-CFNAME          Delimited By Space
             " "               Delimited By Size
             "0 lightblue 0 0 0 0 0 0 0 "
             " {"              Delimited By Size
            "username "        Delimited By "  "
             "} {"             Delimited By Size
            "programname "     Delimited By Space
             "}"
        Delimited By Size into  Tcl-Command.
     CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result.

*> Vue3 forms are always 80x27, so we need to relay this to the pdf.
     Move LOW-VALUES To Tcl-Command.
     String "LP2pdf_pagearea 80 27"
      Delimited by Size into Tcl-Command.
     CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result.

*> Write out the Static text.
     Perform PDF-Courier.
     PERFORM VARYING ISTAT-IDX FROM 1 BY 1 UNTIL ISTAT-IDX > ISTAT-CNT
      Move ISTAT-ROW(ISTAT-IDX)     To PDF-ROW
      Move ISTAT-COL(ISTAT-IDX)     To PDF-COL
      Move ISTAT-TEXT(ISTAT-IDX)    To PDF-TEXT
      Move LOW-VALUES To Tcl-Command
      Perform Get-actual-length
      String "LP2pdf_textarea " Delimited by size
             MM-CFNAME          Delimited By Space
             " {" PDF-TEXT(1:StrIdx) "} "
             PDF-COL " " PDF-ROW
       Delimited By Size Into Tcl-Command
      End-String
      CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result End-Call
     End-Perform.

*> Write out the data buffer.
     Perform PDF-Courier-Bold.
     Perform Varying IFLD-IDX from 1 by 1  Until (IFLD-IDX > AF-AppFld-Cnt)
      IF HFLD-TYPE(IFLD-IDX) NOT = "FK"
       Move HFLD-ROW(IFLD-IDX)         To PDF-ROW
       Move HFLD-COLUMN(IFLD-IDX)      To PDF-COL
       MOVE HFLD-ENH(IFLD-IDX)         TO PDF-ENH
       Move LOW-VALUES To Tcl-Command
       String "LP2pdf_textarea " Delimited by size
              MM-CFNAME          Delimited By Space
              " {" MM-CD-DATABUFFER(HFLD-BUFSTART(IFLD-IDX):HFLD-LENGTH(IFLD-IDX)) "} "
              PDF-COL " " PDF-ROW
        Delimited By Size Into Tcl-Command
       End-String
       CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result End-Call
      End-If
     End-Perform.
     Go To Vprintform-Return.

*>---------------------------------------------------------------
 PDF-Courier-Bold.
     Move LOW-VALUES To Tcl-Command.

     String "LP2pdf_fontarea "  Delimited by Size
             MM-CFNAME          Delimited By Space
             " "                Delimited By Size
             "Courier-Bold"     Delimited By Size
       Into Tcl-Command
      End-String
     CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result.
*>---------------------------------------------------------------
 PDF-Courier.
     Move LOW-VALUES To Tcl-Command.
     String "LP2pdf_fontarea "  Delimited by Size
             MM-CFNAME          Delimited By Space
             " "                Delimited By Size
             "Courier"          Delimited By Size
       Into Tcl-Command
      End-String
     CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result.
*>---------------------------------------------------------------
 Get-actual-length.
     Perform Varying StrIdx From 128 By -1 Until StrIdx < 2 or PDF-TEXT(StrIdx:1) > " "
      Continue
     End-Perform.
     Add 1 To StrIdx.
*>---------------------------------------------------------------
 Vprintform-Return.
     Move LOW-VALUES To Tcl-Command.
     Move Zero To Tcl-Result.
     String "LP2pdf_close " Delimited By Size MM-CFNAME
      Delimited By Space Into Tcl-Command.
     CALL "TCLEVAL" USING MY-Tcl Tcl-Command Tcl-Result.

     Perform GET-XDS-CONNTYPE.
     Move Spaces To Pdf-Name.
     STRING MM-CFNAME DELIMITED BY Space
            ".pdf" delimited by size INTO Pdf-Name.

     If Web-Client
        Perform PDF-To-Web
     Else
        Perform PDF-To-TC.
     Goback.
*>----------------------------------------------------------------
 GET-XDS-CONNTYPE.
     CALL "VPLUSSDEV" USING SLDEV CONNTYPE RETURN-ERROR.
     MOVE SLDEV TO SD-TERMINAL-NUMBER.
     CALL "GETXDS-HEADER" USING DRIVER-AREA, USER-AREA.
     Move XDS-CONNTYPE To Session-Type.

*>-----------------------------------------------------------
 PDF-To-TC.
     Move 1 To SP2-BF-LEN.

     Move LOW-VALUES To SP2-BUFFER.
     Move  Pdf-Name  To SP2-BF-DATA.
     Move  Pdf-Name  To SP2-BF-DATA(41:24).
     Move 80 To SP2-BF-LEN.
     Call "SP2" Using SP2-COPY-FILE-TO-CLIENT SP2-BUFFER.

     Move LOW-VALUES To SP2-BUFFER.
     String
        "CMD.exe /C start " Delimited by Size
        Pdf-Name      Delimited by Space
      InTo SP2-BF-DATA.

     Move 80 To SP2-BF-LEN.
     Call "SP2" Using SP2-EXECUTE-CLIENT-PROG SP2-BUFFER.


*>-------------------------------------------------------------
 PDF-To-Web.
*>      newWindow = window.open(showURL, VPLUSAPPMENU.WindowFeatures);
     Accept hostname from ENVIRONMENT "HPHOSTNAME".
     Accept jobnum from ENVIRONMENT "VPLUS_MASTER".
     Move Spaces To TheURL-String.
     Move 1 To StrIdx.
     Compute url-offset = AF-AppBuf-Len + 65.
     String
            "http://"            Delimited By Size
            hostname             Delimited by Space
            "/cgi-bin/showurl.cgi?auth=" Delimited By Size
            AF-Auth-ID           Delimited By Space
            "&jobnum="           Delimited By Size
            Jobnum               Delimited by Space
            "&filename="         Delimited By Size
            Pdf-Name             Delimited by Space
      InTo TheURL-String With pointer StrIdx.
     Move TheURL-String To MM-CD-DATABUFFER(url-offset:StrIdx).
     Call "VREADFIELDS" USING COMAREA.
     Move Spaces To MM-CD-DATABUFFER(url-offset:StrIdx).

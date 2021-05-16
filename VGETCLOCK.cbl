>>source format free.
*>*****************************************************************
*>                       V G E T C L O C K                        *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETCLOCK.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  GLOBAL-ADDRESS1   USAGE POINTER.
 01  NUM1                        PIC X(14)  Value Spaces.
 01  NUM2                        PIC S9(14) Value Zero.
 01  NUMDEC                      PIC S9(4)  Comp-5 Value 0.
 01  NUMERR                      PIC S9(4)  Comp-5 Value 0.
     COPY "vplus/TCLDBMEM".
 01 DebugBUF     pic x(256)  VALUE SPACES.

*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01  My-Tcl                    USAGE POINTER.
 01  Link-CLOCK                PIC S9(18) Comp-5 Value 0.
*>########################################################
 PROCEDURE DIVISION USING COMAREA Link-CLOCK.
 0000-BEGIN-VGETCLOCK.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
*>-----------------------------
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.

     Move LOW-VALUES TO TCL-BUFFER.
     Move 0 To TCL-RESULT.
     Move 0 To Link-CLOCK.
     String "set STIME [clock seconds]" delimited by size into TCL-BUFFER.
     CALL "TCLEVAL" USING My-Tcl TCL-BUFFER TCL-RESULT.

     MOVE LOW-VALUES TO TCL-VARNAME.
     MOVE SPACES TO TCL-BUFFER.
     STRING "STIME" DELIMITED BY SIZE INTO TCL-VARNAME.
     CALL "TCLGETVAR" USING My-Tcl TCL-VARNAME TCL-BUFFER.
     Initialize num1 num2 numdec numerr.
     String TCL-BUFFER delimited by x"00" into num1
     Call "NUMGET" Using num1 num2 numdec numerr
     If Numdec = 0 and Numerr = 0
        Move num2 To Link-CLOCK
     Else
        Move 0 To Link-CLOCK.
     Goback.

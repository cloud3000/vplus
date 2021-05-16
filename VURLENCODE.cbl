>>source format free.
*>******************************************************************
*>*                       V U R L E N C O D E                      *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VURLENCODE.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.

 01  TCL-COMMAND                 PIC X(2048)      value LOW-VALUES.
 01  TCL-buffer                  PIC X(2048)      value LOW-VALUES.
 01  TCL-VARNAME                 Pic X(32)        Value LOW-VALUES.
 01  TCL-RESULT                  PIC S9(9) COMP-5 Value 0.
 01  TCL-ERROR                   PIC X(2048)      VALUE SPACES.

 01  TheURL-String               Pic X(160) Value Spaces.
    COPY "vplus/SP250".

 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 VURL    PIC X(160).
 01 EncUrl  PIC X(160).

    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.
*>*########################################################
 PROCEDURE DIVISION USING COMAREA VURL EncUrl.
 0000-BEGIN-VURLENCODE.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET My-Tcl TO TCL of VPLUSMEM.

     move LOW-VALUES to DebugBUF.
     string "VURLENCODE: " VURL DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.


     Move LOW-VALUES TO TCL-BUFFER.
     MOVE LOW-VALUES TO TCL-VARNAME
     Move 0 To TCL-RESULT.
     Move VURL TO TheURL-String.
     String "set URLAUTH [EncodeAuth " Delimited By Size
             TheURL-String     Delimited By " "
            "]"                Delimited by size into TCL-BUFFER.

     CALL "TCLEVAL" USING My-Tcl TCL-BUFFER TCL-RESULT.

     MOVE SPACES TO EncUrl TCL-BUFFER.
     STRING "URLAUTH" DELIMITED BY SIZE INTO TCL-VARNAME
     CALL "TCLGETVAR" USING My-Tcl TCL-VARNAME TCL-BUFFER.

     Inspect TCL-BUFFER Replacing all X"00" by " ".
     String TCL-BUFFER Delimited By " " INTO EncUrl.

     Goback.


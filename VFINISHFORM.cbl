>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                     V F I N I S H F O R M                      *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VFINISHFORM.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
   COPY "vplus/TCLDBMEM".
 01 TCL-NEW-BUFFER PIC X(2048).
 01 TCL-BYTE-VALUE PIC X.
    88 VALID-BYTE VALUES ARE X"20" THROUGH X"7E".
 01 TCL-EVAL-STR pic x(4096) VALUE LOW-VALUES.
 01 TCL-BUFVALUE pic x(2048)   VALUE SPACES.
 01 TCL-VARVALUE pic x(80)   VALUE SPACES.
 01 STRIDX       PIC S9(4) COMP-5 VALUE 0.
 01 STRCNT       PIC S9(4) COMP-5 VALUE 0.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
*>---------------------------------------------------------*
 0000-BEGIN-VFINISHFORM.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.

     Move LOW-VALUES to TCL-VARVALUE.
     String "VFINISHFORM: " CFNAME DELIMITED BY size into TCL-VARVALUE.
     CALL "LogDebug" USING TCL-VARVALUE.
     Move Spaces To TCL-VARVALUE.
     PERFORM TCL-FINISHEDITS.
     GOBACK.

*>-----------------------------------------------------------
 TCL-FINISHEDITS.
     Move Spaces To TCL-NEW-BUFFER.
     MOVE LOW-VALUES TO TCL-VARNAME.
     STRING "VPLUSBUFFER"
       DELIMITED BY SIZE INTO TCL-VARNAME
     end-string.

     MOVE LOW-VALUES TO TCL-BUFVALUE.
     STRING MM-CD-DATABUFFER(1:dbuflen)
      DELIMITED BY SIZE INTO TCL-BUFVALUE
     END-STRING.
     CALL "TCLSETVAR" USING MY-TCL TCL-VARNAME TCL-BUFVALUE.

     MOVE LOW-VALUES TO TCL-EVAL-STR.
     MOVE 0 TO TCL-RESULT.

     MOVE LOW-VALUES TO TCL-EVAL-STR.
     MOVE 0 TO TCL-RESULT.

     STRING "::viewplus::PROCESSEDITSPECS " delimited by size
            MPE-FORMS-FILE                  delimited by space
            ".vform "                       delimited by size
            HPAN-NAME                       delimited by space
            " FINISH $VPLUSBUFFER"          delimited by size
            INTO TCL-EVAL-STR.
     CALL "LogDebug" USING TCL-EVAL-STR end-call
     CALL "TCLEVAL" USING BY REFERENCE MY-TCL
                          BY REFERENCE TCL-EVAL-STR
                          BY REFERENCE TCL-RESULT
     end-call.
     IF TCL-RESULT NOT = 0
      DISPLAY "TCLEVAL returned: " TCL-RESULT
      MOVE "ERROR TCL calling VFIELDEDITS" TO END-MSG
     else
      MOVE LOW-VALUES TO TCL-VARNAME
      STRING "VPLUSBUFFER" DELIMITED BY SIZE INTO TCL-VARNAME end-string
      MOVE SPACES TO LAST-BUFFER
      CALL "TCLGETVAR" USING BY REFERENCE MY-TCL
                             BY REFERENCE TCL-VARNAME
                             BY REFERENCE TCL-NEW-BUFFER
      end-call
      IF TCL-NEW-BUFFER(1:10) = "can't read"
       Move Spaces To TCL-NEW-BUFFER
      Else
       PERFORM VARYING STRIDX FROM 1 BY 1 UNTIL STRIDX > AF-AppBuf-Len

        IF (MM-CD-DATABUFFER(STRIDX:1) NOT =
            TCL-NEW-BUFFER(STRIDX:1))
            MOVE TCL-NEW-BUFFER(STRIDX:1) TO TCL-BYTE-VALUE

            IF VALID-BYTE
               MOVE TCL-NEW-BUFFER(STRIDX:1) TO MM-CD-DATABUFFER(STRIDX:1)
            ELSE
               MOVE " " TO MM-CD-DATABUFFER(STRIDX:1)
            END-IF
        END-IF

       END-PERFORM
      END-IF.

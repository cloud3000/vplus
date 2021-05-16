>>source format free.
*>*****************************************************************
*>                       V G E T B U F F E R                      *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETBUFFER.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  disp-err          pic ----9.
 01  MYLEN             PIC S9(4) COMP-5.
 01  MYBUFLEN          PIC S9(4) COMP-5.
 01  GLOBAL-ADDRESS1   USAGE POINTER.
 01  DebugBUF          pic x(256)  VALUE SPACES.

*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01  FORM-FILE PIC X(36).
    COPY "vplus/VPLUSMEM".
 01  BUFFER.
     02 BUFSIZ PIC X OCCURS 1 TO 2000 DEPENDING ON BUFFER-LEN.
 01  BUFFER-LEN PIC S9(4) COMP-5.
*>########################################################
 PROCEDURE DIVISION USING COMAREA BUFFER, BUFFER-LEN.
 0000-BEGIN-VGETBUFFER.

     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES TO INTR-CALLNAME
     string "VGETBUFFER "
      delimited by " " into INTR-CALLNAME.

     Compute MYBUFLEN = function length (Buffer).

     If  Dbuflen < 1
      Go To VGETBUFFER-RETURN.

     If Dbuflen > 2000
      Move -2 To cstatus
      Go To VGETBUFFER-RETURN.

     If Buffer-Len < 1
      Move -3 To cstatus
      Go To VGETBUFFER-RETURN.

     If Buffer-Len > Dbuflen
      Move Dbuflen To mylen
     Else
      Move Buffer-Len To mylen.

     If mylen > MYBUFLEN
      Move MYBUFLEN To mylen.

     Move MM-CD-DATABUFFER(1:mylen) To Buffer.
*>-----------------------------------------------------------
 VGETBUFFER-RETURN.
     Goback.

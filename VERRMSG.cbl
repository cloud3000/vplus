>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                        V E R R M S G                           *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VERRMSG.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
  COPY "vplus/SP250".
 01 disp-err pic ----9.
 01 MYMGS      PIC X(80).
 01 MYbuffer      PIC X(180).
 01 REALBUFLEN    PIC S9(4) COMP-5.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
 01 buffer      PIC X(80).
 01 buflen      PIC S9(4) COMP-5.
 01 actualen    PIC S9(4) COMP-5.
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA buffer buflen actualen.
 0000-BEGIN-VERRMSG.
*>-------------- INITIALIZATION AND PARM-EDITS.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES TO INTR-CALLNAME
     string "VERRMSG "
      delimited by " " into INTR-CALLNAME.

     move LOW-VALUES to MYbuffer.
     string "VERRMSG: " CFNAME DELIMITED BY size into MYbuffer.
     CALL "LogDebug" USING MYbuffer.

     MOVE SPACES      TO BUFFER.
     MOVE SPACES      TO MYBUFFER.
     MOVE 0           TO actualen.

     IF AF-AppFld-Cnt < 1 AND INTR-ERRNUM = 0
      GO TO END-OF-SUB.

     IF INTR-ERRNUM > 0
      MOVE SPACES TO INTR-ERRMSG
      GO TO RETRIVE-MSG.

     IF NUMERRS < 1
      GO TO END-OF-SUB.

     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt OR HFLD-ERRFLG(IFLD-IDX) > 0
      CONTINUE
     END-PERFORM.
     MOVE HFLD-ERRFLG(IFLD-IDX) TO INTR-ERRNUM.
     MOVE HFLD-ERRMSG(IFLD-IDX) TO INTR-ERRMSG.
*>----------------------------------------------------------------
 RETRIVE-MSG.
     IF  INTR-ERRMSG > SPACES
      MOVE INTR-ERRMSG TO MYBUFFER
      GO TO TRIM-MSG.

     IF INTR-ERRNUM > 0
      CALL "VGETERRMSG" USING COMAREA
      MOVE INTR-ERRMSG TO MYBUFFER.
*>----------------------------------------------------------------
 TRIM-MSG.
     IF MYBUFFER > SPACES
      PERFORM VARYING actualen FROM 180 BY -1
        UNTIL actualen < 1 OR MYBUFFER(actualen:1) > SPACES
       CONTINUE
      END-PERFORM.
*>----------------------------------------------------------------
 END-OF-SUB.
     COMPUTE REALBUFLEN = FUNCTION LENGTH(BUFFER).
     IF BUFLEN > REALBUFLEN
      MOVE REALBUFLEN TO BUFLEN.
     IF ACTUALEN > BUFLEN
      MOVE MYBUFFER(1:BUFLEN) TO BUFFER
     ELSE
      IF ACTUALEN > 1
       MOVE MYBUFFER(1:ACTUALEN) TO BUFFER.
     move LOW-VALUES to MYMGS.
     string "end buf=" buffer(1:30) DELIMITED BY size into MYMGS.
     Goback.

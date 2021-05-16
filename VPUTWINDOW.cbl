>>source format free.
*>******************************************************************
*>*                      V P U T W I N D O W                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VPUTWINDOW is INITIAL.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  PutBufEscCnt      PIC S9(4) COMP-5 Value 0.
 01  PutBufNumCnt      PIC S9(4) COMP-5 Value 0.
 01  PutBufAlphaCnt    PIC S9(4) COMP-5 Value 0.
 01  PutBufSpaceCnt    PIC S9(4) COMP-5 Value 0.
 01  BUFIDX   PIC S9(4) COMP-5 Value 0.
 01  MYLEN    PIC S9(4) COMP-5 Value 0.
 01  MYBUFLEN PIC S9(4) COMP-5 Value 0.
 01  MYBUF    PIC X(102) VALUE SPACES.
 01  DebugBUF    PIC X(256) VALUE SPACES.
 01  CharTest Pic X Value Space.
 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 BUFFER.
    05 BUFFERARRAY PIC X OCCURS 1 TO 100 DEPENDING ON BUFLEN.
 01 BUFLEN      PIC S9(4) COMP-5.
*>*########################################################

 PROCEDURE DIVISION USING COMAREA Buffer Buflen.
 0000-VPUTWINDOW.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES to DebugBUF.
     string "VPUTWINDOW: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     If Not INITFORM-CALLED
        move LOW-VALUES to DebugBUF
        String "VPUTWINDOW Warning: "
           "VINITFORM was not called since the last VGETNEXTFORM"
           Delimited by size into DebugBUF
        End-String
        MOVE 1         TO INTR-ERRNUM
        Move DebugBUF  TO INTR-ERRMSG
        CALL "LogDebug" USING DebugBUF
     End-If.

     COMPUTE MYBUFLEN = FUNCTION LENGTH (BUFFER).
     MOVE BUFLEN TO MYLEN.
     MOVE MYBUFLEN TO BUFLEN.
     MOVE BUFFER(1:MYBUFLEN) TO MYBUF.

     PERFORM VARYING BUFIDX FROM 1 BY 1 UNTIL (BUFIDX >= MYBUFLEN)

        Move mybuf(BUFIDX:1) To CharTest
        If CharTest is Numeric
           Add 1 To PutBufNumCnt
        End-If
        If CharTest is ALPHABETIC
           Add 1 To PutBufAlphaCnt
        End-If
        If CharTest = " "
           Add 1 To PutBufSpaceCnt
        End-If
        If CharTest = X"1B"
           Add 1 To PutBufEscCnt
        End-If

     End-Perform.

     If PutBufEscCnt = 0 and PutBufNumCnt = 4
         and PutBufAlphaCnt = PutBufSpaceCnt
        move LOW-VALUES to DebugBUF
        String "VPUTWINDOW Warning: "
           "Extranious LDEV found in VPUTWINDOW Buffer, IGNORED"
           Delimited by size into DebugBUF
        End-String
        CALL "LogDebug" USING DebugBUF
        Move Spaces To MYBUF
        Go To End-VPUTWINDOW.

     MOVE SPACES TO PUTWINDOW-BUFFER.

     IF MYBUF NOT = SPACES
        MOVE MYBUF TO PUTWINDOW-BUFFER.

 End-VPUTWINDOW.
     GOBACK.

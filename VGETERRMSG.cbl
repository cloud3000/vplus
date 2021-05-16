>>source format free.
*>*****************************************************************
*>                      V G E T E R R M S G                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETERRMSG.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.

     SELECT TEXT-FILE
        ASSIGN TO FILENAME
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS TXT-STATUS1.


 DATA DIVISION.
 FILE SECTION.
*>
*>--------- F I L E   D A T A   R E C O R D
 FD  TEXT-FILE.
 01  TEXT-RECORD.
     02 TR-NUM          PIC X(4).
     02 TR-FILLER       PIC X.
     02 TR-RECORD       PIC X(128).
 WORKING-STORAGE SECTION.
 01 FILE-ERR-MSG PIC X(80) VALUE SPACES.

   COPY "vplus/SP250".
 01  TXT-STATUS1            PIC XX.
 01  TXT-SW             PIC X     VALUE "0".
     88 TXT-CLOSED  VALUE "0".
     88 TXT-OPEN    VALUE "1".
     88 TXT-EOF     VALUE "4".
     88 TXT-ERROR   VALUE "9".
 01 TXCNT PIC S9(9) COMP-5 VALUE 0.
 01 disp-err pic ----9.
 01 FILENAME        PIC X(64) VALUE "VERRMSGS.DAT".
 01 FILEACCESS-MODE pic S9(9) COMP-5.
 01 FILEDENY-MODE   pic S9(9) COMP-5.
 01 FILEDEVICE      pic S9(9) COMP-5.
 01 FILEHANDLE      pic S9(9) COMP-5.
 01 FILEOFFSET      pic S9(9) COMP-5.
 01 FILECOUNT       pic S9(9) COMP-5.
 01 FILEFLAGS       pic S9(9) COMP-5.
 01 FILEBUF         pic x(256) VALUE SPACES.

 01 FILESIZE PIC S9(9) COMP-5 VALUE 0.
 01 RECCNT   PIC S9(9) COMP-5 VALUE 0.
*> INTR-ERRMSG
 01 INTRCNT   PIC S9(4) COMP-5 VALUE 0.
 01 TXTIDX    PIC S9(4) COMP-5 VALUE 0.
 01 STRPTR    PIC S9(4) COMP-5 VALUE 0.
 01 VERRNUM   PIC S9(4) COMP-5 VALUE 0.
 01 VERR-TEXT PIC X(150) VALUE SPACES.
 01 MSGPART1  PIC X(80) VALUE SPACES.
 01 MSGPART2  PIC X(80) VALUE SPACES.

 01  NUM1.
     02  NUMX1                PIC X  OCCURS 14 TIMES.

 01  NUM2.
     02  NUMX2                PIC X  OCCURS 14 TIMES.
 01  FILLER REDEFINES NUM2.
     02  NUM2N            PIC S9(14).

 01  NUMDEC                 PIC 9(4) COMP-5.
 01  NUMERR                 PIC 9(4) COMP-5.

 01 MYRECORD.
    02 MYIDX        PIC S9(9) COMP-5 VALUE 0.
    02 MYPTR        PIC S9(9) COMP-5 VALUE 0.
    02 MYCNT        PIC S9(9) COMP-5 VALUE 0.
    02 MYSIZE       PIC S9(9) COMP-5 VALUE 0.
    02 MYOFFSET     PIC S9(9) COMP-5 VALUE 0.
    02 MYREC        PIC X(256) VALUE SPACES.
 01 COMANDLINE-INPUT.
    02 CMD1 PIC X(32) VALUE SPACES.
    02 CMD2 PIC X(14) VALUE SPACES.
    02 CMD3 PIC X(2)  VALUE SPACES.
 01 CMDVAR  PIC X(80) VALUE SPACES.
 01 MYCALL  PIC X(32) VALUE SPACES.
 01 MYSET        PIC X(6) VALUE SPACES.
 01 MYINTR       PIC S9(9) COMP-5 VALUE 0.
 01 MYERRNUM     PIC S9(9) COMP-5 VALUE 0.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 GLOBAL-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
 0000-BEGIN-VGETERRMSG.
*>-------------- INITIALIZATION AND PARM-EDITS.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.
     move LOW-VALUES TO INTR-CALLNAME
     string "VGETERRMSG "
      delimited by "  " into INTR-CALLNAME.

     move LOW-VALUES to DebugBUF.
     string "VGETERRMSG: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     MOVE INTR-ERRNAME TO CMD1.
     MOVE LOW-VALUE TO FILENAME.
     String "/volume1/panels/VERRMSGS.DAT"
      DELIMITED BY SIZE INTO FILENAME.
     open INPUT text-file.
      if TXT-STATUS1 not = "00"
       PERFORM GET-FILE-ERR
       MOVE 28    TO INTR-ERRLEN
       STRING " VERRMSGS.DAT "  FILE-ERR-MSG DELIMITED BY SIZE INTO INTR-ERRMSG END-STRING
       GO TO END-OF-SUB.
     MOVE "00" TO TXT-STATUS1.
     MOVE "0"  TO TXT-SW.
     MOVE 0    TO TXCNT.
     PERFORM VARYING TXCNT FROM 1 BY 1
        UNTIL TXT-EOF OR TXCNT > 256
      INITIALIZE TEXT-RECORD
      READ TEXT-FILE
       AT END
        MOVE "4" TO TXT-SW
       NOT AT END
        MOVE TR-NUM TO NUM1
        INITIALIZE NUM2 NUMDEC NUMERR
        CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR
        MOVE NUM2N TO MYERRNUM
        IF INTR-ERRNUM = MYERRNUM
         MOVE "4" TO TXT-SW
         MOVE 0 TO MYSIZE
         PERFORM VARYING STRPTR FROM 128 BY -1
           UNTIL STRPTR < 2 OR MYSIZE > 0
          IF TR-RECORD(STRPTR:1) > " "
           MOVE STRPTR TO MYSIZE
          END-IF
         END-PERFORM
        END-IF
      END-READ
     END-PERFORM.
     MOVE MYSIZE    TO INTR-ERRLEN.
     MOVE TR-RECORD TO INTR-ERRMSG.
     DISPLAY "INTR-ERRLEN " INTR-ERRLEN.
     DISPLAY "INTR-ERRMSG [" INTR-ERRMSG(1:79) "]".
     CLOSE TEXT-FILE.
     GO TO END-OF-SUB.
*>--------------------------------------------------------------
 END-OF-SUB.
     Goback.
*>--------------------------------------------------------------
 Insert-Decimal.
     Move 1 To StrPtr.
     Move TR-RECORD(1:TxtIdx) To MsgPart1.
     Add 9 To TxtIdx.
     Move TR-RECORD(TxtIdx:) To MsgPart2.
*>     String  MsgPart1 Delimited By Size into Intr-ErrMsg
*>      With Pointer StrPtr.
*>     String Intr-Parm Delimited by Space
*>            " decimal" Delimited by size
*>            MsgPart2 Delimited by Size
*>      Into Intr-ErrMsg
*>      With Pointer StrPtr.
*>     Move Intr-ErrMsg To TR-RECORD.
*>====================================================================
 GET-FILE-ERR.
     MOVE SPACES TO FILE-ERR-MSG.
     EVALUATE TXT-STATUS1
      WHEN 00 MOVE 'SUCCESS ' TO FILE-ERR-MSG
      WHEN 02 MOVE 'SUCCESS DUPLICATE ' TO FILE-ERR-MSG
      WHEN 04 MOVE 'SUCCESS INCOMPLETE ' TO FILE-ERR-MSG
      WHEN 05 MOVE 'SUCCESS OPTIONAL ' TO FILE-ERR-MSG
      WHEN 07 MOVE 'SUCCESS NO UNIT ' TO FILE-ERR-MSG
      WHEN 10 MOVE 'END OF FILE ' TO FILE-ERR-MSG
      WHEN 14 MOVE 'OUT OF KEY RANGE ' TO FILE-ERR-MSG
      WHEN 21 MOVE 'KEY INVALID ' TO FILE-ERR-MSG
      WHEN 22 MOVE 'KEY EXISTS ' TO FILE-ERR-MSG
      WHEN 23 MOVE 'KEY NOT EXISTS ' TO FILE-ERR-MSG
      WHEN 30 MOVE 'PERMANENT ERROR ' TO FILE-ERR-MSG
      WHEN 31 MOVE 'INCONSISTENT FILENAME ' TO FILE-ERR-MSG
      WHEN 34 MOVE 'BOUNDARY VIOLATION ' TO FILE-ERR-MSG
      WHEN 35 MOVE 'FILE NOT FOUND ' TO FILE-ERR-MSG
      WHEN 37 MOVE 'PERMISSION DENIED ' TO FILE-ERR-MSG
      WHEN 38 MOVE 'CLOSED WITH LOCK ' TO FILE-ERR-MSG
      WHEN 39 MOVE 'CONFLICT ATTRIBUTE ' TO FILE-ERR-MSG
      WHEN 41 MOVE 'ALREADY OPEN ' TO FILE-ERR-MSG
      WHEN 42 MOVE 'NOT OPEN ' TO FILE-ERR-MSG
      WHEN 43 MOVE 'READ NOT DONE ' TO FILE-ERR-MSG
      WHEN 44 MOVE 'RECORD OVERFLOW ' TO FILE-ERR-MSG
      WHEN 46 MOVE 'READ ERROR ' TO FILE-ERR-MSG
      WHEN 47 MOVE 'INPUT DENIED ' TO FILE-ERR-MSG
      WHEN 48 MOVE 'OUTPUT DENIED ' TO FILE-ERR-MSG
      WHEN 49 MOVE 'I/O DENIED ' TO FILE-ERR-MSG
      WHEN 51 MOVE 'RECORD LOCKED ' TO FILE-ERR-MSG
      WHEN 52 MOVE 'END-OF-PAGE ' TO FILE-ERR-MSG
      WHEN 57 MOVE 'I/O LINAGE ' TO FILE-ERR-MSG
      WHEN 61 MOVE 'FILE SHARING FAILURE ' TO FILE-ERR-MSG
      WHEN 91 MOVE 'FILE NOT AVAILABLE ' TO FILE-ERR-MSG
     END-EVALUATE.

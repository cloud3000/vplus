>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                    V F I E L D E D I T S                       *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VFIELDEDITS.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 NOOP-SW      PIC X     VALUE "0".
    88 NOOP    VALUE "1".
   COPY "vplus/SP250".
   COPY "vplus/TCLDBMEM".
 01 TCL-NEW-BUFFER PIC X(2048).
 01 TCL-BYTE-VALUE PIC X.
    88 VALID-BYTE VALUES ARE X"20" THROUGH X"7E"
                             X"80" through X"FE".

*>******************************

 01 MONTH-TABLE.
    05 MONTH-LIST.
       10 FILLER PIC X(10) VALUE "JANUARY   ".
       10 FILLER PIC X(10) VALUE "FEBUARY   ".
       10 FILLER PIC X(10) VALUE "MARCH     ".
       10 FILLER PIC X(10) VALUE "APRIL     ".
       10 FILLER PIC X(10) VALUE "MAY       ".
       10 FILLER PIC X(10) VALUE "JUNE      ".
       10 FILLER PIC X(10) VALUE "JULY      ".
       10 FILLER PIC X(10) VALUE "AUGUST    ".
       10 FILLER PIC X(10) VALUE "SEPTEMBER ".
       10 FILLER PIC X(10) VALUE "OCTOBER   ".
       10 FILLER PIC X(10) VALUE "NOVEMBER  ".
       10 FILLER PIC X(10) VALUE "DECEMBER  ".
     05 MONTH-ARRAY REDEFINES MONTH-LIST.
       10 MONTH-ENTRYS OCCURS 12 TIMES.
          15 MO PIC X(10).

 01 CALDATE.
    02 CAL-YEAR PIC 9(4) VALUE ZERO.
    02 CAL-MO   PIC 99 VALUE ZERO.
    02 CAL-DA   PIC 99 VALUE ZERO.
 01 NUMDATE     PIC S9(9) COMP-5 VALUE 0.
 01 DNUM        PIC S9(4) COMP-5 VALUE 0.
 01 disp-err     pic ----9.
 01 disp-err2     pic ----9.
 01 MY-DATA-TYPE PIC X(4)  VALUE SPACES.
 01 MYERR        PIC X(80) VALUE SPACES.
 01 MYOFFSET     PIC S9(4) COMP-5 VALUE 0.
 01 MYLENGTH     PIC S9(4) COMP-5 VALUE 0.
 01 MYDATA       PIC X(80) VALUE SPACES.

 01 ERROR-SW       PIC X     VALUE "0".
    88 ERRORS      VALUE "1".
 01 SPACES-SW      PIC X     VALUE "0".
    88 IN-SPACE    VALUE "1".

 01 PIC9COMP     PIC S9(4) COMP-5 VALUE 0.
 01 NUMEDIT.
   05 ETNUMER PIC X(20) VALUE SPACES.
   05 PICZZ0 REDEFINES ETNUMER.
      10 PICZZV0 PIC ------------------9.
      10 FILLER  PIC X(1).
   05 PICZZ1 REDEFINES ETNUMER.
      10 PICZZV1 PIC ----------------9.9.
      10 FILLER  PIC X(1).
   05 PICZZ2 REDEFINES ETNUMER.
      10 PICZZV2 PIC ---------------9.99.
      10 FILLER  PIC X(1).
   05 PICZZ3 REDEFINES ETNUMER.
      10 PICZZV3 PIC --------------9.999.
      10 FILLER  PIC X(1).
   05 PICZZ4 REDEFINES ETNUMER.
      10 PICZZV4 PIC -------------9.9999.
      10 FILLER  PIC X(1).
   05 PICZZ5 REDEFINES ETNUMER.
      10 PICZZV5 PIC ------------9.99999.
      10 FILLER  PIC X(1).
   05 PICZZ6 REDEFINES ETNUMER.
      10 PICZZV6 PIC -----------9.999999.
      10 FILLER  PIC X(1).
   05 PICZZ7 REDEFINES ETNUMER.
      10 PICZZV7 PIC ----------9.9999999.
      10 FILLER  PIC X(1).
   05 PICZZ8 REDEFINES ETNUMER.
      10 PICZZV8 PIC ---------9.99999999.
      10 FILLER  PIC X(1).
   05 PICZZ9 REDEFINES ETNUMER.
      10 PICZZV9 PIC --------9.999999999.
      10 FILLER  PIC X(1).
   05 PIC990 REDEFINES ETNUMER.
      10 PIC99V0 PIC S999999999999999999.
      10 FILLER  PIC X(1).
   05 PIC991 REDEFINES ETNUMER.
      10 PIC99V1 PIC S99999999999999999V9.
      10 FILLER  PIC X(1).
   05 PIC992 REDEFINES ETNUMER.
      10 PIC99V2 PIC S9999999999999999V99.
      10 FILLER  PIC X(1).
   05 PIC993 REDEFINES ETNUMER.
      10 PIC99V3 PIC S999999999999999V999.
      10 FILLER  PIC X(1).
   05 PIC994 REDEFINES ETNUMER.
      10 PIC99V4 PIC S99999999999999V9999.
      10 FILLER  PIC X(1).
   05 PIC995 REDEFINES ETNUMER.
      10 PIC99V5 PIC S9999999999999V99999.
      10 FILLER  PIC X(1).
   05 PIC996 REDEFINES ETNUMER.
      10 PIC99V6 PIC S999999999999V999999.
      10 FILLER  PIC X(1).
   05 PIC997 REDEFINES ETNUMER.
      10 PIC99V7 PIC S99999999999V9999999.
      10 FILLER  PIC X(1).
   05 PIC998 REDEFINES ETNUMER.
      10 PIC99V8 PIC S9999999999V99999999.
      10 FILLER  PIC X(1).
   05 PIC999 REDEFINES ETNUMER.
      10 PIC99V9 PIC S999999999V999999999.
      10 FILLER  PIC X(1).



 01 THISLEN      PIC S9(4) COMP-5 VALUE 0.
 01 THISCHAR     PIC X VALUE " ".
 01 MOIDX        PIC S9(4) COMP-5 VALUE 0.
 01 SPACE-CNT    PIC S9(4) COMP-5 VALUE 0.
 01 INSPECTION   PIC S9(4) COMP-5 VALUE 0.
 01 PIC9         PIC 9     VALUE ZERO.
 01 MD-MONTH     PIC X(10) VALUE SPACES.
 01 MD-DAY       PIC X(4)  VALUE SPACES.
 01 MD-YEAR      PIC X(4)  VALUE SPACES.
 01 MYMONTH      PIC 99    VALUE ZERO.
 01 MYDAY        PIC 99    VALUE ZERO.
 01 MYYEAR       PIC 9(4)  VALUE ZERO.
 01 MYSTRING     PIC X(80) VALUE SPACES.
 01 TCL-DISP-STR pic x(4096) VALUE LOW-VALUES.
 01 TCL-EVAL-STR pic x(4096) VALUE LOW-VALUES.
 01 TCL-VARVALUE pic x(80)   VALUE SPACES.
 01 TCL-BUFVALUE pic x(2048) VALUE SPACES.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01 MYSTR-LEN    PIC S9(4) COMP-5 VALUE 0.
 01 MYBIGNUM     PIC S9(12)V9(6) COMP-5 VALUE 0.
 01 STRIDX       PIC S9(4) COMP-5 VALUE 0.
 01 STRCNT       PIC S9(4) COMP-5 VALUE 0.
 01 ED-TALLY     PIC S9(4) COMP-5 VALUE 0.

 01  NUM1.
     02  NUMX1                PIC X  OCCURS 14 TIMES.

 01  NUM2.
     02  NUMX2                PIC X  OCCURS 14 TIMES.
 01  FILLER REDEFINES NUM2.
     02  NUM2N            PIC S9(14).

 01  NUMDEC                 PIC 9(4) COMP-5.
 01  NUMERR                 PIC 9(4) COMP-5.
 01  quotequote             Pic XX VALUE Spaces.
 01  Quote-Tracking-Table.
     02 Quote-Found         Pic X OCCURS 256 Times.

 01 set-ADDRESS1       USAGE POINTER.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 My-Tcl             USAGE POINTER.
*>########################################################
 PROCEDURE DIVISION USING COMAREA.
*>---------------------------------------------------------*
 0000-BEGIN-VFIELDEDITS.
     CALL "GetPtrVplusBlock1" USING set-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO set-ADDRESS1.
     SET ADDRESS OF My-Tcl TO ADDRESS of TCL of VPLUSMEM.
     move LOW-VALUES TO INTR-CALLNAME
     string "VFIELDEDITS "
      delimited by " " into INTR-CALLNAME.
     move LOW-VALUES to DebugBUF.
     string "VFIELDEDITS: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     If Not INITFORM-CALLED
        move LOW-VALUES to DebugBUF
        String "VFIELDEDITS Warning: "
           "VINITFORM was not called since the last VGETNEXTFORM"
           Delimited by size into DebugBUF
        End-String
        MOVE 1         TO INTR-ERRNUM
        Move DebugBUF  TO INTR-ERRMSG
        CALL "LogDebug" USING DebugBUF
     End-If.

     MOVE "0" TO NOOP-SW.
     Move quote To quotequote(1:1).
     Move quote To quotequote(2:1).
     Move all "0" To Quote-Tracking-Table.

     MOVE 0 TO NUMERRS.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1  UNTIL IFLD-IDX > AF-AppFld-Cnt
        move LOW-VALUES to END-MSG
        string  " EDITING HFLD-NAME [" DELIMITED BY SIZE
               HFLD-NAME(IFLD-IDX) delimited by " "
               "]" DELIMITED BY SIZE
           into END-MSG END-STRING
*       CALL "LogDebug" USING END-MSG END-CALL
        IF HFLD-ERRFLG(IFLD-IDX) = -1
           MOVE HFLD-DTYPE(IFLD-IDX) TO MY-DATA-TYPE
           PERFORM Before-Tcl-Edits THRU Before-Tcl-Edits-EXIT
      END-IF
     END-PERFORM.
*>---------------------------------------------------------*
 Tcl-Processing.
     MOVE 0 TO NUMERRS.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
      IF HFLD-ERRFLG(IFLD-IDX) > 0
       ADD 1 TO NUMERRS
      END-IF
     END-PERFORM.

     IF NUMERRS = 0
      IF NOT NOOP
       PERFORM TCL-FIELDEDITS
       PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
        IF HFLD-ERRFLG(IFLD-IDX) > 0
         ADD 1 TO NUMERRS
         move 222 to INTR-ERRNUM
         CALL "VGETERRMSG" USING COMAREA
         If HFLD-ERRMSG(IFLD-IDX) > SPACES
          move HFLD-ERRMSG(IFLD-IDX) to INTR-ERRMSG
         ELSE
          MOVE INTR-ERRMSG to HFLD-ERRMSG(IFLD-IDX)
         END-IF
         move LOW-VALUES to END-MSG
         string " FIELD err: " delimited by SIZE
                HFLD-NAME (IFLD-IDX) delimited by " "
                ": " delimited by SIZE
                HFLD-ERRMSG(IFLD-IDX) delimited by " "
                into END-MSG
         END-STRING
*        CALL "LogDebug" USING END-MSG END-CALL
        END-IF
       END-PERFORM
      END-IF
     END-IF.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1  UNTIL IFLD-IDX > AF-AppFld-Cnt
        PERFORM After-Tcl-Edits
     END-PERFORM.

     move NUMERRS to disp-err.
     move LOW-VALUES to END-MSG.
     string Cfname delimited by " "
        "NUMERRS set to " disp-err DELIMITED by SIZE
        into END-MSG.
*    CALL "LogDebug" USING END-MSG.
     MOVE MM-CD-DATABUFFER(1:AF-AppBuf-Len) TO SVT-BUFFER.
     MOVE MM-CD-DATABUFFER(1:AF-AppBuf-Len) TO LAST-BUFFER.
     Goback.

*>---------------------------------------------------------*
 Before-Tcl-Edits.
     MOVE SPACES TO MYERR.
     MOVE "0" TO ERROR-SW.
     MOVE HFLD-BUFSTART(IFLD-IDX) TO MYOFFSET.
     MOVE HFLD-LENGTH(IFLD-IDX)   TO MYLENGTH.
     MOVE MM-CD-DATABUFFER(MYOFFSET:MYLENGTH) TO MYDATA.
     move LOW-VALUES to END-MSG.

     IF MYDATA = SPACES AND HFLD-TYPE(IFLD-IDX) = "R "
        move LOW-VALUES to END-MSG
        string " 202 REQUIRED FLD EMPTY" delimited by SIZE
           into END-MSG
        END-STRING
        MOVE 202 TO INTR-ERRNUM
        MOVE "1" TO ERROR-SW.

     IF MYDATA > SPACES OR HFLD-TYPE(IFLD-IDX) = "P "
      EVALUATE MY-DATA-TYPE(1:3)
       WHEN "CHA" PERFORM CHAR-EDIT
       WHEN "DIG"
        PERFORM DIG-EDIT THRU DIG-EDIT-EXIT
       WHEN "NUM"
        PERFORM NUM-EDIT THRU NUM-EDIT-EXIT
       WHEN "IMP"
        PERFORM IMP-EDIT THRU IMP-EDIT-EXIT
       WHEN "MDY"
        PERFORM MDY-EDIT THRU MDY-EXIT
       WHEN "YMD"
        PERFORM YMD-EDIT THRU YMD-EXIT
       WHEN "DMY"
        PERFORM DMY-EDIT THRU DMY-EXIT
       WHEN OTHER
        MOVE 211 TO INTR-ERRNUM
        MOVE "1" TO ERROR-SW
      END-EVALUATE.
     IF ERRORS
       PERFORM ADD-TO-ERRTBL
     ELSE
       PERFORM REMOVE-FROM-ERRTBL.
 Before-Tcl-Edits-EXIT.
     EXIT.

*>---------------------------------------------------------*
 After-Tcl-Edits.
     MOVE HFLD-BUFSTART(IFLD-IDX) TO MYOFFSET.
     MOVE HFLD-LENGTH(IFLD-IDX)   TO MYLENGTH.
*>---------------------------------------------------------*
 CHAR-EDIT.
     MOVE MYDATA TO MYSTRING.
     PERFORM GET-STRING-LEN.
     INITIALIZE MYDATA STRIDX STRCNT
     PERFORM VARYING STRIDX FROM 1 BY 1
       UNTIL  ERRORS OR STRIDX > MYSTR-LEN
      IF MYSTRING(STRIDX:1) < SPACES
       MOVE "1" TO ERROR-SW
       MOVE "ILLEGAL ASCII CHARACTER" TO MYERR
      END-IF
     END-PERFORM.
*>---------------------------------------------------------*
 DIG-EDIT.
     INITIALIZE NUM2 NUMDEC NUMERR.
     MOVE MYDATA TO NUM1.
     CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR.
     IF NUMERR NOT = 0 OR NUMDEC NOT = 0
      MOVE "1" TO ERROR-SW
      MOVE 204 TO INTR-ERRNUM
      GO TO DIG-EDIT-EXIT.
     IF NUM2N < 0
      MOVE "1" TO ERROR-SW
      MOVE 204 TO INTR-ERRNUM
      GO TO DIG-EDIT-EXIT.
     COMPUTE THISLEN = FUNCTION LENGTH(ETNUMER)
     MOVE NUM2N TO MYBIGNUM.

     MOVE MYBIGNUM TO PICZZV0.
     MOVE ETNUMER TO MYDATA.

     CALL "VJUSTLEFT" USING MYDATA THISLEN.
     MOVE "0" TO THISCHAR.
     CALL "VSTRIPLEADING" USING THISCHAR MYDATA THISLEN.

     CALL "VJUSTLEFT" USING MYDATA THISLEN.

     CALL "VJUSTRIGHT" USING MYDATA MYLENGTH.

     MOVE "0" TO THISCHAR.
     CALL "VFILLLEADING" USING THISCHAR MYDATA THISLEN.

 DIG-EDIT-EXIT.
     EXIT.
*>---------------------------------------------------------*
 NUM-EDIT.
     IF (MY-DATA-TYPE(4:1) = " ") OR
        (MY-DATA-TYPE(4:1) IS NOT NUMERIC)
      MOVE 0 TO PIC9
     ELSE
      MOVE MY-DATA-TYPE(4:1) TO PIC9.
     MOVE PIC9 TO PIC9COMP.
     MOVE MYDATA TO NUM1.
     CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR.
     IF NUMERR NOT = 0
      MOVE "1" TO ERROR-SW
      MOVE 205 TO INTR-ERRNUM
      GO TO NUM-EDIT-EXIT.
     IF NUMDEC > PIC9COMP
      MOVE "1" TO ERROR-SW
      MOVE 206 TO INTR-ERRNUM
      GO TO NUM-EDIT-EXIT.
     IF NUMDEC > 0
      COMPUTE MYBIGNUM = NUM2N / 10 ** NUMDEC
     ELSE
      MOVE NUM2N TO MYBIGNUM.
     EVALUATE PIC9COMP
        WHEN 0
         MOVE MYBIGNUM TO PICZZV0
         INSPECT PICZZV0 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 1
         MOVE MYBIGNUM TO PICZZV1
         INSPECT PICZZV1 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 2
         MOVE MYBIGNUM TO PICZZV2
         INSPECT PICZZV2 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 3
         MOVE MYBIGNUM TO PICZZV3
         INSPECT PICZZV3 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 4
         MOVE MYBIGNUM TO PICZZV4
         INSPECT PICZZV4 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 5
         MOVE MYBIGNUM TO PICZZV5
         INSPECT PICZZV5 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 6
         MOVE MYBIGNUM TO PICZZV6
         INSPECT PICZZV6 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 7
         MOVE MYBIGNUM TO PICZZV7
         INSPECT PICZZV7 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 8
         MOVE MYBIGNUM TO PICZZV8
         INSPECT PICZZV8 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 9
         MOVE MYBIGNUM TO PICZZV9
         INSPECT PICZZV9 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
     END-EVALUATE.
     COMPUTE THISLEN = FUNCTION LENGTH(ETNUMER)
     CALL "VJUSTLEFT" USING MYDATA THISLEN.
     MOVE "0" TO THISCHAR.
     CALL "VSTRIPLEADING" USING THISCHAR MYDATA THISLEN.
     CALL "VJUSTLEFT" USING MYDATA THISLEN.
     CALL "VJUSTRIGHT" USING MYDATA MYLENGTH.
     MOVE "0" TO THISCHAR.
     CALL "VFILLLEADING" USING THISCHAR MYDATA THISLEN.
 NUM-EDIT-EXIT.
     EXIT.
*>---------------------------------------------------------*
 IMP-EDIT.
     IF (MY-DATA-TYPE(4:1) = " ") OR
        (MY-DATA-TYPE(4:1) IS NOT NUMERIC)
      MOVE 0 TO PIC9
     ELSE
      MOVE MY-DATA-TYPE(4:1) TO PIC9.
     MOVE PIC9 TO PIC9COMP.
     MOVE MYDATA TO NUM1.
     CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR.
     IF NUMERR NOT = 0
      MOVE "1" TO ERROR-SW
      MOVE 205 TO INTR-ERRNUM
      GO TO IMP-EDIT-EXIT.
     IF NUMDEC > PIC9COMP
      MOVE "1" TO ERROR-SW
      MOVE 206 TO INTR-ERRNUM
      GO TO IMP-EDIT-EXIT.
     IF NUMDEC > 0
      COMPUTE MYBIGNUM = NUM2N / 10 ** NUMDEC
     ELSE
      MOVE NUM2N TO MYBIGNUM.
     EVALUATE PIC9COMP
        WHEN 0
         MOVE MYBIGNUM TO PICZZV0
         INSPECT PICZZV0 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 1
         MOVE MYBIGNUM TO PICZZV1
         INSPECT PICZZV1 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 2
         MOVE MYBIGNUM TO PICZZV2
         INSPECT PICZZV2 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 3
         MOVE MYBIGNUM TO PICZZV3
         INSPECT PICZZV3 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 4
         MOVE MYBIGNUM TO PICZZV4
         INSPECT PICZZV4 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 5
         MOVE MYBIGNUM TO PICZZV5
         INSPECT PICZZV5 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 6
         MOVE MYBIGNUM TO PICZZV6
         INSPECT PICZZV6 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 7
         MOVE MYBIGNUM TO PICZZV7
         INSPECT PICZZV7 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 8
         MOVE MYBIGNUM TO PICZZV8
         INSPECT PICZZV8 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
        WHEN 9
         MOVE MYBIGNUM TO PICZZV9
         INSPECT PICZZV9 REPLACING ALL " " BY "0"
         MOVE ETNUMER TO MYDATA
     END-EVALUATE.
     COMPUTE THISLEN = FUNCTION LENGTH(ETNUMER)
     CALL "VJUSTLEFT" USING MYDATA THISLEN.
     MOVE "0" TO THISCHAR.
     CALL "VSTRIPLEADING" USING THISCHAR MYDATA THISLEN.
     CALL "VJUSTLEFT" USING MYDATA THISLEN.
     CALL "VJUSTRIGHT" USING MYDATA MYLENGTH.
     MOVE "." TO THISCHAR.
     CALL "VSTRIPALL" USING THISCHAR MYDATA THISLEN.
     MOVE "0" TO THISCHAR.
     CALL "VFILLLEADING" USING THISCHAR MYDATA THISLEN.
 IMP-EDIT-EXIT.
     EXIT.
*>---------------------------------------------------------*
 DMY-EDIT.
     MOVE "0" TO ERROR-SW.
     MOVE 0 TO SPACE-CNT.
     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL "/".
     IF INSPECTION > 0
       INSPECT MYDATA REPLACING ALL "/" BY " ".

     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL "-".
     IF INSPECTION > 0
      INSPECT MYDATA REPLACING ALL "-" BY " ".

     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL ",".
     IF INSPECTION > 0
      INSPECT MYDATA REPLACING ALL "," BY " ".

     CALL "VXDBLSPACE" USING MYDATA MYLENGTH SPACE-CNT.
     IF SPACE-CNT = 2
      UNSTRING MYDATA DELIMITED BY " "
       INTO MD-DAY MD-MONTH MD-YEAR
       ON OVERFLOW
       GO TO DMY-error
      END-UNSTRING
     END-IF.

     IF SPACE-CNT = 0 AND MYLENGTH = 6
      MOVE MYDATA(1:2) TO  MD-DAY
      MOVE MYDATA(3:2) TO  MD-MONTH
      MOVE MYDATA(5:2) TO  MD-YEAR.

     IF SPACE-CNT = 0 AND MYLENGTH = 8
      MOVE MYDATA(1:2) TO  MD-DAY
      MOVE MYDATA(3:2) TO  MD-MONTH
      MOVE MYDATA(5:4) TO  MD-YEAR.

     PERFORM EDIT-MD-MONTH.
     If Not Errors PERFORM EDIT-MD-DAY.
     If Not Errors PERFORM EDIT-MD-YEAR.
     If Not Errors PERFORM MAKE-MYDATE.
     If Not Errors Go To DMY-EXIT.
 DMY-error.
     MOVE "1" TO ERROR-SW.
     MOVE 208 TO INTR-ERRNUM.
 DMY-EXIT.
     EXIT.

*>---------------------------------------------------------*
 MDY-EDIT.
     MOVE "0" TO ERROR-SW.
     MOVE 0 TO SPACE-CNT.
     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL "/".
     IF INSPECTION > 0
       INSPECT MYDATA REPLACING ALL "/" BY " ".

     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL "-".
     IF INSPECTION > 0
      INSPECT MYDATA REPLACING ALL "-" BY " ".

     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL ",".
     IF INSPECTION > 0
      INSPECT MYDATA REPLACING ALL "," BY " ".

     CALL "VXDBLSPACE" USING MYDATA MYLENGTH SPACE-CNT.
     IF SPACE-CNT = 2
      UNSTRING MYDATA DELIMITED BY " "
        INTO MD-MONTH MD-DAY MD-YEAR
        ON OVERFLOW
       GO TO MDY-error
      END-UNSTRING
     END-IF.

     IF SPACE-CNT = 0 AND MYLENGTH = 6
      MOVE MYDATA(1:2) TO  MD-MONTH
      MOVE MYDATA(3:2) TO  MD-DAY
      MOVE MYDATA(5:2) TO  MD-YEAR.

     IF SPACE-CNT = 0 AND MYLENGTH = 8
      MOVE MYDATA(1:2) TO  MD-MONTH
      MOVE MYDATA(3:2) TO  MD-DAY
      MOVE MYDATA(5:4) TO  MD-YEAR.

     If Not Errors PERFORM EDIT-MD-MONTH.
     If Not Errors PERFORM EDIT-MD-DAY.
     If Not Errors PERFORM EDIT-MD-YEAR.
     If Not Errors PERFORM MAKE-MYDATE.
     If Not Errors Go To MDY-EXIT.
 MDY-error.
     MOVE "1" TO ERROR-SW.
     MOVE 209 TO INTR-ERRNUM.
 MDY-EXIT.
     EXIT.
*>---------------------------------------------------------*
 YMD-EDIT.
     MOVE "0" TO ERROR-SW.
     MOVE 0 TO SPACE-CNT.
     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL "/".
     IF INSPECTION > 0
       INSPECT MYDATA REPLACING ALL "/" BY " ".

     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL "-".
     IF INSPECTION > 0
      INSPECT MYDATA REPLACING ALL "-" BY " ".

     MOVE 0 TO INSPECTION.
     INSPECT MYDATA TALLYING INSPECTION
       FOR ALL ",".
     IF INSPECTION > 0
      INSPECT MYDATA REPLACING ALL "," BY " ".

     CALL "VXDBLSPACE" USING MYDATA MYLENGTH SPACE-CNT.
     IF SPACE-CNT = 2
     UNSTRING MYDATA DELIMITED BY " "
      INTO MD-YEAR MD-MONTH MD-DAY
      ON OVERFLOW
       GO TO YMD-error
      END-UNSTRING
     END-IF.

     IF SPACE-CNT = 0 AND MYLENGTH = 6
      MOVE MYDATA(1:2) TO  MD-YEAR.
      MOVE MYDATA(3:2) TO  MD-MONTH
      MOVE MYDATA(5:2) TO  MD-DAY

     IF SPACE-CNT = 0 AND MYLENGTH = 8
      MOVE MYDATA(1:4) TO  MD-YEAR.
      MOVE MYDATA(3:2) TO  MD-MONTH
      MOVE MYDATA(5:2) TO  MD-DAY

     PERFORM EDIT-MD-MONTH.
     If Not Errors PERFORM EDIT-MD-DAY.
     If Not Errors PERFORM EDIT-MD-YEAR.
     If Not Errors PERFORM MAKE-MYDATE.
     If Not Errors Go To YMD-EXIT.
 YMD-error.
     MOVE "1" TO ERROR-SW.
     MOVE 210 TO INTR-ERRNUM.
 YMD-EXIT.
     EXIT.
*>---------------------------------------------------------*
 EDIT-MD-MONTH.
     MOVE "0" TO ERROR-SW.
     MOVE ZERO TO MYMONTH.
     MOVE MD-MONTH TO MYSTRING.
     PERFORM GET-STRING-LEN.
     IF MYSTR-LEN < 1
       MOVE "1" TO ERROR-SW
       MOVE "INVALID MONTH" TO MYERR.

     IF MYSTR-LEN = 1 OR 2
      MOVE MYSTRING(1:2) TO NUM1
      INITIALIZE NUM2 NUMDEC NUMERR
      CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR
      IF NUMERR = 0
       MOVE NUM2N TO MYMONTH
      ELSE
       MOVE "1" TO ERROR-SW
       MOVE "INVALID MONTH" TO MYERR
      END-IF
     ELSE IF MYSTR-LEN > 2
           PERFORM VARYING MOIDX FROM 1 BY 1
             UNTIL MOIDX > 12
            IF MD-MONTH(1:MYSTR-LEN) = MO(MOIDX)(1:MYSTR-LEN)
             MOVE MOIDX TO MYMONTH
            END-IF
           END-PERFORM.

*>---------------------------------------------------------*
 EDIT-MD-DAY.
     MOVE "0" TO ERROR-SW.
     MOVE ZERO TO MYDAY.
     MOVE MD-DAY TO MYSTRING.
     PERFORM GET-STRING-LEN.
     IF MYSTR-LEN < 1
       MOVE "1" TO ERROR-SW
       MOVE "INVALID DAY" TO MYERR.

     IF MYSTR-LEN = 1 OR 2
      MOVE MYSTRING(1:2) TO NUM1
      INITIALIZE NUM2 NUMDEC NUMERR
      CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR
      IF NUMERR = 0
       MOVE NUM2N TO MYDAY
      ELSE
       MOVE "1" TO ERROR-SW
       MOVE "INVALID DAY" TO MYERR.

*>---------------------------------------------------------*
 EDIT-MD-YEAR.
     MOVE "0" TO ERROR-SW.
     MOVE ZERO TO MYYEAR.
     MOVE MD-YEAR TO MYSTRING.
     PERFORM GET-STRING-LEN.
     IF MYSTR-LEN < 1
       MOVE "1" TO ERROR-SW
       MOVE "INVALID YEAR" TO MYERR.

     IF MYSTR-LEN = 1 OR 2 OR 4
      MOVE MYSTRING(1:2) TO NUM1
      INITIALIZE NUM2 NUMDEC NUMERR
      CALL "NUMGET" USING NUM1 NUM2 NUMDEC NUMERR
      IF NUMERR = 0
       MOVE NUM2N TO MYYEAR
      ELSE
       MOVE "1" TO ERROR-SW
       MOVE "INVALID YEAR" TO MYERR
     ELSE
      MOVE "1" TO ERROR-SW
      MOVE "INVALID YEAR" TO MYERR.

     IF ERROR-SW NOT = "1"
      IF NUM2N < 100
       COMPUTE MYYEAR = NUM2N + 1900.
     IF ERROR-SW NOT = "1"
      IF NUM2N < 51
       COMPUTE MYYEAR = NUM2N + 2000.



*>---------------------------------------------------------*
 MAKE-MYDATE.
     MOVE "0" TO ERROR-SW.
     INITIALIZE CALDATE NUMDATE DNUM.
     STRING MYYEAR MYMONTH MYDAY
      DELIMITED BY SIZE INTO CALDATE.
     CALL "DAYNUM" USING CALDATE NUMDATE DNUM.
     IF DNUM = 0
      MOVE "1" TO ERROR-SW
      MOVE "INVALID DATE" TO MYERR.
*>---------------------------------------------------------*
 ADD-TO-ERRTBL.
     ADD 1 TO NUMERRS.
     MOVE INTR-ERRNUM TO HFLD-ERRFLG(IFLD-IDX).
     MOVE SPACES      TO HFLD-ERRMSG(IFLD-IDX).
     IF INTR-ERRNUM > 0
        move LOW-VALUES to END-MSG
        string " ADD-TO-ERRTBL, FIELD: " delimited by SIZE
               HFLD-NAME (IFLD-IDX) delimited by SIZE
         into END-MSG
        END-STRING
        DISPLAY END-MSG
        MOVE INTR-ERRMSG TO HFLD-ERRMSG(IFLD-IDX)
        DISPLAY "INTR-ERRNUM; " INTR-ERRNUM
      CALL "VGETERRMSG" USING COMAREA
      MOVE INTR-ERRMSG TO HFLD-ERRMSG(IFLD-IDX).
*>---------------------------------------------------------*
 REMOVE-FROM-ERRTBL.
     SUBTRACT 1 FROM NUMERRS.
     move LOW-VALUES to END-MSG.
     string " REMOVE-FROM-ERRTBL FIELD: " delimited by SIZE
      HFLD-NAME (IFLD-IDX) delimited by SIZE
       into END-MSG
     END-STRING.
     IF NUMERRS < 0
      MOVE 0 TO NUMERRS.
     MOVE 0           TO HFLD-ERRFLG(IFLD-IDX).
     MOVE SPACES      TO HFLD-ERRMSG(IFLD-IDX).
*>---------------------------------------------------------*
 GET-STRING-LEN.
     MOVE 0 TO MYSTR-LEN.
     PERFORM VARYING STRIDX FROM 80 BY -1
       UNTIL STRIDX < 1 OR MYSTR-LEN > 0
      IF MYSTRING(STRIDX:1) > " "
       MOVE STRIDX TO MYSTR-LEN
      END-IF
     END-PERFORM.
*>---------------------------------------------------------*
 NO-DBL-SPACES.
     MOVE MYDATA TO MYSTRING.
     PERFORM GET-STRING-LEN.
     INITIALIZE MYDATA STRIDX STRCNT
     PERFORM VARYING STRIDX FROM 1 BY 1
       UNTIL STRIDX > MYSTR-LEN
      IF MYSTRING(STRIDX:1) > SPACES
       MOVE "0" TO SPACES-SW
       ADD 1 TO STRCNT
       MOVE MYSTRING(STRIDX:1) TO MYDATA(STRCNT:1)
      END-IF
      IF NOT IN-SPACE
       IF MYSTRING(STRIDX:1) = " "
        MOVE "1" TO SPACES-SW
        ADD 1 TO STRCNT
        MOVE MYSTRING(STRIDX:1) TO MYDATA(STRCNT:1)
       END-IF
      END-IF
     END-PERFORM.
*>-----------------------------------------------------------
 TCL-FIELDEDITS.
     Perform PROCESSEDITSPECS.
     IF TCL-RESULT = 0
      Perform GET-RC-VALUES
      PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
       IF HFLD-ERRFLG(IFLD-IDX) NOT = 0
        PERFORM GET-CODE-MSG
       END-IF
      END-PERFORM
      MOVE LOW-VALUES TO TCL-VARNAME
      STRING "VPLUSBUFFER" x"00"
       DELIMITED BY SIZE INTO TCL-VARNAME
      end-string
      MOVE SPACES TO TCL-NEW-BUFFER
      Move x"00" to TCL-NEW-BUFFER(2048:1)
      CALL "TCLGETVAR" USING MY-TCL
                             BY REFERENCE TCL-VARNAME
                             BY REFERENCE TCL-NEW-BUFFER
      end-call
      IF TCL-NEW-BUFFER(1:10) = "can't read"
       Move Spaces To TCL-NEW-BUFFER
      Else
       PERFORM VARYING STRIDX FROM 1 BY 1
         UNTIL STRIDX > AF-AppBuf-Len
        IF (MM-CD-DATABUFFER(STRIDX:1) NOT = TCL-NEW-BUFFER(STRIDX:1))
         MOVE TCL-NEW-BUFFER(STRIDX:1) TO TCL-BYTE-VALUE
         IF VALID-BYTE
          MOVE TCL-NEW-BUFFER(STRIDX:1) TO MM-CD-DATABUFFER(STRIDX:1)
         ELSE
          MOVE " " TO MM-CD-DATABUFFER(STRIDX:1)
         END-IF
        END-IF
       END-PERFORM
      END-IF
     End-If.
*>-----------------------------------------------------------
 PROCESSEDITSPECS.

     MOVE LOW-VALUES TO TCL-VARNAME
     STRING "VPLUSBUFFER"
       DELIMITED BY SIZE INTO TCL-VARNAME
     end-string.

     MOVE LOW-VALUES TO TCL-BUFVALUE
     STRING MM-CD-DATABUFFER(1:AF-AppBuf-Len)
      DELIMITED BY SIZE INTO TCL-BUFVALUE
     END-STRING
     CALL "TCLSETVAR" USING MY-TCL TCL-VARNAME TCL-BUFVALUE.

     MOVE LOW-VALUES TO TCL-EVAL-STR.
     MOVE 0 TO TCL-RESULT.

     STRING "::viewplus::PROCESSEDITSPECS " delimited by size
            MPE-FORMS-FILE                  delimited by space
            ".vform "                       delimited by size
            HPAN-NAME                       delimited by space
            " FIELD $VPLUSBUFFER"           DELIMITED BY SIZE
            INTO TCL-EVAL-STR.

     MOVE TCL-EVAL-STR TO TCL-DISP-STR
     INSPECT TCL-DISP-STR REPLACING ALL X"00" BY " "
     CALL "LogDebug" USING TCL-EVAL-STR end-call
     CALL "TCLEVAL" USING BY REFERENCE MY-TCL
                          BY REFERENCE TCL-EVAL-STR
                          BY REFERENCE TCL-RESULT
     end-call.
     IF TCL-RESULT NOT = 0
      DISPLAY "TCLEVAL returned: " TCL-RESULT
      MOVE "ERROR TCL calling PROCESSEDITSPECS" TO END-MSG.
*>-----------------------------------------------------------
 GET-RC-VALUES.
     PERFORM VARYING IFLD-IDX FROM 1 BY 1 UNTIL IFLD-IDX > AF-AppFld-Cnt
      MOVE LOW-VALUES TO TCL-EVAL-STR
      STRING "set MYRC [dict get "  delimited by size
             "$::viewplus::VARERRORS " delimited by size
             HFLD-NAME(IFLD-IDX)       delimited by space
             " RC]"
      DELIMITED BY SIZE INTO TCL-EVAL-STR
      END-STRING
      MOVE TCL-EVAL-STR TO TCL-DISP-STR
      INSPECT TCL-DISP-STR REPLACING ALL X"00" BY " "
      CALL "TCLEVAL" USING BY REFERENCE MY-TCL
                           BY REFERENCE TCL-EVAL-STR
                           BY REFERENCE TCL-RESULT
      end-call
      MOVE LOW-VALUES TO TCL-VARNAME
      STRING "MYRC" DELIMITED BY SIZE INTO TCL-VARNAME
      MOVE SPACES TO TCL-VARVALUE
      CALL "TCLGETVAR" USING BY REFERENCE MY-TCL
                             BY REFERENCE TCL-VARNAME
                             BY REFERENCE TCL-VARVALUE
      end-call
      IF TCL-VARVALUE(1:10) = "can't read"
       Move Spaces To TCL-VARVALUE
      Else
       INSPECT TCL-VARVALUE REPLACING ALL X"00" BY " "
       IF TCL-VARVALUE(1:1) IS NUMERIC
        MOVE TCL-VARVALUE(1:1) To PIC9
        MOVE PIC9 TO HFLD-ERRFLG(IFLD-IDX)
       ELSE
        MOVE 0 TO HFLD-ERRFLG(IFLD-IDX)
       END-IF
      End-If
      MOVE LOW-VALUES TO TCL-VARNAME
      STRING "MYRC" DELIMITED BY SIZE INTO TCL-VARNAME
      MOVE LOW-VALUES TO TCL-VARVALUE
      STRING "0" DELIMITED BY SIZE INTO TCL-VARVALUE END-STRING
      CALL "TCLSETVAR" USING MY-TCL TCL-VARNAME TCL-VARVALUE END-CALL
     END-PERFORM.

*>-----------------------------------------------------------
 GET-CODE-MSG.
     MOVE LOW-VALUES TO TCL-EVAL-STR.
     STRING "set MYCODE [dict get "  delimited by size
            "$::viewplus::VARERRORS "   delimited by size
            HFLD-NAME(IFLD-IDX)         delimited by space
            " ERRORDETAIL -code]"
     DELIMITED BY SIZE INTO TCL-EVAL-STR
     END-STRING.
     MOVE TCL-EVAL-STR TO TCL-DISP-STR.
     INSPECT TCL-DISP-STR REPLACING ALL X"00" BY " ".
     CALL "TCLEVAL" USING BY REFERENCE MY-TCL
                          BY REFERENCE TCL-EVAL-STR
                          BY REFERENCE TCL-RESULT
     end-call.
     MOVE LOW-VALUES TO TCL-VARNAME.
     STRING "MYCODE" DELIMITED BY SIZE INTO TCL-VARNAME.
     MOVE SPACES TO TCL-VARVALUE.
     CALL "TCLGETVAR" USING BY REFERENCE MY-TCL
                            BY REFERENCE TCL-VARNAME
                            BY REFERENCE TCL-VARVALUE
     end-call.
     IF TCL-VARVALUE(1:10) = "can't read"
      Move Spaces To TCL-VARVALUE
     Else
      IF TCL-VARVALUE(1:1) = "1" OR TCL-VARVALUE(2:1) = "1"
       MOVE 222 TO HFLD-ERRFLG(IFLD-IDX)
      ELSE
       MOVE 0 TO HFLD-ERRFLG(IFLD-IDX)
      END-IF
     END-IF.
     MOVE LOW-VALUES TO TCL-EVAL-STR.
     STRING "set MYMSG [dict get "  delimited by size
            "$::viewplus::VARERRORS "   delimited by size
            HFLD-NAME(IFLD-IDX)         delimited by space
            " MSG]"
     DELIMITED BY SIZE INTO TCL-EVAL-STR
     END-STRING.
     MOVE TCL-EVAL-STR TO TCL-DISP-STR.
     INSPECT TCL-DISP-STR REPLACING ALL X"00" BY " ".
     CALL "TCLEVAL" USING BY REFERENCE MY-TCL
                          BY REFERENCE TCL-EVAL-STR
                          BY REFERENCE TCL-RESULT
     end-call.
     MOVE LOW-VALUES TO TCL-VARNAME.
     STRING "MYMSG" DELIMITED BY SIZE INTO TCL-VARNAME.
     MOVE SPACES TO TCL-VARVALUE.
     MOVE SPACES TO HFLD-ERRMSG(IFLD-IDX).
     CALL "TCLGETVAR" USING BY REFERENCE MY-TCL
                            BY REFERENCE TCL-VARNAME
                            BY REFERENCE TCL-VARVALUE
     end-call.
     IF TCL-VARVALUE(1:10) = "can't read"
      Move Spaces To TCL-VARVALUE
     Else
      MOVE TCL-VARVALUE to HFLD-ERRMSG(IFLD-IDX)
      INSPECT HFLD-ERRMSG(IFLD-IDX) REPLACING ALL LOW-VALUE by " "
      move 0 to ed-tally
      INSPECT HFLD-ERRMSG(IFLD-IDX) TALLYING ED-TALLY FOR ALL "no such variable"
      IF ED-TALLY > 0
       MOVE SPACES TO HFLD-ERRMSG(IFLD-IDX)
       MOVE 0 TO HFLD-ERRFLG(IFLD-IDX)
      ELSE
       move LOW-VALUES to END-MSG
       string " GOT-CODE-MSG: " delimited by SIZE
              HFLD-NAME (IFLD-IDX) delimited by " "
              ": " delimited by SIZE
              HFLD-ERRMSG(IFLD-IDX) delimited by LOW-VALUE
         into END-MSG
       END-STRING
      END-IF
     END-IF.

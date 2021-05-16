>>source format free.
*>_________________________________________________________________
*>*****************************************************************
*>                      V G E T S P E C S                         *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETSPECS.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 GLOBAL-ADDRESS1   USAGE POINTER.
 01  IDX1                 PIC S9(4) COMP-5 Value 0.
 01  IDX2                 PIC S9(4) COMP-5 Value 0.
 01  IDX3                 PIC S9(4) COMP-5 Value 0.
 01  FIELD-NUMBER         PIC S9(4) COMP-5 Value 0.
 01  X                    PIC S9(4) COMP-5 Value 0.
 01 DebugBUF     pic x(256)  VALUE SPACES.

 01  INFOBUF1.
     02  FILLER              PIC S9(4) COMP-5 VALUE 1.
     02  FILLER              PIC S9(4) COMP-5 VALUE 10.
     02  INFOBUF1-TABLE.
         03  INFOBUF1-FORM          PIC X(16).
         03  INFOBUF1-FORMNUM       PIC S9(4) COMP-5.
         03  INFOBUF1-NUM-FIELDS    PIC S9(4) COMP-5.
         03  INFOBUF1-BUF-LENGTH    PIC S9(4) COMP-5.
         03  INFOBUF1-NEXT-FORM     PIC X(16).
         03  INFOBUF1-REPEAT-OPTION PIC X.
         03  INFOBUF1-NFORM-OPTION  PIC X.

 01  INFOBUF1-LEN          PIC S9(4) COMP-5 VALUE 12.

 01  INFOBUF2.
     02  INFOBUF2-NUMOFENTRIES  PIC S9(4) COMP-5 VALUE 256.
     02  INFOBUF2-ENTRYLENGTH   PIC S9(4) COMP-5 VALUE 17.
     02  INFOBUF2-FORM          PIC X(16).
     02  INFOBUF2-TABLE OCCURS 256.
         03  INFOBUF2-FIELDNAME PIC X(16) VALUE SPACES.
         03  INFOBUF2-FIELD     PIC S9(4) COMP-5.
         03  INFOBUF2-ORDER     PIC S9(4) COMP-5 VALUE ZERO.
         03  INFOBUF2-FIELD-LEN PIC S9(4) COMP-5.
         03  INFOBUF2-OFFSET    PIC S9(4) COMP-5.
         03  INFOBUF2-ENH       PIC X(4).
         03  INFOBUF2-DATA-TYPE PIC X(4).
         03  INFOBUF2-TYPE      PIC XX.

 01  INFOBUF2-LEN           PIC S9(4) COMP-5 VALUE 27.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01  LINKBUF.
     02  LINKBUF-ENTRY OCCURS 384.
         03  LINKBUF-FIELD    PIC S9(4) COMP-5.
         03  LINKBUF-TYPE     PIC S9(4) COMP-5.
         03  LINKBUF-SPEC     PIC X(4).

 01  LINKBUF-ENTRIES       PIC S9(4) COMP-5.
*>########################################################
 PROCEDURE DIVISION USING COMAREA LINKBUF LINKBUF-ENTRIES.
 0000-BEGIN-VGETSPECS.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM To GLOBAL-ADDRESS1.

     Initialize LINKBUF LINKBUF-ENTRIES.
     Perform GET-FIELDSPEC-INFO THRU GET-FIELDSPEC-INFO-EXIT.
     Goback.

 GET-FIELDSPEC-INFO.
     MOVE SPACES TO INFOBUF1-TABLE.
     MOVE CFNAME TO INFOBUF1-FORM.
     MOVE ZERO   TO CSTATUS.
     CALL "VGETFORMINFO" USING COMAREA INFOBUF1 INFOBUF1-LEN.

     IF CSTATUS NOT = ZERO
         GO TO GET-FIELDSPEC-INFO-EXIT.

     MOVE SPACES TO INFOBUF2.

     COMPUTE INFOBUF2-LEN = FUNCTION LENGTH(INFOBUF2) / 2.

     COMPUTE INFOBUF2-ENTRYLENGTH = (FUNCTION LENGTH(INFOBUF2-TABLE(1)) / 2).

     COMPUTE INFOBUF2-NUMOFENTRIES = (((INFOBUF2-LEN * 2) - 4) / (INFOBUF2-ENTRYLENGTH * 2)).

     MOVE CFNAME TO INFOBUF2-FORM.
     MOVE ZERO TO CSTATUS.
     CALL "VGETFIELDINFO" USING COMAREA INFOBUF2 INFOBUF2-LEN.

     IF CSTATUS NOT = ZERO
        GO TO GET-FIELDSPEC-INFO-EXIT.

     PERFORM VARYING X FROM 1 BY 1 UNTIL X > INFOBUF2-NUMOFENTRIES
        Compute FIELD-NUMBER = X * -1 End-Compute
        Compute IDX1 = (x * 3) - 2    End-Compute
        Compute IDX2 = (x * 3) - 1    End-Compute
        Compute IDX3 = (x * 3)        End-Compute

        Move FIELD-NUMBER          To LINKBUF-FIELD(IDX1)
        Move 1                     TO LINKBUF-TYPE (IDX1)
        Move INFOBUF2-ENH(X)       TO LINKBUF-SPEC (IDX1)

        Move FIELD-NUMBER          To LINKBUF-FIELD(IDX2)
        Move 2                     TO LINKBUF-TYPE (IDX2)
        Move INFOBUF2-TYPE(X)      TO LINKBUF-SPEC (IDX2)

        Move FIELD-NUMBER          To LINKBUF-FIELD(IDX3)
        Move 3                     TO LINKBUF-TYPE (IDX3)
        Move INFOBUF2-DATA-TYPE(X) TO LINKBUF-SPEC (IDX3)

     END-PERFORM.

     MOVE IDX3 TO LINKBUF-ENTRIES.

  GET-FIELDSPEC-INFO-EXIT.
        EXIT.

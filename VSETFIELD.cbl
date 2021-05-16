>>source format free.
*>******************************************************************
*>*                        V S E T F I E L D                       *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSETFIELD.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Y                   PIC S9(4) COMP-5 VALUE 0.
 01 X                   PIC S9(4) COMP-5 VALUE 0.
 01 LAST-CHAR           PIC X            VALUE SPACE.
 01 NSTRING             PIC X(80)        VALUE SPACES.
 01 BEGFLD-SW           PIC X            VALUE "0".
    88 BEGIN-OF-FIELD                    VALUE "1".
 01 GLOBAL-ADDRESS1       USAGE POINTER.
 01 TEMP-CALLNAME       PIC X(32)        VALUE SPACES.
 01 TEMP-ERRNAME        PIC X(32)        VALUE SPACES.
 01 TEMP-ERRNUM         PIC S9(4) COMP-5 VALUE 0.
 01 TEMP-ERRLEN         PIC S9(4) COMP-5 VALUE 0.
 01 TEMP-ERRMSG         PIC X(160)       VALUE SPACES.
 01 DebugBUF     pic x(256)  VALUE SPACES.

*>*########################################################
 LINKAGE SECTION.
 01 SET-STATEMENT     PIC X(80).
 01 JSTRING.
     02 JSTR  PIC X OCCURS 1 TO 80 DEPENDING ON JLEN.
 01 JLEN      PIC S9(4) COMP-5.
    COPY "vplus/VPLUSMEM".
*>*########################################################
 PROCEDURE DIVISION USING SET-STATEMENT JSTRING JLEN.
*>*---------------------------------------------------------*
 0000-BEGIN-VSETFIELD.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES TO TEMP-CALLNAME
     string "VSETFIELD "
      delimited by "  " into TEMP-CALLNAME.

     MOVE 0 TO X Y.
     MOVE "0" TO BEGFLD-SW.
     MOVE SPACES TO LAST-CHAR.
     MOVE NSTRING TO JSTRING
     Goback.


>>source format free
*>******************************************************************
*>*                        T r i m S t r i n g                     *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TrimString.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 mylength          PIC S9(4) Comp-5.
 LINKAGE SECTION.
 01 LINKSTRING        PIC X(512).
 01 LINK1             PIC S9(4) Comp-5.
 01 LINK2             PIC S9(4) Comp-5.
 Procedure Division Using LINKSTRING LINK1 LINK2.
 0000-TrimString.
     Move LINK2 To mylength.
     Initialize LINK1 LINK2.
     Perform Varying LINK1 From 1 by 1 until  LINKSTRING(LINK1:1) > " "  Continue  End-Perform.
     Perform Varying LINK2 From mylength by -1 until  LINKSTRING(LINK2:1) > " " Continue End-Perform.
     Goback.

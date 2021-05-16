>>source format free.
*>******************************************************************
*>*                        V S P 2 I N I T                         *
*>******************************************************************
*>***************** T H I S   I S   A   N O _ O P ******************
*>******************************************************************
*>*     Catch anyone calling this one, tell them to stop.          *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VSP2INIT.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.

 WORKING-STORAGE SECTION.
 01 DebugBUF     pic x(256)  VALUE SPACES.


 LINKAGE SECTION.
   COPY "vplus/COMAREA".
 PROCEDURE DIVISION USING COMAREA.
 0000-VSP2INIT.
     move LOW-VALUES to DebugBUF.
     string "**** VSP2INIT ****" DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     GOBACK.

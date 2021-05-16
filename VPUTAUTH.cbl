>>source format free.
*>******************************************************************
*>*                      V P U T A U T H                           *
*>******************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VPUTAUTH.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 GLOBAL-ADDRESS1   USAGE POINTER.
 01 DebugBUF     pic x(256)  VALUE SPACES.

*>*########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 Link-Auth.
    05 LF-Auth-ID                       Pic X(48).
    05 LF-IPADDR                        Pic X(16).
*>*########################################################
 PROCEDURE DIVISION USING COMAREA Link-Auth.
 0000-BEGIN-VPUTAUTH.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM To GLOBAL-ADDRESS1.

     Move LF-Auth-ID         To AF-Auth-ID.
     Move LF-IPADDR          To AF-IPADDR.
     Move 0                  To AF-Status.
     Move "OK"               To AF-Lockcode.
     Move 0                  To CSTATUS.
     Goback.

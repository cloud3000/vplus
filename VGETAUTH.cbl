>>source format free.
*>*****************************************************************
*>                       V G E T A U T H                          *
*>*****************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VGETAUTH.
 AUTHOR.     MICHAEL ANDERSON.
 ENVIRONMENT DIVISION.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  GLOBAL-ADDRESS1   USAGE POINTER.
 01  DebugBUF          pic x(256)  VALUE SPACES.
*>########################################################
 LINKAGE SECTION.
    COPY "vplus/COMAREA".
    COPY "vplus/VPLUSMEM".
 01 Link-Auth.
    05 LF-Auth-ID                       Pic X(48).
    05 LF-IPADDR                        Pic X(16).
*>########################################################
 PROCEDURE DIVISION USING COMAREA Link-Auth.
 0000-BEGIN-VGETAUTH.
     CALL "GetPtrVplusBlock1" USING GLOBAL-ADDRESS1.
     SET ADDRESS OF VPLUSMEM TO GLOBAL-ADDRESS1.

     move LOW-VALUES to DebugBUF.
     string "VGETAUTH: " CFNAME DELIMITED BY size into DebugBUF.
     CALL "LogDebug" USING DebugBUF.

     Move AF-Auth-ID To LF-Auth-ID.
     Move AF-IPADDR  To LF-IPADDR.
     Move AF-Status  To CSTATUS.
     Goback.

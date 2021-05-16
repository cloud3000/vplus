@echo off
rem ***********************************************************************
rem *   Created by Michael Anderson. (How to build xl_VPLUS.lib)
rem *   Compiles all Vplus (aka VIEW3000) (aka Vue3) library into "xl_VPLUS.lib"
rem *   The name xl_VPLUS.lib was intended for all intrinsics, however
rem *   it only contains Vplus intrinsics.
rem *   Or all *.cbl files found in:
rem *       C:\vplus\
rem *
rem *   NOTE: This script is 100% usable, 
rem *         however it was written as documentation.
rem ***********************************************************************
rem *   
rem *   Change Directory (CD) where the vplus source is located.
rem *
PUSHD C:\vplus
FOR %%A IN (*.cbl) DO (
  Echo Compiling %%A
  ccbl32 -Wl -Sp C:\copylib -v -Dw64 -Cp -x -Ga %%A
)
rem
rem Process the ACU's
rem
rem The 'S:' drive is '\\source' 
DEL s:\newexe\xl_VPLUS.lib
cblutl32 -lib -o s:\newexe\xl_VPLUS.lib TrimString.acu
cblutl32 -lib -v -o s:\newexe\xl_VPLUS.lib *.acu
DEL *.acu
)
rem ***********************************************************************
rem *   Change Directory (CD) Back to where we were.
rem ***********************************************************************
POPD


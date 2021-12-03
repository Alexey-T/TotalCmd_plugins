@echo off
set arc=Readme\Src.rar
if exist %arc% del %arc%
start /wait winrar a %arc% *.pas *.dpr ..\Proc\*.pas Tests\*.dpr *.bat

set arc=wdx_OOInfo.zip
if exist %arc% del %arc%

copy /b 64\OOInfo.wdx .\OOInfo.wdx64
start /min /wait winrar a %arc% *.wdx* *.lng *.ini *.inf unzip.exe Readme

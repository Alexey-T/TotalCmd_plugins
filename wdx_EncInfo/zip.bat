@echo off
set arc=Readme\Src.rar
if exist %arc% del %arc%
start /wait winrar a %arc% *.lng *.inf *.dpr *.pas *.bat

set arc=wdx_EncInfo.zip
if exist %arc% del %arc%
copy /b 64\EncInfo.wdx .\EncInfo.wdx64
start /wait winrar a %arc% *.wdx* *.lng *.inf EncInfo.example.ini Readme

@echo off
set arc=Readme\Src.zip
if exist %arc% del %arc%
start /min /wait winrar a %arc% *.lpr *.lpi *.pas *.bat

set arc=wdx_TextSearch.zip
if exist %arc% del %arc%

start /min /wait winrar a %arc% *.wdx* *.Sample.ini *.inf Conv Readme

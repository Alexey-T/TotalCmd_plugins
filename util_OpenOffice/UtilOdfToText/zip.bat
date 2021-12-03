@echo off
set arc=util_OdfToTxt.zip
if exist %arc% del %arc%

start /wait winrar a %arc% *.exe unzip32.dll Readme.*

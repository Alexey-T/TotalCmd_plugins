@echo off
set arc=util_OONotepadView.zip
if exist %arc% del %arc%

start /wait winrar a %arc% OONotepadView.exe unzip32.dll Readme.txt


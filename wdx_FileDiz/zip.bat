@echo off
if exist *.rar del *.rar
if exist *.zip del *.zip
start /wait winrar a wdx_FileDiz.zip Readme*.txt *.wdx *.lng *.ini *.inf

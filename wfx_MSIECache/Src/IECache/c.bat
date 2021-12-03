@echo off
dcc32.exe -m -uKOL -ikol IECache.dpr
if errorlevel 1 exit

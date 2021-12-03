@echo off
dcc32 -m Guide.dpr
if errorlevel 1 exit
Guide

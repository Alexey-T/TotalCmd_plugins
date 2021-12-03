@echo off
set arc=util_TCPlugman_src.rar
if exist %arc% del %arc%
start /min /wait winrar a -r -s -rr10p -x@exclude.lst %arc% Plugman Tests Components Docs C:\Components\PJButton *.doc *.bat exclude.lst

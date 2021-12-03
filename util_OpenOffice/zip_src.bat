@echo off
set arc=util_OOProg_src.rar
if exist %arc% del %arc%

start /wait /min winrar.exe a -r -x*.dcu -x*.exe -x*.cfg %arc% C:\Components\PNGImageOld
start /wait /min winrar.exe a -r %arc% *.pas *.inc *.dpr *.dfm *.txt *.lng *.inf zip*.bat *.res *.rc dcc32.cfg unzip32.dll *.odt *.xsl *.html

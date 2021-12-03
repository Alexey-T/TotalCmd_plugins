@echo off
set a=readme\Src.rar
del %a%
start /min /wait winrar a -r -s %a% *.pas *.dpr *.cfg *.dfm *.res *.bat



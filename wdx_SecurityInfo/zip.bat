@echo off
del Readme\src.rar
start /wait winrar a -r Readme\src.rar *.dpr *.pas *.lng *.ini *.inf *.bat *.rc

set a=wdx_SecInfo.zip
del %a%

copy /b 32\SecInfo.wdx .\SecInfo.wdx
copy /b 64\SecInfo.wdx .\SecInfo.wdx64
start /wait winrar a %a% *.wdx* *.lng *.inf Readme

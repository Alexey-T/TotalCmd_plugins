@echo off
set arc=wlx_OOoViewer.rar
if exist %arc% del %arc%

md Temp
md Temp\OdfConverter
md Temp\Readme

copy /b *.wlx Temp  >nul
copy /b *.xsl Temp  >nul
copy /b *.inf Temp  >nul
copy /b unzip32.dll Temp >nul
copy /b OdfConverter\*.* Temp\OdfConverter >nul
copy /b Readme\*.html Temp\Readme >nul
copy /b ..\util_OOProg_src.rar Temp\Readme >nul

cd Temp
start /min /wait winrar a -r ..\%arc% .
cd ..

echo y|del Temp\OdfConverter\*.*
echo y|del Temp\Readme\*.*
echo y|del Temp\*.*
rd Temp\OdfConverter
rd Temp\Readme
rd Temp

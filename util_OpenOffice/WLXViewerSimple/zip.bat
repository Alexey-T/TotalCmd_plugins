@echo off
set arc=wlx_OOoViewerSimple.zip
if exist %arc% del %arc%

md Temp

copy /b Readme*.txt Temp
copy /b *.wlx Temp
copy /b *.inf Temp
copy /b unzip32.dll Temp

cd Temp
start /wait winrar a -r ..\%arc% .
cd ..

echo y|del Temp\*.*
rd Temp

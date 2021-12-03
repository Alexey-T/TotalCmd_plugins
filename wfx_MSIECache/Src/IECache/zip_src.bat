@echo off
set arc=Readme\src.rar
if exist %arc% del %arc%

md Temp
md Temp\IECache
md Temp\IECache\Lang
md Temp\IECache\Tests
md Temp\Proc

copy /b *.pas Temp\IECache
copy /b *.dpr Temp\IECache
copy /b *.dfm Temp\IECache
copy /b *.lng Temp\IECache
copy /b *.inf Temp\IECache
copy /b *.bat Temp\IECache
copy /b *.rc Temp\IECache
copy /b *.ico Temp\IECache
copy /b *.txt Temp\IECache
copy /b dcc32_src.cfg Temp\IECache\dcc32.cfg

copy /b Lang\*.lng Temp\IECache\Lang
copy /b Tests\*.dpr Temp\IECache\Tests

copy /b C:\Prog\Proc\RegProc.pas Temp\Proc
copy /b C:\Prog\Proc\FProc.pas Temp\Proc
copy /b C:\Prog\Proc\SProc.pas Temp\Proc
copy /b C:\Prog\Proc\IniFile.pas Temp\Proc
copy /b C:\Prog\Proc\FUnpack.pas Temp\Proc

cd Temp
start /min /w winrar a -r -x*.dcu ..\%arc% . ..\KOL
cd ..

echo y|del Temp\IECache\Tests\*.*
echo y|del Temp\IECache\Lang\*.*
echo y|del Temp\IECache\*.*
echo y|del Temp\Proc\*.*
echo y|del Temp\*.*

rd Temp\IECache\Tests
rd Temp\IECache\Lang
rd Temp\IECache
rd Temp\Proc
rd Temp

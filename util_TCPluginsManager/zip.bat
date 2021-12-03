@echo off
set setup=util_TCPlugman.zip
if exist %setup% del %setup%
call zip_src.bat

md Temp
md Temp\Readme
md Temp\Language

copy/b Plugman\Plugman.exe Temp  >nul
copy/b Plugman\Config.exe Temp   >nul
copy/b Plugman\UnRAR.exe Temp    >nul
copy/b Plugman\UnZip.exe Temp    >nul
copy/b Plugman\Readme Temp\Readme     >nul
copy/b Plugman\Language Temp\Language >nul
copy/b util_TCPlugman_src.rar Temp\Readme\Src.rar >nul

cd Temp
start /min /wait winrar a -r -s- ..\%setup% *.*
cd ..

echo y|del Temp\Language
echo y|del Temp\Readme
echo y|del Temp
rmdir Temp\Language
rmdir Temp\Readme
rmdir Temp

cls

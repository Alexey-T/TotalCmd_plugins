@echo off
if exist *.rar del *.rar
if exist *.zip del *.zip
copy /b Config\Config.exe .
start /wait winrar a -s wdx_FileDesc.rar *.wdx *.lng *.inf FileDesc.example.ini Config.exe Plugins\DFileLists.dll Plugins\DHTML.dll Plugins\DText.dll Plugins\DVersionInfo.dll Plugins\Plugins.ini Plugins\*.dfp Plugins\AudioInfo Plugins\ImageInfo Readme\*.*



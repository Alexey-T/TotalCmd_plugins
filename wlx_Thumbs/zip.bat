@echo off
if exist *.rar del *.rar
if exist *.zip del *.zip
start /wait winrar a wlx_Thumbs.zip Readme*.html *.wlx *.ini *.inf Plugins

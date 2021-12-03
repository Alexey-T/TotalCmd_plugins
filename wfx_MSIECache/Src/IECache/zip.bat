@echo off
if exist wfx*.zip del wfx*.zip
:if exist *.wfx copy /b *.wfx *.wfx64
start /min /w winrar a -r wfx_MSIECache.zip Readme Lang *.wfx *.wfx64 *.inf

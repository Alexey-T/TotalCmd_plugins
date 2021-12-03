@echo off
set arc=wdx_FileDesc_src.rar
if exist %arc% del %arc%
rar a %arc% *.lng *.cfg zip*.bat *.rc FileDesc.example.ini FileDesc.dpr *.pas PluginsSrc\*.pas PluginsSrc\*.dpr PluginsSrc\*.cfg Config\*.pas Config\*.dpr Config\*.dfm Config\*.cfg Config\*.res Readme\*.*


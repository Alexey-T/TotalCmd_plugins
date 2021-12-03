-----------------------------------------------------------------------
TC Plugins Manager er et hj�lpeprogram til Total Commander
(http://ghisler.com), som g�r det nemt at h�ndtere installerede TC plugins.
Et udvalg af disse plugins kan findes p�: http://totalcmd.net.

Programmet kan:

- H�ndtere alle plugintyper
- Installere plugins, ogs� fra pakkede filer og forgrenede mapper
- Sl� plugins til/fra - det kan TC ikke!
- Starte/genstarte Total Commander
- Logf�re installationer og udpakninger


Bem�rkninger:

  1. Der vises om TC k�rer for �jeblikket; hvis det er tilf�ldet, skal TC
     genstartes, n�r der er lavet �ndringer, for at disse �ndringer kan
     f� effekt.

  2. Funktionen "Installer fra arkiv" kr�ver, at der er en ekstern udpakker
     installeret:

     - for ZIP en af disse udpakkere: WinRAR, UnZip, PKUnZip, WZUnZip, PKZipC
     - for RAR en af disse udpakkere: WinRAR, Rar.exe, UnRar.exe

     Det anbefales at bruge WinRAR (http://rarlab.com), som h�ndterer begge
     arkivtyper.

     UnZip/UnRAR freeware udpakkere er inkluderet i distributionen.  De skal
     ikke kopieres nogen steder hen, men skal bruges direkte fra Plugman
     mappen.  De kan slettes, hvis WinRAR er installeret.

  3. Funktionen "Installer fra arkiv" muligg�r installation af meget store
     pluginpakker!  Der kan v�re mange plugins i �t arkiv, eller i separate
     mapper af enhver dybde.  Bem�rk: Det er ikke tilladt at have mere end en
     .W?X file i den samme mappe i et arkiv - det vil udl�se en fejlmeddelse!

  4. Funktionen "Installer fra mappe" er et alternativ til "Installer fra
     arkiv".  Men ved brug af et pakket plugin anbefales det at anvende
     "Installer fra arkiv" i stedet for f�rst at udpakke arkivet - det vil
     sikre en korrekt struktur af pluginmappen efter installationen.


kommandolinjeparametre:

---------------------

  1. /Restart               - Genstart Total Commander og luk ned.
  2. /OrderWLX              - Vis pluginr�kkef�lge for Lister-,
     /OrderWDX                Indholds- eller
     /OrderWCX                Pakke-plugins
  3. <FileNavn>             - Installer en given pluginfil.
  4. /Uninstall <FileNavn>  - Afinstaller en given pluginfil.


---------------------------------------------------------------------------------
Freeware / Open Source.

Copyright (c) 2004-2007 Alexey Torgashin
<atorg@yandex.ru>

http://atorg.net.ru
http://totalcmd.net/plugring/tc_plugman.html
http://sourceforge.net/projects/tc-plugman

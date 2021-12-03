-----------------------------------------------------------------------
TC Plugins Manager er et hjælpeprogram til Total Commander
(http://ghisler.com), som gør det nemt at håndtere installerede TC plugins.
Et udvalg af disse plugins kan findes på: http://totalcmd.net.

Programmet kan:

- Håndtere alle plugintyper
- Installere plugins, også fra pakkede filer og forgrenede mapper
- Slå plugins til/fra - det kan TC ikke!
- Starte/genstarte Total Commander
- Logføre installationer og udpakninger


Bemærkninger:

  1. Der vises om TC kører for øjeblikket; hvis det er tilfældet, skal TC
     genstartes, når der er lavet ændringer, for at disse ændringer kan
     få effekt.

  2. Funktionen "Installer fra arkiv" kræver, at der er en ekstern udpakker
     installeret:

     - for ZIP en af disse udpakkere: WinRAR, UnZip, PKUnZip, WZUnZip, PKZipC
     - for RAR en af disse udpakkere: WinRAR, Rar.exe, UnRar.exe

     Det anbefales at bruge WinRAR (http://rarlab.com), som håndterer begge
     arkivtyper.

     UnZip/UnRAR freeware udpakkere er inkluderet i distributionen.  De skal
     ikke kopieres nogen steder hen, men skal bruges direkte fra Plugman
     mappen.  De kan slettes, hvis WinRAR er installeret.

  3. Funktionen "Installer fra arkiv" muliggør installation af meget store
     pluginpakker!  Der kan være mange plugins i ét arkiv, eller i separate
     mapper af enhver dybde.  Bemærk: Det er ikke tilladt at have mere end en
     .W?X file i den samme mappe i et arkiv - det vil udløse en fejlmeddelse!

  4. Funktionen "Installer fra mappe" er et alternativ til "Installer fra
     arkiv".  Men ved brug af et pakket plugin anbefales det at anvende
     "Installer fra arkiv" i stedet for først at udpakke arkivet - det vil
     sikre en korrekt struktur af pluginmappen efter installationen.


kommandolinjeparametre:

---------------------

  1. /Restart               - Genstart Total Commander og luk ned.
  2. /OrderWLX              - Vis pluginrækkefølge for Lister-,
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

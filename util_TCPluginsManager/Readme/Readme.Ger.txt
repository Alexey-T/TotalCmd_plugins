TC Plugins Manager ist ein Werkzeug f�r Total Commander, das eine einfache
Verwaltung der TC-Plugins erm�glicht.

Erm�glicht das:
~~~~~~~~~~~~~~~
 * Verwalten aller Typen von Plugins
 * Installieren von Plugins, auch aus gezippten Packs und Unterverzeichnissen
 * Deaktivieren/Aktivieren von Plugins - TC kann das nicht!
 * Starten/Neu starten des Total Commander
 * Loggen von Installations- und Entpackprozessen


Eine Plugin-Zusammenstellung kann hier bezogen werden: <http://totalcmd.net>


Anmerkungen:
~~~~~~~~~~~~
1. Es wird angezeigt, ob der TC gerade l�uft. Falls ja, muss der TC nach
   �nderungen der Konfiguration neu gestartet werden, damit diese wirksam
   werden.

2. Der Befehl "Aus Archiv installieren" ben�tigt installierte externe
   Entpacker:

   - f�r ZIP einer von diesen: WinRAR, UnZip, PKUnZip, WZUnZip, PKZipC
   - f�r RAR einer von diesen: WinRAR, Rar.exe, UnRar.exe

   WinRAR, welcher beide Typen unterst�tzt, wird empfohlen:
   <http://rarlab.com>

   F�r diejenigen, die kein WinRAR installiert haben, sind die Freeware-
   Entpacker UnZip.exe und UnRar.exe in der Distribution enthalten.
   Diese sollten direkt aus dem Plugman-Ordner laufen.

3. Der Befehl "Aus Archiv installieren" erlaubt die Installation gro�er
   Plugin-Packs! Du kannst mehrere Plugins in einem einzigen Archiv haben,
   in mehreren Unterordnern jeglicher Tiefe.
   Hinweis:
   Es ist nicht erlaubt, mehr als eine .W?X-Datei im selben Archiv-Ordner
   zu haben - in diesem Fall wird eine Fehlermeldung generiert!

4. Der Befehl "Aus Ordner installieren" ist eine Alternative zu "Aus Archiv
   installieren". Falls das Plugin-Pack jedoch in gezippter Form vorliegt,
   wird empfohlen, "Aus Archiv installieren" zu verwenden - dies erzeugt
   bei der Installation die korrekte Verzeichnisstruktur.


Kommandozeilen Optionen:
~~~~~~~~~~~~~~~~~~~~~~~~
 1. /restart               = Plugman beenden und Total Commander neu starten
 2. /orderWLX              = Dialog "Plugin-Reihenfolge" f�r Lister-, Inhalte-
    /orderWDX                oder Packer-Plugins anzeigen
    /orderWCX
 3. <Dateiname>            = Angegebenes Plugin installieren
 4. /Uninstall <Dateiname> = Angegebenes Plugin deinstallieren


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Freeware/Open-Source (Quellcode verf�gbar auf Homepage)

Urheberrecht � 2004-2007 Alexey Torgashin <atorg@yandex.ru>
http://atorg.net.ru
http://totalcmd.net/plugring/tc_plugman.html

TC Plugins Manager ist ein Werkzeug für Total Commander, das eine einfache
Verwaltung der TC-Plugins ermöglicht.

Ermöglicht das:
~~~~~~~~~~~~~~~
 * Verwalten aller Typen von Plugins
 * Installieren von Plugins, auch aus gezippten Packs und Unterverzeichnissen
 * Deaktivieren/Aktivieren von Plugins - TC kann das nicht!
 * Starten/Neu starten des Total Commander
 * Loggen von Installations- und Entpackprozessen


Eine Plugin-Zusammenstellung kann hier bezogen werden: <http://totalcmd.net>


Anmerkungen:
~~~~~~~~~~~~
1. Es wird angezeigt, ob der TC gerade läuft. Falls ja, muss der TC nach
   Änderungen der Konfiguration neu gestartet werden, damit diese wirksam
   werden.

2. Der Befehl "Aus Archiv installieren" benötigt installierte externe
   Entpacker:

   - für ZIP einer von diesen: WinRAR, UnZip, PKUnZip, WZUnZip, PKZipC
   - für RAR einer von diesen: WinRAR, Rar.exe, UnRar.exe

   WinRAR, welcher beide Typen unterstützt, wird empfohlen:
   <http://rarlab.com>

   Für diejenigen, die kein WinRAR installiert haben, sind die Freeware-
   Entpacker UnZip.exe und UnRar.exe in der Distribution enthalten.
   Diese sollten direkt aus dem Plugman-Ordner laufen.

3. Der Befehl "Aus Archiv installieren" erlaubt die Installation großer
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
 2. /orderWLX              = Dialog "Plugin-Reihenfolge" für Lister-, Inhalte-
    /orderWDX                oder Packer-Plugins anzeigen
    /orderWCX
 3. <Dateiname>            = Angegebenes Plugin installieren
 4. /Uninstall <Dateiname> = Angegebenes Plugin deinstallieren


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Freeware/Open-Source (Quellcode verfügbar auf Homepage)

Urheberrecht © 2004-2007 Alexey Torgashin <atorg@yandex.ru>
http://atorg.net.ru
http://totalcmd.net/plugring/tc_plugman.html

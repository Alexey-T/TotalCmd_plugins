TC Plugins Manager � un'utility per Total Commander (http://ghisler.com),
che ne permette una facile gestione dei plugin. Per ottenere i plugin,
fare riferimento alla pagina http://totalcmd.net

Caratteristiche:
----------------

- Gestisce plugin di ogni tipo
- Installa/Disinstalla/Configura i plugin, anche da archivi compressi e
  cartelle nidificate
- Disabilita/Abilita i plugin - TC non lo pu� fare!
- Avvia/Riavvia Total Commander
- Crea un log delle installazioni e delle operazioni di decompressione


Note:
-----

1. Mostra se TC � attualmente in esecuzione; nel caso lo fosse, dopo aver
   effettuato eventuali modifiche, � necessario riavviare Total Commander
   poich� tali modifiche diventino attive.

2. La funzione "Installa dall'archivio" � attiva solo con un decompressore
   esterno installato:

   - per ZIP, uno dei seguenti: WinRAR, UnZip, PKUnZip, WZUnZip, PKZipC
   - per RAR, uno dei seguenti: WinRAR, Rar.exe, UnRar.exe

   Quello raccomandato � WinRAR, che pu� gestire entrambi i tipi di archivio.

   I decompressori freeware UnZip/UnRAR sono inclusi nel pacchetto di
   distribuzione. Non � pi� necessario copiarli altrove, dato che posso essere
   eseguiti direttamente dalla cartella di Plugman.
   Posso essere rimossi, nel caso WinRAR sia installato nel sistema.

3. La funzione "Installa dall'archivio" permette di installare collezioni di
   plugin di grandi dimensioni! Potrai mettere pi� plugin in un unico archivio,
   in cartelle differenti, a qualsiasi profondit� di percorso.
   Attenzione, per�, che non � permesso mettere pi� di un .W?X nella stessa
   sottocartella dell'archivio. In tal caso, otterrai un messaggio d'errore.

4. La funzione "Installa dalla cartella" � un'alternativa alla funzione
   "Installa dall'archivio". Ad ogni modo, se hai compresso un pacchetto di
   installazione di plugin, � meglio utilizzare "Installa dall'archivio",
   piuttosto che estrarre manualmente il contenuto e installarlo.
   Ci� consentir� di ottenere la corretta struttura delle cartelle in fase di
   installazione.


Command line options:
---------------------

  1. /Restart       - Riavvia  Total Commander e termina.
  2. /OrderWLX      - Mostra l'ordine di caricamento per plugin di Visualizzazione,
     /OrderWDX        Contenuti e
     /OrderWCX        Compressione.
  3. <FileName>     - Installa il plugin con il nome specificato.
  4. /Uninstall <FileName>
                    - Disinstalla il plugin con il nome specificato.


---------------------------------------------------------------------------------
Freeware / Open Source (sorgenti disponibili in homepage).

Copyright (c) 2004-2011 Alexey Torgashin <atorg@yandex.ru>
http://atorg.net.ru
http://totalcmd.net/plugring/tc_plugman.html
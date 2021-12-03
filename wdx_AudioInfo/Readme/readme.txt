AudioInfo content plugin for Total Commander
--------------------------------------------
This plugin can show information about audio files.
Supported stream formats:
  MP3, MP4, M4A, M4B, M4R, MP2, MP1, OGG, WMA, WAV, VQF, AAC, APE, MPC, FLAC, CDA, OPUS, SPX, OFR, WV, TTA, AC3, DTS.
Supported tracker formats:
  IT, XM, S3M, MTM, MOD, UMX, MO3.

Fields available for stream formats:
  Channels, Duration, Sample rate, Bitrate, Bitrate type, Title, Artist, Album,
  Track, Date, Genre, Comment, Composer, Copyright, URL, Encoder;
for tracker formats:
  Channels, Duration, Title.

In MP3 files, comments can be in different frames and still be different, 
so you can output up to three first comments separately or all contained in the file.

Installation
------------
1. With TC 6.50+, just open archive and TC will install plugin automatically.
   If you already have AudioInfo installed, you may first need to uninstall
   previous version (so plugin's detect string will be updated).

2. Go to Configuration -> Options -> Custom columns;
   create new view named "Audio files" and add several columns to it;
   for each column press "+" and select some field from AudioInfo list.
   I recommend to add fields:
   Duration, Artist, Title, Album.

3. Turn on custom view:
   Show -> Custom columns mode -> Audio files;
   go to music folder and enjoy! :-)

4. If you don't need tracker formats support, you can safely remove
   bass.dll and bass64.dll libraries from plugin folder.

Copyrights
----------
Copyright (c) 2017 Dmitry Yudin <loopback@darkhost.ru>
http://total.darkhost.ru

Copyright (c) 2004-2005 Alexey Torgashin
http://uvviewsoft.com
http://totalcmd.net/plugring/AudioInfo.html - source code available

Audio Tools Library (c) 2001-2002 Jurgen Faul
http://jfaul.de/atl

Key Objects Library (c) 1999-2003 Vladimir Kladov
http://bonanzas.rinet.ru

ATL ported to KOL by Dmitry Matveev
http://www.mdvkol.narod.ru

BASSMOD (c) 1999-2004 Ian Luck
http://www.un4seen.com

Плагин AudioInfo для Total Commander
------------------------------------
Данный content-плагин позволяет показывать в колонках панелей Total Commander
различную информацию об аудио-файлах.
Поддерживаемые потоковые форматы:
  MP3, MP4, M4A, M4B, M4R, MP2, MP1, OGG, WMA, WAV, VQF, AAC, APE, MPC, FLAC, CDA, OPUS, SPX, OFR, WV, TTA, AC3, DTS.
Поддерживаемые трекерные форматы:
  IT, XM, S3M, MTM, MOD, UMX, MO3.

Поля, показываемые для потоковых форматов:
  Каналы, Время, Частота, Битрейт, Тип битрейта, Название, Исполнитель, Альбом,
  Трек, Дата, Жанр, Комментарий, Композитор, Авторское право, Ссылка, Кодировщик;
для трекерных форматов:
  Каналы, Время, Название.

В MP3-файлах комментарии могут содержаться в разных фреймах и при этом быть различными, 
поэтому была добавлена возможность вывода до трех первых комментариев раздельно или 
всех имеющихся в файле.

Установка
---------
1. При наличии TC 6.50+ просто откройте архив, и TC сам установит плагин.
   Если до этого у Вас была установлена предыдущая версия плагина, то вначале
   удалите ее (для того чтобы при установке обновить "detect string" плагина).

2. Перейдите в меню Конфигурация -> Настройка -> Наборы колонок;
   добавьте новый набор, назовите его "Аудио-файлы" и добавьте в него несколько
   колонок; для каждой колонки нажмите "+" и выберите поле из списка AudioInfo.

3. Включите настроенный набор колонок:
   Вид -> Пользовательский набор колонок -> Аудио-файлы;
   наконец перейдите в каталог со звуковыми файлами.

4. Если вам не нужна поддержка трекерных форматов, можно удалить 
   библиотеки bass.dll и bass64.dll из каталога установки плагина.


Авторские права
---------------
Copyright (c) 2017 Dmitry Yudin <loopback@darkhost.ru>
http://total.darkhost.ru

Copyright (c) 2004-2005 Alexey Torgashin
http://uvviewsoft.com
http://wincmd.ru/plugring/AudioInfo.html - доступен исходный код

Audio Tools Library (c) 2001-2002 Jurgen Faul
http://jfaul.de/atl

Key Objects Library (c) 1999-2003 Vladimir Kladov
http://bonanzas.rinet.ru

ATL портирована в KOL - Dmitry Matveev
http://www.mdvkol.narod.ru

BASSMOD (c) 1999-2004 Ian Luck
http://www.un4seen.com

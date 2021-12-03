"File Descriptions" plugin for Total Commander
----------------------------------------------
This content plugin allows to display file descriptions:

- descriptions from Descript.ion/Files.bbs;
- for text files: file contents;
- for executables/dlls: version information;
- for HTML files: contents of Title/Meta tags;
- for URL files: target location.


Installation
------------
1. With TC 6.50+, just open archive and TC will install plugin automatically.

2. Go to Configuration -> Options -> Custom columns;
   create new view named "Descriptions" and add several columns to it;
   for each column press "+" and select some field from FileDiz list.

3. Turn on custom view:
   Show -> Custom columns mode -> Descriptions.


Versions history
----------------
17.11.05: you may specify empty extension in ini-file;
          you may specify CR (#13) as lines delimiter - useful for tooltips;
          field "Short description" is removed
15.11.05: shows .URL targets
03.06.05: fixed displaying of Files.bbs multi-line descriptions
15.05.05: shows Meta tag for HTML files
15.04.05: you may specify text replace strings in FileDiz.ini
18.03.05: fixed Descript.ion reading
15.03.05: added source code
07.11.04: added subfileds "Private build", "Special build"
06.11.04: added field "Version info" with all standard subfields
05.11.04: shows version info for executables
05.11.04: shows HTML/Text descriptions
28.10.04: shows descriptions from Files.bbs
25.10.04: initial version


(c) 2005 Alexey Torgashin <atorg@yandex.ru>
http://alextpp.narod.ru
http://totalcmd.net/plugring/FileDiz.html - source code available

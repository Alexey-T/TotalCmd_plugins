TC Plugins Manager is a utility for Total Commander (http://ghisler.com)
which allows to easily manage TC plugins. You can obtain a collection of
these plugins at: http://totalcmd.net.

 Allows to:
 ----------

 - Manage plugins of all types
 - Install plugins, even from zipped packs and nested folders
 - Disable/enable plugins - TC cannot do it!
 - Run/restart Total Commander
 - Log installation and unpacking operations


Notes:
------

  1. It's shown if TC is currently running; if yes, then after you made some
     changes you must restart TC for these changes to take effect.

  2. "Install from archive" command requires external unpackers installed:

     - for ZIP one of these: WinRAR, UnZip, PKUnZip, WZUnZip, PKZipC
     - for RAR one of these: WinRAR, Rar.exe, UnRar.exe

     Its recommended to use WinRAR (http://rarlab.com) which handles both types.

     UnZip/UnRAR freeware unpackers are included in the distribution. There is no
     need to copy them anywhere, they should run directly from the Plugman folder.
     You may delete them if you have WinRAR installed.

  3. "Install from archive" command allows you to install huge plugins packs!
     You can have multiple plugins in one archive, in separate folders of any depth.
     Note: It's not allowed to have more than one .W?X file in the same archive
     folder - you will get an error message in this case!

  4. "Install from folder" command is an alternative for "Install from archive".
     However, if you have a zipped plugin pack, it's recommended to use
     "Install from archive" - this will create the proper plugin folder structure
     during installation.


Command line options:
---------------------

  1. /Restart       - Restart Total Commander and quit.
  2. /OrderWLX      - Show Loading Order dialog for Lister, Content or Packer plugins.
     /OrderWDX
     /OrderWCX
  3. <FileName>     - Install given plugin filename.
  4. /Uninstall <FileName>
                    - Uninstall given plugin filename.


---------------------------------------------------------------------------------
Open Source.
Copyright (c) 2004-2011 Alexey Torgashin

http://uvviewsoft.com
http://totalcmd.net/plugring/tc_plugman.html
http://sourceforge.net/projects/tc-plugman

NTFS Descriptions
Content-plugin for Total Commander 7.0+
Author: Aleksey Fomin (ledsoft@tut.by)

Description
-----------
Plugin can be used to display, create and edit comments for files and directories.
These comments are stored in streams on the NTFS file system.

Advantages:
-no spare files in every directory (such as descript.ion or files.bbs);
-comment are preserved even after copying with explorer;
-you can create a lot of comments with different names (in different streams).

Disadvantages:
-streams can be created only on NTFS,
 Windows NT, 2000, XP, Vista etc are required for this.
 Comments (streams) will be lost during copying to FAT32 (or other) file system.

Using
-----
You can define your oun stream names in NTFS_diz.ini (10 max).
There is one name (Comment) by default. These names are displayed in the menu of plugin fields.

Examples of using several comments:
-mark some important files (then search with plugins);
-colorize some dirs to different color (set values red/green/blue in comment and setup colorizing by predefinied searches);
-mark your books, music, video (good/bad, read/no read etc);
-and much more...
-all of this you can use simultaneously :)

For viewing comments - create custom columns and choose ntfs_diz.
For creating/editing - open change attributes dialog and choose ntfs_diz.

Don't forget that you can use change attributes dialog to copy your comments from descript.ion to NTFS streams and vice-versa!

Total Commander 7.0 is required for all!
For viewing (not editing) - Total Commander 6.5.

If TC don't copy streams, please set in your wincmd.ini: [Configuration] CopyStreams=1

By default file date is updated while changing comments.
If you don't like this, please change in NTFS_diz.ini, section [Options], parameter KeepTime=1
After that date will keep as it was, but TC will not autorefresh panels.
For manual refresh change dir or tab and go back.

Licence
-------
as is, no warranty, freeware :)
Sources included.

Thanks
------
Christian Ghisler for Total Commander;
Alexey Torgashin for sources of File Descriptions.

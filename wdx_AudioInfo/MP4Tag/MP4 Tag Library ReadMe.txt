MP4 Tag Library - Copyright © 2012-2016 3delite. All rights reserved.
=====================================================================

What's the point?
=================

MP4 Tag Library is a component for use in Win32 (9x/ME/2K/XP/Vista/7/8), Win64, OSX, iOS and Android software.
Reads and writes MP4 Tags (tags in mp4, m4a, m4b audio and video files).

Features:

- Loading of MP4 tags
- Saving of MP4 tags
- Removing of MP4 tags
- Supports tags at start and at end of file
- Supports binary frames and cover pictures
- Access directly all atom datas as a TMemoryStream (full controll of the atom contents)
- Supports 64bit atom sizes (files >4GB)
- Updates 'stco' and 'co64' atom when required
- Full unicode support
- Pure Delphi code, no external dependencies
- Delphi XE2 64bit and OSX, Delphi XE5 iOS and Android compatible
- Lazarus/Free pascal compatible

You should also see the included example program Tutorial's source-code for example of how to use MP4 Tag Library in your own programs.

If you are interested in ID3v1 and ID3v2 tagging unit, please follow this link:
http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html

If you are interested in APEv2 tagging unit, please follow this link:
http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/APEv2Library.html

If you are interested in Ogg Vorbis and Opus Tag Library unit, please follow this link:
http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/OpusTagLibrary.html

If you are interested in Flac tagging unit, please follow this link:
http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/FlacTagLibrary.html

If you are interested in WMA tagging unit, please follow this link:
http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WMATagLibrary.html


Requirements:
=============

Delphi 2009 and above.


Latest Version
==============

The latest version of MP4 Tag Library can always be found at 3delite's website:

http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MP4TagLibrary.html


Copyright, Disclaimer, and all that other jazz
==============================================

This software is provided "as is", without warranty of ANY KIND, either expressed or implied, including but not limited to the implied warranties of merchantability and/or fitness for a particular purpose. The author shall NOT be held liable for ANY damage to you, your computer, or to anyone or anything else, that may result from its use, or misuse. Basically, you use it at YOUR OWN RISK.

Usage of MP4 Tag Library indicates that you agree to the above conditions.

You may freely distribute the MP4 Tag Library package as long as NO FEE is charged and all the files remain INTACT AND UNMODIFIED.

All trademarks and other registered names contained in the MP4 Tag Library package are the property of their respective owners.


MP4 Tag Library in shareware and commercial software?
=====================================================

You can use this component in your free programs for free. If like it and use it for shareware or commercial you have to buy a license.

Shareware License: €50, for usage of the component in an unlimited number of your shareware software.

http://www.shareit.com/product.html?productid=300548330

Commercial License: €250 Euros, for usage of the component in a single commercial product.

http://www.shareit.com/product.html?productid=300548328

In all cases there are no royalties to pay, and you can use all future updates without further cost, all you need to do is just obtain the newest version.

There is a discount when purchasing MP4 Tag Library and ID3v2 and/or APEv2 Library together. Please checkmark the ID3v2 Library and/or APEv2 Library on the order page when purchasing MP4 Tag Library.

If none of these licenses match your requirements, or if you have any questions, get in touch (3delite@3delite.hu).


Installation:
=============

Add the directory to the search path, and to Uses list add: MP4TagLibrary.


Notes on padding:
====================

SaveToFile() and RemoveMP4TagFromFile() have a 'KeepPadding' parameter. If 'KeepPadding' is True (as by default) then if the new tag is smaller then the available space, padding will be added (free atom) to fill the available space. If you need smallest file size call the function with False.
The benefit of having a padding is that it's not needed to re-write the whole file if there is enough padding space available when writing a new tag. When removing a cover art, it's advised to compact the file with calling the functions with 'KeepPadding = False'.


Usefull information:
====================

iTunes Metadata atom list and their data formats docs: http://code.google.com/p/mp4v2/wiki/iTunesMetadata


Credits
=======

C++ Builder tutorial by Robert Jackson (hsialinboy@yahoo.com)
Enhancements to audio/video attributes parsing by Ivan Llanas


Bug reports, Suggestions, Comments, Enquiries, etc...
=====================================================

If you have any of the aforementioned please email:

3delite@3delite.hu


History
=======

1.0.4.8 29/10/2012
------------------
First release.

1.0.5.9 27/11/2012
------------------
+ Improved compatibility ('----' atoms are supported with 'mean' and 'name' sub-atoms)

1.0.6.10 20/01/2013
-------------------
+ Fixed saving tag to files with more then 2 'free' atoms

1.0.7.11 06/04/2013
-------------------
+ Fixed saving tags corrupting the file in some cases

1.0.8.14 11/04/2013
-------------------
+ Fixed removing tags going in an endless loop in some cases

1.0.9.16 15/04/2013
-------------------
+ Fixed writing tag corrupting the output file in some cases

1.0.10.18 31/07/2013
--------------------
+ Fixed writing tag corrupting the output file if there was a 'free' atom after the 'ilst' atom

1.0.11.19 03/08/2013
--------------------
+ Fixed writing tag corrupting the output file if there were multiple 'free' atoms after the 'ilst' atom

1.0.12.22 12/08/2013
--------------------
+ Fixed writing tag corrupting the output file if there was no 'meta' atom

1.0.13.24 13/08/2013
--------------------
+ Fixed writing tag corrupting the output file if there were some missing meta atoms

1.0.14.25 14/08/2013
--------------------
+ Fixed writing tag corrupting the output file if there wes a missing 'udta' atom

1.0.15.26 31/10/2013
--------------------
+ Added support for Delphi XE5 iOS and Android build target

1.0.16.27 06/11/2013
--------------------
+ Fixed reading/writing files with a 0 length 'mdat' atom

1.0.17.28 07/11/2013
--------------------
+ Fixed updating 'co64' atom

1.0.18.34 15/11/2013
--------------------
+ Added support for reading/writing MP4 tags with 64bit atom sizes (files >4GB)

1.0.19.35 08/03/2014
--------------------
+ When saving the tag and the new tag + padding is smaller then the existing tag, the tag is re-written with new padding (this means if for example a large cover art is removed the new file size will be smaller)

1.0.20.36 23/04/2014
--------------------
+ Added MP4TagErrorCode2String() function
* Fixed reading and writing of atoms with a 0 length data

1.0.21.37 15/05/2014
--------------------
+ Added TMP4Tag.CoverArtCount function
+ TMP4AtomData.Delete returns a Boolean

1.0.22.38 24/11/2014
--------------------
+ Modified the code for Lazarus/Free pascal compatibility

1.0.23.40 24/11/2014
--------------------
+ Added options to get/set 'name' and 'mean' sub atom content

1.0.24.53 10/12/2014
--------------------
+ Completely re-written tag writing
* Fixes for 64bit size atoms

1.0.26.57 31/12/2014
--------------------
+ Added functions for loading/saving/removing tags to TStream

1.0.27.58 22/01/2015
--------------------
+ Added functions for getting/setting list tags (TIPL, TMCL)

1.0.28.62 03/02/2015
--------------------
+ Added parsing of play time (TMP4Tag.Playtime)
+ Added parsing of MP4 audio attributes
+ Added C++ Builder tutorial
* Fixed atom data delete function

1.0.29.63 10/02/2015
--------------------
+ Added functions for managing '----' atoms

1.0.30.64 18/02/2015
--------------------
+ GetCommon/SetCommon function parameters are searched case insensitive

1.0.31.66 03/10/2015
--------------------
* Fixed padding writing slow-down

1.0.32.73 23/11/2016
--------------------
* Fixed double parsing in GetTrack() and GetDisc()
* Fixed SetGenre() when switching atom
* Renamed functions that deal with multiple atoms
* Fixed parsing audio (now video width/height too) attributes when audio track is not the first track

1.0.33.74 24/11/2016
--------------------
+ Added audio track attribute support of ALAC and AC3 format

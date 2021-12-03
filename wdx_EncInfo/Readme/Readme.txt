EncInfo plugin for Total Commander
----------------------------------
This content plugin shows information about actual encoding of text files.
It can give following results:

- "UTF-16 LE" (detection by signature)
- "UTF-16 BE" (detection by signature)
- "UTF-8 BOM" (detection by signature)
- "UTF-8 no BOM" (detection by distribution of utf-8 unicode fragments)
- "RTF" (detection by signature)
- "DOS" (used in many .nfo files; when frequency of at last one OEM char is higher than ~20%)
- "DOS Ru" (russian texts; when amount of Russian DOS chars #80..#AF is higher than ~30%)
- "ANSI Ru" (russian texts; when amount of Russian ANSI chars #C0..#FF is higher than ~30%)
- "ANSI" (all other text files)
- "Binary" (binary files, containing special chars #00..#1F) 


(c) Alexey Torgashin
www.uvviewsoft.com

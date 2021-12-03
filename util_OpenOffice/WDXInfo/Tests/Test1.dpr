{$apptype console}
uses Windows, OOData, SProc, XmlProc;

var
  fn: string;
begin
  fn:= 'C:\TEMP\Resume.odt';
  fn:= 'C:\DOWNLOAD\Файл текстовый.odt';
  GetZipData(fn);

  Writeln('Generator: "',       ToOEM(fGenerator), '"');
  Writeln('Title: "',           ToOEM(fTitle), '"');
  Writeln('Description: "',     ToOEM(fDescription), '"');
  Writeln('Subject: "',         ToOEM(fSubject), '"');
  Writeln('Initial creator: "', ToOEM(fInitialCreator), '"');
  Writeln('Creator: "',         ToOEM(fCreator), '"');
  Writeln('Creation date: "',   fCreationDate, '"');
  Writeln('Modif. date: "',     fModifDate, '"');
  Writeln('Keywords: "',        ToOEM(fKeywords), '"');
  Writeln('Language: "',        ToOEM(fLanguage), '"');
  Writeln('UserInfo1: "',       ToOEM(fUserInfo1), '"');
  Writeln('UserInfo2: "',       ToOEM(fUserInfo2), '"');
  Writeln('UserInfo3: "',       ToOEM(fUserInfo3), '"');
  Writeln('UserInfo4: "',       ToOEM(fUserInfo4), '"');
  Writeln('URL: "',             ToOEM(fURL), '"');

  with fEditionTime do
    Writeln('Edition time: ', wHour, ':', wMinute, ':', wSecond);
  Writeln('Edition cycles: ', fEditionCycles);
  Writeln('Tables: ', fTables, ' Images: ', fImages, ' Objects: ', fObjects, ' Pages: ', fPages, ' Paragraphs: ', fParags, ' Words: ', fWords, ' Chars: ', fChars, ' Cells: ', fCells);

  //s:= '<a name="some">Link</a>';
  //Writeln('Between: "', SBetween(s, '"', '"'), '"');

end.

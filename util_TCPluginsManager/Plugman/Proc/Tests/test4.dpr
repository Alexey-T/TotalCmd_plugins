{$apptype console}

uses IniFile;

begin
  SetIniFilename('.\_test.ini');

  SetIniKey('Options', 'Path1', '""C:\some path\filename.txt""');
  Writeln('Key: ', GetIniKey('Options', 'Path1', '-'));
end.

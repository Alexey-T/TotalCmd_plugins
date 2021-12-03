{$apptype console}

uses FPermissions;

procedure Show(const fn: string);
var
  s: string;
begin
  Writeln('File: ', fn);
  if GetFilePermissions(fn, s) then
    begin
    Writeln('Permissions:');
    Writeln(s); 
    end
  else
    Writeln('Cannot get permissions');
  Writeln;
end;

begin
  Show('C:\MSDOS.SYS');
  Show('C:\Documents and Settings');
  Show('C:\Program Files');
  Show('C:\TC');
end.

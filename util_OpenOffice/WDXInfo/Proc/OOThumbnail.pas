unit OOThumbnail;

interface

uses
  SysUtils, Windows;

function GetOOoThumbnail(const FileName: string; SizeX, SizeY: integer): HBitmap;


implementation

uses
  Classes, PNGImage, SProc, FProc,
  UnzipDll, BitmapProc;

function GetOOoThumbnail(const FileName: string; SizeX, SizeY: integer): HBitmap;
var
  Dir, fnPNG: string;
  fPNG: TPNGImage;
  fStream: TMemoryStream;
begin
  Result:= 0;
  Dir:= FGetTempPathOOo;

  if not UnzipSingle(FileName, Dir, ['Thumbnails/thumbnail.png']) then Exit;
  fnPNG:= Dir+'\Thumbnails\thumbnail.png';

  fStream:= TMemoryStream.Create;
  try
    fStream.LoadFromFile(fnPNG);
    fStream.Seek(0, soFromBeginning);
    fPNG:= TPNGImage.Create;
    try
      fPNG.LoadFromStream(fStream);
      fStream.Free;
      fStream:= nil;
      Result:= CopyBitmap(fPNG.Handle, SizeX, SizeY);
    finally
      fPNG.Free;
    end;
  finally
    if Assigned(fStream) then
      fStream.Free;
  end;

  DeleteFile(PChar(fnPNG));
  RemoveDirectory(PChar(Dir+'\Thumbnails'));
end;

end.

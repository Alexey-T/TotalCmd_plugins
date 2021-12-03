unit OOThumbnail;

interface

uses
  SysUtils, Windows;

function GetOOoThumbnail(const FileName: string; SizeX, SizeY: integer): HBitmap;


implementation

uses
  Classes, PNGImage, SProc, FProc,
  UnzipDll, BitmapProc, Graphics;

function GetOOoThumbnail(const FileName: string; SizeX, SizeY: integer): HBitmap;
var
  Dir, fnPNG: string;
  fPNG: TPNGImage;
  b: TBitmap;
begin
  Result:= 0;
  Dir:= FGetTempPathOOo;

  if not UnzipSingle(FileName, Dir, ['Thumbnails/thumbnail.png']) then Exit;
  fnPNG:= Dir+'\Thumbnails\thumbnail.png';

  fPNG:= TPNGImage.Create;
  b:= TBitmap.Create;
  try
    fPNG.LoadFromFile(fnPNG);
    b.Width:= fPng.Width;
    b.Height:= fPng.Height;
    b.Canvas.Brush.Color:= clWhite;
    b.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));
    b.Canvas.Draw(0, 0, fPng);
    Result:= CopyBitmap(b.Handle, SizeX, SizeY);
  finally
    FreeAndNil(b);
    FreeAndNil(fPNG);
  end;

  DeleteFile(PChar(fnPNG));
  RemoveDirectory(PChar(Dir+'\Thumbnails'));
end;

end.

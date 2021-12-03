unit BitmapProc;

interface

uses Windows;

procedure ResizeBitmap(var bmp_image: HBitmap; width, height: integer);
function CopyBitmap(bmp_image: HBitmap; width, height: integer): HBitmap;

procedure ShowBitmap(const fn: string; bmp_image: HBitmap);
procedure ShowBitmap2(const fn: string; bmp_image: HBitmap);

implementation

uses SProc;

//--------------------------------------------
procedure ShowBitmap(const fn: string; bmp_image: HBitmap);
var
  w, h: integer;
  bmpobj: BITMAP;
begin
  if (bmp_image<>0) and (GetObject(bmp_image, SizeOf(bmpobj), @bmpobj)<>0) then
    begin
    w:= bmpobj.bmWidth;
    h:= bmpobj.bmHeight;
    MessageBox(0, PChar(SFormat('Bitmap size: %dx%d', [w, h])), PChar(fn), MB_OK or MB_SETFOREGROUND);
    end;
end;

//--------------------------------------------
procedure ShowBitmap2(const fn: string; bmp_image: HBitmap);
var
  w, h: integer;
  bmpobj: BITMAP;
begin
  if (bmp_image<>0) and (GetObject(bmp_image, SizeOf(bmpobj), @bmpobj)<>0) then
    begin
    w:= bmpobj.bmWidth;
    h:= bmpobj.bmHeight;
    Writeln(SFormat('Bitmap size: %dx%d', [w, h]));
    end;
end;

//--------------------------------------------
procedure ResizeBitmap(var bmp_image: HBitmap; width, height: integer);
var
  w, h: integer;
  bigx, bigy: integer;
  stretchx, stretchy: integer;
  vx: OSVERSIONINFO;
  is_nt: boolean;
  bmpobj: BITMAP;
  bmp_thumbnail, oldbmp_image, oldbmp_thumbnail: HBITMAP;
  maindc, dc_thumbnail, dc_image: HDC;
  pt: TPOINT;
begin
  // check for operating system: Windows 9x does NOT support the HALFTONE stretchblt mode!
  vx.dwOSVersionInfoSize:= SizeOf(vx);
  GetVersionEx(vx);
  is_nt:= vx.dwPlatformId=VER_PLATFORM_WIN32_NT;

  if (bmp_image<>0) and (GetObject(bmp_image, SizeOf(bmpobj), @bmpobj)<>0) then
    begin
    bigx:= bmpobj.bmWidth;
    bigy:= bmpobj.bmHeight;

    // do we need to stretch?
    if ((bigx>=width) or (bigy>=height)) and ((bigx>0) and (bigy>0)) then
      begin
      stretchy:= MulDiv(width, bigy, bigx);
      if (stretchy<=height) then
        begin
        w:= width;
        h:= stretchy;
        if (h<1) then h:= 1;
        end
      else
        begin
        stretchx:= MulDiv(height, bigx, bigy);
        w:= stretchx;
        if (w<1) then w:= 1;
        h:= height;
        end;

      maindc:= GetDC(GetDesktopWindow());
      dc_thumbnail:= CreateCompatibleDC(maindc);
      dc_image:= CreateCompatibleDC(maindc);
      bmp_thumbnail:= CreateCompatibleBitmap(maindc, w, h);
      ReleaseDC(GetDesktopWindow(), maindc);
      oldbmp_image:= HBITMAP(SelectObject(dc_image, bmp_image));
      oldbmp_thumbnail:= HBITMAP(SelectObject(dc_thumbnail, bmp_thumbnail));

      if is_nt then
        begin
        SetStretchBltMode(dc_thumbnail, HALFTONE);
        SetBrushOrgEx(dc_thumbnail, 0, 0, @pt);
        end
      else
        SetStretchBltMode(dc_thumbnail, COLORONCOLOR);

      StretchBlt(dc_thumbnail, 0, 0, w, h, dc_image, 0, 0, bigx, bigy, SRCCOPY);
      SelectObject(dc_image, oldbmp_image);
      SelectObject(dc_thumbnail, oldbmp_thumbnail);
      DeleteDC(dc_image);
      DeleteDC(dc_thumbnail);
      DeleteObject(bmp_image);
      bmp_image:= bmp_thumbnail;
      end;
    end;
end;

//--------------------------------------------
function CopyBitmap(bmp_image: HBitmap; width, height: integer): HBitmap;
var
  w, h: integer;
  bigx, bigy: integer;
  stretchx, stretchy: integer;
  vx: OSVERSIONINFO;
  is_nt: boolean;
  bmpobj: BITMAP;
  bmp_thumbnail, oldbmp_image, oldbmp_thumbnail: HBITMAP;
  maindc, dc_thumbnail, dc_image: HDC;
  pt: TPOINT;
begin
  // check for operating system: Windows 9x does NOT support the HALFTONE stretchblt mode!
  vx.dwOSVersionInfoSize:= SizeOf(vx);
  GetVersionEx(vx);
  is_nt:= vx.dwPlatformId=VER_PLATFORM_WIN32_NT;

  Result:= 0;
  if (bmp_image<>0) and (GetObject(bmp_image, SizeOf(bmpobj), @bmpobj)<>0) then
    begin
    bigx:= bmpobj.bmWidth;
    bigy:= bmpobj.bmHeight;

    w:= bigx;
    h:= bigy;
    // do we need to stretch?
    if ((bigx>=width) or (bigy>=height)) and ((bigx>0) and (bigy>0)) then
      begin
      stretchy:= MulDiv(width, bigy, bigx);
      if (stretchy<=height) then
        begin
        w:= width;
        h:= stretchy;
        if (h<1) then h:= 1;
        end
      else
        begin
        stretchx:= MulDiv(height, bigx, bigy);
        w:= stretchx;
        if (w<1) then w:= 1;
        h:= height;
        end;
      end;

    maindc:= GetDC(GetDesktopWindow());
    dc_thumbnail:= CreateCompatibleDC(maindc);
    dc_image:= CreateCompatibleDC(maindc);
    bmp_thumbnail:= CreateCompatibleBitmap(maindc, w, h);
    ReleaseDC(GetDesktopWindow(), maindc);
    oldbmp_image:= HBITMAP(SelectObject(dc_image, bmp_image));
    oldbmp_thumbnail:= HBITMAP(SelectObject(dc_thumbnail, bmp_thumbnail));

    if is_nt then
      begin
      SetStretchBltMode(dc_thumbnail, HALFTONE);
      SetBrushOrgEx(dc_thumbnail, 0, 0, @pt);
      end
    else
      SetStretchBltMode(dc_thumbnail, COLORONCOLOR);

    StretchBlt(dc_thumbnail, 0, 0, w, h, dc_image, 0, 0, bigx, bigy, SRCCOPY);
    SelectObject(dc_image, oldbmp_image);
    SelectObject(dc_thumbnail, oldbmp_thumbnail);
    DeleteDC(dc_image);
    DeleteDC(dc_thumbnail);
    Result:= bmp_thumbnail;
    end;
end;

end.

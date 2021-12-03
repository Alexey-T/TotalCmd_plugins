unit PNGdemo;

interface

uses
  ShellAPI,Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, ToolWin, ComCtrls, PNGImage,JPEG, StdCtrls;
type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    OpenBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    FilterCombo: TComboBox;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    SaveDialog1: TSaveDialog;
    CalcCheck: TCheckBox;
    procedure OpenFile;
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ComputeBMPJPGsize;//slow
    procedure ComputePNGSize; //veryslow
    procedure FilterComboChange(Sender: TObject);
    function PNGFilterSize(lFilter: integer): integer;
    procedure FormCreate(Sender: TObject);
    procedure CalcCheckClick(Sender: TObject);
  private
  procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
var
   gPNGSaveFilters: TEncodeFilterSet;
procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
         OpenDialog1.FileName := CFileName;
  OpenFile;
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;
procedure TForm1.OpenBtnClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  OpenFile;
end;

procedure TForm1.OpenFile;
var
   lPNG: TPNGImage;
   lJPG: TJPEGImage;
   lI: integer;
   lExt: string;
   lStreamLoaded: boolean;
   lStream: TmemoryStream;
begin
  lExt := ExtractFileExt(OpenDialog1.Filename);
  if length(lExt) > 0 then
    for lI := 1 to length(lExt) do
       lExt[lI] := upcase(lExt[lI]);
  if (lExt ='.BMP')  then begin
     Image1.Picture.Bitmap.LoadFromFile(OpenDialog1.Filename);
  end else if (lExt = '.PNG') then begin
           lStreamLoaded := true;
           lStream := TMemoryStream.Create;
           try
              lStream.LoadFromFile(OpenDialog1.Filename);
              lStream.Seek(0, soFromBeginning);
              lPNG := TPNGImage.Create;
              try
                 lPNG.LoadFromStream(lStream);
                 lStream.Free;
                 lStreamLoaded := false;
                 Image1.Picture.Bitmap.PixelFormat := lPNG.PixelFormat;
                 Image1.Picture.Bitmap.Height := lPNG.Height;
                 Image1.Picture.Bitmap.Width := lPNG.Width;
                 if lPNG.PixelFormat = pf8Bit then Image1.Picture.Bitmap.Palette := lPNG.Palette;
                 Image1.Canvas.Draw(0,0,lPNG);
                 Image1.Picture.Bitmap.PaletteModified := true;
              finally
                     lPNG.Free;
              end;
           finally
                if lStreamLoaded then lStream.Free;
           end;  //try..finally
  end {PNG} else if ('.JPG'= lExt) or  ('.JPG'= lExt) then begin
           lStreamLoaded := true;
           lStream := TMemoryStream.Create;
           try
              lStream.LoadFromFile(OpenDialog1.Filename);
              lStream.Seek(0, soFromBeginning);
              lJpg := TJPEGImage.Create;
              try
                 lJpg.LoadFromStream(lStream);
                 lStream.Free;
                 lStreamLoaded := false;
                 Image1.Picture.Bitmap.PixelFormat := pf24bit;
                 Image1.Picture.Bitmap.Height := lJpg.Height;
                 Image1.Picture.Bitmap.Width := lJpg.Width;
                 Image1.Canvas.Draw(0,0,lJpg);
              finally
                     lJPG.Free;
              end;
           finally
                  if lStreamLoaded then lStream.Free;
           end; //try..finally
        end {JPG} else
            showmessage('Unknown file extension: what is the format of this image?');
  Image1.Height := Image1.Picture.Height;
  Image1.Width := Image1.Picture.Width;
  Image1.refresh;
  StatusBar1.Panels[0].text := '';
  StatusBar1.Panels[1].text := '';
  StatusBar1.Panels[2].text := '';
  if CalcCheck.checked then begin
     ComputeBMPJPGsize;//slow: remove for a speedup
     ComputePNGSize;  //very slow: remove for drastic speedup
  end;
end;

procedure TForm1.SaveBtnClick(Sender: TObject);
var
lFilter: integer;
lJPG:  TJPEGImage;
begin
     if not SaveDialog1.Execute then exit;
     lFilter := SaveDialog1.FilterIndex;
     if Image1.Picture.Graphic = nil then begin
            showmessage('There is no image currently loaded to save.');
            exit;
     end;
     if {('.JPG'= lExt) or (lExt='.JPEG')}lFilter = 3 then begin
           lJPG := TJPEGImage.Create;
           TRY
              lJPG.CompressionQuality := 80;
              lJPG.Assign(Image1.Picture.Bitmap);
              lJPG.SaveToFile(ChangeFileExt(SaveDialog1.FileName,'.jpg'));
           FINALLY
               lJPG.Free
           END;
     end else if  {('.PNG'= lExt)}lFilter =1 then begin
         with TPNGImage.Create do begin
           //filters(efNone, efSub, efUp, efAverage, efPaeth);
           if (not CalcCheck.checked) and (FilterCombo.ItemIndex=5) then
              ComputePNGSize; //compute optimum compression technique
           Filter := gPNGSaveFilters;
           Assign(Image1.Picture.Graphic);
           SaveToFile(ChangeFileExt(SaveDialog1.FileName,'.png'));
           free;
         end;
   end else
  	Image1.Picture.Bitmap.SaveToFile( ChangeFileExt(SaveDialog1.FileName,'.bmp' ));
end;

procedure TForm1.ComputeBMPJPGsize;//slow
var
   lStream : TMemoryStream;
   lJPG : TJPEGImage;
begin
  //1st: BMP
  lStream := TMemoryStream.Create;
  try
     Image1.Picture.Bitmap.SaveToStream(lStream);
     StatusBar1.Panels[0].text := 'BMP '+ inttostr(lStream.Size);
  finally
         lStream.Free;
  end;
  //2nd: JPEG size [quality = 80]
  lJPG := TJPEGImage.Create;
  try
     lJPG.CompressionQuality := 80;
     lJPG.Assign(Image1.Picture.Bitmap);
     lStream := TMemoryStream.Create;
     try
        lJPG.SaveToStream(lStream);
        StatusBar1.Panels[1].text := 'JPG[80] '+ inttostr(lStream.Size);
     finally
            lStream.Free;
     end;
  finally
         lJPG.Free;
  end;
end;//computeBMPJPGsize

function TForm1.PNGFilterSize(lFilter: integer): integer;
var
   lStream: TMemoryStream;
begin
  result := 0;
  if Image1.Picture.Graphic = nil then exit;
  case lFilter of
       1: gPNGSaveFilters := [efSub];
       2: gPNGSaveFilters := [efUp];
       3: gPNGSaveFilters := [efAverage];
       4: gPNGSaveFilters := [efPaeth];//Include(SaveFilters, efPaeth);
       else gPNGSaveFilters := [efNone];//[efNone,efSub,efUp,efAverage,efPaeth];
  end;
  lStream := TMemoryStream.Create;
  try
     with TPNGImage.Create do begin
          Filter := gPNGSaveFilters;
          Assign(Image1.Picture.Graphic);
          SaveToStream(lStream);
          result := (lStream.Size);
     end;
  finally
         lStream.Free;
  end; //Stream TRY..FINALLY
end; //PNGfiltersize

procedure TForm1.FilterComboChange(Sender: TObject);
begin
 if not CalcCheck.Checked then exit;
 ComputePNGSize;
end;
procedure TForm1.ComputePNGSize;
var
   lStr: string;
   lFilter,lSz,lMinFilter,lMinFilterSz: integer;
   lOptimize: boolean;
begin
  lFilter :=FilterCombo.ItemIndex;
  if lFilter = 5 then lOptimize := true
  else lOptimize := false;
  if Image1.Picture.Graphic = nil then begin
     StatusBar1.Panels[2].text := '';
     exit;
  end;
  //Next: Compute PNG size
  if lOptimize then begin
     lMinFilter := 0;
     lMinFilterSz := PNGFilterSize(0);
     for lFilter := 1 to 4 do begin
         Application.ProcessMessages;
         lSz := PNGFilterSize(lFilter);
         if lSz < lMinFilterSz then begin
            lMinFilter := lFilter;
            lMinFilterSz := lSz;
         end;
     end;
     case lMinFilter of
          1: lStr := 'Sub';
          2: lStr := 'Up';
          3:lStr := 'Average';
          4: lStr := 'Paeth';
          else {0} lStr := 'None';
     end;
     case lMinFilter of
                1: gPNGSaveFilters := [efSub];
                2: gPNGSaveFilters := [efUp];
                3: gPNGSaveFilters := [efAverage];
                4: gPNGSaveFilters := [efPaeth];
                else gPNGSaveFilters := [efNone];//[efNone,efSub,efUp,efAverage,efPaeth];
     end;
     StatusBar1.Panels[2].text := 'PNG ['+lStr+'] '+inttostr(lMinFilterSz)+' bytes';
  end else begin
     lSz := PNGFilterSize(lFilter);
     StatusBar1.Panels[2].text := 'PNG '+ inttostr(lSz)+' bytes';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 FilterCombo.itemindex := 0;
 gPNGSaveFilters := [efNone];
 DragAcceptFiles(Handle, True);
end;

procedure TForm1.CalcCheckClick(Sender: TObject);
begin
     StatusBar1.Panels[0].text := '';
     StatusBar1.Panels[1].text := '';
     StatusBar1.Panels[2].text := '';
     if Image1.Picture.Graphic = nil then exit;
     if CalcCheck.Checked then begin
        ComputeBMPJPGSize;
        ComputePNGSize;
    end;
end;

end.

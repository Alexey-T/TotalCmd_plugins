unit UFormDetect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormDetect = class(TForm)
    btnDetect: TButton;
    btnCancel: TButton;
    btnOk: TButton;
    Label1: TLabel;
    ffDetect: TEdit;
    ffFormat1: TEdit;
    Label2: TLabel;
    btnFormat1: TButton;
    Label3: TLabel;
    ffFormat2: TEdit;
    btnFormat2: TButton;
    Label4: TLabel;
    ffFormat3: TEdit;
    btnFormat3: TButton;
    Label5: TLabel;
    ffFormat4: TEdit;
    btnFormat4: TButton;
    Label6: TLabel;
    ffFormat5: TEdit;
    btnFormat5: TButton;
    Label7: TLabel;
    ffFormat6: TEdit;
    btnFormat6: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label8: TLabel;
    ffFormat7: TEdit;
    btnFormat7: TButton;
    Label9: TLabel;
    ffFormat8: TEdit;
    btnFormat8: TButton;
    procedure btnDetectClick(Sender: TObject);
    procedure btnFormat1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PluginNum: integer;
    FormatStr: string;
    function WDXGetFormatLine(Sender: TButton; PluginNum: integer): string;
  end;

//var
//  FormDetect: TFormDetect;

implementation

uses ContPlug, SProc, DescPlugin;

{$R *.DFM}

function WDXGetDetectLine(PluginNum: integer): string;
var
  buf1: array[0..5*1024] of char;
  s, ss: string;
  n, pos1, pos2: integer;
begin
  Result:= '';
  with DescPlugins[PluginNum] do
    if fType=TypeWDX then
      with fRecordWDX do
        if Assigned(fContentGetDetectString) then
          begin
          FillChar(buf1, SizeOf(buf1), 0);
          fContentGetDetectString(buf1, SizeOf(buf1));
          s:= buf1;
          //Application.MessageBox(PChar(s), 'Detect', MB_OK);
          repeat
            n:= Pos('EXT=', s); if n=0 then Break;
            pos1:= PosFrom('"', s, n); if pos1=0 then Break;
            pos2:= PosFrom('"', s, pos1+1); if pos2=0 then Break;
            ss:= '*.'+Copy(s, pos1+1, pos2-pos1-1);
            if Result=''
              then Result:= ss
              else Result:= Result+','+ss;
            Delete(s, 1, pos2);
          until false;
          end;
end;

procedure TFormDetect.btnDetectClick(Sender: TObject);
begin
  ffDetect.Text:= WDXGetDetectLine(PluginNum);
end;

function TFormDetect.WDXGetFormatLine(Sender: TButton; PluginNum: integer): string;
var
  i, n: integer;
  hMenu: THandle;
begin
  Result:= '';
  with DescPlugins[PluginNum] do
    if fType=TypeWDX then
      with fRecordWDX do
        begin
        //create
        hMenu:= CreatePopupMenu;
        for i:= Low(fFields) to High(fFields) do
          if (fFields[i].fName<>'') and (fFields[i].fType<>ft_fulltext) then
            AppendMenu(hMenu, MF_STRING or MF_UNCHECKED, 100+i,
              PChar(fFields[i].fName));

        n:= integer(TrackPopupMenu(hMenu, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RETURNCMD,
          ClientOrigin.X+Sender.Left,
          ClientOrigin.Y+Sender.Top,
          0, Handle, nil))-100;
        SendMessage(Handle, WM_NULL, 0, 0);

        //free
        for i:= High(fFields) downto Low(fFields) do
          if fFields[i].fName<>'' then
            DeleteMenu(hMenu, 100+i, MF_BYCOMMAND);
        DestroyMenu(hMenu);

        if n>=0 then
          Result:= '['+fFields[n].fName+']';
        end;
end;


procedure TFormDetect.btnFormat1Click(Sender: TObject);
var
  s: string;
  Edit: TEdit;
begin
  if Sender=btnFormat1 then Edit:= ffFormat1 else
   if Sender=btnFormat2 then Edit:= ffFormat2 else
    if Sender=btnFormat3 then Edit:= ffFormat3 else
     if Sender=btnFormat4 then Edit:= ffFormat4 else
      if Sender=btnFormat5 then Edit:= ffFormat5 else
       if Sender=btnFormat6 then Edit:= ffFormat6 else
        if Sender=btnFormat7 then Edit:= ffFormat7 else
         if Sender=btnFormat8 then Edit:= ffFormat8 else
          Exit;

  s:= WDXGetFormatLine(TButton(Sender), PluginNum);
  if s<>'' then
    if Edit.Text=''
      then Edit.Text:= s
      else Edit.Text:= Edit.Text+' '+s;
end;

function SGetStr(var s: string): string;
var
  k: integer;
begin
  k:= Pos('\n', s); if k=0 then k:= MaxInt-1;
  Result:= Copy(s, 1, k-1);
  Delete(s, 1, k+1);
end;

procedure TFormDetect.FormShow(Sender: TObject);
var
  s: string;
begin
  s:= FormatStr;
  ffFormat1.Text:= SGetStr(s);
  ffFormat2.Text:= SGetStr(s);
  ffFormat3.Text:= SGetStr(s);
  ffFormat4.Text:= SGetStr(s);
  ffFormat5.Text:= SGetStr(s);
  ffFormat6.Text:= SGetStr(s);
  ffFormat7.Text:= SGetStr(s);
  ffFormat8.Text:= SGetStr(s);
end;

procedure TFormDetect.btnOkClick(Sender: TObject);
begin
  FormatStr:=
    ffFormat1.Text+'\n'+
    ffFormat2.Text+'\n'+
    ffFormat3.Text+'\n'+
    ffFormat4.Text+'\n'+
    ffFormat5.Text+'\n'+
    ffFormat6.Text+'\n'+
    ffFormat7.Text+'\n'+
    ffFormat8.Text+'\n';
end;

end.

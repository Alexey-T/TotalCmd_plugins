unit UFormTweak;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tcProc;

type
  TFormTweak = class(TForm)
    Panel1: TGroupBox;
    ffFile: TEdit;
    ffExt: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    ffFlag0: TCheckBox;
    ffFlag1: TCheckBox;
    ffFlag2: TCheckBox;
    ffFlag3: TCheckBox;
    ffFlag4: TCheckBox;
    ffFlag5: TCheckBox;
    ffFlag6: TCheckBox;
    ffFlag7: TCheckBox;
    ffFlag8: TCheckBox;
    ffFlag9: TCheckBox;
    LabelFlags: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnChange: TButton;  //Led
    btnDefault: TButton;
    btnSet: TButton;
    procedure ffFlag0Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ffExtChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject); //Led
    procedure btnDefaultClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
  private
    { Private declarations }
    Flags: TPackerFlags;
  public
    { Public declarations }
    procedure SetFN(const fn: string);
  end;

//var
//  FormTweak: TFormTweak;

implementation

uses
  Msg, MsgFile, UFormAddExt;

{$R *.DFM}

procedure TFormTweak.ffFlag0Click(Sender: TObject);
var
  n: integer;
begin
  n:= (integer(ffFlag0.Checked)*1)+
      (integer(ffFlag1.Checked)*2)+
      (integer(ffFlag2.Checked)*4)+
      (integer(ffFlag3.Checked)*8)+
      (integer(ffFlag4.Checked)*16)+
      (integer(ffFlag5.Checked)*32)+
      (integer(ffFlag6.Checked)*64)+
      (integer(ffFlag7.Checked)*128)+
      (integer(ffFlag8.Checked)*256)+
      (integer(ffFlag9.Checked)*512);

  LabelFlags.Caption:= Format('(%d)', [n]);
  with Flags do
   if sel>=0 then
    ext[sel].flag:= n;
end;

procedure TFormTweak.ffExtChange(Sender: TObject);
var
  n: integer;
  En: boolean;
begin
  with Flags do
    begin
    sel:= ffExt.ItemIndex;
    if sel>=0
      then n:= ext[sel].flag
      else n:= 0;
    end;

  ffFlag0.Checked:= (n and 1)>0;
  ffFlag1.Checked:= (n and 2)>0;
  ffFlag2.Checked:= (n and 4)>0;
  ffFlag3.Checked:= (n and 8)>0;
  ffFlag4.Checked:= (n and 16)>0;
  ffFlag5.Checked:= (n and 32)>0;
  ffFlag6.Checked:= (n and 64)>0;
  ffFlag7.Checked:= (n and 128)>0;
  ffFlag8.Checked:= (n and 256)>0;
  ffFlag9.Checked:= (n and 512)>0;
  ffFlag0Click(Self);

  En:= Flags.num>0;
  ffFlag0.Enabled:= En;
  ffFlag1.Enabled:= En;
  ffFlag2.Enabled:= En;
  ffFlag3.Enabled:= En;
  ffFlag4.Enabled:= En;
  ffFlag5.Enabled:= En;
  ffFlag6.Enabled:= En;
  ffFlag7.Enabled:= En;
  ffFlag8.Enabled:= En;
  ffFlag9.Enabled:= En;

  btnRemove.Enabled:= En and (ffExt.Items.Count>1);
  btnChange.Enabled:= En;
  btnDefault.Enabled:= En;
  btnSet.Enabled:= En;
end;

procedure TFormTweak.FormCreate(Sender: TObject);
begin
  FillChar(Flags, SizeOf(Flags), 0);
end;

procedure TFormTweak.SetFN(const fn: string);
begin
  ffFile.Text:= fn;
  tcReadPackerFlags(fn, Flags);
end;


procedure TFormTweak.FormShow(Sender: TObject);
var
  i: integer;
begin
  {$I Init_UFormTweak.pas}

  ffExt.Items.BeginUpdate;
  ffExt.Items.Clear;
  for i:= 0 to Flags.num-1 do
    ffExt.Items.Add(Flags.ext[i].str);
  ffExt.Items.EndUpdate;

  if Flags.sel>=0
    then ffExt.ItemIndex:= Flags.sel
    else ffExt.ItemIndex:= -1;
  ffExtChange(Self);

  SetForegroundWindow(Handle);
end;

procedure TFormTweak.btnRemoveClick(Sender: TObject);
var
  i: integer;
begin
  with Flags do
    if num>0 then
      begin
      Dec(num);
      for i:= sel to num-1 do
        begin
        ext[i].str:= ext[i+1].str;
        ext[i].flag:= ext[i+1].flag;
        end;
      if sel>num-1 then Dec(sel);  
      end;
  FormShow(Self);    
end;

procedure TFormTweak.btnAddClick(Sender: TObject);
var
  s, sName: string;
begin
  s:= '';
  with TFormAddExt.Create(nil) do
    try
      Caption:= Self.btnAdd.Caption;
      Label1.Caption:= Self.Label3.Caption;
      ffDisableEmpty:= true;
      ffExt.MaxLength:= cMaxPackerExtLen;
      if ShowModal=mrOk then
        s:= Trim(ffExt.Text);
    finally
      Release;
    end;

  //Check is entered extension duplicate:
  if tcPackerExtensionPresent(s, sName) then
    if not MsgConfirmed(Format(S0078, [s, sName])) then
      s:='';

  if s<>'' then
    with Flags do
      begin
      Inc(num);
      sel:= num-1;
      ext[sel].str:= s;
      ext[sel].flag:= flagDefault;
      FormShow(Self);
      end;
end;

procedure TFormTweak.btnChangeClick(Sender: TObject);  //Led
var
  s, sName: string;
begin
  s:= '';
  with TFormAddExt.Create(nil) do
    try
      Caption:= Self.btnChange.Caption;
      Label1.Caption:= Self.Label3.Caption;
      ffDisableEmpty:= true;
      ffExt.MaxLength:= cMaxPackerExtLen;
      ffExt.Text:= flags.ext[flags.sel].str;
      if ShowModal=mrOk then
        s:= Trim(ffExt.Text);
    finally
      Release;
    end;

  //Check is entered extension duplicate:
  if tcPackerExtensionPresent(s, sName) then
    if not MsgConfirmed(Format(S0078, [s, sName])) then
      s:='';

  if s<>'' then
    begin
    flags.ext[flags.sel].str:= s;
    FormShow(Self);
    end;
end;

procedure TFormTweak.btnDefaultClick(Sender: TObject);
begin
  with Flags do
    if sel>=0 then
      begin
      ext[sel].flag:= flagDefault;
      FormShow(Self);
      end;
end;

procedure TFormTweak.btnOkClick(Sender: TObject);
begin
  tcWritePackerFlags(ffFile.Text, Flags);
end;

procedure TFormTweak.btnSetClick(Sender: TObject);
var
  n: integer;
begin
  n:= Flags.flagDefault;
  with Flags do
   if sel>=0 then
    n:= ext[sel].flag;

  n:= StrToIntDef(InputBox(Caption, Label4.Caption, IntToStr(n)), n);
  LabelFlags.Caption:= Format('(%d)', [n]);
  with Flags do
   if sel>=0 then
    ext[sel].flag:= n;

  ffExtChange(Self);
end;

end.

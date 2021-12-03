{
fLister, fContent:
  reordering of LibList[i].fIndex values,
  then writing section with new indexes.
fPacker:
  reordering of ListView1.Items values,
  then writing section with new order.
Drag-drop enabled only for Packer.  
}
unit UFormOrder;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, Buttons, tcProc;

type
  TFormOrder = class(TForm)
    ListView1: TListView;
    Label1: TLabel;
    ImageList1: TImageList;
    btnOk: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOkClick(Sender: TObject);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
    procedure LoadOptions;
    procedure SaveOptions;
    procedure SwapItems(n1, n2: integer);
    procedure ListSelect(n: integer);
    procedure UpdateList;
    procedure SaveList;
  public
    { Public declarations }
    FOrderFileName: string;
    FOrderType: TLibType;
  end;

var
  FormOrder: TFormOrder;

implementation

uses SProc, IniProc, Msg, MsgFile;

{$R *.DFM}

//swap indexes of two plugins in LibList
procedure Swap(n1, n2: integer);
var
  n: integer;
begin
  n:= libList[n1].fIndex;
  libList[n1].fIndex:= libList[n2].fIndex;
  libList[n2].fIndex:= n;
end;

//swap items in TListView
procedure TFormOrder.SwapItems(n1, n2: integer);
var
  s: string;
begin
  with ListView1 do
    begin
    Items.BeginUpdate;
    s:= Items[n1].Caption;
    Items[n1].Caption:= Items[n2].Caption;
    Items[n2].Caption:= s;
    s:= Items[n1].SubItems[0];
    Items[n1].SubItems[0]:= Items[n2].SubItems[0];
    Items[n2].SubItems[0]:= s;
    s:= Items[n1].SubItems[1];
    Items[n1].SubItems[1]:= Items[n2].SubItems[1];
    Items[n2].SubItems[1]:= s;
    Items.EndUpdate;
    end;
end;

procedure TFormOrder.ListSelect(n: integer);
begin
  with ListView1 do
    if (n>=0) and (n<=Items.Count-1) then
      begin
      Selected:= Items[n];
      ItemFocused:= Selected;
      Selected.MakeVisible(false);
      end;
end;


procedure TFormOrder.UpdateList;
var
  Keys: TStringList;
  skey, sval, sfn: string;
  section: string;
  i, j, top_num: integer;
begin
  with ListView1 do
    begin
    if TopItem=nil                  //Led
      then top_num:=0               //Led
      else top_num:= TopItem.Index; //Led
  
    Items.BeginUpdate;
    Items.Clear;

    case FOrderType of
      fLister,
      fContent:
        for j:= 0 to LibNum-1 do
          for i:= 1 to LibNum do
            with LibList[i] do
              if (fType=FOrderType) and (not fDisabled) and (fIndex=j) then
                with Items.Add do
                  begin
                  Data:= pointer(i);
                  ImageIndex:= integer(fType=fContent);
                  Caption:= Format('%d:  %s', [fIndex, title]);
                  SubItems.Add(params);
                  end;

      fPacker:
        begin
        Keys:= TStringList.Create;
        try
          section:= cLibSections[FOrderType];
          tcReadKeys(section, Keys);
          for i:= 0 to Keys.Count-1 do
            with Items.Add do
              begin
              skey:= Keys[i];
              sval:= tcOption(section, skey);
              sfn:= tcOptionPath(section, skey);
              Data:= nil;
              ImageIndex:= 2;
              Caption:= LibDesc(sfn);
              SubItems.Add(skey);
              SubItems.Add(sval);
              end;
        finally
          Keys.Free;
        end;
        end;
    end;

    Items.EndUpdate;
    Enabled:= Items.Count>0;

    if (Items.Count>VisibleRowCount) and (top_num<>0)  //Led
      then ListView1.Scroll(0, 5+17*top_num);          //Led
    end;

  ActiveControl:= ListView1;

  if not Assigned(ListView1.Selected) then
    ListSelect(0);
end;

procedure TFormOrder.FormShow(Sender: TObject);
begin
  LoadOptions;
  {$I Init_UFormOrder.pas}
  tcSearchPlugins(true);
  UpdateList;

  with ListView1 do
    if FOrderType = fPacker then
      begin
      DragMode:= dmAutomatic;
      MultiSelect:= True;
      end;
end;


procedure TFormOrder.btnUpClick(Sender: TObject);
var
  n1, n2: integer;
begin
  with ListView1 do
    if (Selected<>nil) and (Selected.Index>0) then
      case FOrderType of
        fLister, fContent:
          begin
          n1:= integer(Selected.Data);
          n2:= integer(Items[Selected.Index-1].Data);
          Swap(n1, n2);
          UpdateList;
          ListSelect(LibList[n1].fIndex);
          end;
        fPacker:
          begin
          SwapItems(Selected.Index, Selected.Index-1);
          ListSelect(Selected.Index-1);
          end;
      end;
end;

procedure TFormOrder.btnDownClick(Sender: TObject);
var
  n1, n2: integer;
begin
  with ListView1 do
    if (Selected<>nil) and (Selected.Index<Items.Count-1) then
      case FOrderType of
        fLister, fContent:
          begin
          n1:= integer(Selected.Data);
          n2:= integer(Items[Selected.Index+1].Data);
          Swap(n1, n2);
          UpdateList;
          ListSelect(LibList[n1].fIndex);
          end;
        fPacker:
          begin
          SwapItems(Selected.Index, Selected.Index+1);
          ListSelect(Selected.Index+1);
          end;
      end;
end;


procedure TFormOrder.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  with ListView1 do
    begin
    btnUp.Enabled:= (Selected<>nil) and (Selected.Index>0);
    btnDown.Enabled:= (Selected<>nil) and (Selected.Index<Items.Count-1);
    end;
end;


procedure TFormOrder.LoadOptions;
var
  i: integer;
begin
  Left:= GetIniKey(FOrderFileName, 'OrderWindow', 'Left', Left);
  Top:= GetIniKey(FOrderFileName, 'OrderWindow', 'Top', Top);
  Width:= GetIniKey(FOrderFileName, 'OrderWindow', 'Width', Width);
  Height:= GetIniKey(FOrderFileName, 'OrderWindow', 'Height', Height);

  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      Columns[i].Width:= GetIniKey(FOrderFileName, 'OrderWindow', Format('Column%dW', [i]), Columns[i].Width);
end;

procedure TFormOrder.SaveOptions;
var
  i: integer;
begin
  SetIniKey(FOrderFileName, 'OrderWindow', 'Left', Left);
  SetIniKey(FOrderFileName, 'OrderWindow', 'Top', Top);
  SetIniKey(FOrderFileName, 'OrderWindow', 'Width', Width);
  SetIniKey(FOrderFileName, 'OrderWindow', 'Height', Height);

  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      SetIniKey(FOrderFileName, 'OrderWindow', Format('Column%dW', [i]), Columns[i].Width);
end;

procedure TFormOrder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveOptions;
end;


//Requested by Led <radioled@mail.ru>: form must not update list on
//every Up/Down press, only on Ok press.
procedure TFormOrder.SaveList;
var
  i, n: integer;
  section: string;
begin
  section:= cLibSections[FOrderType];

  case FOrderType of
    fLister, fContent:
      begin
      //delete all in section
      for i:= 0 to libNum-1 do
        begin
        tcOptionDel(section, LSKey(i), false);
        tcOptionDel(section, LSKeyD(i), false);
        end;

      //write with new order
      for i:= 0 to libNum-1 do
       for n:= 1 to libNum do
        with libList[n] do
          if (fType=FOrderType) and (not fDisabled) and (fIndex=i) then
            begin
            //MsgInfo(Format('N%d I%d %s', [n, fIndex, title]));
            tcOptionSetWithDetect(section, fIndex, fn, params);
            end;
      end;

    fPacker:
      begin
      //delete all in section
      with ListView1 do
       for i:= 0 to Items.Count-1 do
        with Items[i] do
         tcOptionDel(section, SubItems[0], false);

      //write with new order
      with ListView1 do
       for i:= 0 to Items.Count-1 do
        with Items[i] do
         tcOptionSet(section, SubItems[0], SubItems[1]);
      end;
  end;
end;


procedure TFormOrder.btnOkClick(Sender: TObject);
begin
  SaveList;
end;

procedure TFormOrder.ListView1DragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:= Sender = ListView1;
end;

//DRKB code
procedure TFormOrder.ListView1DragDrop(Sender, Source: TObject;
  X, Y: Integer);
var 
  DragItem, DropItem, CurrentItem, NextItem: TListItem;
begin
  if Sender = Source then
   with TListView(Sender) do
   begin
     DropItem    := GetItemAt(X, Y);
     CurrentItem := Selected;
     while CurrentItem <> nil do
     begin
       NextItem := GetNextItem(CurrentItem, SdAll, [IsSelected]);
       if DropItem = nil then DragItem := Items.Add
       else
         DragItem := Items.Insert(DropItem.Index);
       DragItem.Assign(CurrentItem);
       CurrentItem.Free;
       CurrentItem := NextItem;
     end;
   end;
end;


end.

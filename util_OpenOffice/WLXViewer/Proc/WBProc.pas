//These functions are from:
(**************************************************************)
(*                                                            *)
(*  TWebbrowser functions by toms                             *)
(*  Version 1.9                                               *)
(*  E-Mail: tom@swissdelphicenter.ch                          *)
(*                                                            *)
(*  Contributors: www.swissdelphicenter.ch                    *)
(*                                                            *)
(**************************************************************)

unit WBProc;

interface

uses SHDocVw;

procedure WB_SetFocus(WB: TWebbrowser);
procedure WB_Set3DBorderStyle(WB: TWebBrowser; bValue: Boolean);
procedure WB_Copy(WB: TWebbrowser);
procedure WB_SelectAll(WB: TWebbrowser);
procedure WB_ShowPrintDialog(WB: TWebbrowser);
procedure WB_ShowPrintPreview(WB: TWebbrowser);
procedure WB_ShowPageSetup(WB: TWebbrowser);
procedure WB_ShowFindDialog(WB: TWebbrowser);

implementation

uses
  Windows, SysUtils, Variants,
  ActiveX, MSHTML, Dialogs;

function InvokeCMD(WB: TWebbrowser; nCmdID: DWORD): Boolean; overload; forward;
function InvokeCMD(WB: TWebbrowser; InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): Boolean; overload; forward;

const
  CGID_WebBrowser: TGUID = '{ED016940-BD5B-11cf-BA4E-00C04FD70816}';
  HTMLID_FIND = 1;

function InvokeCMD(WB: TWebbrowser; nCmdID: DWORD): Boolean;
var
  vaIn, vaOut: OleVariant;
begin
  Result := InvokeCMD(WB, True, nCmdID, unassigned, vaIn, vaOut);
end;

function InvokeCMD(WB: TWebbrowser; InvokeIE: Boolean; Value1, Value2: Integer; var vaIn, vaOut: OleVariant): Boolean;
var
  CmdTarget: IOleCommandTarget;
  PtrGUID: PGUID;
begin
  Result:= False;
  New(PtrGUID);
  if InvokeIE then
    PtrGUID^ := CGID_WebBrowser
  else
    PtrGuid := PGUID(nil);
  if WB.ControlInterface.Document <> nil then
  try
    WB.ControlInterface.Document.QueryInterface(IOleCommandTarget, CmdTarget);
    if CmdTarget <> nil then
    try
      CmdTarget.Exec(PtrGuid, Value1, Value2, vaIn, vaOut);
      Result:= True;
    finally
      CmdTarget._Release;
    end;
  except end;
  Dispose(PtrGUID);
end;

function WB_DocumentLoaded(WB: TWebbrowser): Boolean;
var
  iDoc: IHtmlDocument2;
begin
  Result := False;
  if Assigned(WB) then
  begin
    if WB.ControlInterface.Document <> nil then
    begin
      WB.ControlInterface.Document.QueryInterface(IHtmlDocument2, iDoc);
      Result := Assigned(iDoc);
    end;
  end;
end;

procedure WB_SetFocus(WB: TWebbrowser);
begin
  try
    if WB_DocumentLoaded(WB) then
      (WB.ControlInterface.Document as IHTMLDocument2).ParentWindow.Focus;
  except
  end;
end;

procedure WB_Set3DBorderStyle(WB: TWebBrowser; bValue: Boolean);
{
  bValue: True: Show a 3D border style
          False: Show no border
}
var
  Document: IHTMLDocument2;
  Element: IHTMLElement;
  StrBorderStyle: string;
begin
  if Assigned(WB) then
  try
    Document := WB.ControlInterface.Document as IHTMLDocument2;
    if Assigned(Document) then
    begin
      Element := Document.Body;
      if Element <> nil then
      begin
        case bValue of
          False: StrBorderStyle := 'none';
          True: StrBorderStyle := '';
        end;
        Element.Style.BorderStyle := StrBorderStyle;
      end;
    end;
  except
  end;
end;

procedure WB_Copy(WB: TWebbrowser);
var
  vaIn, vaOut: Olevariant;
begin
  InvokeCmd(WB, FALSE, OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

procedure WB_SelectAll(WB: TWebbrowser);
var
  vaIn, vaOut: Olevariant;
begin
  InvokeCmd(WB, FALSE, OLECMDID_SELECTALL, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

procedure WB_ShowPrintDialog(WB: TWebbrowser);
var
  OleCommandTarget: IOleCommandTarget;
  Command: TOleCmd;
  Success: Integer;
  Emp: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  begin
    WB.ControlInterface.Document.QueryInterface(IOleCommandTarget, OleCommandTarget);
    Command.cmdID := OLECMDID_PRINT;
    if OleCommandTarget.QueryStatus(nil, 1, @Command, nil) <> S_OK then
    begin
      // ShowMessage('Nothing to print');
      Exit;
    end;
    if (Command.cmdf and OLECMDF_ENABLED) <> 0 then
    begin
      Emp:= EmptyParam;
      Success := OleCommandTarget.Exec(nil,
        OLECMDID_PRINT,
        OLECMDEXECOPT_PROMPTUSER,
        Emp,
        Emp);
      case Success of
        S_OK: ;
        OLECMDERR_E_CANCELED: ShowMessage('Canceled by user');
      else ShowMessage('Error while printing');
      end;
    end
    else
   // ShowMessage('Printing not possible');
  end;
end;

procedure WB_ShowPrintPreview(WB: TWebbrowser);
var
  vaIn, vaOut: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  try
    // Execute the print preview command.
    WB.ControlInterface.ExecWB(OLECMDID_PRINTPREVIEW,
      OLECMDEXECOPT_DONTPROMPTUSER, vaIn, vaOut);
  except
  end;
end;

procedure WB_ShowPageSetup(WB: TWebbrowser);
var
  vaIn, vaOut: OleVariant;
begin
  if WB_DocumentLoaded(WB) then
  try
    // Execute the page setup command.
    WB.ControlInterface.ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_PROMPTUSER,
      vaIn, vaOut);
  except
  end;
end;

procedure WB_ShowFindDialog(WB: TWebbrowser);
begin
  InvokeCMD(WB, HTMLID_FIND);
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.

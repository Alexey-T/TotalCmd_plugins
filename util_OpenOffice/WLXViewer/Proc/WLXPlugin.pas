// This unit is written by Christian Ghisler, it's from Total Commander's
// Lister API Guide, that can be found at http://ghisler.com.
// API version: 1.50.

unit WLXPlugin;

interface

uses Windows;

const
  lc_copy=1;
  lc_newparams=2;
  lc_selectall=3;
  lc_setpercent=4;
  lcp_wraptext=1;
  lcp_fittowindow=2;
  lcp_ansi=4;
  lcp_ascii=8;
  lcp_variable=12;
  lcp_forceshow=16;
  lcs_findfirst=1;
  lcs_matchcase=2;
  lcs_wholewords=4;
  lcs_backwards=8;
  itm_percent=$FFFE;
  itm_fontstyle=$FFFD;
  itm_wrap=$FFFC;
  itm_fit=$FFFB;
  itm_next=$FFFA;
  LISTPLUGIN_OK=0;
  LISTPLUGIN_ERROR=1;

type
  tListDefaultParamStruct = record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: DWORD;
    DefaultIniName: array[0..MAX_PATH-1] of char;
  end;
  pListDefaultParamStruct=^tListDefaultParamStruct;

{ Function prototypes: Functions need to be defined exactly like this!}
{
function ListLoad(ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle; stdcall;
procedure ListCloseWindow(ListWin:thandle); stdcall;
procedure ListGetDetectString(DetectString:pchar;maxlen:integer); stdcall;
function ListSearchText(ListWin:thandle;SearchString:pchar;
                        SearchParameter:integer):integer; stdcall;
function ListSendCommand(ListWin:thandle;Command,Parameter:integer):integer; stdcall;
function ListPrint(ListWin:thandle;FileToPrint,DefPrinter:pchar;
                   PrintFlags:integer;var Margins:trect):integer; stdcall;
function ListNotificationReceived(ListWin:thandle;Message,wParam,lParam:integer):integer; stdcall;
procedure ListSetDefaultParams(dps:pListDefaultParamStruct); stdcall;
}

implementation

end.

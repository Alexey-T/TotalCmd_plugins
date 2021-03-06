//Interface version 1.2
unit ContPlug;

interface

uses Windows;

const 
  FT_NOMOREFIELDS = 0;
  FT_NUMERIC_32 = 1;
  FT_NUMERIC_64 = 2;
  FT_NUMERIC_FLOATING = 3;
  FT_DATE = 4;
  FT_TIME = 5;
  FT_BOOLEAN = 6;
  FT_MULTIPLECHOICE = 7;
  FT_STRING = 8;
  FT_FULLTEXT = 9;
  FT_DATETIME = 10;
  FT_STRINGW = 11;
  FT_FULLTEXTW = 12;
  ft_comparecontent = 100;



  // for ContentGetValue
  FT_NOSUCHFIELD = -1;
  FT_FILEERROR = -2;
  FT_FIELDEMPTY = -3;
  FT_ONDEMAND = -4;
  ft_NotSupported   = -5;
  ft_SetCancel      = -6;
  FT_DELAYED = 0;

  CONTENT_DELAYIFSLOW = 1;  // ContentGetValue called in foreground

  // for ContentSendStateInformation
  contst_ReadNewDir        = 1;
  contst_RefreshPressed    = 2;
  contst_ShowHint          = 4;

type
  tContentDefaultParamStruct = record
    size, 
    PluginInterfaceVersionLow, 
    PluginInterfaceVersionHi: longint;
    DefaultIniName: array[0..MAX_PATH-1] of AnsiChar;
    end;
  pContentDefaultParamStruct = ^tContentDefaultParamStruct;

  tdateformat = record
    wYear, wMonth, wDay: word;
    end;
  pdateformat = ^tdateformat;

  ttimeformat = record
    wHour, wMinute, wSecond: word;
    end;
  ptimeformat = ^ttimeformat;

implementation

{
procedure ContentGetDetectString(DetectString: pchar;maxlen: integer); stdcall;

function ContentGetSupportedField(FieldIndex: integer;FieldName: pchar;
  Units: pchar;maxlen: integer): integer; stdcall;

function ContentGetValue(FileName: pchar;FieldIndex, UnitIndex: integer;
  FieldValue: pointer;
  maxlen, flags: integer): integer; stdcall;

procedure ContentSetDefaultParams(dps: pContentDefaultParamStruct); stdcall;
}


end.

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
  FT_stringw = 11;

  // for ContentGetValue
  FT_NOSUCHFIELD = -1;
  FT_FILEERROR = -2;
  FT_FIELDEMPTY = -3;
  FT_ONDEMAND = -4;
  FT_DELAYED = 0;

  CONTENT_DELAYIFSLOW = 1;  // ContentGetValue called in foreground

type
  tContentDefaultParamStruct = record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: longint;
    DefaultIniName: array[0..MAX_PATH-1] of char;
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

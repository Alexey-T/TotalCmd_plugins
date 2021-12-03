unit ContPlug;
interface
uses Windows;

const FT_NOMOREFIELDS=0;
      FT_NUMERIC_32=1;
      FT_NUMERIC_64=2;
      FT_NUMERIC_FLOATING=3;
      FT_DATE=4;
      FT_TIME=5;
      FT_BOOLEAN=6;
      FT_MULTIPLECHOICE=7;
      FT_STRING=8;
      FT_FULLTEXT=9;
      FT_DATETIME=10;
      ft_StringW = 11;

// for ContentGetValue
      FT_NOSUCHFIELD=-1;
      FT_FILEERROR=-2;
      FT_FIELDEMPTY=-3;
      FT_ONDEMAND=-4;
      FT_NOTSUPPORTED=-5;
      FT_SETCANCEL=-6;
      FT_DELAYED=0;

// for ContentSetValue
      FT_SETSUCCESS=0;     // setting of the attribute succeeded

// for ContentGetSupportedFieldFlags
      CONTFLAGS_EDIT=1;
      CONTFLAGS_SUBSTSIZE=2;
      CONTFLAGS_SUBSTDATETIME=4;
      CONTFLAGS_SUBSTDATE=6;
      CONTFLAGS_SUBSTTIME=8;
      CONTFLAGS_SUBSTATTRIBUTES=10;
      CONTFLAGS_SUBSTATTRIBUTESTR=12;
      CONTFLAGS_PASSTHROUGH_SIZE_FLOAT=14;
      CONTFLAGS_SUBSTMASK=14;
      CONTFLAGS_FIELDEDIT=16;

// for ContentSendStateInformation
      CONTST_READNEWDIR=1;
      CONTST_REFRESHPRESSED=2;
      CONTST_SHOWHINT=4;
      SETFLAGS_FIRST_ATTRIBUTE=1;    // First attribute of this file
      SETFLAGS_LAST_ATTRIBUTE=2;     // Last attribute of this file
      SETFLAGS_ONLY_DATE=4;          // Only set the date of the datetime value!
      CONTENT_DELAYIFSLOW=1;         // ContentGetValue called in foreground
      CONTENT_PASSTHROUGH=2;         // If requested via contflags_passthrough_size_float: The size
                                     // is passed in as floating value, TC expects correct value
                                     // from the given units value, and optionally a text string

type tContentDefaultParamStruct=record
      size,
      PluginInterfaceVersionLow,
      PluginInterfaceVersionHi:longint;
      DefaultIniName:array[0..MAX_PATH-1] of char;
    end;

    pContentDefaultParamStruct=^tContentDefaultParamStruct;

type tdateformat=record
       wYear,wMonth,wDay:word;
     end;

     pdateformat=^tdateformat;

type ttimeformat=record
       wHour,wMinute,wSecond:word;
     end;

     ptimeformat=^ttimeformat;

implementation
{
procedure ContentGetDetectString(DetectString:pchar;maxlen:integer); stdcall;

function ContentGetSupportedField(FieldIndex:integer;FieldName:pchar;
  Units:pchar;maxlen:integer):integer; stdcall;

function ContentGetValue(FileName:pchar;FieldIndex,UnitIndex:integer;FieldValue:pbyte;
  maxlen,flags:integer):integer; stdcall;

procedure ContentSetDefaultParams(dps:pContentDefaultParamStruct); stdcall;

procedure ContentPluginUnloading; stdcall;

procedure ContentStopGetValue(FileName:pchar); stdcall;

function ContentGetDefaultSortOrder(FieldIndex:integer):integer; stdcall;

function ContentGetSupportedFieldFlags(FieldIndex:integer):integer; stdcall;

function ContentSetValue(FileName:pchar;FieldIndex,UnitIndex,FieldType:integer;
  FieldValue:pbyte;flags:integer):integer; stdcall;

procedure ContentSendStateInformation(state:integer;path:pchar); stdcall;

function ContentEditValue(handle:thandle;FieldIndex,UnitIndex,FieldType:integer;
    FieldValue:pchar;maxlen:integer;flags:integer;langidentifier:pchar):integer; stdcall;
}
end.

{$E wdx}
{$IFDEF WIN64}
  {$E wdx64}
{$ENDIF}
{$WEAKLINKRTTI ON}

library AudioInfo;

{$R *.RES}

uses
  Windows,
  ContPlug,
  AudioData in 'AudioData.pas',
  SProc,
  AAC in 'ATL_KOL\AAC.pas',
  APE in 'ATL_KOL\APE.pas',
  CDAtrack in 'ATL_KOL\CDAtrack.pas',
  FLAC in 'ATL_KOL\FLAC.pas',
  ID3v1 in 'ATL_KOL\ID3v1.pas',
  ID3v2 in 'ATL_KOL\ID3v2.pas',
  Monkey in 'ATL_KOL\Monkey.pas',
  MPEGaudio in 'ATL_KOL\MPEGaudio.pas',
  OggVorbis in 'ATL_KOL\OggVorbis.pas',
  TwinVQ in 'ATL_KOL\TwinVQ.pas',
  WMA in 'ATL_KOL\WMA.pas',
  kol in 'KOL\kol.pas',
  ModData in 'Bass\ModData.pas',
  bass in 'Bass\bass.pas',
  AC3 in 'ATL_KOL\AC3.pas',
  DTS in 'ATL_KOL\DTS.pas',
  WAVPack in 'ATL_KOL\WAVPack.pas',
  Musepack in 'ATL_KOL\Musepack.pas',
  OptimFROG in 'ATL_KOL\OptimFROG.pas',
  TTA in 'ATL_KOL\TTA.pas',
  MP4TagLibrary in 'MP4Tag\MP4TagLibrary.pas',
  ID3Gen in 'MP4Tag\ID3Gen.pas',
  WAV in 'ATL_KOL\WAV.pas',
  WAVFile in 'ATL_KOL\WAVFile.pas', // !!!!
  DSFFile in 'ATL_KOL\DSFFile.pas', // !!!!
  AIFFile in 'ATL_KOL\AIFFile.pas', // !!!!
  MidiFile2 in 'ATL_KOL\MidiFile2.pas',
  VGMFile in 'ATL_KOL\VGMFile.pas';

const
  _DetectString: PAnsiChar =
    'EXT="MP3"|EXT="MP2"|EXT="MP1"|EXT="OGG"|EXT="OPUS"|EXT="WMA"|'+
    'EXT="WAV"|EXT="VQF"|EXT="AAC"|EXT="APE"|EXT="MPC"|EXT="FLAC"|'+
    'EXT="AC3"|EXT="DTS"|EXT="WV"|EXT="OFR"|EXT="TTA"|EXT="SPX"|'+
    'EXT="MP4"|EXT="M4A"|EXT="M4R"|EXT="M4B"|EXT="DSF"|EXT="DFF"|'+
    'EXT="AIF"|EXT="AIFF"|EXT="AIFC" | EXT="CDA" | '+
    'EXT="IT"|EXT="XM"|EXT="S3M"|EXT="MTM"|EXT="MOD"|EXT="UMX"|EXT="MO3"';

  _FieldsNum = 23;
  _Fields: array[0.._FieldsNum-1] of PAnsiChar = (
    'Channels',
    'Duration',
    'Duration (H/M/S)',
    'Sample rate',
    'Bitrate',
    'Bitrate type',
    'Title',
    'Artist',
    'Album',
    'Track',
    'Track (zero-filled)',
    'Date',
    'Genre',
    'Comment',
    'Composer',
    'Copyright',
    'URL',
    'Encoder',
    'Tags',
    'Codec',
    'Duration Ext (H/M/S/MS)',
    'Has Cover',
    'Full text'
    );

  _FieldTypes: array[0.._FieldsNum-1] of integer = (
    ft_multiplechoice,
    ft_time,
    ft_string,
    ft_numeric_32,
    ft_numeric_32,
    ft_multiplechoice,
    ft_string,
    ft_string,
    ft_string,
    ft_numeric_32,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_string,
    ft_numeric_floating,
    ft_boolean,
    ft_fulltext // ft_fulltextw will be set on return if supports
    );

  _FieldUnits: array[0.._FieldsNum-1] of PAnsiChar = (
    'Unknown|Mono|Stereo|Joint Stereo|Dual Channel', '', '', 'Hz|KHz',
    '', 'CBR|VBR|Unknown',
    '0|1250|1251|1252|932|65001',
    '0|1250|1251|1252|932|65001',
    '0|1250|1251|1252|932|65001',
    '', '', '', '',
    'Comment 1|Comment 2|Comment 3|All', '', '', '', '', '', '',
    'Auto|Full|Auto Locale|Full Locale', '', '');

var
  RTL: Windows.TRTLCriticalSection;
  CallerSupportsFullTextW: Boolean = False;

//--------------------------------------------
procedure ContentGetDetectString(DetectString: PAnsiChar; maxlen: integer); stdcall;
begin
  StrLCpyA(DetectString, _DetectString, MaxLen);
end;

//--------------------------------------------
function ContentGetSupportedField(FieldIndex: integer; FieldName: PAnsiChar;
  Units: PAnsiChar; maxlen: integer): integer; stdcall;
begin
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOMOREFIELDS; Exit end;

  StrLCpyA(FieldName, _Fields[FieldIndex], MaxLen);
  StrLCpyA(Units, _FieldUnits[FieldIndex], MaxLen);
  Result:= _FieldTypes[FieldIndex];
end;

//--------------------------------------------
function IntToStr2(n: integer): string;
begin
  Result:= IntToStr(n);
  if Length(Result)<2 then
    Insert('0', Result, 1);
end;

function _ConvertToCodepage(const Text: string; ToCodepage: Integer): string;
begin
  if (ToCodepage <= 0) or (Text = '') then
    Exit(Text);

  Result := AStrToWStr(WStrToAStr(Text, 0), ToCodepage);
  if Result = '' then
    Result := Text;
end;

function ConvertToCP(const Text: string; const Units: AnsiString; UnitIndex: Integer): string; inline;
begin
  if UnitIndex <= 0 then
    Exit(Text);

  // TODO: get Unit from Units and convert to Integer

  case UnitIndex of
    1: Result := _ConvertToCodepage(Text, 1250);
    2: Result := _ConvertToCodepage(Text, 1251);
    3: Result := _ConvertToCodepage(Text, 1252);
    4: Result := _ConvertToCodepage(Text, 932);
    5: Result := _ConvertToCodepage(Text, 65001);
  else
    Result := Text;
  end;
end;

//--------------------------------------------
function GetValueW(fn: PWideChar; FieldIndex, UnitIndex: integer;
  FieldValue: PWideChar; maxlen, flags: integer): integer; stdcall;
var
  PVal: PInteger;
  PTime: PTimeFormat;
begin
  //Text field
  if (FieldIndex=Pred(_FieldsNum)) then
  begin
    if UnitIndex=-1 then
      begin fFullText:= ''; Result:= ft_fieldempty; Exit end;

    //MessageBox(0, PChar(IntToStr(UnitIndex)), 'UnitIndex', MB_OK);
    if UnitIndex > 0 then
      Exit(ft_fieldempty);

    if UnitIndex=0 then
      if not GetAudioData(fn) then
        begin Result:= FT_FILEERROR; Exit end;

//    StrLCpy(FieldValue, PWideChar(Copy(AudioData.fFullText, UnitIndex+1, MaxInt)), MaxLen div 2);
    //StrLCpyA(PAnsiChar(FieldValue), PAnsiChar(WideToANSI(AudioData.fFullText)), MaxLen div 2);

    if Length(AudioData.fFullText) = 0 then
      Exit(ft_fieldempty);

    if CallerSupportsFullTextW then
    begin
      StrLCpy(PWideChar(FieldValue), PWideChar(AudioData.fFullText), MaxLen div 2);
      Result:= ft_fulltextw;
    end else begin
      StrLCpyA(PAnsiChar(FieldValue), PAnsiChar(WideToANSI(AudioData.fFullText)), MaxLen);
      Result:= ft_fulltext;
    end;

    Exit;
  end;

  //ordinary fields
  if (FieldIndex<0) or (FieldIndex>=_FieldsNum) then
    begin Result:= FT_NOSUCHFIELD; Exit end;

  if not GetAudioData(fn) then
    begin Result:= FT_FILEERROR; Exit end;

  Result:= _FieldTypes[FieldIndex];
  PVal:= Pointer(FieldValue);
  PTime:= Pointer(FieldValue);

  if Result = ft_string then Result := ft_stringw;

  // 'Channels', 'Duration', 'Duration (H/M/S)', 'Sample rate', 'Bitrate',
  // 'Bitrate type', 'Title', 'Artist', 'Album', 'Track', 'Track (zero-filled)',
  // 'Date', 'Genre', 'Comment', 'Composer', 'Copyright', 'URL', 'Encoder', 'Tags'
  case FieldIndex of
     0:
         begin
           // No unicode support for ft_multiplechoice
           StrLCpyA(PAnsiChar(FieldValue), PAnsiChar(WideToAnsi(AudioData.fChannels)), MaxLen div 2);
           Result := ft_string;
         end;
     1:
         begin
         PTime^.wHour:= AudioData.fDuration div 3600;
         PTime^.wMinute:= AudioData.fDuration mod 3600 div 60;
         PTime^.wSecond:= AudioData.fDuration mod 60;
         end;
     2:
         StrLCpy(FieldValue, PWideChar(SFormatDuration(AudioData.fDuration)), MaxLen div 2);
     3:
       if AudioData.fSampleRate>0
         then
           case UnitIndex of
             0: PVal^:= AudioData.fSampleRate;
             1: PVal^:= AudioData.fSampleRate div 1000;
           end
         else Result:= FT_FIELDEMPTY;
     4:
       if fBitRate>0
         then PVal^:= AudioData.fBitRate
         else Result:= FT_FIELDEMPTY;

     5:
         begin
           // No unicode support for ft_multiplechoice
           StrLCpyA(PAnsiChar(FieldValue), PAnsiChar(WideToAnsi(AudioData.fBitRateType)), MaxLen div 2);
           Result := ft_string;
         end;

     6: StrLCpy(FieldValue, PWideChar(ConvertToCP(AudioData.fTitle, AnsiString(_FieldUnits[FieldIndex]), UnitIndex)), MaxLen div 2);
     7: StrLCpy(FieldValue, PWideChar(ConvertToCP(AudioData.fArtist, AnsiString(_FieldUnits[FieldIndex]), UnitIndex)), MaxLen div 2);
     8: StrLCpy(FieldValue, PWideChar(ConvertToCP(AudioData.fAlbum, AnsiString(_FieldUnits[FieldIndex]), UnitIndex)), MaxLen div 2);

     9:
        if fTrack>0
          then PVal^:= fTrack
          else Result:= FT_FIELDEMPTY;

    10: if fTrack > 0 then StrLCpy(FieldValue, PWideChar(IntToStr2(AudioData.fTrack)), MaxLen div 2) else Result := FT_FIELDEMPTY;
    11: StrLCpy(FieldValue, PWideChar(AudioData.fDate), MaxLen div 2);
    12: StrLCpy(FieldValue, PWideChar(AudioData.fGenre), MaxLen div 2);
    13:
        begin
          case UnitIndex of
            0: StrLCpy(FieldValue, PWideChar(AudioData.fComment), MaxLen div 2);
            1: StrLCpy(FieldValue, PWideChar(AudioData.fComments[1]), MaxLen div 2);
            2: StrLCpy(FieldValue, PWideChar(AudioData.fComments[2]), MaxLen div 2);
            3: StrLCpy(FieldValue, PWideChar(AudioData.fCommentsAll), MaxLen div 2);
          end;
        end;
    14: StrLCpy(FieldValue, PWideChar(AudioData.fComposer), MaxLen div 2);
    15: StrLCpy(FieldValue, PWideChar(AudioData.fCopyright), MaxLen div 2);
    16: StrLCpy(FieldValue, PWideChar(AudioData.fURL), MaxLen div 2);
    17: StrLCpy(FieldValue, PWideChar(AudioData.fEncoder), MaxLen div 2);
    18: StrLCpy(FieldValue, PWideChar(AudioData.fTags), MaxLen div 2);
    19: StrLCpy(FieldValue, PWideChar(AudioData.fCodec), MaxLen div 2);
    20: begin
          PDouble(FieldValue)^ := fPlaytime;
          StrCopyW(PWideChar(FieldValue) + SizeOf(Double) div SizeOf(Char),
            PWideChar(SFormatDuration(AudioData.fPlayTime,
            (UnitIndex = 1) or (UnitIndex = 3), UnitIndex > 1)));
        end;
    21: if fTags = '' then Result:= FT_FIELDEMPTY else PVal^ := Integer(fHasCoverArt);
  end;

    if (Result=FT_STRINGW) and (lstrlenW(FieldValue)=0) then
      Result:= FT_FIELDEMPTY;
end;

//--------------------------------------------
function ContentGetValueW(fn: PWideChar; FieldIndex, UnitIndex: integer;
  FieldValue: PWideChar; maxlen, flags: integer): integer; stdcall;
begin
  if (flags and CONTENT_DELAYIFSLOW)>0 then
    begin
      // Avoid double read file (icon blinking)
      StrLCpy(FieldValue, PWideChar(' '), MaxLen div 2);
      Result:= FT_DELAYED; Exit;
    end;

  EnterCriticalSection(RTL);
  try
    Result := GetValueW(fn, FieldIndex, UnitIndex, FieldValue, maxlen, flags);
  finally
    LeaveCriticalSection(RTL);
  end;
end;

function ContentGetValue(fn: PAnsiChar; FieldIndex, UnitIndex: integer;
  FieldValue: PAnsiChar; maxlen, flags: integer): integer; stdcall;
var
  WideFieldValue: PWideChar;
begin
  GetMem(WideFieldValue, maxlen);
  try
    Result:=ContentGetValueW(PWideChar(string(AnsiString(fn))),
                             FieldIndex,
                             UnitIndex,
                             WideFieldValue,
                             Maxlen,
                             Flags);

    case Result of
      ft_stringw:
      begin
        Result:=ft_string;
        lstrcpynA(FieldValue,PAnsiChar(WideToAnsi(WideFieldValue)), maxlen);
      end;
      ft_numeric_32, ft_numeric_64, ft_datetime, ft_numeric_floating:
      begin
        CopyMemory(FieldValue, WideFieldValue, maxlen);
      end;
    end;
  finally
    FreeMem(WideFieldValue);
  end;
end;

procedure ContentPluginUnloading; stdcall;
begin
  UnloadBass;
end;

procedure ContentSendStateInformationW(State: integer; Path: PWideChar); stdcall;
begin
  if {(State = contst_readnewdir) or} (State = contst_RefreshPressed) then
    fLastFilename := '';
end;

procedure ContentSetDefaultParams(dps: pContentDefaultParamStruct); stdcall;
begin
  CallerSupportsFullTextW := ((dps.PluginInterfaceVersionLow >= 11) and
    (dps.PluginInterfaceVersionHi >= 2)) or (dps.PluginInterfaceVersionHi > 2);
end;

exports
  ContentGetDetectString,
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW,
  ContentPluginUnloading,
  ContentSetDefaultParams,
  ContentSendStateInformationW;

procedure DLLEntryProc(EntryCode: integer);
begin
  case EntryCode of
    DLL_PROCESS_ATTACH:
    begin
    end;
    DLL_PROCESS_DETACH:
    begin
      DeleteCriticalSection(RTL);
    end;
    DLL_THREAD_ATTACH:
    begin
    end;
    DLL_THREAD_DETACH:
    begin
    end;
  end;
end;

begin
  //ReportMemoryLeaksOnShutdown := True;

  InitializeCriticalSection(RTL);
  DLLProc := @DLLEntryProc;
  DLLEntryProc(DLL_PROCESS_ATTACH);
  DisableThreadLibraryCalls(HInstance);
end.

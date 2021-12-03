// IT / XM / S3M / MTM / MOD / UMX

unit ModData;

interface

var
  fModTitle: string;
  fModAuthor: string;
  fModComment: string;
  fModChannels: integer;
  fModInstruments: integer;
  fModDuration: integer;

  BASSIntialized: boolean = false;
  BASSLoaded: boolean = false;

function GetModData(const fn: string): boolean;
procedure UnloadBass;

implementation

uses Windows, Bass;

function GetPluginFilename: string;
var
  buf: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buf, GetModuleFileName(hInstance, buf, SizeOf(buf)));
end;

function ExtractFilePath(const fn: string): string;
var
  i: integer;
begin
  i:= Length(fn);
  while (i>0) and (fn[i]<>'\') do Dec(i);
  Result:= Copy(fn, 1, i);
end;

function GetModData(const fn: string): boolean;
var
  Music: HMUSIC;
  Res: integer;
  Attr: single;
begin
  fModTitle:= '';
  fModAuthor:='';
  fModComment:='';
  fModChannels:= 0;
  fModInstruments:= 0;
  fModDuration:= 0;

  // looks like incorrect to place LoadLibrary calls in the
  // initialization section, load library here once
  if not BASSIntialized then
  begin
    BASSLoaded := BASS_LoadLibrary(ExtractFilePath(GetPluginFilename) +
      {$IFDEF WIN64} 'bass64.dll' {$ELSE} 'bass.dll' {$ENDIF});
    if BASSLoaded then BASS_Init(-1, 44100, 0, 0, nil);
    BASSIntialized:=true;
  end;

  if not BASSLoaded then
  begin
    fModTitle:= 'BASS not loaded.';
    Exit;
  end;

  Music  := BASS_MusicLoad(False, PChar(fn), 0, 0, BASS_UNICODE or BASS_MUSIC_STOPBACK or BASS_MUSIC_PRESCAN or BASS_MUSIC_NOSAMPLE, 1);
  Result := (Music <> 0);

  if not Result then Exit;

  fModTitle := BASS_ChannelGetTags(Music, BASS_TAG_MUSIC_NAME);
  fModAuthor := BASS_ChannelGetTags(Music, BASS_TAG_MUSIC_AUTH);
  fModComment := BASS_ChannelGetTags(Music, BASS_TAG_MUSIC_MESSAGE);

  fModDuration := BASS_ChannelGetLength(Music, BASS_POS_BYTE) div (44100*2*2);

  while (BASS_ChannelGetAttribute(Music, BASS_ATTRIB_MUSIC_VOL_CHAN + fModChannels, Attr) <> BOOL(0)) do
    Inc(fModChannels);
  while (BASS_ChannelGetAttribute(Music, BASS_ATTRIB_MUSIC_VOL_INST + fModInstruments, Attr) <> BOOL(0)) do
    Inc(fModInstruments);

  BASS_MusicFree(Music);
end;

procedure UnloadBass;
begin
  if BASSLoaded then
  begin
    BASS_Free;
    BASS_FreeLibrary;
  end;
end;

//initialization
//  BASSLoaded := BASS_LoadLibrary(ExtractFilePath(GetPluginFilename) +
//  {$IFDEF WIN64} 'bass64.dll' {$ELSE} 'bass.dll' {$ENDIF});
//  if BASSLoaded then
//    BASS_Init(-1, 44100, 0, 0, nil);

//finalization
//  if BASSLoaded then
//    BASS_FreeLibrary;
//    BASS_Free;

end.

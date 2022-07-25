//Wrapper for classes TMPEGaudio, TOggVorbis etc
//to get their fields to normal variables.

unit AudioData;
{$B-}
interface

const
  MAX_COMMENTS = 3;

var
  fDuration, fSampleRate, fBitRate: integer;
  fBitRateType, fChannels, fTitle, fArtist, fAlbum, fDate, fGenre, fComment, fCommentsAll,
  fEncoder, fComposer, fCopyright, fURL, fCodec: string;
  fTrack: integer;
  fPlayTime: Double;
  fTags, fFullText, fLastFilename: string;
  fHasCoverArt: Boolean;

  fComments: array [0..MAX_COMMENTS-1] of string;

function GetAudioData(fn: string): boolean;
function SFormatDuration(Duration: integer): string; overload;
function SFormatDuration(Duration: Double; FullTime, UseLocale: Boolean): string; overload;

implementation

uses
  Windows, KOL, SProc,
  WAVFile, WMA, AAC, TwinVQ, APE, Monkey, OggVorbis, MPEGaudio, Musepack, FLAC,
  CDAtrack, ID3v1, ID3v2, ModData,
  AC3, DTS, WAVPack, OptimFROG, TTA, MP4TagLibrary, DSFFile, AIFFile, MidiFile2,
  VGMFile;

const
  ExtendedGetFrameText = True;
  WaveReadID3v2Tag = True;

//-------------------------------------------------
procedure ClearData;
begin
  fDuration:= 0;
  fSampleRate:= 0;
  fBitRate:= 0;
  fTrack:= 0;
  fPlayTime := 0;
  Finalize(fBitRateType);
  Finalize(fChannels);
  Finalize(fTitle);
  Finalize(fArtist);
  Finalize(fAlbum);
  Finalize(fDate);
  Finalize(fGenre);
  Finalize(fComment);
  Finalize(fEncoder);
  Finalize(fComposer);
  Finalize(fCopyright);
  Finalize(fURL);
  Finalize(fTags);
  Finalize(fCodec);
  Finalize(fFullText);
  Finalize(fComments);
  fComments[0]:='';
  fComments[1]:='';
  fComments[2]:='';
  Finalize(fCommentsAll);
  fHasCoverArt := false;
end;

//-------------------------------------------------
procedure SAssign(var s: string; const value: string);
begin
  if value <> '' then s:= value;
end;

procedure IAssign(var n: integer; const value: integer);
begin
  if value<>0 then n:= value;
end;

procedure SAppend(var s: string; const value: string; condition: boolean);
begin
  if condition and (value<>'') then
    if s=''
      then s:= value
      else s:= s+' '+value;
end;

// +Functions from SysUtils
type
  TTimeStamp = record
    Time: Integer;      { Number of milliseconds since midnight }
    Date: Integer;      { One plus number of days since 1/1/0001 }
  end;

const
  FMSecsPerDay: Single = MSecsPerDay;
  IMSecsPerDay: Integer = MSecsPerDay;
  DateDelta = 693594;
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  SecsPerHour   = SecsPerMin * MinsPerHour;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
var
  LTemp, LTemp2: Int64;
begin
  LTemp := Round(DateTime * FMSecsPerDay);
  LTemp2 := (LTemp div IMSecsPerDay);
  Result.Date := DateDelta + LTemp2;
  Result.Time := Abs(LTemp) mod IMSecsPerDay;
end;

procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);
var
  MinCount, MSecCount: Word;
begin
  DivMod(DateTimeToTimeStamp(DateTime).Time, SecsPerMin * MSecsPerSec, MinCount, MSecCount);
  DivMod(MinCount, MinsPerHour, Hour, Min);
  DivMod(MSecCount, MSecsPerSec, Sec, MSec);
end;

function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
var
  Buffer: array[0..8] of AnsiChar;
begin
  if GetLocaleInfo(Locale, LocaleType, Buffer, Length(Buffer)) > 0 then
    Result := Char(Buffer[0]) else
    Result := Default;
end;
// -Functions from SysUtils

var
  DecimalSeparator: Char = '.';
  TimeSeparator: Char = ':';
  DurationFullStr, DurationLongStr, DurationShortStr, DurationShortStrMs:
    array [Boolean] of string;

function MakeFormatString(TimePeaces: Integer; MS, UseLocale: Boolean): string;
const
  PeaceOfTime = '%02d';
  PeaseOfMS = '%03d';
var
  I: Integer;
  ATimeSeparator, ADecimalSeparator: Char;
begin
  Result := '';
  if UseLocale then
  begin
    ATimeSeparator := TimeSeparator;
    ADecimalSeparator := DecimalSeparator;
  end else begin
    ATimeSeparator := ':';
    ADecimalSeparator := '.';
  end;
  for I := 0 to TimePeaces - 1 do
    if Result <> '' then
      Result := Result + ATimeSeparator + PeaceOfTime
    else
      Result := PeaceOfTime;
  if MS then
    Result := Result + ADecimalSeparator + PeaseOfMS;
end;

function SFormatDuration(Duration: Double; FullTime, UseLocale: Boolean): string; overload;
const
  OneSec = 1/24/3600;
var
  Total: Extended;
  NeedShowMS: Boolean;
  _wHour, _wMinute, _wSecond, _wMS: Word;
begin
  Total := Duration * OneSec;
  DecodeTime(Frac(Total), _wHour, _wMinute, _wSecond, _wMS);
  if FullTime then
    //Result := Format('%02d:%02d:%02d.%03d', [_wHour, _wMinute, _wSecond, _wMS])
    Result := Format(DurationFullStr[UseLocale], [_wHour, _wMinute, _wSecond, _wMS])
  else begin
    NeedShowMS := Duration < 1;
    if _wHour > 0 then
      //Result := Format('%02d:%02d:%02d', [_wHour, _wMinute, _wSecond])
      Result := Format(DurationLongStr[UseLocale], [_wHour, _wMinute, _wSecond])
    else if NeedShowMS then
      //Result := Format('%02d:%02d.%03d', [_wMinute, _wSecond, _wMS])
      Result := Format(DurationShortStrMs[UseLocale], [_wMinute, _wSecond, _wMS])
    else
      //Result := Format('%02d:%02d', [_wMinute, _wSecond]);
      Result := Format(DurationShortStr[UseLocale], [_wMinute, _wSecond]);
  end;
end;

function SFormatDuration(Duration: integer): string;
var
  _wHour, _wMinute, _wSecond: integer;
begin
  _wHour:= Duration div 3600;
  _wMinute:= Duration mod 3600 div 60;
  _wSecond:= Duration mod 60;
  if _wHour>0
    then Result:= Format('%02d:%02d:%02d', [_wHour, _wMinute, _wSecond])
    else Result:= Format('%02d:%02d', [_wMinute, _wSecond]);
end;


function SFormatChannels(num: integer): string;
begin
  case num of
    0: Result:= 'Unknown';
    1: Result:= 'Mono';
    2: Result:= 'Stereo';
    else Result:= IntToStr(num)+' ch.';
  end;
end;

//-------------------------------------------------
function ReadID3v1(ID3v1: PID3v1): boolean;
begin
  with ID3v1^ do
    begin
    Result:= Exists;
    if Result then
      begin
      SAppend(fTags, 'ID3v1', true);
      if LyricsSize > 0 then
        fTags := fTags + '+Lyrics';
      SAssign(fTitle, string(Title));
      SAssign(fArtist,string( Artist));
      SAssign(fAlbum, string(Album));
      IAssign(fTrack, Track);
      SAssign(fDate, string(Year));
      SAssign(fGenre, string(Genre));
      SAssign(fComment, string(Comment));
      end;
    end;
end;

function ReadID3v2(ID3v2: PID3v2): boolean;
var
  i, Cnt: integer;
  FComm: string;
  TagVer: string;
  ATrack: string;
  Str: string;
  C: Char;
begin
  with ID3v2^ do
    begin
    Result:= Exists;
    if Result then
      begin
      case FVersionID of
        TAG_VERSION_2_2: TagVer := ' (2.2)';
        TAG_VERSION_2_3: TagVer := ' (2.3)';
        TAG_VERSION_2_4: TagVer := ' (2.4)';
      else
        TagVer:='';
      end;
      SAppend(fTags, 'ID3v2' + TagVer, true);
      SAssign(fTitle, Title);
      SAssign(fArtist, Artist);
      SAssign(fAlbum, Album);

      ATrack := PTrim(Track);
      IAssign(fTrack, StrToIntDef(ATrack, 0));
      if (fTrack = 0) and (Length(ATrack) > 0) then
      begin
        Str := '';
        for C in ATrack do
          if AnsiChar(C) in ['0'..'9'] then
            Str := Str + C
          else
            Break;
        IAssign(fTrack, StrToIntDef(Str, 0));
      end;

      SAssign(fDate, Year);
      SAssign(fGenre, Genre);
      if ExtendedGetFrameText then
        SReplaceAll(fGenre, #0, '; '); // split genres
      SAssign(fComment, Comment);

      // get all comments
      Cnt:=CommentCount;
      if Cnt > 1 then
      begin
        for i := 0 to Cnt-1 do
        begin
          FComm:=Comments[i];
          if i < MAX_COMMENTS then fComments[i] := FComm;
          if fCommentsAll <> '' then fCommentsAll := fCommentsAll + '~~';
          fCommentsAll := fCommentsAll + FComm;
        end;
      end;

      SAssign(fEncoder, Encoder);
      SAssign(fComposer, Composer);
      SAssign(fCopyright, Copyright);
      SAssign(fURL, Link);
      end;
    end;
  Finalize(FComm);
end;

function ReadAPE(APE: PAPE): boolean;
begin
  with APE^ do
    begin
    Result:= Exists;
    if Result then
      begin
      SAppend(fTags, 'APE', true);
      SAssign(fTitle, Title);
      SAssign(fArtist, Artist);
      SAssign(fAlbum, Album);
      IAssign(fTrack, Track);
      SAssign(fDate, Year);
      SAssign(fGenre, Genre);
      SAssign(fComment, Comment);
      SAssign(fCopyright, Copyright);
      end;
    end;
end;

function ReadFLAC(FLAC: PFLAC): boolean;
begin
  with FLAC^ do
    begin
    Result:= Exists;
    if Result then
      begin
      SAppend(fTags, 'FLAC', true);
      SAssign(fTitle, Title);
      SAssign(fArtist, Artist);
      SAssign(fAlbum, Album);
      IAssign(fTrack, StrToIntDef(Track, 0));
      SAssign(fDate, Year);
      //SAssign(fGenre, Genre);
      SAssign(fGenre, Genres);
      SAssign(fComment, Comment);
      SAssign(fCopyright, Copyright);
      end;
    end;
end;

//-------------------------------------------------
var
  FVorbis: POggVorbis = nil;
  FMPEG: PMPEGaudio = nil;
  FMPEGPlus: PMPEGplus = nil;
  FWave: PWAV = nil;
  FWMA: PWMA = nil;
  FAAC: PAAC = nil;
  FVQF: PTwinVQ = nil;
  FMonkey: PMonkey = nil;
  FFLAC: PFLAC = nil;
  FAC3: PAC3 = nil;
  FDTS: PDTS = nil;
  FCDAtrack: PCDAtrack = nil;
  FWAVPack: PWAVPackfile = nil;
  FOptimFrog: POptimFrog = nil;
  FTTA: PTTA = nil;
  MP4Tag: PMP4Tag = nil;
  FDSF: PDSF = nil;
  FAIF: PAIFF = nil;
  FMIDI: PMidiFile2 = nil;
  FVGM: PVGM = nil;

function LoadData(const fn: string): boolean;
var
  fExt: string;
  ARes, I: Integer;
begin
  Result:= (fn<>'') and IsFileExist(fn);
  if not Result then Exit;

  fExt:= UpperCase(ExtractFileExt(fn));

  //--------------------------------------------------
  if (fExt='.OGG') or (fExt='.OPUS') or (fExt='.SPX') then
    with FVorbis^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        SAppend(fTags, 'Vorbis', true);
        SAppend(fTags, 'ID3v2', ID3v2);
        fChannels:=         ChannelMode;
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate;
        fDuration:=         Trunc(Duration);
        fTitle:=            Title;
        fArtist:=           Artist;
        fAlbum:=            Album;
        fTrack:=            Track;
        fDate:=             Date;
        fGenre:=            Genre;
        fComment:=          Comment;
        fEncoder:=          Vendor;
        if fEncoder = '' then
          fEncoder := EncoderTag;
        fCodec:=            Codec;
        fPlayTime:=         Duration;
        fHasCoverArt:=      HasCoverArt;
        end;
      Exit;
      end;

  //--------------------------------------------------
  if (fExt='.FLAC') then
    with FFLAC^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fChannels:=         SFormatChannels(Channels);
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate;
        fDuration:=         Trunc(Duration);
        ReadID3v1(ID3v1);
        ReadID3v2(ID3v2);
        ReadFLAC(FFLAC);
        fCodec:=            'FLAC';
        fPlayTime:=         Duration;
        fEncoder :=         Encoder;
        if fEncoder = '' then
          fEncoder :=       Vendor;
        if fEncoder <> '' then
          fEncoder :=       fEncoder + '  (Ratio: ' + string(Int2Str(Trunc(Ratio))) + '%)'
        else
          fEncoder :=       '(Ratio: ' + string(Int2Str(Trunc(Ratio))) + '%)';
        fHasCoverArt:=      FFLAC.HasCoverArt;
        end;
      Exit;
      end;

  //--------------------------------------------------
  if (fExt='.MP3') or (fExt='.MP2') or (fExt='.MP1') then
    with FMPEG^ do
      begin
      ID3v2.UseExtendedGetFrameText := ExtendedGetFrameText;
      ID3v2.FixTag24Sync := ExtendedGetFrameText;
      ID3v2.CalcAllID3SizesInFile := True;

      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        if VBR.Found
          then fBitRateType:= 'VBR'
          else fBitRateType:= 'CBR';
        fChannels:=         ChannelMode;
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate;
        fDuration:=         Trunc(Duration);
        fEncoder:=          Encoder;
        ReadID3v1(ID3v1);
        ReadAPE(APETag);
        ReadID3v2(ID3v2);
        fCodec:=            'Mpeg Layer 3';   { TODO : Complete }
        fPlayTime:=         Duration;
        fHasCoverArt:=      (ID3v2.Exists and ID3v2.HasCoverArt) or APETag.HasCoverArt;
        end;
      Exit;
      end;

  //--------------------------------------------------
  if (fExt='.APE') then
    with FMonkey^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fChannels:=         ChannelMode;
        fSampleRate:=       SampleRate;
        fBitRate:=          Round(BitRate / 1000000);
        fDuration:=         Trunc(Duration);
        ReadID3v1(ID3v1);
        ReadID3v2(ID3v2);
        ReadAPE(APEtag);
        fCodec:=            'Monkey Audio';
        fEncoder:=          CompressionModeStr + ' (Ratio: ' + IntToStr(Trunc(Ratio)) + '%)';
        fPlayTime:=         Duration;
        fHasCoverArt:=      APEtag.HasCoverArt;
        end;
      Exit;
      end;

  //--------------------------------------------------
  if (fExt='.AAC') then
  begin
    if FAAC = nil then
      FAAC := NewAAC;
    with FAAC^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fChannels:=         SFormatChannels(Channels);
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate div 1000;
        fBitRateType:=      BitRateType;
        fDuration:=         Trunc(Duration);
        ReadID3v1(ID3v1);
        ReadID3v2(ID3v2);
        fCodec:=            'AAC';  { TODO : Complete }
        fPlayTime:=         Duration;
        fHasCoverArt:=      (ID3v2.Exists and ID3v2.HasCoverArt);
        end;
      Exit;
      end;
  end;

  //--------------------------------------------------
  if fExt='.WMA' then
    with FWMA^ do
      begin
      Result:= ReadFromFile(fn) and IsValidFile;
      if Result then
        begin
        SAppend(fTags, 'WMA', IsTag);
        SAppend(fTags, 'WMAEx', IsExTag);
        fChannels:=         ChannelMode;
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate;
        if IsVBR
          then fBitRateType:= 'VBR'
          else fBitRateType:= 'CBR';
        fDuration:=         Trunc(Duration);
        //Tags
        fTitle:=            Title;
        fArtist:=           Artist;
        fComment:=          Comment;
        fCopyright:=        Copyright;
        fEncoder:=          EncoderName;
        fPlayTime:=         Duration;
        //ExTags
        SAssign(fTitle, TitleEx);
        if fTitle = '' then
          fTitle := ExTagByNameString['WM/AlbumArtist'];
        SAssign(fArtist, ArtistEx);
        SAssign(fAlbum, AlbumEx);
        IAssign(fTrack, StrToIntDef(TrackEx, 0));
        SAssign(fDate, YearEx);
        SAssign(fGenre, GenreEx);
        SAssign(fComposer, ComposerEx);
        SAssign(fEncoder, EncoderEx);
        SAssign(fCopyright, CopyrightEx);
        SAssign(fURL, LinkEx);
        fCodec:=            'Windows Media Audio';  { TODO : Complete (8, 9, 10 etc) }
        fHasCoverArt:=      Assigned(ExTagByName['WM/Picture']);
        end;
      Exit;
      end;

  if (fExt='.WV') then
    with FWAVPack^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fDuration:= Trunc(Duration);
        fChannels:= SFormatChannels(Channels);
        fSampleRate:=SampleRate;
        fBitRate:=Trunc(BitRate);
        fEncoder:= Encoder + '  (Ratio: ' + IntToStr(Trunc(Ratio)) + '%)';
        ReadID3v1(ID3v1);
        ReadAPE(APEtag);
        fCodec:=            'WAVPack'; { TODO : Complete }
        fPlayTime:=         Duration;
        fHasCoverArt:=      APEtag.HasCoverArt;
        end;
      Exit;
      end;

  //--------------------------------------------------
  if fExt='.WAV' then
    with FWave^ do
      begin
      FWave^.ID3v2.UseExtendedGetFrameText := ExtendedGetFrameText;
      FWave^.ID3v2.FixTag24Sync := ExtendedGetFrameText;
      FWave^.ReadID3v2Tag := WaveReadID3v2Tag;
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fChannels:=         SFormatChannels(ChannelNumber);
        fSampleRate:=       SampleRate;
        fBitRate:=          Trunc(Bitrate); //BytesPerSecond*8 div 1000;
        fDuration:=         Trunc(Duration);
        fCodec:=            SProc.Format('%s, %d bit', [Format, BitsPerSample]);
        fPlayTime:=         Duration;

        for I := Low(Frames) to High(Frames) do
        begin
          if Frames[I].Name = 'ICRD' then
            fDate := string(Frames[I].Data)
          else if Frames[I].Name = 'ICOP' then
            fCopyright := string(Frames[I].Data)
          else if Frames[I].Name = 'IPRD' then
            fAlbum := string(Frames[I].Data)
          else if Frames[I].Name = 'ISFT' then
            fEncoder := string(Frames[I].Data)
          else if Frames[I].Name = 'IART' then
            fArtist := string(Frames[I].Data)
          else if Frames[I].Name = 'INAM' then
            fTitle := string(Frames[I].Data)
          else if Frames[I].Name = 'IGNR' then
            fGenre := string(Frames[I].Data)
          else if Frames[I].Name = 'ITRK' then
            fTrack := StrToIntDef(string(Frames[I].Data), 0)
          else if Frames[I].Name = 'ICMT' then
            fComment := string(Frames[I].Data)
        end;

        ReadID3v2(ID3v2);
        fHasCoverArt:=      ID3v2^.HasCoverArt;
        end;
      Exit;
      end;

  if (fExt='.AC3') then
  begin
    if FAC3 = nil then
      FAC3 := NewAC3;
    with FAC3^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fDuration:= Trunc(Duration);
        fChannels:= SFormatChannels(Channels);
        fSampleRate:=SampleRate;
        fBitRate:=BitRate;
        fCodec:=            'AC3';
        fPlayTime:=         Duration;
        end;
      Exit;
      end;
  end;

  if (fExt='.DTS') then
  begin
    if FDTS = nil then
      FDTS := NewDTS;
    with FDTS^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fDuration:= Trunc(Duration);
        fChannels:= SFormatChannels(Channels);
        fSampleRate:=SampleRate;
        fBitRate:=BitRate;
        fCodec:=            'DTS';
        fPlayTime:=         Duration;
        end;
      Exit;
      end;
  end;

  //--------------------------------------------------
  if (fExt='.MPC') then
  begin
    if FMPEGPlus = nil then
      FMPEGPlus := NewMPEGplus;
    with FMPEGplus^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        if Channels > 2 then
          fChannels:=       IntToStr(Channels)
        else
          fChannels:=       ChannelMode;
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate;
        fDuration:=         Trunc(Duration);
        fEncoder:=          Encoder;
        ReadID3v1(ID3v1);
        ReadID3v2(ID3v2);
        ReadAPE(APEtag);
        fCodec:=            'Musepack';
        fPlayTime:=         Duration;
        fHasCoverArt:=      APETag.HasCoverArt;
        end;
      Exit;
      end;
  end;

  //--------------------------------------------------

  if (fExt='.IT') or (fExt='.XM') or (fExt='.S3M') or
     (fExt='.MTM') or (fExt='.MOD') or (fExt='.UMX') or
     (fExt='.MO3') then
    begin
    Result:= GetModData(fn);
    if Result then
      begin
      fTitle:=    fModTitle;
      fComposer:= fModAuthor;
      fChannels:= IntToStr(fModChannels)+' ch.';
      fDuration:= fModDuration;
      fCodec:=    'Tracker Music';
      fPlayTime:= fModPlayTime;
      end;
    Exit;
    end;

  //--------------------------------------------------
  if (fExt='.CDA') then
  begin
    if FCDAtrack = nil then
      FCDAtrack := NewCDAtrack;
    with FCDAtrack^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        SAppend(fTags, 'CDA', true);
        fDuration:= Trunc(Duration);
        fTitle:= Title;
        fArtist:= Artist;
        fAlbum:= Album;
        fTrack:= Track;
        fComment:= Format('Position: %s', [SFormatDuration(Trunc(Position))]);
        fCodec:=            'CD Digital Audio';
        fPlayTime:=         Duration;
        end;
      Exit;
      end;
  end;

  if (fExt='.OFR') then
  begin
    if FOptimFrog = nil then
      FOptimFrog := NewOptimFrog;
    with FOptimFrog^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fDuration:= Trunc(Duration);
        fChannels:= SFormatChannels(Channels);
        fSampleRate:=SampleRate;
        fBitRate:=Trunc(BitRate);
//        fEncoder:= Encoder;
        ReadID3v1(ID3v1);
        ReadID3v2(ID3v2);
        ReadAPE(APEtag);
        fCodec:=            'OptimFrog';
        fPlayTime:=         Duration;
        end;
      Exit;
      end;
  end;

  if (fExt='.TTA') then
  begin
    if FTTA = nil then
      FTTA := NewTTA;
    with FTTA^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fDuration:= Trunc(Duration);
        fChannels:= SFormatChannels(Channels);
        fSampleRate:=SampleRate;
        fBitRate:=Trunc(BitRate);
//        fEncoder:= Encoder;
        ReadID3v1(ID3v1);
        ReadID3v2(ID3v2);
        ReadAPE(APEtag);
        fCodec:=            'TTA';
        fPlayTime:=         Duration;
        end;
      Exit;
      end;
  end;

  //--------------------------------------------------
  if fExt='.VQF' then
  begin
    if FVQF = nil then
      FVQF := NewTwinVQ;
    with FVQF^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        SAppend(fTags, 'TwinVQ', Exists);
        fChannels:=         ChannelMode;
        fSampleRate:=       SampleRate;
        fBitRate:=          BitRate;
        fDuration:=         Trunc(Duration);
        fTitle:=            Title;
        fArtist:=           Author;
        fAlbum:=            Album;
        //fTrack:=
        //fDate:=
        //fGenre:=
        fComment:=          Comment;
        fCopyright:=        Copyright;
        fCodec:=            'VQF';
        fPlayTime:=         Duration;
        end;
      Exit;
      end;
  end;

  if (fExt='.M4A') or (fExt='.MP4') or (fExt='.M4B') or (fExt='.M4R') then
    with MP4Tag^ do
      begin
        ARes := LoadFromFile(fn);
        Result := (ARes = MP4TAGLIBRARY_SUCCESS) or (ARes = MP4TAGLIBRARY_ERROR_NO_TAG_FOUND);
        if Result then
        begin
          fChannels:=         SFormatChannels(AudioChannelCount);
          fSampleRate:=       AudioSampleRate;
          fBitRate:=          BitRate;
          fDuration:=         Trunc(Playtime);
          fPlayTime:=         Playtime;
          if ARes = MP4TAGLIBRARY_SUCCESS then
          begin
            SAppend(fTags, 'MP4 ', true);
            fTitle:=            GetText(#169'nam');
            fArtist:=           GetText(#169'ART');
            fAlbum:=            GetText(#169'alb');
            fTrack:=            GetTrack;
            fDate:=             GetText(#169'day');
            fGenre:=            GetGenre;
            fComment:=          GetText(#169'cmt');
            fEncoder:=          GetText(#169'too');
            fComposer:=         GetText(#169'wrt');
            fCopyright:=        GetText('cprt');
            fURL:=              GetCommon('WWW', 'com.apple.iTunes');
            fHasCoverArt:=      Assigned(FindAtom('covr'));
          end;

          case AudioFormat of
            mp4afAAC: fCodec:='AAC';
            mp4afALAC: fCodec:='Apple Lossles';
            mpfafAC3: fCodec:='AC3'
            else fTags:=fTags + '(Unknown)';
          end;

          Clear;
        end;
        Exit;
      end;

  if (fExt='.DSF') or (fExt='.DFF') then
  begin
    if FDSF = nil then
      FDSF := NewDSF;
    with FDSF^ do
      begin
      FDSF^.ID3v2^.UseExtendedGetFrameText := ExtendedGetFrameText;
      FDSF^.ID3v2^.FixTag24Sync := ExtendedGetFrameText;
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fCodec:=            Format;
        if DSDFileType = dsdDSF then
          fCodec := fCodec + ' (DSF)'
        else if DSDFileType = dsdDFF then begin
          fCodec := fCodec + ' (DFF';
          if FFormatVersion <> '' then
            fCodec := fCodec + ', ' + FFormatVersion;
          fCodec := fCodec + ', ' + FCompressionName + ')';
        end;
        fChannels:=         SFormatChannels(ChannelCount);
        fSampleRate:=       SampleRate;
        fBitRate:=          Trunc(BitRate);
        fDuration:=         Trunc(Duration);
        fSampleRate:=       SampleRate;
        fPlayTime:=         Duration;
        fArtist:=           string(Artist);
        fTitle:=            string(Title);
        fComment:=          string(Comment);
        ReadID3v2(ID3v2);
        fHasCoverArt:=      ID3v2^.HasCoverArt;
        end;
      Exit;
      end;
  end;

  if (fExt='.AIF') or (fExt='.AIFF') or (fExt='.AIFC') then
  begin
    if FAIF = nil then
      FAIF := NewAIFF;
    with FAIF^ do
      begin
      FAIF^.ID3v2^.UseExtendedGetFrameText := ExtendedGetFrameText;
      FAIF^.ID3v2^.FixTag24Sync := ExtendedGetFrameText;
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        if LowerCase(string(CompressionStr)) = 'not compressed' then
          fCodec:=          'PCM'
        else
          fCodec:=          string(CompressionStr);
        if (fCodec = '') and (AIFFFileType = aftAIFF) then
          fCodec:=          'PCM';
        if CompressionID <> '' then
          fCodec:=          fCodec + ' / ' + string(CompressionID);
        fChannels:=         SFormatChannels(ChannelCount);
        fSampleRate:=       Trunc(SampleRate);
        fBitRate:=          Trunc(BitRate);
        fDuration:=         Trunc(Duration);
        fSampleRate:=       Trunc(SampleRate);
        fPlayTime:=         Duration;
        fArtist:=           string(Author);
        fTitle:=            string(Name);
        fComment:=          string(Annotation);
        fCopyright:=        string(Copyright);
        ReadID3v2(ID3v2);
        fHasCoverArt:=      ID3v2^.HasCoverArt;
        end;
      Exit;
      end;
  end;

  if (fExt='.MID') or (fExt='.MIDI') or (fExt='.KAR') or (fExt='.RMI') then
  begin
    if FMIDI = nil then
      FMIDI := NewMIDI;
    with FMIDI^ do
      begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
        begin
        fPlayTime:=         Duration / 1000;
        fDuration:=         Duration div 1000;
        fChannels:=         IntToStr(TrackCount);
        fComment:=          string(Lyrics);
        if IsSoftKaraoke then
          fCodec:=          'Midi Karaoke'
        else
          fCodec:=          'Midi';
        end;
        if TrackCount > 0 then
        begin
          fTitle:=            string(GetTrack(0).TrackName);
          fCopyright:=        string(GetTrack(0).Copyright);
          fArtist:=           string(GetTrack(0).TrackKeyword);
        end;

        Clear;
      Exit;
      end;
  end;

  if (fExt='.VGM') then
  begin
    if FVGM = nil then
      FVGM := NewVGM;
    with FVGM^ do
    begin
      Result:= ReadFromFile(fn) and Valid;
      if Result then
      begin
        fTitle:=            Track;
        fArtist:=           Author;
        fAlbum:=            Game;
        fDate:=             Date;
        fComment:=          Note;
        fCodec:=            'VGM ' + Version;
        if TagsPresent then
          fTags:=           'GD3';

        ResetData;
        Exit;
      end;
    end;
  end;

  //invalid file
  Result:= false;
end;

//-------------------------------------------------
function GetVeryLongFileName(const FullFileName: string): string; inline;
begin
  Result := FullFileName;
  if Length(FullFileName) < 256 then
    Exit;
  if Pos('\\?\', FullFileName) > 0 then
    Exit;

  if Pos('\\', FullFileName) > 0 then
    Result := '\\?\UNC\' + Copy(FullFileName, 3, MaxInt)
  else
    Result := '\\?\' + FullFileName;
end;


var
  fLastResult: boolean = false;

function GetAudioData(fn: string): boolean;
const
  CR: string = #13#10;
begin
  fn := GetVeryLongFileName(fn);
  if fn=fLastFilename then
    begin Result:= fLastResult; Exit end;
  ClearData;

  Result:= LoadData(fn);
  fLastFilename:= fn;
  fLastResult:= Result;
  if Result then
    fFullText:= fTitle+CR+
                fArtist+CR+
                fAlbum+CR+
                fGenre+CR+
                fComment+CR+
                fCommentsAll+CR+
                fComposer+CR+
                fCopyright+CR+
                fURL+CR+
                fEncoder+CR;
end;

//-------------------------------------------------
initialization

  DecimalSeparator := GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_SDECIMAL, '.');
  TimeSeparator := GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_STIME, ':');
  DurationFullStr[False] := MakeFormatString(3, True, False);
  DurationLongStr[False] := MakeFormatString(3, False, False);
  DurationShortStr[False] := MakeFormatString(2, False, False);
  DurationShortStrMs[False] := MakeFormatString(2, True, False);
  DurationFullStr[True] := MakeFormatString(3, True, True);
  DurationLongStr[True] := MakeFormatString(3, False, True);
  DurationShortStr[True] := MakeFormatString(2, False, True);
  DurationShortStrMs[True] := MakeFormatString(2, True, True);

  ClearData;
  FVorbis:= NewOggVorbis;
  FMPEG:= NewMPEGaudio;
  FWave:= NewWAV;
  FWMA:= NewWMA;
  FMonkey:= NewMonkey;
  FFLAC:= NewFLAC;
  FWAVPack:=NewWAVPack;
  MP4Tag:=NewMP4Tag;

  //FMPEGplus:= NewMPEGplus;
  //FCDAtrack:= NewCDAtrack;
  //FAC3:= NewAC3;
  //FDTS:= NewDTS;
  //FOptimFrog:=NewOptimFrog;
  //FTTA:=NewTTA;
  //FAAC:= NewAAC;
  //FVQF:= NewTwinVQ;
  //FDSF := NewDSF;
  //FAIF := NewAIFF;
  //FMIDI := NewMIDI;
  //FVGM := NewVGM;

finalization

  ClearData;

  FCDAtrack.Free;
  FFLAC.Free;
  FMonkey.Free;
  FVQF.Free;
  FAAC.Free;
  FWMA.Free;
  FWave.Free;
  FMPEGplus.Free;
  FMPEG.Free;
  FVorbis.Free;
  FAC3.Free;
  FDTS.Free;
  FWAVPack.Free;
  FOptimFrog.Free;
  FTTA.Free;
  MP4Tag.Free;
  FDSF.Free;
  FAIF.Free;
  FMIDI.Free;
  FVGM.Free;

end.

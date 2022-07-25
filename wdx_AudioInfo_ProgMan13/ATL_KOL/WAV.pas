{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TWAVfile - for extracting information from WAV file header            }
{                                                                             }
{ Copyright (c) 2001,2002 by Jurgen Faul                                      }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.2 (14 January 2002)                                               }
{   - Fixed bug with calculating of duration                                  }
{   - Some class properties added/changed                                     }
{                                                                             }
{ Version 1.1 (9 October 2001)                                                }
{   - Fixed bug with WAV header detection                                     }
{                                                                             }
{ Version 1.0 (31 July 2001)                                                  }
{   - Info: channel mode, sample rate, bits per sample, file size, duration   }
{                                                                             }
{ Портировано в KOL - Матвеев дмитрий                                         }
{                                                                             }
{ *************************************************************************** }

// Дата: 09.11.2005 Версия: 1.01
// Отключил контроль допустимого числа каналов, т.к. Mono и Stereo стало не актуально (например 8.1)
// Сответственно убил и свойство ChannelMode, т.к. как называется система 7.1 или 8.0 или 8.1
// я не знаю....

unit WAV;

interface

uses KOL;

const
  { Format type names }
  WAV_FORMAT_UNKNOWN = 'Unknown';
  WAV_FORMAT_PCM = 'Windows PCM';
  WAV_FORMAT_ADPCM = 'Microsoft ADPCM';
  WAV_FORMAT_ALAW = 'A-LAW';
  WAV_FORMAT_MULAW = 'MU-LAW';
  WAV_FORMAT_DVI_IMA_ADPCM = 'DVI/IMA ADPCM';
  WAV_FORMAT_MP3 = 'MPEG Layer III';
  WAV_FORMAT_TRUESPEECH = 'Truespeech';
  WAV_FORMAT_IEEE_FLOAT = 'IEEE Float';
  WAV_FORMAT_GSM = 'GSM';
  WAV_FORMAT_EXTENSIBLE = 'EXTENSIBLE';

  { Channel mode names }
//  WAV_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');

type
  PWAV = ^TWAV;
  TWAV = object(TObj)
    private
      FValid: Boolean;
      FFormatID: Word;
      FChannels: Byte;
      FSampleRate: Cardinal;
      FBytesPerSecond: Cardinal;
      FBlockAlign: Word;
      FBitsPerSample: Byte;
      FSampleNumber: Integer;
      FHeaderSize: Word;
      FFileSize: Cardinal;
      procedure ResetData;
      function GetFormat: string;
      function GetDuration: Double;
      procedure InitFields;
    public
      { Public declarations }
      function ReadFromFile(const FileName: string): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property FormatID: Word read FFormatID;              { Format type code }
      property Format: string read GetFormat;             { Format type name }
      property Channels: Byte read FChannels;            { Number of channels }
      property SampleRate: Cardinal read FSampleRate;      { Sample rate (hz) }
      property BytesPerSecond: Cardinal read FBytesPerSecond;  { Bytes/second }
      property BlockAlign: Word read FBlockAlign;           { Block alignment }
      property BitsPerSample: Byte read FBitsPerSample;         { Bits/sample }
      property HeaderSize: Word read FHeaderSize;       { Header size (bytes) }
      property FileSize: Cardinal read FFileSize;         { File size (bytes) }
      property Duration: Double read GetDuration;       { Duration (seconds) }
  end;

  function NewWAV: PWAV;

implementation

const
  DATA_CHUNK = 'data';                                        { Data chunk ID }

type
  { WAV file header data }
  WAVRecord = packed record
    { RIFF file header }
    RIFFHeader: array [1..4] of AnsiChar;                        { Must be "RIFF" }
    FileSize: Integer;                           { Must be "RealFileSize - 8" }
    WAVEHeader: array [1..4] of AnsiChar;                        { Must be "WAVE" }
    { Format information }
    FormatHeader: array [1..4] of AnsiChar;                      { Must be "fmt " }
    FormatSize: Integer;                                        { Format size }
    FormatID: Word;                                        { Format type code }
    ChannelNumber: Word;                                 { Number of channels }
    SampleRate: Integer;                                   { Sample rate (hz) }
    BytesPerSecond: Integer;                                   { Bytes/second }
    BlockAlign: Word;                                       { Block alignment }
    BitsPerSample: Word;                                        { Bits/sample }
    DataHeader: array [1..4] of AnsiChar;                         { Can be "data" }
    SampleNumber: Integer;                     { Number of samples (optional) }
  end;

function NewWAV: PWAV;
begin
    New(Result, Create);
    Result.InitFields;
end;

{ ********************* Auxiliary functions & procedures ******************** }

{$I-}
function ReadWAV(const FileName: string; var WAVData: WAVRecord): Boolean;
var
  SourceFile: PStream;
begin
  try
    Result := true;
    { Set read-access and open file }
    SourceFile:= NewReadFileStreamW(FileName);
    try
      { Read header }
      FillChar(WAVData, 40, 0);
      SourceFile.Read(WAVData, 40);
      { Read number of samples if exists }
      if WAVData.DataHeader <> DATA_CHUNK then
      begin
        SourceFile.Seek(WAVData.FormatSize + 28, spBegin);
        SourceFile.Read(WAVData.SampleNumber, 4);
      end;
    finally
      SourceFile.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;
{$I+}
{ --------------------------------------------------------------------------- }

function HeaderIsValid(const WAVData: WAVRecord): Boolean;
begin
  { Header validation }
  Result:=
     (WAVData.RIFFHeader = 'RIFF') and
     (WAVData.WAVEHeader = 'WAVE') and
     (WAVData.FormatHeader = 'fmt ');
end;

{ ********************** Private functions & procedures ********************* }

procedure TWAV.ResetData;
begin
  { Reset all data }
  FValid := false;
  FFormatID := 0;
  FChannels := 0;
  FSampleRate := 0;
  FBytesPerSecond := 0;
  FBlockAlign := 0;
  FBitsPerSample := 0;
  FSampleNumber := 0;
  FHeaderSize := 0;
  FFileSize := 0;
end;

{ --------------------------------------------------------------------------- }

function TWAV.GetFormat: string;
begin
  { Get format type name }
  case FFormatID of
    1: Result := WAV_FORMAT_PCM;
    2: Result := WAV_FORMAT_ADPCM;
    3: Result := WAV_FORMAT_IEEE_FLOAT;
    6: Result := WAV_FORMAT_ALAW;
    7: Result := WAV_FORMAT_MULAW;
    17: Result := WAV_FORMAT_DVI_IMA_ADPCM;
    34: Result := WAV_FORMAT_TRUESPEECH;
    49: Result := WAV_FORMAT_GSM;
    85: Result := WAV_FORMAT_MP3;
    65534: Result := WAV_FORMAT_EXTENSIBLE;
  else
    Result := WAV_FORMAT_UNKNOWN;
  end;
end;

{ --------------------------------------------------------------------------- }

function TWAV.GetDuration: Double;
begin
  { Get duration }
  Result := 0;
  if FValid then
  begin
    if (FSampleNumber = 0) and (FBytesPerSecond > 0) then
      Result := (FFileSize - FHeaderSize) / FBytesPerSecond;
    if (FSampleNumber > 0) and (FSampleRate > 0) then
      Result := FSampleNumber / FSampleRate;
  end;
end;

{ ********************** Public functions & procedures ********************** }

procedure TWAV.InitFields;
begin
    ResetData;
end;

{ --------------------------------------------------------------------------- }

function TWAV.ReadFromFile(const FileName: string): Boolean;
var
  WAVData: WAVRecord;
begin
  { Reset and load header data from file to variable }
  try
    ResetData;
    FillChar(WAVData, SizeOf(WAVData), 0);
    Result := ReadWAV(FileName, WAVData);
    { Process data if loaded and header valid }
    if (Result) and (HeaderIsValid(WAVData)) then
    begin
      FValid := true;
      { Fill properties with header data }
      FFormatID := WAVData.FormatID;
      FChannels := WAVData.ChannelNumber;
      FSampleRate := WAVData.SampleRate;
      FBytesPerSecond := WAVData.BytesPerSecond;
      FBlockAlign := WAVData.BlockAlign;
      FBitsPerSample := WAVData.BitsPerSample;
      FSampleNumber := WAVData.SampleNumber;
      if WAVData.DataHeader = DATA_CHUNK then FHeaderSize := 44
      else FHeaderSize := WAVData.FormatSize + 40;
      FFileSize := WAVData.FileSize + 8;
      if FHeaderSize > FFileSize then FHeaderSize := FFileSize;
    end;
  except
    Result := False;
  end;
end;

end.

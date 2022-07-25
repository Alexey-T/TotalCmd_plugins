{ ********************************************************************************* }
{                                                                                   }
{ Based on one of latest free version of ID3v2 Library                              }
{                                                                                   }
{ https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WAVTagLibrary.html }
{                                                                                   }
{ ********************************************************************************* }

unit WAVfile;

interface

uses KOL, ID3v2;

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
  WAV_MODE: array [0..3] of string = ('Unknown', 'Mono', 'Stereo', 'Multichannel');

type
  DWORD = Cardinal;

type
    TRIFFID = Array [0..3] of AnsiChar;
    TRIFFChunkID = Array [0..3] of AnsiChar;

    TWaveHeader = record
        ident1: TRIFFID;                    // Must be "RIFF"
        len: DWORD;                         // Remaining length after this header
    end;

type
    TWaveds64 = record
        ds64Size: DWORD;
        RIFFSizeLow: DWORD;
        RIFFSizeHigh: DWORD;
        DataSizeLow: DWORD;
        DataSizeHigh: DWORD;
        SampleCountLow: DWORD;
        SampleCountHigh: DWORD;
        TableLength: DWORD;
    end;

type
    TWaveFmt = record
        //ident2: TWAVIdent;                // Must be "WAVE"
        //ident3: TWAVIdent;                // Must be "fmt "
        fmtSize: DWORD;                     // Reserved 4 bytes
        FormatTag: Word;                    // format type
        Channels: Word;                     // number of channels (i.e. mono, stereo, etc.)
        SamplesPerSec: DWORD;               // sample rate
        AvgBytesPerSec: DWORD;              // for buffer estimation
        BlockAlign: Word;                   // block size of data
        BitsPerSample: Word;                // number of bits per sample of mono data
        //* WAVE_FORMAT_EXTENSIBLE
        cbSize: Word;	                      // Size of the extension: 22
        ValidBitsPerSample: Word;	          // at most 8 *  M
        ChannelMask: DWORD;	                // Speaker position mask: 0
        SubFormat: Array[0..15] of Byte;    // 16
    end;

type
  { WAV file header data }
  WAVRecord = packed record
    { RIFF file header }
    RIFFHeader: array [1..4] of AnsiChar;                        { Must be "RIFF" }
    FileSize: Integer;                           { Must be "RealFileSize - 8" }
    WAVEHeader: array [1..4] of AnsiChar;                        { Must be "WAVE" }
    { Format information }
    FormatHeader: array [1..4] of AnsiChar;                      { Must be "fmt " }
    FormatSize: Cardinal;                                        { Format size }
    FormatID: Word;                                        { Format type code }
    ChannelNumber: Word;                                 { Number of channels }
    SampleRate: Integer;                                   { Sample rate (hz) }
    BytesPerSecond: Integer;                                   { Bytes/second }
    BlockAlign: Word;                                       { Block alignment }
    BitsPerSample: Word;                                        { Bits/sample }
    DataHeader: array [1..4] of AnsiChar;                         { Can be "data" }
    SampleNumber: Cardinal;                     { Number of samples (optional) }
  end;

  TFrame = record
    Name, Data: AnsiString;
  end;

type
  PWAV = ^TWAV;
  TWAV = object(TObj)
    private
      { Private declarations }
      FValid: Boolean;
      FFormatSize: Cardinal;
      FFormatID: Word;
      FChannelNumber: Byte;
      FSampleRate: Cardinal;
      FBytesPerSecond: Cardinal;
      FBlockAlign: Word;
      FBitsPerSample: Byte;
      FSampleNumber: Cardinal;
      FHeaderSize: Cardinal;
      FFileSize: Cardinal;
      FAmountTrimBegin: Cardinal;
      FAmountTrimEnd: Cardinal;
      FBitrate: Double;
      FDuration: Double;
      FID3v2: PID3v2;
      FReadID3v2Tag: Boolean;
      procedure FResetData;
      function FGetFormat: string;
      function FGetChannelMode: string;
      function FGetDuration: Double;
      function FGetRatio: Double;
      function GetWAVEInformation(const Stream: PStream): TWaveFmt;
      function ReadWAVStream(const Stream: PStream; var WAVData: WAVRecord; var WaveFmt: TWaveFmt): Boolean;
    public
      FrameCount: Integer;
      Frames: array of TFrame;
      function AddFrame: Integer;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      destructor Destroy; virtual;
      function ReadFromFile(const FileName: string): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property FormatSize: Cardinal read FFormatSize;
      property FormatID: Word read FFormatID;              { Format type code }
      property Format: string read FGetFormat;             { Format type name }
      property ChannelNumber: Byte read FChannelNumber;  { Number of channels }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property SampleRate: Cardinal read FSampleRate;      { Sample rate (hz) }
      property BytesPerSecond: Cardinal read FBytesPerSecond;  { Bytes/second }
      property BlockAlign: Word read FBlockAlign;           { Block alignment }
      property BitsPerSample: Byte read FBitsPerSample;         { Bits/sample }
      property HeaderSize: Cardinal read FHeaderSize;   { Header size (bytes) }
      property FileSize: Cardinal read FFileSize;         { File size (bytes) }
      property Duration: Double read FGetDuration;       { Duration (seconds) }
      property SampleNumber: Cardinal read FSampleNumber;
      property Ratio: Double read FGetRatio;          { Compression ratio (%) }
      property AmountTrimBegin: Cardinal read FAmountTrimBegin;
      property AmountTrimEnd: Cardinal read FAmountTrimEnd;
      property Bitrate: Double read FBitrate;
      property ID3v2: PID3v2 read FID3v2;
      property ReadID3v2Tag: Boolean read FReadID3v2Tag write FReadID3v2Tag;
  end;

  function NewWAV: PWAV;

implementation

const
  DATA_CHUNK: AnsiString = 'data';                                        { Data chunk ID }

const
  RIFFID: AnsiString = 'RIFF';
  RF64ID: AnsiString = 'RF64';
  RIFFWAVEID: AnsiString = 'WAVE';
  RIFFLISTID: AnsiString = 'LIST';
  RIFFINFOID: AnsiString = 'INFO';

function NewWAV: PWAV;
begin
    New(Result, Create);
    Result.FResetData;
end;


procedure FixOdd(var Value: Cardinal);
begin
  if Odd(Value) then
    Inc(Value)
end;

function MakeUInt64(LowDWord, HiDWord: DWord): UInt64; inline;
begin
    Result := LowDWord OR (UInt64(HiDWord) SHL 32);
end;

function HighDWordOfUInt64(Value: UInt64): Cardinal; inline;
begin
    Result := Value SHR 32;
end;

function ID3v2ValidTag(const Stream: PStream): Boolean;
var
    Identification: array [0..2] of AnsiChar;
begin
    Result := False;
    try
        FillChar(Identification, SizeOf(Identification), 0);
        Stream.Read(Identification[0], 3);
        if Identification = 'ID3'
        then begin
            Result := True;
        end;
    except
        //*
    end;
end;

function SeekRIFF(const Stream: PStream): Integer;
var
    RIFFChunkSize: DWord;
    ChunkID: array [0..3] of AnsiChar;
    ChunkSize: DWord;
begin
    Result := 0;
    try
        //* Find ID3v2
        FillChar(ChunkID, SizeOf(ChunkID), 0);
        Stream.Read(RIFFChunkSize, 4);
        Stream.Read(ChunkID, 4);
        if ChunkID = 'WAVE'
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read(ChunkID, 4);
                Stream.Read(ChunkSize, 4);
                if (ChunkID = 'id3 ')
                OR
                (ChunkID = 'ID32')
                then begin
                    Result := ChunkSize;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, spCurrent);
                end;
            end;
        end;
    except
        Result := 0;
    end;
end;

function SeekRIFFEx(TagStream: PStream): Integer;
var
    RIFFChunkSize: Cardinal;
    ChunkID: TRIFFChunkID;
    ChunkSize: Cardinal;
begin
    Result := 0;
    try
        TagStream.Seek(4, spCurrent);
        TagStream.Read(RIFFChunkSize, 4);
        TagStream.Read(ChunkID, 4);
        if ChunkID = RIFFWAVEID
        then begin
            ChunkSize := 0;
            while TagStream.Position + 8 < TagStream.Size do begin
                TagStream.Read(ChunkID, 4);
                TagStream.Read(ChunkSize, 4);
                FixOdd(ChunkSize);
                if ChunkID = RIFFLISTID
                then begin
                    TagStream.Read(ChunkID, 4);
                    if ChunkID = RIFFINFOID
                    then begin
                        Result := ChunkSize + 8;
                        Exit;
                    end;
                end else begin
                    TagStream.Seek(ChunkSize, spCurrent);
                end;
            end;
        end;
    except
        Result := 0;
    end;
end;

procedure UnSyncSafe(var Source; const SourceSize: Integer; var Dest: Cardinal);
type
    TBytes = array [0..MaxInt - 1] of Byte;
var
    I: Byte;
begin
    { Test : Source = $01 $80 -> Dest = 255
             Source = $02 $00 -> Dest = 256
             Source = $02 $01 -> Dest = 257 etc.
    }
    Dest := 0;
    for I := 0 to SourceSize - 1 do begin
        Dest := Dest shl 7;
        Dest := Dest or (TBytes(Source)[I] and $7F); // $7F = %01111111
    end;
end;

function GetID3v2Size(const Source: PStream): Cardinal;
type
    ID3v2Header = packed record
        ID: array [0..2] of AnsiChar;
        Version: Byte;
        Revision: Byte;
        Flags: Byte;
        Size: Cardinal;
    end;
var
    Header: ID3v2Header;
begin
    // Get ID3v2 tag size (if exists)
    Result := 0;
    Source.Seek(0, spBegin);
    Source.Read(Header, SizeOf(ID3v2Header));
    if Header.ID = 'ID3'
    then begin
        UnSyncSafe(Header.Size, 4, Result);
        Inc(Result, 10);
        if Result > Source.Size then begin
            Result := 0;
        end;
    end;
end;

function CheckRIFF(const TagStream: PStream): Boolean;
var
    PreviousPosition: Int64;
    Identification: TRIFFID;
begin
    Result := False;
    PreviousPosition := TagStream.Position;
    try
        try
            FillChar(Identification, SizeOf(TRIFFID), 0);
            TagStream.Read(Identification[0], 4);
            if Identification = RIFFID
            then begin
                Result := True;
            end;
        except
            Result := False;
        end;
    finally
        TagStream.Seek(PreviousPosition, spBegin);
    end;
end;

function CheckRF64(const TagStream: PStream): Boolean;
var
    PreviousPosition: Int64;
    Identification: TRIFFID;
begin
    Result := False;
    PreviousPosition := TagStream.Position;
    try
        try
            FillChar(Identification, SizeOf(TRIFFID), 0);
            TagStream.Read(Identification[0], 4);
            if Identification = RF64ID
            then begin
                Result := True;
            end;
        except
            Result := False;
        end;
    finally
        TagStream.Seek(PreviousPosition, spBegin);
    end;
end;

function SeekRF64(TagStream: PStream): Integer;
var
    RIFFChunkSize: DWord;
    ChunkID: TRIFFChunkID;
    ChunkSize: DWord;
    ds64DataSize: UInt64;
    Waveds64: TWaveds64;
begin
    Result := 0;
    try
        TagStream.Seek(4, spCurrent);
        TagStream.Read(RIFFChunkSize, 4);
        TagStream.Read(ChunkID, 4);
        if ChunkID = RIFFWAVEID
        then begin
            ChunkSize := 0;
            while TagStream.Position + 8 < TagStream.Size do begin
                TagStream.Read(ChunkID, 4);
                if ChunkID = 'ds64'
                then begin
                    TagStream.Read(Waveds64, SizeOf(TWaveds64));
                    TagStream.Seek(Waveds64.ds64Size - SizeOf(TWaveds64) + 4 {table?}, spCurrent);
                    Continue;
                end;
                TagStream.Read(ChunkSize, 4);
                FixOdd(ChunkSize);
                if ChunkID = RIFFLISTID
                then begin
                    TagStream.Read(ChunkID, 4);
                    if ChunkID = RIFFINFOID
                    then begin
                        Result := ChunkSize + 8;
                        Exit;
                    end;
                end else begin
                    if (ChunkID = 'data')
                    AND (ChunkSize = $FFFFFFFF)
                    then begin
                        ds64DataSize := MakeUInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh);
                        TagStream.Seek(ds64DataSize, spCurrent);
                    end else begin
                        TagStream.Seek(ChunkSize, spCurrent);
                    end;
                end;
            end;
        end;
    except
        Result := 0;
    end;
end;

{ ********************* Auxiliary functions & procedures ******************** }

{ --------------------------------------------------------------------------- }

function HeaderIsValid(const WAVData: WAVRecord): Boolean;
begin
  Result := True;
  { Header validation }
  if WAVData.RIFFHeader <> 'RIFF' then Result := False;
  if WAVData.WAVEHeader <> 'WAVE' then Result := False;
  if WAVData.FormatHeader <> 'fmt ' then Result := False;
  if WAVData.ChannelNumber = 0 then Result := False;
end;

{ ********************** Private functions & procedures ********************* }

procedure TWAV.FResetData;
begin
  { Reset all data }
  FValid := false;
  FFormatSize := 0;
  FFormatID := 0;
  FChannelNumber := 0;
  FSampleRate := 0;
  FBytesPerSecond := 0;
  FBlockAlign := 0;
  FBitsPerSample := 0;
  FSampleNumber := 0;
  FHeaderSize := 0;
  FFileSize := 0;
  FAmountTrimBegin := 0;
  FAmountTrimEnd := 0;
  FBitrate := 0;
  FDuration := 0;
  FrameCount := 0;
  Finalize(Frames);
  FID3v2.ResetData;
end;

function TWAV.GetWAVEInformation(const Stream: PStream): TWaveFmt;
var
    SourceHeader: TWaveHeader;
    ChunkIdent: TRIFFChunkID;
    ChunkSize: DWord;
    SourceISRF64: Boolean;
    Sourceds64: TWaveds64;
    ASampleNumber: DWord;
    DataSize32: DWord;
    DataPosition: Int64;
    PreviousPosition: Int64;
    FSampleDataSize: Cardinal;
begin
    ASampleNumber := 0;
    DataPosition := 0;
    FSampleDataSize := 0;

    Stream.Seek(0, spBegin);

    Stream.Read(SourceHeader, SizeOf(TWaveHeader));

    SourceISRF64 := (SourceHeader.ident1 = RF64ID);

    Stream.Read(ChunkIdent, 4);
    if (ChunkIdent <> RIFFWAVEID)
    then begin
        Exit;
    end;

    if SourceISRF64 then begin
        Stream.Read(ChunkIdent, 4);
        if (ChunkIdent = 'ds64')
        then begin
            Stream.Read(Sourceds64, SizeOf(TWaveds64));
            FSampleDataSize := MakeUInt64(Sourceds64.DataSizeLow, Sourceds64.DataSizeHigh);
            FSampleNumber := MakeUInt64(Sourceds64.SampleCountLow, Sourceds64.SampleCountHigh);
            Stream.Seek(Sourceds64.ds64Size - SizeOf(TWaveds64) + 4 {table?}, spCurrent);
        end;
    end;

    repeat
        Stream.Read(ChunkIdent, 4);
        if (ChunkIdent <> 'fmt ')
        then begin
            Stream.Read(ChunkSize, 4);
            FixOdd(ChunkSize);
            Stream.Seek(ChunkSize, spCurrent);
        end;
    until ((ChunkIdent = 'fmt '))
    OR (Stream.Position >= Stream.Size);

    PreviousPosition := Stream.Position - 4;

    Stream.Read(Result, SizeOf(TWaveFmt));

    if (Result.FormatTag <> $FFFE)
    OR (Result.fmtSize <> SizeOf(TWaveFmt))
    then begin
        Result.cbSize := 0;
        Result.ValidBitsPerSample := 0;
        Result.ChannelMask := 0;
        FillChar(Result.SubFormat[0], SizeOf(Result.SubFormat), 0);
    end;

    Stream.Seek(PreviousPosition, spBegin);

    repeat
        Stream.Read(ChunkIdent, 4);
        //* fact
        if (ChunkIdent = 'fact')
        then begin
            Stream.Read(ASampleNumber, 4);
            Stream.Seek(- 4, spCurrent);
        end;
        //* data
        if (ChunkIdent <> DATA_CHUNK)
        then begin
            Stream.Read(ChunkSize, 4);
            FixOdd(ChunkSize);
            DataPosition := Stream.Position;
            Stream.Seek(ChunkSize, spCurrent);
        end;
    until (ChunkIdent = DATA_CHUNK)
    OR (Stream.Position >= Stream.Size);

    Stream.Read(DataSize32, 4);
    if DataSize32 > Stream.Size - DataPosition then begin
        DataSize32 := Stream.Size - DataPosition;
    end;

    if NOT SourceISRF64 then begin
        FSampleDataSize := DataSize32;
        FSampleNumber := DataSize32 div Result.BlockAlign;
    end;

    if ASampleNumber div Result.BlockAlign <= 0 then begin
        FDuration := FSampleDataSize / Result.AvgBytesPerSec;
    end else begin
        FDuration := FSampleNumber / Result.SamplesPerSec;
    end;

    FBitRate := Result.AvgBytesPerSec div 125;
end;

{ --------------------------------------------------------------------------- }

function TWAV.AddFrame: Integer;
begin
  Inc(FrameCount);
  if FrameCount > Length(Frames) then
    SetLength(Frames, Length(Frames) + 36);
  Result := FrameCount - 1;

  //SetLength(Frames, Length(Frames) + 1);
  //Result := High(Frames);
end;

function TWAV.ReadWAVStream(const Stream: PStream; var WAVData: WAVRecord;
  var WaveFmt: TWaveFmt): Boolean;
var
  ID3v2Size, I: Integer;
  LISTINFOChunkPosition: Int64;
  LISTChunkSize: Cardinal;
  PreviousPosition: Int64;
  ChunkID: array [0..3] of AnsiChar;
  ChunkSize: Cardinal;
begin
  try
    Result := False;

    { Reset and load header data from file to variable }
    FResetData;
    FillChar(WAVData, SizeOf(WAVData), 0);
    FillChar(WaveFmt, SizeOf(WaveFmt), 0);

    { Read header }
    Stream.Read(WAVData, 36);

    { Read number of samples }
    if Stream.Size > (WAVData.FormatSize + 24) then
    begin
      Stream.Seek(WAVData.FormatSize + 24, spBegin);
      Stream.Read(WAVData.SampleNumber, 4);
    end;

    Result := HeaderIsValid(WAVData);

    if not Result then
      Exit;

    WaveFmt := GetWAVEInformation(Stream);

    { Seek past the ID3v2 tag, if there is one }
    Stream.Seek(0, spBegin);
    PreviousPosition := Stream.Position;
    ID3v2Size := GetID3v2Size(Stream);
    Stream.Seek(ID3v2Size, spBegin);
    try
        if CheckRIFF(Stream) then begin
            LISTChunkSize := SeekRIFFex(Stream);
        end else begin
            Stream.Seek(PreviousPosition, spBegin);
            if CheckRF64(Stream) then begin
                LISTChunkSize := SeekRF64(Stream);
            end else begin
                Exit;
            end;
        end;

        LISTINFOChunkPosition := Stream.Position - 4 - 8;
        while Stream.Position < LISTINFOChunkPosition + LISTChunkSize do begin
            Stream.Read(ChunkID, 4);
            Stream.Read(ChunkSize, 4);
            FixOdd(ChunkSize);
            I := AddFrame;
            begin
                Frames[I].Name := UpperCase(ChunkID);
                SetLength(Frames[I].Data, ChunkSize - 1);
                Stream.Read(Frames[I].Data[1], ChunkSize - 1);
                Stream.Seek(1, spCurrent);
            end;
            if Stream.Position >= Stream.Size then
              Break;
        end;
    finally
      SetLength(Frames, FrameCount)
    end;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function TWAV.FGetFormat: string;
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

destructor TWAV.Destroy;
begin
  FResetData;
  FID3v2.Free;
  inherited;
end;

function TWAV.FGetChannelMode: string;
begin
  { Get channel mode name }            //multichannel
  if FChannelNumber > 2 then Result := WAV_MODE[3] else
    Result := WAV_MODE[FChannelNumber];
end;

{ --------------------------------------------------------------------------- }

function TWAV.FGetDuration: Double;
begin
  Exit(FDuration);

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






constructor TWAV.Create;
begin
  { Create object }
  inherited;
  FReadID3v2Tag := True;
  FID3v2 := NewID3v2;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

function TWAV.ReadFromFile(const FileName: string): Boolean;
var
  WAVData: WAVRecord;
  SourceFile: PStream;
  WaveFmt: TWaveFmt;
begin
  try
    SourceFile := NewReadFileStreamW(FileName);
    try

      Result := ReadWAVStream(SourceFile, WAVData, WaveFmt);

      { Process data if loaded and header valid }
      if (Result) { and (HeaderIsValid(WAVData)) } then
      begin
        FValid := True;
        FChannelNumber := WaveFmt.Channels;
        FFormatID := WaveFmt.FormatTag;
        FSampleRate := WaveFmt.SamplesPerSec;
        FBitsPerSample := WaveFmt.BitsPerSample;
        (*
          { Fill properties with header data }
          FFormatSize := WAVData.FormatSize;
          FFormatID := WAVData.FormatID;
          FChannelNumber := WAVData.ChannelNumber;
          FSampleRate := WAVData.SampleRate;
          FBytesPerSecond := WAVData.BytesPerSecond;
          FBlockAlign := WAVData.BlockAlign;
          FBitsPerSample := WAVData.BitsPerSample;
          FSampleNumber := WAVData.SampleNumber div FBlockAlign;
          if WAVData.DataHeader = DATA_CHUNK then FHeaderSize := 44
          else FHeaderSize := WAVData.FormatSize + 28;
          FFileSize := WAVData.FileSize + 8;
          if FHeaderSize > FFileSize then FHeaderSize := FFileSize;
          FFileName := FileName;
          FBitrate := FBytesPerSecond * 8 / 1000;
        *)
        if FReadID3v2Tag then
        begin
          SourceFile.Seek(4, spBegin);
          if SeekRIFF(SourceFile) = 0 then
            Exit;
          if not ID3v2ValidTag(SourceFile) then
            Exit;
          SourceFile.Seek(-3, spCurrent);
          FID3v2.ReadFromStreamEx(SourceFile);
        end;
      end;

    finally
      SourceFile.Free;
    end;
  except
    Result := False;
  end;
end;

{ --------------------------------------------------------------------------- }

function TWAV.FGetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    if FSampleNumber = 0 then
      Result := FFileSize / ((FFileSize - FHeaderSize) / FBytesPerSecond * FSampleRate * (FChannelNumber * FBitsPerSample / 8) + 44) * 100
    else
      Result := FFileSize / (FSampleNumber * (FChannelNumber * FBitsPerSample / 8) + 44) * 100
  else
    Result := 0;
end;

end.

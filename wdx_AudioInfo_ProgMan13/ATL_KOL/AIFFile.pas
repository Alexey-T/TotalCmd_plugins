{ ******************************************************************************** }
{                                                                                  }
{ Based on one of latest free version of ID3v2 Library                             }
{                                                                                  }
{ https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html }
{                                                                                  }
{ ******************************************************************************** }

unit AIFFile;

interface

uses KOL, ID3v2;

const
  AIFF_CHANNEL_MODE: array [0..3] of string = ('Unknown', 'Mono', 'Stereo', 'Multichannel');

type
   TDSFChannelType = (dsfctUnknown, dsfctMono, dsfctStereo, dsfct3Channels, dsfctQuad, dsfct4Channels, dsfct5Channels, dsfct51Channels);
   TAIFFFileType = (aftUnknown, aftAIFF, aftAIFC);

type
  PAIFF = ^TAIFF;
  TAIFF = object(TObj)
    private
      { Private declarations }
      FValid: Boolean;
      FChannelCount: Word;
      FSampleCount: Cardinal;
      FSampleFrames: Cardinal;
      FSampleSize: Word;
      FSampleRate: Double;
      FFileSize: Int64;
      FAmountTrimBegin: Cardinal;
      FAmountTrimEnd: Cardinal;
      FBitrate: Double;
      FDuration: Double;
      FAIFFFileType: TAIFFFileType;
      FID3v2: PID3v2;
      FCompressionID: AnsiString;  // http://en.wikipedia.org/wiki/Audio_Interchange_File_Format
      FCompressionStr: AnsiString;
      FName, FAuthor, FCopyright, FAnnotation: AnsiString;
      procedure FResetData;
      function FGetChannelMode: string;
      function LoadAIFFInfo(const Stream: PStream): Boolean;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      destructor Destroy; virtual;
      function ReadFromFile(const FileName: string): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property ChannelCount: Word read FChannelCount;  { Number of channels }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property SampleRate: Double read FSampleRate;      { Sample rate (hz) }
      property FileSize: Int64 read FFileSize;         { File size (bytes) }
      property SampleCount: Cardinal read FSampleCount;
      property SampleFrames: Cardinal read FSampleFrames;
      property SampleSize: Word read FSampleSize;
      property Bitrate: Double read FBitrate;
      property Duration: Double read FDuration;
      property AIFFFileType: TAIFFFileType read FAIFFFileType;
      property ID3v2: PID3v2 read FID3v2;
      property CompressionID: AnsiString read FCompressionID;
      property CompressionStr: AnsiString read FCompressionStr;
      property Author: AnsiString read FAuthor;
      property Name: AnsiString read FName;
      property Annotation: AnsiString read FAnnotation;
      property CopyRight: AnsiString read FCopyRight;
  end;

  function NewAIFF: PAIFF;

implementation

type
  Int64Rec = packed record
    case Integer of
      0: (Lo, Hi: Cardinal);
      1: (Cardinals: array [0..1] of Cardinal);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;

  TExtendedBytes = array [0..9] of Byte;


function NewAIFF: PAIFF;
begin
    New(Result, Create);
    Result.FResetData;
end;


procedure FixOdd(var Value: Cardinal);
begin
  if Odd(Value) then
    Inc(Value)
end;

function ReverseBytes16(Value: Word): Word; inline;
begin
  Result := System.Swap(Value);
end;

function ReverseBytes32(Value: Cardinal): Cardinal; inline;
begin
  Result := (Value SHR 24) OR (Value SHL 24) OR ((Value AND $00FF0000) SHR 8) OR ((Value AND $0000FF00) SHL 8);
end;

function ReverseBytes64(const aVal: Int64): Int64; overload; inline;
begin
    Int64Rec(Result).Bytes[0] := Int64Rec(aVal).Bytes[7];
    Int64Rec(Result).Bytes[1] := Int64Rec(aVal).Bytes[6];
    Int64Rec(Result).Bytes[2] := Int64Rec(aVal).Bytes[5];
    Int64Rec(Result).Bytes[3] := Int64Rec(aVal).Bytes[4];
    Int64Rec(Result).Bytes[4] := Int64Rec(aVal).Bytes[3];
    Int64Rec(Result).Bytes[5] := Int64Rec(aVal).Bytes[2];
    Int64Rec(Result).Bytes[6] := Int64Rec(aVal).Bytes[1];
    Int64Rec(Result).Bytes[7] := Int64Rec(aVal).Bytes[0];
end;

function ReverseBytes64(const aVal: UInt64): UInt64; overload; inline;
begin
    Result := UInt64(ReverseBytes64(Int64(aVal)));
end;

{$IFNDEF FPC}
{$IFDEF WIN64}
procedure ReverseExtended80(Source: TExtended80Rec; var Dest: TExtended80Rec);
begin
    Dest.Bytes[0] := Source.Bytes[9];
    Dest.Bytes[1] := Source.Bytes[8];
    Dest.Bytes[2] := Source.Bytes[7];
    Dest.Bytes[3] := Source.Bytes[6];
    Dest.Bytes[4] := Source.Bytes[5];
    Dest.Bytes[5] := Source.Bytes[4];
    Dest.Bytes[6] := Source.Bytes[3];
    Dest.Bytes[7] := Source.Bytes[2];
    Dest.Bytes[8] := Source.Bytes[1];
    Dest.Bytes[9] := Source.Bytes[0];
end;
{$ELSE}
procedure ReverseExtended(Source: TExtendedBytes; var Dest: TExtendedBytes);
begin
    Dest[0] := Source[9];
    Dest[1] := Source[8];
    Dest[2] := Source[7];
    Dest[3] := Source[6];
    Dest[4] := Source[5];
    Dest[5] := Source[4];
    Dest[6] := Source[3];
    Dest[7] := Source[2];
    Dest[8] := Source[1];
    Dest[9] := Source[0];
end;
{$ENDIF}
{$ENDIF}

function SeekAIFF(Stream: PStream): Integer;
var
    AIFFChunkSize: Cardinal;
    ChunkID: array [0..3] of AnsiChar;
    ChunkSize: Cardinal;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Read(AIFFChunkSize, 4);
        AIFFChunkSize := ReverseBytes32(AIFFChunkSize);
        Stream.Read(ChunkID, 4);
        if (ChunkID = 'AIFF') or (ChunkID = 'AIFC')
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read(ChunkID, 4);
                Stream.Read(ChunkSize, 4);
                ChunkSize := ReverseBytes32(ChunkSize);
                FixOdd(ChunkSize);
                if ChunkID = 'ID3 '
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

{ ********************** Private functions & procedures ********************* }

procedure TAIFF.FResetData;
begin
  { Reset all data }
  FValid := false;
  FChannelCount := 0;
  FSampleRate := 0;
  FSampleCount := 0;
  FSampleFrames := 0;
  FSampleSize := 0;
  FFileSize := 0;
  FAmountTrimBegin := 0;
  FAmountTrimEnd := 0;
  FBitrate := 0;
  FDuration := 0;
  FAIFFFileType := aftUnknown;
  FCompressionID := '';
  FCompressionStr := '';
  FName := '';
  FAuthor := '';
  FCopyright := '';
  FAnnotation := '';
  FID3v2.ResetData;
end;

function TAIFF.LoadAIFFInfo(const Stream: PStream): Boolean;
var
    ChunkSize: Cardinal;
    PreviousPosition: Int64;
    ChunkID: array [0..3] of AnsiChar;
    AIFFSize: Cardinal;
    DataByte: Byte;
    DataWord: Word;
    DataDWord: Cardinal;
    {$IFNDEF FPC}
    {$IFDEF WIN64}
    DataExtended: TExtended80Rec;
    {$ELSE}
    DataExtended: Extended;
    {$ENDIF}
    {$ELSE}
    DataExtended: Extended;
    {$ENDIF}
    AIFC: Boolean;
    i: Integer;
    DataSize: Int64;
    ChunksStartPosition: Int64;

  function GetStringData(ChunkSize: Cardinal): AnsiString;
  begin
    Result := '';
    if ChunkSize = 0 then
      Exit;
    SetLength(Result, ChunkSize);
    Stream.Read(Result[1], ChunkSize);
  end;

begin
    Result := False;
    FillChar(ChunkID, SizeOf(ChunkID), 0);
    AIFC := False;
    DataSize := 0;
    PreviousPosition := Stream.Position;
    try
        Stream.Seek(0, spBegin);
        //* Check if AIFF/AIFC
        Stream.Read(ChunkID, SizeOf(ChunkID));
        if ChunkID = 'FORM'
        then begin
            //* AIFF container size
            Stream.Read(AIFFSize, 4);
            AIFFSize := ReverseBytes32(AIFFSize);
            //* Check if AIFC
            Stream.Read(ChunkID, SizeOf(ChunkID));
            if ChunkID = 'AIFC'
            then begin
                AIFC := True;
            end else begin
                AIFC := False;
            end;
            //* If AIFF or AIFC continue
            if (ChunkID = 'AIFF') or (ChunkID = 'AIFC')
            then begin
                //* Store chunks root position
                ChunksStartPosition := Stream.Position;
                //* Search for the COMM chunk
                repeat
                    Stream.Read(ChunkID, 4);
                    Stream.Read(ChunkSize, 4);
                    ChunkSize := ReverseBytes32(ChunkSize);
                    FixOdd(ChunkSize);
                    if ChunkID <> 'COMM'
                    then begin
                        //* Go to next chunk
                        Stream.Seek(ChunkSize, spCurrent);
                    end else begin
                        Result := True;
                        //* Read in COMM content
                        Stream.Read(DataWord, 2);
                        FChannelCount := ReverseBytes16(DataWord);
                        Stream.Read(DataDWord, 4);
                        FSampleFrames := ReverseBytes32(DataDWord);
                        Stream.Read(DataWord, 2);
                        FSampleSize := ReverseBytes16(DataWord);
                        Stream.Read(DataExtended, 10);
                        {$IFNDEF FPC}
                            {$IFDEF WIN64}
                            //ReverseExtended(DataExtended, DataExtended);
                            ReverseExtended80(DataExtended, DataExtended);
                            {$ELSE}
                                {$IFDEF NEXTGEN}
                                //* Not supported
                                //ReverseExtended(DataExtended, DataExtended);
                                DataExtended := 0;
                                {$ELSE}
                                ReverseExtended(TExtendedBytes(DataExtended), TExtendedBytes(DataExtended));
                                {$ENDIF}
                            {$ENDIF}
                        {$ELSE}
                            //* Not supported
                            //ReverseExtended(DataExtended, DataExtended);
                        {$ENDIF}
                        FSampleRate := Extended(DataExtended);
                        //FSampleRate := DataExtended;
                        //* If AIFC we have more content
                        if AIFC then begin
                            //* Read compression ID (4 chars)
                            FCompressionID := '';
                            for i := 1 to 4 do begin
                                Stream.Read(DataByte, 1);
                                FCompressionID := FCompressionID + AnsiChar(DataByte);
                            end;
                            //* Read compression description (string)
                            Stream.Read(DataByte, 1);
                            SetLength(FCompressionStr, DataByte);
                            Stream.Read(FCompressionStr[1], DataByte);
                            if (FCompressionStr = '') and (FCompressionID = 'sowt') then
                              FCompressionStr := 'PCM';
                        end;
                    end;
                until (ChunkID = 'COMM')
                OR (Stream.Position >= AIFFSize);
                //* Go to root position to search for SSND
                Stream.Seek(ChunksStartPosition, spBegin);
                //* Search for SSND
                repeat
                    Stream.Read(ChunkID, 4);
                    Stream.Read(ChunkSize, 4);
                    ChunkSize := ReverseBytes32(ChunkSize);
                    if ChunkID <> 'SSND'
                    then begin
                      if ChunkID = 'NAME' then
                      begin
                        FName := GetStringData(ChunkSize);
                        if Odd(ChunkSize) then
                          Stream.Position := Stream.Position + 1;
                      end else
                      if ChunkID = 'AUTH' then
                      begin
                        FAuthor := GetStringData(ChunkSize);
                        if Odd(ChunkSize) then
                          Stream.Position := Stream.Position + 1;
                      end else
                      if ChunkID = '(c) ' then
                      begin
                        FCopyright := GetStringData(ChunkSize);
                        if Odd(ChunkSize) then
                          Stream.Position := Stream.Position + 1;
                      end else
                      if ChunkID = 'ANNO' then
                      begin
                        FAnnotation := GetStringData(ChunkSize);
                        if Odd(ChunkSize) then
                          Stream.Position := Stream.Position + 1;
                      end else begin
                        //* Go to next chunk
                        if Odd(ChunkSize) then
                          Inc(ChunkSize);
                        Stream.Seek(ChunkSize, spCurrent);
                      end;
                    end else begin
                        //* We have the audio data size
                        DataSize := ChunkSize;
                    end;
                until (ChunkID = 'SSND')
                OR (Stream.Position >= AIFFSize);

                Stream.Seek(ChunksStartPosition, spBegin);
                //* Search for SSND
                repeat
                    Stream.Read(ChunkID, 4);
                    Stream.Read(ChunkSize, 4);
                    ChunkSize := ReverseBytes32(ChunkSize);
                    if (Stream.Position >= AIFFSize) or (Stream.Position >= Stream.Size) then
                      Exit;
                    if ChunkID = 'NAME' then
                    begin
                      FName := GetStringData(ChunkSize);
                      if Odd(ChunkSize) then
                        Stream.Position := Stream.Position + 1;
                    end else
                    if ChunkID = 'AUTH' then
                    begin
                      FAuthor := GetStringData(ChunkSize);
                      if Odd(ChunkSize) then
                        Stream.Position := Stream.Position + 1;
                    end else
                    if ChunkID = '(c) ' then
                    begin
                      FCopyright := GetStringData(ChunkSize);
                      if Odd(ChunkSize) then
                        Stream.Position := Stream.Position + 1;
                    end else
                    if ChunkID = 'ANNO' then
                    begin
                      FAnnotation := GetStringData(ChunkSize);
                      if Odd(ChunkSize) then
                        Stream.Position := Stream.Position + 1;
                    end else begin
                      //* Go to next chunk
                      FixOdd(ChunkSize);
                      Stream.Seek(ChunkSize, spCurrent);
                    end;
                until (Stream.Position >= AIFFSize) or (Stream.Position >= Stream.Size);
            end;
        end;

        if not Result then
          Exit;

        if AIFC then
          FAIFFFileType := aftAIFC
        else
          FAIFFFileType := aftAIFF;

        //* Calculate play time
        if FSampleRate <> 0 then begin
            FDuration := FSampleFrames / FSampleRate;
        end else begin
            FDuration := 0;
        end;
        //* Calculate bit rate
        if FDuration <> 0 then begin
            FBitRate := Trunc(DataSize / (125 * FDuration ) + 0.5); // bitrate (Kbps)
        end else begin
            FBitRate := 0;
        end;
    finally
        Stream.Seek(PreviousPosition, spBegin);
    end;
end;

{ --------------------------------------------------------------------------- }

destructor TAIFF.Destroy;
begin
  FResetData;
  ID3v2.Free;
  inherited;
end;

function TAIFF.FGetChannelMode: string;
begin
  { Get channel mode name }            //multichannel
  if FChannelCount > 2 then Result := AIFF_CHANNEL_MODE[3] else
    Result := AIFF_CHANNEL_MODE[FChannelCount];
end;

{ ********************** Public functions & procedures ********************** }

constructor TAIFF.Create;
begin
  { Create object }
  inherited;
  FID3v2 := NewID3v2;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

function TAIFF.ReadFromFile(const FileName: string): Boolean;
var
  SourceFile: PStream;
  ID: array [0..3] of AnsiChar;
begin
  Result := False;
  { Reset and load header data from file to variable }
  FResetData;
  FillChar(ID, SizeOf(ID), 0);
  try
    SourceFile:= NewReadFileStreamW(FileName);
    try
      FFileSize := SourceFile.Size;
      SourceFile.Read(ID, 4);
      if ID = 'FORM' then
      begin
        Result := LoadAIFFInfo(SourceFile);
        FValid := Result;
        if Result then
        begin
          SourceFile.Seek(4, spBegin);
          if SeekAIFF(SourceFile) = 0 then
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

end.

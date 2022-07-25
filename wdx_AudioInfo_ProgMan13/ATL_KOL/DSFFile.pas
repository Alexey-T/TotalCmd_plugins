{ ******************************************************************************** }
{                                                                                  }
{ Based on one of latest free version of ID3v2 Library                             }
{                                                                                  }
{ https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html }
{                                                                                  }
{ ******************************************************************************** }

unit DSFFile;

interface

uses KOL, ID3v2;

const
  DSF_CHANNEL_MODE: array [0..3] of string = ('Unknown', 'Mono', 'Stereo', 'Multichannel');

type
   TDSFChannelType = (dsfctUnknown, dsfctMono, dsfctStereo, dsfct3Channels, dsfctQuad, dsfct4Channels, dsfct5Channels, dsfct51Channels);
   TDSDFileType = (dsdUnknown, dsdDSF, dsdDFF);
   TDSFInfo = record
    public
        FormatVersion: Cardinal;
        FormatID: Cardinal;
        ChannelType: TDSFChannelType;
        ChannelNumber: Cardinal;
        SamplingFrequency: Cardinal;
        BitsPerSample: Cardinal;
        SampleCount: UInt64;
        BlockSizePerChannel: Cardinal;
        PlayTime: Double;
    end;

type
  PDSF = ^TDSF;
  TDSF = object(TObj)
    private
      { Private declarations }
      FValid: Boolean;
      FChannelCount: Byte;
      FSampleRate: Cardinal;
      FSampleCount: Cardinal;
      FFileSize: Cardinal;
      FAmountTrimBegin: Cardinal;
      FAmountTrimEnd: Cardinal;
      FBitrate: Double;
      FDuration: Double;
      FDSDFileType: TDSDFileType;
      FID3v2: PID3v2;
      FArtist, FTitle, FComment: AnsiString;
      procedure FResetData;
      function FGetChannelMode: string;
      function LoadDSFInfo(const Stream: PStream): Boolean;
      function LoadDFFInfo(const Stream: PStream): Boolean;
    function GetFormat: string;
    public
      { Public declarations }
      FFormatVersion, FCompressionName: string;
      constructor Create;                                     { Create object }
      destructor Destroy; virtual;
      function ReadFromFile(const FileName: string): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property ChannelCount: Byte read FChannelCount;  { Number of channels }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property SampleRate: Cardinal read FSampleRate;      { Sample rate (hz) }
      property FileSize: Cardinal read FFileSize;         { File size (bytes) }
      property SampleCount: Cardinal read FSampleCount;
      property Bitrate: Double read FBitrate;
      property Duration: Double read FDuration;
      property DSDFileType: TDSDFileType read FDSDFileType;
      property ID3v2: PID3v2 read FID3v2;
      property Artist: AnsiString read FArtist;
      property Title: AnsiString read FTitle;
      property Comment: AnsiString read FComment;
      property Format: string read GetFormat;
  end;

  function NewDSF: PDSF;

implementation

const
  DSF_ID: AnsiString = 'DSD ';
  DFF_ID: AnsiString = 'FRM8';
  DSF_FORMAT_ID: AnsiString = 'fmt ';

type
  Int64Rec = packed record
    case Integer of
      0: (Lo, Hi: Cardinal);
      1: (Cardinals: array [0..1] of Cardinal);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;


function NewDSF: PDSF;
begin
    New(Result, Create);
    Result.FResetData;
end;


function ReverseBytes16(AWord: Word): Word; register;
    {
asm
    xchg al,ah
    }
begin
    Result := System.Swap(AWord);
    //Result := AWord shl 8 or AWord shr 8;
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

function GetID3v2Size(const Stream: PStream): Cardinal; overload;
type
    TID3v2Header = packed record
        ID: array [1..3] of Byte;
        Version: Byte;
        Revision: Byte;
        Flags: Byte;
        Size: Cardinal;
    end;
var
    PreviousPosition: Int64;
    Header: TID3v2Header;
begin
    // Get ID3v2 tag size (if exists)
    Result := 0;
    PreviousPosition := Stream.Position;
    try
        Stream.Read(Pointer(@Header)^, SizeOf(TID3v2Header));
        if (Header.ID[1] = Ord('I'))
        AND (Header.ID[2] = Ord('D'))
        AND (Header.ID[3] = Ord('3'))
        then begin
            UnSyncSafe(Header.Size, 4, Result);
            Inc(Result, 10);
        end;
    finally
        Stream.Seek(PreviousPosition, spBegin);
    end;
end;

function SeekDSF(const Stream: PStream): Integer;
var
    ID3v2Pointer: UInt64;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Seek(16, spCurrent);
        Stream.Read(ID3v2Pointer, 8);
        //ID3v2Pointer := ReverseBytes64(ID3v2Pointer);
        if ID3v2Pointer > 0 then begin
            Stream.Seek(ID3v2Pointer, spBegin);
            Result := GetID3v2Size(Stream);
        end;
    except
        Result := 0;
    end;
end;

function SeekDFF(const Stream: PStream): Integer;
var
    FRM8ChunkSize, ChunkSize: UInt64;
    Identification: array [0..3] of AnsiChar;
begin
    Result := 0;
    FRM8ChunkSize := 0;
    FillChar(Identification, SizeOf(Identification), 0);
    ChunkSize := 0;
    try
        //* Find ID3v2
        Stream.Seek(4, spBegin);
        Stream.Read(FRM8ChunkSize, 8);
        FRM8ChunkSize := ReverseBytes64(FRM8ChunkSize);
        Stream.Seek(4, spCurrent);

        repeat
            Stream.Read(Identification[0], 4);
            Stream.Read(ChunkSize, 8);
            ChunkSize := ReverseBytes64(ChunkSize);
            if Identification = 'ID3 '
            then begin
                Result := ChunkSize;
                Break;
            end else begin
                if Odd(ChunkSize) then begin
                    Stream.Seek(ChunkSize + 1, spCurrent);
                end else begin
                    Stream.Seek(ChunkSize, spCurrent);
                end;
            end;
        until Stream.Position >= FRM8ChunkSize + 12;
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

procedure TDSF.FResetData;
begin
  { Reset all data }
  FValid := false;
  FChannelCount := 0;
  FSampleRate := 0;
  FSampleCount := 0;
  FFileSize := 0;
  FAmountTrimBegin := 0;
  FAmountTrimEnd := 0;
  FBitrate := 0;
  FDuration := 0;
  FDSDFileType := dsdUnknown;
  FFormatVersion := '';
  FCompressionName := '';
  FArtist := '';
  FTitle := '';
  FComment := '';
  FID3v2.ResetData;
end;

function TDSF.GetFormat: string;
begin
  if not Valid then
    Exit('');
  case SampleRate of
    2822400: Result := 'DSD64';
    2822400 * 2: Result := 'DSD128';
    2822400 * 2 * 2: Result := 'DSD256';
    2822400 * 2 * 2 * 2: Result := 'DSD512';
    2822400 * 2 * 2 * 2 * 2: Result := 'DSD1024';
    2822400 * 2 * 2 * 2 * 2 * 2: Result := 'DSD2048';
    else Result := 'DSD';
  end;
end;

function TDSF.LoadDFFInfo(const Stream: PStream): Boolean;
var
  ID3v2Size: Cardinal;
  MagicBytes: Array [0..3] of Byte;
  MemoryStream: PStream;
  StreamReadBytesLength, MemoryStreamReadBytesLength, StreamBackupPosition,
  MemoryStreamBackupPosition, SavedPos: Int64;
  N, I: Integer;
  StrLength: Cardinal;
  CommentsCount: Word;
//---
  SoundDataLength: UInt64;
  Ratio: Double;
  DSTFramesCount: Cardinal;
  DSTFramesRate: Word;
  ABitrate: Cardinal;
  AChannelCount: Word;
  ASampleCount: Double;
begin
  Result := False;
  ASampleCount := 0;
  ABitrate := 0;
  StreamBackupPosition := 0;
  FillChar(MagicBytes, SizeOf(MagicBytes), 0);
  MemoryStream := NewMemoryStream;
  try
    Stream.Seek(0, spBegin);
    ID3v2Size := GetID3v2Size(Stream);
    Stream.Seek(ID3v2Size, spBegin);
    Stream.Read(MagicBytes[0], SizeOf(MagicBytes));
    if (MagicBytes[0] = ORD('F')) and
       (MagicBytes[1] = ORD('R')) and
       (MagicBytes[2] = ORD('M')) and
       (MagicBytes[3] = ORD('8')) then
    begin
      Stream.Seek(Int64(ID3v2Size) + 12, spBegin);
      Stream.Read(MagicBytes[0], SizeOf(MagicBytes));
      if (MagicBytes[0] = ORD('D')) and
         (MagicBytes[1] = ORD('S')) and
         (MagicBytes[2] = ORD('D')) and
         (MagicBytes[3] = ORD(' ')) then
      begin
        //??????
        Result := True;
        for N := 1 to 10 do
        begin
          FillChar(MagicBytes, SizeOf(MagicBytes), 0);
          Stream.Read(MagicBytes[0], SizeOf(MagicBytes));
          //For 'FVER' Form (Required)
          if (MagicBytes[0] = ORD('F')) and
             (MagicBytes[1] = ORD('V')) and
             (MagicBytes[2] = ORD('E')) and
             (MagicBytes[3] = ORD('R')) then
          begin
            Stream.Seek(8, spCurrent);
            Stream.Read(MagicBytes[0], SizeOf(MagicBytes));                //Pointer Skip 'FVER' Form Now
            if (MagicBytes[0] = $01) and (MagicBytes[1] = $00) then
              FFormatVersion := '1.0'
            else if (MagicBytes[0] = $01) and (MagicBytes[1] = $03) then
              FFormatVersion := '1.3'
            else if (MagicBytes[0] = $01) and (MagicBytes[1] = $04) then
              FFormatVersion := '1.4'
            else if (MagicBytes[0] = $01) and (MagicBytes[1] = $05) then
              FFormatVersion := '1.5'
            else
              FFormatVersion := '';
          end
          //For 'PROP' Form (Required)
          else if (MagicBytes[0] = ORD('P')) and
                  (MagicBytes[1] = ORD('R')) and
                  (MagicBytes[2] = ORD('O')) and
                  (MagicBytes[3] = ORD('P')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);
            StreamBackupPosition := Stream.Position;
            //MemoryStream.CopyFrom(Stream, StreamReadBytesLength);
            MemoryStream.Size := 0;
            Stream2Stream(MemoryStream, Stream, StreamReadBytesLength);
            MemoryStream.Seek(0, spBegin);
            MemoryStream.Read(MagicBytes[0], SizeOf(MagicBytes));
            if (MagicBytes[0] = ORD('S')) and
               (MagicBytes[1] = ORD('N')) and
               (MagicBytes[2] = ORD('D')) and
               (MagicBytes[3] = ORD(' ')) then
            begin
              for I := 1 to 5 do
              begin
                FillChar(MagicBytes, SizeOf(MagicBytes), 0);
                MemoryStream.Read(MagicBytes[0], SizeOf(MagicBytes));
                if (MagicBytes[0] = ORD('F')) and
                   (MagicBytes[1] = ORD('S')) and
                   (MagicBytes[2] = ORD(' ')) and
                   (MagicBytes[3] = ORD(' ')) then
                begin
                  MemoryStream.Seek(8, spCurrent);
                  MemoryStream.Read(FSampleRate, 4);
                  FSampleRate := ReverseBytes32(FSampleRate);
                end
                else if (MagicBytes[0] = ORD('C')) and
                        (MagicBytes[1] = ORD('H')) and
                        (MagicBytes[2] = ORD('N')) and
                        (MagicBytes[3] = ORD('L')) then
                begin
                  MemoryStream.Read(MemoryStreamReadBytesLength, 8);
                  MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);
                  MemoryStreamBackupPosition := MemoryStream.Position;
                  MemoryStream.Read(AChannelCount, 2);
                  AChannelCount := ReverseBytes16(AChannelCount);
                  MemoryStream.Seek(MemoryStreamBackupPosition, spBegin);
                  MemoryStream.Seek(MemoryStreamReadBytesLength, spCurrent);
                end
                else if (MagicBytes[0] = ORD('C')) and
                        (MagicBytes[1] = ORD('M')) and
                        (MagicBytes[2] = ORD('P')) and
                        (MagicBytes[3] = ORD('R')) then
                begin
                  MemoryStream.Read(MemoryStreamReadBytesLength, 8);
                  MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);
                  MemoryStreamBackupPosition := MemoryStream.Position;
                  MemoryStream.Read(MagicBytes[0], SizeOf(MagicBytes));
                  if (MagicBytes[0] = ORD('D')) and
                     (MagicBytes[1] = ORD('S')) and
                     (MagicBytes[2] = ORD('D')) and
                     (MagicBytes[3] = ORD(' ')) then
                  begin
                    FCompressionName := 'Not Compressed';
                  end
                  else if (MagicBytes[0] = ORD('D')) and
                          (MagicBytes[1] = ORD('S')) and
                          (MagicBytes[2] = ORD('T')) and
                          (MagicBytes[3] = ORD(' ')) then
                  begin
                    FCompressionName := 'DST Encoded';
                  end;
                  MemoryStream.Seek(MemoryStreamBackupPosition, spBegin);
                  MemoryStream.Seek(MemoryStreamReadBytesLength, spCurrent);
                end
                else if (MagicBytes[0] = ORD('A')) and
                        (MagicBytes[1] = ORD('B')) and
                        (MagicBytes[2] = ORD('S')) and
                        (MagicBytes[3] = ORD('S')) then
                begin
                  MemoryStream.Read(MemoryStreamReadBytesLength, 8);
                  MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);
                  MemoryStream.Seek(MemoryStreamReadBytesLength, spCurrent);
                end
                else if (MagicBytes[0] = ORD('L')) and
                        (MagicBytes[1] = ORD('S')) and
                        (MagicBytes[2] = ORD('C')) and
                        (MagicBytes[3] = ORD('O')) then
                begin
                  MemoryStream.Seek(4, spCurrent);
                end
                else
                  MemoryStream.Seek(-4, spCurrent);  //Pointer Return Positon Before Read MagicBytes
              end;
            end;
            Stream.Seek(StreamBackupPosition, spBegin);
            Stream.Seek(StreamReadBytesLength, spCurrent);   //Pointer Skip 'PROP' Form Now
          end
          //For 'DSD ' or 'DST ' Form (Required)
          else if ((MagicBytes[0] = ORD('D')) and
                   (MagicBytes[1] = ORD('S')) and
                   (MagicBytes[2] = ORD('D')) and
                   (MagicBytes[3] = ORD(' '))) or
                  ((MagicBytes[0] = ORD('D')) and
                   (MagicBytes[1] = ORD('S')) and
                   (MagicBytes[2] = ORD('T')) and
                   (MagicBytes[3] = ORD(' '))) then
          begin
            if (MagicBytes[0] = ORD('D')) and
               (MagicBytes[1] = ORD('S')) and
               (MagicBytes[2] = ORD('D')) and
               (MagicBytes[3] = ORD(' ')) then
            begin
              Stream.Read(StreamReadBytesLength, 8);
              StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);
              StreamBackupPosition := Stream.Position;
              SoundDataLength := StreamReadBytesLength;
              ASampleCount := (SoundDataLength * 2 * AChannelCount / FSampleRate) * FSampleRate;

              { Magic "fix" (?) }
              if AChannelCount = 6 then
                ASampleCount := ASampleCount / (AChannelCount * 1.5);

              FDuration := ASampleCount / FSampleRate;
              ABitrate := Round((((ASampleCount * AChannelCount * 1) / 8) / FDuration) / 125);
              Ratio := SoundDataLength / (ASampleCount * (AChannelCount * 1 / 8) + (SoundDataLength - ASampleCount / 2 / AChannelCount));
            end
            else if (MagicBytes[0] = ORD('D')) and
                    (MagicBytes[1] = ORD('S')) and
                    (MagicBytes[2] = ORD('T')) and
                    (MagicBytes[3] = ORD(' ')) then
            begin
              Stream.Read(StreamReadBytesLength, 8);
              StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);
              StreamBackupPosition := Stream.Position;
              SoundDataLength := StreamReadBytesLength;
              Stream.Read(MagicBytes[0], SizeOf(MagicBytes));
              if (MagicBytes[0] = ORD('F')) and
                 (MagicBytes[1] = ORD('R')) and
                 (MagicBytes[2] = ORD('T')) and
                 (MagicBytes[3] = ORD('E')) then
              begin
                Stream.Seek(8, spCurrent);
                Stream.Read(DSTFramesCount, 4);
                DSTFramesCount := ReverseBytes32(DSTFramesCount);
                Stream.Read(DSTFramesRate, 2);
                DSTFramesRate := ReverseBytes16(DSTFramesRate);
                FDuration := DSTFramesCount / DSTFramesRate;
                ASampleCount := (FDuration * FSampleRate);
                ABitrate := Round((((ASampleCount * AChannelCount * 1) / 8) / FDuration) / 125);
                Ratio := SoundDataLength / (ASampleCount * (AChannelCount * 1 / 8));
              end;
            end;
            Stream.Seek(StreamBackupPosition, spBegin);
            Stream.Seek(StreamReadBytesLength, spCurrent);   //Pointer Skip 'DSD ' or 'DST ' Form Now
          end
          //For 'DSTI' Form (Optional)
          else if (MagicBytes[0] = ORD('D')) and
                  (MagicBytes[1] = ORD('S')) and
                  (MagicBytes[2] = ORD('T')) and
                  (MagicBytes[3] = ORD('I')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'DSTI' Form Size
            Stream.Seek(StreamReadBytesLength, spCurrent);  //Pointer Skip 'DSTI' Form Now
          end
          //For 'COMT' Form (Optional)
          else if (MagicBytes[0] = ORD('C')) and
                  (MagicBytes[1] = ORD('O')) and
                  (MagicBytes[2] = ORD('M')) and
                  (MagicBytes[3] = ORD('T')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'COMT' Form Size
            SavedPos := Stream.Position;
            Stream.Read(CommentsCount, 2);
            Stream.Seek(10, spCurrent);
            Stream.Read(StrLength, 4);
            StrLength := ReverseBytes32(StrLength);
            if StrLength > 0 then
            begin
              SetLength(fComment, StrLength);
              Stream.Read(fComment[1], StrLength);
            end;
            Stream.Position := SavedPos + StreamReadBytesLength;  //Pointer Skip 'COMT' Form Now
            //Stream.Seek(StreamReadBytesLength, spCurrent);  //Pointer Skip 'COMT' Form Now
          end
          //For 'DIIN' Form (Optional)
          else if (MagicBytes[0] = ORD('D')) and
                  (MagicBytes[1] = ORD('I')) and
                  (MagicBytes[2] = ORD('I')) and
                  (MagicBytes[3] = ORD('N')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'DIIN' Form Size
            //Stream.Seek(StreamReadBytesLength, spCurrent);  //Pointer Skip 'DIIN' Form Now
            StreamBackupPosition := Stream.Position;
            //MemoryStream.CopyFrom(Stream, StreamReadBytesLength);
            MemoryStream.Size := 0;
            Stream2Stream(MemoryStream, Stream, StreamReadBytesLength);
            MemoryStream.Seek(0, spBegin);
            for I := 1 to 4 do
            begin
              FillChar(MagicBytes, SizeOf(MagicBytes), 0);
              MemoryStream.Read(MagicBytes[0], SizeOf(MagicBytes));
              if (MagicBytes[0] = ORD('D')) and
                 (MagicBytes[1] = ORD('I')) and
                 (MagicBytes[2] = ORD('A')) and
                 (MagicBytes[3] = ORD('R')) then
              begin
                MemoryStream.Read(MemoryStreamReadBytesLength, 8);
                MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);  //'DIAR' Form Size
                SavedPos := MemoryStream.Position;
                MemoryStream.Read(StrLength, 4);
                StrLength := ReverseBytes32(StrLength);
                if StrLength > 0 then
                begin
                  SetLength(FArtist, StrLength);
                  MemoryStream.Read(FArtist[1], StrLength);
                end;
                MemoryStream.Position := SavedPos + MemoryStreamReadBytesLength;  //Pointer Skip 'DIAR' Form Now
              end
              else if (MagicBytes[0] = ORD('D')) and
                      (MagicBytes[1] = ORD('I')) and
                      (MagicBytes[2] = ORD('T')) and
                      (MagicBytes[3] = ORD('I')) then
              begin
                MemoryStream.Read(MemoryStreamReadBytesLength, 8);
                MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);  //'DITI' Form Size
                SavedPos := MemoryStream.Position;
                MemoryStream.Read(StrLength, 4);
                StrLength := ReverseBytes32(StrLength);
                if StrLength > 0 then
                begin
                  SetLength(FTitle, StrLength);
                  MemoryStream.Read(FTitle[1], StrLength);
                end;
                MemoryStream.Position := SavedPos + MemoryStreamReadBytesLength;  //Pointer Skip 'DITI' Form Now
              end
              else
                MemoryStream.Seek(-4, spCurrent);  //Pointer Return Positon Before Read MagicBytes
            end;
            Stream.Seek(StreamBackupPosition, spBegin);
            Stream.Seek(StreamReadBytesLength, spCurrent);   //Pointer Skip 'PROP' Form Now
          end
          //For 'DIAR' Form (Optional)
          else if (MagicBytes[0] = ORD('D')) and
                  (MagicBytes[1] = ORD('I')) and
                  (MagicBytes[2] = ORD('A')) and
                  (MagicBytes[3] = ORD('R')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'DIAR' Form Size
            SavedPos := Stream.Position;
            Stream.Read(StrLength, 4);
            StrLength := ReverseBytes32(StrLength);
            if StrLength > 0 then
            begin
              SetLength(FArtist, StrLength);
              Stream.Read(FArtist[1], StrLength);
            end;
            Stream.Position := SavedPos + StreamReadBytesLength;  //Pointer Skip 'DIAR' Form Now
          end
          //For 'DITI' Form (Optional)
          else if (MagicBytes[0] = ORD('D')) and
                  (MagicBytes[1] = ORD('I')) and
                  (MagicBytes[2] = ORD('T')) and
                  (MagicBytes[3] = ORD('I')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'DITI' Form Size
            SavedPos := Stream.Position;
            Stream.Read(StrLength, 4);
            StrLength := ReverseBytes32(StrLength);
            if StrLength > 0 then
            begin
              SetLength(FTitle, StrLength);
              Stream.Read(FTitle[1], StrLength);
            end;
            Stream.Position := SavedPos + StreamReadBytesLength;  //Pointer Skip 'DITI' Form Now
          end
          //For 'MANF' Form (Optional)
          else if (MagicBytes[0] = ORD('M')) and
                  (MagicBytes[1] = ORD('A')) and
                  (MagicBytes[2] = ORD('N')) and
                  (MagicBytes[3] = ORD('F')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'MANF' Form Size
            Stream.Seek(StreamReadBytesLength, spCurrent);  //Pointer Skip 'MANF' Form Now
          end
          //For 'ID3 ' Form (Optional)
          else if (MagicBytes[0] = ORD('I')) and
                  (MagicBytes[1] = ORD('D')) and
                  (MagicBytes[2] = ORD('3')) and
                  (MagicBytes[3] = ORD(' ')) then
          begin
            Stream.Read(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'ID3 ' Form Size
            Stream.Seek(StreamReadBytesLength, spCurrent);  //Pointer Skip 'ID3 ' Form Now
          end
          else
            Stream.Seek(-4, spCurrent);  //Pointer Return Positon Before Read MagicBytes
        end;
      end;
    end;

    if Result then
    begin
      Self.FBitrate := ABitrate;
      Self.FChannelCount := AChannelCount;
      Self.FSampleCount := Trunc(ASampleCount);
    end;

  finally
    MemoryStream.Free;
  end;
end;

function TDSF.LoadDSFInfo(const Stream: PStream): Boolean;
var
    PreviousPosition: Int64;
    ChunkSize: UInt64;
    ChunkID: array [0..3] of AnsiChar;
    ChannelType: Cardinal;
    DSFInfo: TDSFInfo;
begin
    Result := False;
    FillChar(ChunkID, SizeOf(ChunkID), 0);
    FillChar(DSFInfo, SizeOf(DSFInfo), 0);

    PreviousPosition := Stream.Position;
    try
        Stream.Seek(4, spBegin);
        Stream.Read(ChunkSize, 8);
        Stream.Seek(ChunkSize - 12, spCurrent);
        Stream.Read(ChunkID[0], 4);
        if ChunkID = DSF_Format_ID
        then begin
            Stream.Seek(8, spCurrent);
            Stream.Read(DSFInfo.FormatVersion, 4);
            Stream.Read(DSFInfo.FormatID, 4);
            Stream.Read(ChannelType, 4);
            case ChannelType of
                1: DSFInfo.ChannelType := dsfctMono;
                2: DSFInfo.ChannelType := dsfctStereo;
                3: DSFInfo.ChannelType := dsfct3Channels;
                4: DSFInfo.ChannelType := dsfctQuad;
                5: DSFInfo.ChannelType := dsfct4Channels;
                6: DSFInfo.ChannelType := dsfct5Channels;
                7: DSFInfo.ChannelType := dsfct51Channels;
            else
                DSFInfo.ChannelType := dsfctUnknown;
            end;
            Stream.Read(DSFInfo.ChannelNumber, 4);
            Stream.Read(DSFInfo.SamplingFrequency, 4);
            Stream.Read(DSFInfo.BitsPerSample, 4);
            Stream.Read(DSFInfo.SampleCount, 8);
            Stream.Read(DSFInfo.BlockSizePerChannel, 4);
            //* Calculate playtime
            DSFInfo.PlayTime := DSFInfo.SampleCount / DSFInfo.SamplingFrequency;
            //* Set attrbiutes
            FDuration := DSFInfo.PlayTime;
            FSampleCount := DSFInfo.SampleCount;
            FBitRate := Round((((DSFInfo.SampleCount * DSFInfo.ChannelNumber * DSFInfo.BitsPerSample) / 8) / DSFInfo.PlayTime) / 125);
            FChannelCount := DSFInfo.ChannelNumber;
            FSampleRate := DSFInfo.SamplingFrequency;
            Result := True;
        end;
    finally
        Stream.Seek(PreviousPosition, spBegin);
    end;
end;

{ --------------------------------------------------------------------------- }

destructor TDSF.Destroy;
begin
  FResetData;
  ID3v2.Free;
  inherited;
end;

function TDSF.FGetChannelMode: string;
begin
  { Get channel mode name }            //multichannel
  if FChannelCount > 2 then Result := DSF_CHANNEL_MODE[3] else
    Result := DSF_CHANNEL_MODE[FChannelCount];
end;

{ ********************** Public functions & procedures ********************** }

constructor TDSF.Create;
begin
  { Create object }
  inherited;
  FID3v2 := NewID3v2;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

function TDSF.ReadFromFile(const FileName: string): Boolean;
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
      if ID = DSF_ID then
      begin
        Result := LoadDSFInfo(SourceFile);
        FValid := Result;
        if Result then
        begin
          FDSDFileType := dsdDSF;
          if True {ReadID3v2tag} then
          begin
            SourceFile.Seek(4, spBegin);
            if SeekDSF(SourceFile) = 0 then
              Exit;
            if not ID3v2ValidTag(SourceFile) then
              Exit;
            SourceFile.Seek(-3, spCurrent);
            FID3v2.ReadFromStreamEx(SourceFile);
          end;
        end;
      end else if ID = DFF_ID then begin
        FillChar(ID, SizeOf(ID), 0);
        SourceFile.Seek(8, spCurrent);
        SourceFile.Read(ID, 4);
        if ID = DSF_ID then
        begin
          Result := LoadDFFInfo(SourceFile);
          FValid := Result;
          if Result then
          begin
            FDSDFileType := dsdDFF;
            if True {ReadID3v2tag} then
            begin
              //SourceFile.Seek(4, spBegin);
              if SeekDFF(SourceFile) = 0 then
                Exit;
              if not ID3v2ValidTag(SourceFile) then
                Exit;
              SourceFile.Seek(-3, spCurrent);
              FID3v2.ReadFromStreamEx(SourceFile);
            end;
          end;
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

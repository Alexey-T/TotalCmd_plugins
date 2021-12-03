{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TID3v2 - for manipulating with ID3v2 tags                             }
{                                                                             }
{ Copyright (c) 2001,2002 by Jurgen Faul                                      }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.7 (2 October 2002)                                                }
{   - Added property TrackString                                              }
{                                                                             }
{ Version 1.6 (29 July 2002)                                                  }
{   - Reading support for Unicode                                             }
{   - Removed limitation for the track number                                 }
{                                                                             }
{ Version 1.5 (23 May 2002)                                                   }
{   - Support for padding                                                     }
{                                                                             }
{ Version 1.4 (24 March 2002)                                                 }
{   - Reading support for ID3v2.2.x & ID3v2.4.x tags                          }
{                                                                             }
{ Version 1.3 (16 February 2002)                                              }
{   - Fixed bug with property Comment                                         }
{   - Added info: composer, encoder, copyright, language, link                }
{                                                                             }
{ Version 1.2 (17 October 2001)                                               }
{   - Writing support for ID3v2.3.x tags                                      }
{   - Fixed bug with track number detection                                   }
{   - Fixed bug with tag reading                                              }
{                                                                             }
{ Version 1.1 (31 August 2001)                                                }
{   - Added public procedure ResetData                                        }
{                                                                             }
{ Version 1.0 (14 August 2001)                                                }
{   - Reading support for ID3v2.3.x tags                                      }
{   - Tag info: title, artist, album, track, year, genre, comment             }
{                                                                             }
{ Портировано в KOL - Матвеев дмитрий                                         }
{                                                                             }
{ *************************************************************************** }

// - История -

// Дата: 16.11.2005 Версия: 1.06
// [!] - Переписал декодинг из Юникода, а также правильное определение кодировки  

// Дата: 26.09.2005 Версия: 1.05
// В результате долгого и тщательного тестирования внесено много изменений и получена
// вполне работоспособная версия. Тестирование осуществлял Alexander Splinter, за что ему
// отдельное спасибо большое спасибо.

// Дата: 17.01.2005 Версия: 1.03
// Добавил свойства для наиболее используемых тегов

// Дата: 10.01.2004 Версия: 1.02
// [*] - Переделал много

// Дата: 14.11.2003 Версия: 1.01
// [!] - исправил ошибку при сохранении в файл, ранее ничего и не сохранялось. (Спасибо PA)

// Версия: 1.00
   {Стартовая версия}


unit ID3v2;

interface

uses Windows, KOL;

type
  TTagsOfID3 = (tiTitle, tiArtist, tiAlbum, tiTrack, tiYear, tiGenre, tiComment, tiComposer, tiEncoder, tiCopyright, tiLanguage, tiLink, tiOrigYear, tiOrigArtist, tiDescription, tiOrigAlbum);

  TID = array [1..4] of AnsiChar;

  PFrame = ^TFrame;
  TFrame = packed record
    ID: TID;                                      { Frame ID }
    Size: Integer;                                    { Size excluding header }
    Flags: Word;                                                      { Flags }
    Data: Pointer;
  end;

  TTagInfo = packed record
    { Real structure of ID3v2 header }
    ID: array [1..3] of AnsiChar;                              { Always "ID3" }
    Version: Byte;                                           { Version number }
    Revision: Byte;                                         { Revision number }
    Flags: Byte;                                               { Flags of tag }
    Size: array [1..4] of Byte;                   { Tag size excluding header }
    { Extended data }
    FileSize: DWord;                                    { File size (bytes) }
    NeedRewrite: Boolean;                           { Tag should be rewritten }
    PaddingSize: Integer;                              { Padding size (bytes) }
  end;

  TDuplicateFrameMethod = (dfmFirst, dfmFirstNonEmpty, dfmIgnoreDuplicate, dfmIgnoreDuplicateNonEmpty);
{        dfmFirst - добавляется только один тег, последующие игнорируются
         dfmFirstNonEmpty - добавляется только один тег, существующий пустой тег заменяется на непустой
         dfmIgnoreDuplicate - добавляются все один теги
         dfmIgnoreDuplicateNonEmpty - добавляются все теги, но существующие пустые заменяются на непустые
         }

const
  TAG_VERSION_2_2 = 2;                               { Code for ID3v2.2.x tag }
  TAG_VERSION_2_3 = 3;                               { Code for ID3v2.3.x tag }
  TAG_VERSION_2_4 = 4;                               { Code for ID3v2.4.x tag }

type
  PID3v2 = ^TID3v2;
  TID3v2 = object(TObj)
  private
    FTagList: PList;

    FExists: Boolean;
    FSize: Integer;
    FDuplicateFrameMethod: TDuplicateFrameMethod;

    function ReadHeader(const FileName: string; var ATag: TTagInfo): Boolean;
    procedure ReadFrames(const FileName: string; var ATag: TTagInfo; IsNew: Boolean);
    procedure BuildHeader(var ATag: TTagInfo);
    function RebuildFile(const FileName: string; TagData: PStream): Boolean;
    function ReplaceTag(const FileName: string; TagData: PStream): Boolean;
    function SaveTag(const FileName: string; ATag: TTagInfo): Boolean;
    function GetTagSize(const ATag: TTagInfo): DWord;
    function GetTagsFrame(Index: Integer): PFrame;
    function GetTagsByName(Index: TID): PFrame;
    function GetTagCount: Integer;
    function GetTagsByNameString(Index: TID): String;

    function GetTags(const Index: TTagsOfID3): string;
    procedure SetTags(const Index: TTagsOfID3; Value: string);

    function  _AddTagEx(AID: TID; AFlags: Word = 0): Integer;
    function GetFrameText(Frame: PFrame): String;
    function GetComments(Index: integer): string;
    function GetCommentCount: integer;

  public
    FVersionID: Byte;

    destructor Destroy; virtual;
    procedure ResetData;

    function ReadFromFile(const FileName: string): Boolean;                     // Load tag
    function SaveToFile(const FileName: string): Boolean;                       // Save tag
    function RemoveFromFile(const FileName: string): Boolean;                   // Delete tag

    procedure SetTagEx(AID: TID; AValue: String; AFlags: Word = 0);             // Устанавливает значение тега, если он есть, а если нет - добавляет
    function  AddTagEx(AID: TID; AFlags: Word = 0): Integer;                       // Добавляет тег
    function  IndexOfTag(AID: TID): Integer;

    function  AddTag(AID: TID; AFlags: Word = 0): Integer;                       // Добавляет тег
    procedure DelTag(Index: Integer);                                           // Удаляет тег
    procedure SetTag(AID: TID; AValue: String);                                 // Устанавливает значение тега, если он есть, а если нет - добавляет

    property DuplicateFrameMethod: TDuplicateFrameMethod read FDuplicateFrameMethod write FDuplicateFrameMethod;

    property TagCount: Integer read GetTagCount;                                // Количество тегов
    property Tags[Index: Integer]: PFrame read GetTagsFrame;                         // Теги (список)
    property TagsByName[Index: TID]: PFrame read GetTagsByName;                 // Теги (по имени)
    property TagsByNameString[Index: TID]: String read GetTagsByNameString;     // Строковое значени тега (по имени)

    property Exists: Boolean read FExists;                                      // True if tag found
    property VersionID: Byte read FVersionID;                                   // Version code }
    property Size: Integer read FSize;                                          // Total tag size }

    property Title: string index tiTitle read GetTags write SetTags;            // Song title
    property Artist: string index tiArtist read GetTags write SetTags;          // Artist name
    property Album: string index tiAlbum read GetTags write SetTags;            // Album title
    property Track: string index tiTrack read GetTags write SetTags;            // Track number (string)
    property Year: string index tiYear read GetTags write SetTags;              // Release year
    property Genre: string index tiGenre read GetTags write SetTags;            // Genre name
    property Comment: string index tiComment read GetTags write SetTags;        // Comment
    property Comments[Index: integer]: string read GetComments;                 // All comments
    property CommentCount: integer read GetCommentCount;                        // All comments
    property Composer: string index tiComposer read GetTags write SetTags;      // Composer
    property Encoder: string index tiEncoder read GetTags write SetTags;        // Encoder
    property Copyright: string index tiCopyright read GetTags write SetTags;    // (c)
    property Language: string index tiLanguage read GetTags write SetTags;      // Language
    property Link: string index tiLink read GetTags write SetTags;              // URL link
    property OrigYear: string index tiOrigYear read GetTags write SetTags;      // Orig. Year
    property OrigArtist: string index tiOrigArtist read GetTags write SetTags;  // Orig. Artist
    property Description: string index tiDescription read GetTags write SetTags;// Description
    property OrigAlbum: string index tiOrigAlbum read GetTags write SetTags;    // Orig. Album
end;

function NewID3v2: PID3v2;

implementation

uses SProc, ID3Gen;

function NewID3v2: PID3v2;
begin
    New(Result, Create);
    Result.FTagList:= NewList;
    Result.FDuplicateFrameMethod:= dfmIgnoreDuplicateNonEmpty;
    Result.ResetData;
end;

const
  { ID3v2 tag ID }
  ID3V2_ID = 'ID3';

  ID3V2_Frames: array [TTagsOfID3] of record
                                  Old: array[1..3] of AnsiChar; //(ID3v2.2.x)
                                  New: TID; //(ID3v2.3.x & ID3v2.4.x)
                                end = ((Old: 'TT2'; New: 'TIT2'),
                                       (Old: 'TP1'; New: 'TPE1'),
                                       (Old: 'TAL'; New: 'TALB'),
                                       (Old: 'TRK'; New: 'TRCK'),
                                       (Old: 'TYE'; New: 'TYER'),
                                       (Old: 'TCO'; New: 'TCON'),
                                       (Old: 'COM'; New: 'COMM'),
                                       (Old: 'TCM'; New: 'TCOM'),
                                       (Old: 'TEN'; New: 'TENC'),
                                       (Old: 'TCR'; New: 'TCOP'),
                                       (Old: 'TLA'; New: 'TLAN'),
                                       (Old: 'WXX'; New: 'WXXX'),
                                       (Old: 'TOR'; New: 'TDRC'),
                                       (Old: 'TOA'; New: 'TOPE'),
                                       (Old: 'TT1'; New: 'TIT1'),
                                       (Old: 'TOT'; New: 'TOAL'));

  { Max. tag size for saving }
  ID3V2_MAX_SIZE = 4096;
  { Unicode ID }
  UNICODE_ID = #1;
type
  TFrameHeader = packed record
    case Boolean of
      { Frame header (ID3v2.3.x & ID3v2.4.x) }
      True: (IDNew: TID;                                      { Frame ID }
             SizeNew: Integer;                                    { Size excluding header }
             FlagsNew: Word);                                                      { Flags }
      { Frame header (ID3v2.2.x) }
      False: (IDOld: array [1..3] of AnsiChar;                                      { Frame ID }
              SizeOld: array [1..3] of Byte);                       { Size excluding header }
  end;

function UnSyncSafe(var Source; const SourceSize: Integer = 4): cardinal;
type
    TBytes = array [0..MaxInt - 1] of Byte;
var
    I: Byte;
begin
    { Test : Source = $01 $80 -> Dest = 255
             Source = $02 $00 -> Dest = 256
             Source = $02 $01 -> Dest = 257 etc.
    }
    Result := 0;
    for I := 0 to SourceSize - 1 do begin
        Result := Result shl 7;
        Result := Result or (TBytes(Source)[I] and $7F); // $7F = %01111111
    end;
end;

function Swap32(const Figure: Integer): Integer;
var ByteArray: array [1..4] of Byte absolute Figure;
begin
    Result:= ByteArray[1] * $1000000 + ByteArray[2] * $10000 + ByteArray[3] * $100 + ByteArray[4];
end;

procedure TID3v2.ResetData;
var i: Integer;
begin
    FExists := false;
    FVersionID := 0; FSize := 0;
    for i:= FTagList.Count-1 downto 0 do DelTag(i);
end;

function TID3v2.ReadFromFile(const FileName: string): Boolean;
var _Tag: TTagInfo;
begin
    { Reset data and load header from file to variable }
    ResetData;
    Result := ReadHeader(FileName, _Tag);
    { Process data if loaded and header valid }
    if (Result) and (_Tag.ID = ID3V2_ID) then begin
      FExists := true;
      { Fill properties with header data }
      FVersionID := _Tag.Version;
      FSize := GetTagSize(_Tag);
      { Get information from frames if version supported }
      if (FVersionID in [TAG_VERSION_2_2..TAG_VERSION_2_4]) and (FSize > 0) then begin
        ReadFrames(FileName, _Tag, FVersionID > TAG_VERSION_2_2);
      end;
    end;
end;

function TID3v2.SaveToFile(const FileName: string): Boolean;
var _Tag: TTagInfo;
begin
    { Check for existing tag }
    FillChar(_Tag, SizeOf(_Tag), 0);
    ReadHeader(FileName, _Tag);
    { Prepare tag data and save to file }
    Result := SaveTag(FileName, _Tag);
end;

function TID3v2.RemoveFromFile(const FileName: string): Boolean;
begin
    Result := RebuildFile(FileName, nil);
end;

destructor TID3v2.Destroy;
begin
    ResetData;
    FTagList.Free;
    inherited;
end;

procedure TID3v2.ReadFrames(const FileName: string; var ATag: TTagInfo; IsNew: Boolean);
const FrameSize : array [Boolean] of Integer = (6, 10);
var SourceFile: PStream;
    Frame: TFrameHeader;
    Data: Pointer;
    DataPosition, DataSize, TagSize, k: Integer;
    ID: TID;
    Flags: Word;

    function GetNewID: TID;
    var
      i: TTagsOfID3;
    begin
      for i := Low(ID3V2_Frames) to High(ID3V2_Frames) do
        if Frame.IDOld = ID3V2_Frames[i].Old then
        begin
          Result:=ID3V2_Frames[i].New;
          Exit;
        end;
    end;

begin
    try
      SourceFile:= NewReadFileStreamW(FileName);
      try
        SourceFile.Seek(10, spBegin);

        TagSize:= GetTagSize(ATag);
        while (SourceFile.Position < TagSize) and (SourceFile.Position < SourceFile.Size) do begin
          SourceFile.Read(Frame, FrameSize[IsNew]);
          if not (Frame.IDOld[1] in ['A'..'Z']) then break;
          DataPosition := SourceFile.Position;

          if ATag.Version = TAG_VERSION_2_4 then
            DataSize := UnSyncSafe(Frame.SizeNew)
          else if ATag.Version = TAG_VERSION_2_3 then
            DataSize := Swap32(Frame.SizeNew)
          else
            DataSize := Frame.SizeOld[1] shl 16 + Frame.SizeOld[2] shl 8 + Frame.SizeOld[3];

          if DataSize > TagSize - DataPosition then Break;

          Data:= AllocMem(DataSize);
          //FillChar(Data^, DataSize, 0);
          SourceFile.Read(Data^, DataSize);

          if IsNew then begin
            ID:= Frame.IDNew; Flags:= Frame.FlagsNew;
          end
          else begin
            ID:=GetNewID; Flags:= 0;
//            YDY: failed to understand code below... this will not work that way
//            ID:= Frame.IDNew; Flags:= 0;
//            ID[4]:= ID[3];
          end;
          k:= AddTagEx(ID, Flags);
          if k <> -1 then begin
            Tags[k].Size:= DataSize;
            FreeMem(Tags[k].Data);
            Tags[k].Data:= Data;
          end else
            FreeMem(Data);

          SourceFile.Seek(DataPosition + DataSize, spBegin);
        end;
      finally
        SourceFile.Free;
      end;
    except
    end;
end;

function TID3v2.ReadHeader(const FileName: string; var ATag: TTagInfo): Boolean;
var SourceFile: PStream;
    Transferred: Integer;
begin
    try
      Result := true;
      { Set read-access and open file }
      SourceFile:= NewReadFileStreamW(FileName);
      try
        { Read header and get file size }
        Transferred:= SourceFile.Read(ATag, 10);
        ATag.FileSize := SourceFile.Size;
        { if transfer is not complete }
        if Transferred < 10 then Result := false;
      finally
        SourceFile.Free;
      end;
    except
      { Error }
      Result := false;
    end;
end;

procedure TID3v2.BuildHeader(var ATag: TTagInfo);
var i: Integer;
    j, TagSize: DWord;
begin
    { Calculate new tag size (without padding) }
    TagSize := 10;
    for i:= 0 to FTagList.Count-1 do
      if Tags[i].Size > 0 then Inc(TagSize, Tags[i].Size + 10);
    { Check for ability to change existing tag }
    ATag.NeedRewrite := (ATag.ID <> ID3V2_ID) or
                       (GetTagSize(ATag) < TagSize) or
                       (GetTagSize(ATag) > ID3V2_MAX_SIZE);
    { Calculate padding size and set padded tag size }
    if ATag.NeedRewrite then ATag.PaddingSize := ID3V2_MAX_SIZE - TagSize
    else ATag.PaddingSize := GetTagSize(ATag) - TagSize;
    if ATag.PaddingSize > 0 then Inc(TagSize, ATag.PaddingSize);
    { Build tag header }
    ATag.ID := ID3V2_ID;
    ATag.Version := TAG_VERSION_2_3;
    ATag.Revision := 0;
    ATag.Flags := 0;
    { Convert tag size }
    for j := 1 to 4 do ATag.Size[j] := ((TagSize - 10) shr ((4 - j) * 7)) and $7F;
end;

function TID3v2.SaveTag(const FileName: string; ATag: TTagInfo): Boolean;
var i: Integer;
    TagData: PStream;
    FrameSize: Integer;
    Padding: array [1..ID3V2_MAX_SIZE] of Byte;
    S: String;
begin
    { Build and write tag header and frames to stream }
    TagData := NewMemoryStream;
    BuildHeader(ATag);
    TagData.Write(ATag, 10);
    for i:= 0 to FTagList.Count-1 do
      if Tags[i].Size > 0 then begin
        TagData.Write(Tags[i].ID, SizeOf(TID));
        FrameSize := Swap32(Tags[i].Size);
        TagData.Write(FrameSize, SizeOf(FrameSize));
        Setlength(S, Tags[i].Size);
        Move(Tags[i].Data^, S[1], Tags[i].Size);
        S:= #0#0 + S;
        TagData.Write(S[1], Tags[i].Size+2);
      end;
    { Add padding }
    FillChar(Padding, SizeOf(Padding), 0);
    if ATag.PaddingSize > 0 then TagData.Write(Padding, ATag.PaddingSize);
    { Rebuild file or replace tag with new tag data }
    if ATag.NeedRewrite then Result := RebuildFile(FileName, TagData)
    else Result := ReplaceTag(FileName, TagData);
    TagData.Free;
end;

function TID3v2.RebuildFile(const FileName: string; TagData: PStream): Boolean;
var _Tag: TTagInfo;
    Source, Destination: PStream;
    BufferName: string;
begin
    { Rebuild file with old file data and new tag data (optional) }
    Result := false;
    if (not FileExists(FileName)) or (FileSetAttr(FileName, 0) <> 0) then exit;
    if not ReadHeader(FileName, _Tag) then exit;
    if (TagData = nil) and (_Tag.ID <> ID3V2_ID) then exit;
    try
      { Create file streams }
      BufferName := FileName + '~';
      Source := NewReadFileStream(FileName);
      Destination := NewFileStream(BufferName, ofCreateAlways or ofOpenWrite or ofShareExclusive);
      try
        if (Source.Handle = INVALID_HANDLE_VALUE)or(Destination.Handle = INVALID_HANDLE_VALUE) then Exit;
        { Copy data blocks }
        if _Tag.ID = ID3V2_ID then Source.Seek(GetTagSize(_Tag), spBegin);

        if TagData <> nil then begin
          TagData.Position:= 0; Stream2Stream(Destination, TagData, TagData.Size);
        end;
        Destination.Seek(0, spEnd);
        Stream2Stream(Destination, Source, Source.Size - Source.Position);
      finally
        Source.Free;
        Destination.Free;
      end;
      { Replace old file and delete temporary file }
      if DeleteFile(PKOLChar(FileName)) and MoveFile(PKOLChar(BufferName), PKOLChar(FileName)) then Result := true;
    except
      if FileExists(BufferName) then DeleteFile(PKOLChar(BufferName));
    end;
end;

function TID3v2.ReplaceTag(const FileName: string; TagData: PStream): Boolean;
var Destination: PStream;
begin
    { Replace old tag with new tag data }
    Result := false;
    if (not FileExists(FileName)) or (FileSetAttr(FileName, 0) <> 0) then exit;
    try
      TagData.Position := 0;
      Destination := NewReadWriteFileStream(FileName);
      TagData.Position:= 0;
      Stream2Stream(Destination, TagData, TagData.Size);
      Destination.Free;
      Result := true;
    except
      { Access error }
    end;
end;

function TID3v2.GetTagSize(const ATag: TTagInfo): DWord;
begin
    Result:= ATag.Size[1] * $200000 + ATag.Size[2] * $4000 + ATag.Size[3] * $80 + ATag.Size[4] + 10;
    if ATag.Flags and $10 = $10 then Inc(Result, 10);
    if Result > ATag.FileSize then Result := 0;
end;

function TID3v2.GetTagsFrame(Index: Integer): PFrame;
begin
    Result:= FTagList.Items[Index];
end;

function TID3v2.GetTagsByName(Index: TID): PFrame;
var i: Integer;
begin
    Result:= nil;
    for i:= 0 to FTagList.Count-1 do
      if AnsiUpperCase(Tags[i].ID) = AnsiUpperCase(Index) then begin
        Result:= Tags[i];
        Break;
      end;
end;

function TID3v2.GetTagCount: Integer;
begin
    Result:= FTagList.Count;
end;

function TID3v2._AddTagEx(AID: TID; AFlags: Word = 0): Integer;
var Frame: PFrame;
begin
    New(Frame); Frame.ID:= AID; Frame.Flags:= AFlags; Frame.Size:= 0;
    Frame.Data:= nil;//AllocMem(Frame.Size);
    FTagList.Add(Frame);
    Result:= FTagList.Count-1;
end;

procedure TID3v2.DelTag(Index: Integer);
var Frame: PFrame;
begin
    Frame := Tags[Index];
    FreeMem(Frame.Data);
    Finalize(Frame);
    Dispose(Frame);
    FTagList.Delete(Index);
end;

procedure TID3v2.SetTagEx(AID: TID; AValue: String; AFlags: Word = 0);
var Frame: PFrame;
begin
    Frame:= TagsByName[AID];
    if Frame <> nil then begin
      DelTag(FTagList.IndexOf(Frame));
    end;
    if AValue <> '' then begin
      AValue:= #0+AValue;
      Frame:= Tags[_AddTagEx(AID, AFlags)];
      Frame.Size:= Length(AValue);
      Frame.Data:= AllocMem(Frame.Size);
      Move(Avalue[1], Frame.Data^, Frame.Size);
    end;
end;

function TID3v2.GetFrameText(Frame: PFrame): String;
var
    WC: PAnsiChar;
    Temp: AnsiString;
    Len: Integer;
    S: AnsiString;
    IsUnicode, IsUnicodeBE, IsUTF8: Boolean;

    procedure SwapBytes;
    var i: integer;
        WM: PAnsiChar;
        W: Word;
    begin
        WM:=WC;
        for i := 1 to Len div 2 do
        begin
          W := PWord(WC)^;
          W := W shl 8 or W shr 8;
          PWord(WC)^ := W;
          Inc(WC, 2);
        end;
        WC:=WM;
    end;

begin
    Result:= '';
    if Frame <> nil then begin
      if Frame.Size > 0 then
        WC:= Frame.Data;
        Len:= Frame.Size-1;

        IsUnicode   := (AnsiChar(WC^) = #1);
        IsUnicodeBE := (AnsiChar(WC^) = #2);
        IsUTF8      := (AnsiChar(WC^) = #3);
        Inc(WC);

        // remove language descriptor
        if Frame.ID = ID3V2_Frames[tiComment].New then begin // Index
          while (AnsiChar(WC^) <> #0)and(Len > 0)  do begin inc(WC); dec(Len); end;
          while (AnsiChar(WC^) = #0) and(Len > 0) do begin inc(WC); dec(Len); end;
        end;

        // Swap bytes in source data in case of Big Endian byte order
        if IsUnicodeBE then SwapBytes;

        if IsUnicode or IsUnicodeBE then
        begin
          Inc(WC, 2);
          Result:= WideCharLenToString(PWideChar(WC), (Len-2) div 2);
        end
        else
         begin
          SetLength(Temp, Len);
          Move(WC^, Temp[1], Len);
          // Check if string is UTF8
          if {IsUTF8 or} IsUTF8Memory(PByte(Temp), Length(Temp)) then
            Result:=UTF8Decode(Temp)
          else
            Result:=AnsiString(Temp);
        end;
    end;
end;

function TID3v2.GetCommentCount: integer;
var
  i, cnt: Integer;
begin
    Result:=0;
    for i:= 0 to FTagList.Count-1 do
      if AnsiUpperCase(Tags[i].ID) = 'COMM' then Inc(Result);
end;

function TID3v2.GetComments(Index: integer): string;
var
  i, cnt: Integer;
begin
    // In case of multicomments, collect them all
    Cnt:=-1;
    for i:= 0 to FTagList.Count-1 do
    begin
      if AnsiUpperCase(Tags[i].ID) = 'COMM' then
      begin
        Inc(Cnt);
        if (Cnt <> Index) then Continue;
        Result := GetFrameText(Tags[i]);
        Exit;
      end;
    end;
end;

function TID3v2.GetTagsByNameString(Index: TID): String;
var
  Frame: PFrame;
begin
    Result:= '';
    Frame:= TagsByName[Index];
    Result:=GetFrameText(Frame);
end;

function TID3v2.GetTags(const Index: TTagsOfID3): string;
var
    i, GenreNum: integer;
    Str: string;
begin
    Result:= TagsByNameString[ID3V2_Frames[Index].New];
    //correct '(NN)Genre' to 'Genre' or convert (NN) to Genre
    if Index=tiGenre then begin
      Str := Trim(Result);
      i:= Pos(')', Str);
      if i>0 then
      begin
        if i = Length(Str) then
        begin
          GenreNum := StrToIntDef(Copy(Result, Pos('(', Result) + 1, i - 2), -2) + 1;
          if (GenreNum > -1) and (GenreNum <= High(ID3Genres)) then
            Result := ID3Genres[GenreNum]
          else
            Result := Str;
        end else begin
          Delete(Str, 1, i);
          Result := Str;
        end;
      end else
        Result := Str;
    end;
    // Sometimes year is written to TDRC (recording date) frame
    if Index=tiYear then
    begin
      if Result = '' then Result:= TagsByNameString['TDRC'];
    end;
    //correct #0'URL' to 'URL'
    repeat
      i:= Pos(#0, Result);
      if i=0 then Break else Delete(Result, i, 1);
    until false;

    Finalize(Str);
end;

procedure TID3v2.SetTags(const Index: TTagsOfID3; Value: string);
begin
    case Index of
      tiLink: Value:= #0 + Value;
      tiComment: Value:= 'lng' + #0 + Value;
      else Value:= Value;
    end;
    SetTagEx(ID3V2_Frames[Index].New, Value);
end;

procedure TID3v2.SetTag(AID: TID; AValue: String);
begin
    SetTagEx(AID, AValue);
end;

function TID3v2.AddTag(AID: TID; AFlags: Word): Integer;
begin
    Result:= AddTagEx(AID, 0)
end;

function TID3v2.AddTagEx(AID: TID; AFlags: Word): Integer;
begin
    Result:= -1;
    case FDuplicateFrameMethod of
      dfmFirst:
      begin
        Result:= IndexOfTag(AID);
        if Result = -1 then Result:= _AddTagEx(AID, AFlags)
        else Result:= -1;
      end;
      dfmFirstNonEmpty:
      begin
        Result:= IndexOfTag(AID);
        if Result = -1 then Result:= _AddTagEx(AID, AFlags)
        else
           if TagsByNameString[AID] <> '' then Result:= -1;
      end;
      dfmIgnoreDuplicate: Result:= _AddTagEx(AID, AFlags);
      dfmIgnoreDuplicateNonEmpty:
      begin
        Result:= IndexOfTag(AID);
        if Result = -1 then Result:= _AddTagEx(AID, AFlags)
        else
           if TagsByNameString[AID] <> '' then Result := _AddTagEx(AID, AFlags);
      end;
    end;

end;

function TID3v2.IndexOfTag(AID: TID): Integer;
var i: Integer;
begin
    Result:= -1;
    for i:= 0 to TagCount-1 do
      if Tags[i].ID = AID then begin
        Result:= i; Break;
      end;
end;

end.

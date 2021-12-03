{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TWMAfile - for extracting information from WMA file header            }
{                                                                             }
{ Copyright (c) 2001,2002 by Jurgen Faul                                      }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.0 (29 April 2002)                                                 }
{   - Support for Windows Media Audio (versions 7, 8)                         }
{   - File info: file size, channel mode, sample rate, duration, bit rate     }
{   - WMA tag info: title, artist, album, track, year, genre, comment         }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ This file is part of Media Tagger v1.3.x                                    }
{ Copyright (c) 2001-2004 by Ladislav Dufek                                   }
{ E-mail: mediatagger@centrum.cz                                              }
{ http://www.mediatagger.zde.cz                                               }
{                                                                             }
{ Version 1.2.5  (05 November 2003)                                           }
{                                                                             }
{ Based on Audio Tools Library, Copyright (c) 2001,2002 by Jurgen Faul        }
{ Class TWMAfile - for extracting information from WMA file header            }
{ Version 1.0 (29 April 2002)                                                 }
{                                                                             }
{ *************************************************************************** }
{                                                                             }
{ Портировано в KOL - Матвеев дмитрий                                         }
{                                                                             }
{ *************************************************************************** }

// - История -

// Дата: 26.09.2005 Версия: 1.05
// В результате долгого и тщательного тестирования внесено много изменений и получена
// вполне работоспособная версия. Тестирование осуществлял Alexander Splinter, за что ему
// отдельное спасибо большое спасибо.

// Дата: 17.01.2005 Версия: 1.04
// Добавил свойства для наиболее используемых тегов

// Дата: 22.12.2004 Версия: 1.03
// Переделал много

// Дата: 03.12.2004 Версия: 1.02
// Портировал в KOL файл из Media Tagger by Dufek Ladislav <mediatagger@centrum.cz>,
// ноги у него также растут от Audio Tools Library by Jurgen Faul.
// Теперь есть полноценная запись WMA тегов.
// Отдельное спасибо Alexander Splinter <splinty@music4Jesus.ru> за предоставленные материалы.

unit WMA;

interface

uses Windows, KOL;

type
  TWMATagsEx = (tgTitle, tgAuthor, tgAlbumTitle, tgTrack, tgTrackNumber, tgYear, tgGenre,
                tgDescription, tgGenreID, tgPromotionURL, tgCopyright, tgRating, tgEncoder,
                tgStreamType, tgLyrics, tgMCDI, tgComposer, tgFSDKVersion, tgFSDKNeeded);

const
  //Расширенные теги, встречающиеся в WMA
  WMATagsEx: array [TWMATagsEx] of AnsiString = ('WM/Title', 'WM/Author', 'WM/AlbumTitle', 'WM/Track',
                                             'WM/TrackNumber', 'WM/Year', 'WM/Genre', 'WM/Description',
                                             'WM/GenreID', 'WM/PromotionURL', 'WM/Copyright', 'WM/Rating',
                                             'WM/Encoder', 'WM/StreamType', 'WM/Lyrics', 'WM/MCDI',
                                             'WM/Composer', 'WMFSDKVersion', 'WMFSDKNeeded');

  // Тип тегов (WM_StrTag - строковый; WM_IntTag - целый; WM_Binary - бинарный)
  WM_StrTag = 0;
  WM_IntTag = 3;
  WM_Binary = 1;

type
  TObjectID = array [1..16] of AnsiChar;

  //Рекорд для хранения расширенных тегов
  PExTag = ^TExTag;
  TExTag = record
    Name, Value: WideString;      // Название тега и его значение
    ExTagType: Word;              // Тип тега (WM_StrTag - строковый; WM_IntTag - целый; WM_Binary - бинарный)
    BinBuff: Pointer;             // Буфер с данными для типа поля WM_Binary
    BinSize: Word;                // Размер буфера с данными для типа поля WM_Binary
  end;

  PWMA = ^TWMA;
  TWMA = object(TObj)
  private
    FIsValidFile, FIsTAG, FIsExTAG, FIsVBR : Boolean;
    FFileSize, FTagSize : Integer;
    FSampleRate, FBitRate, FMaxBitRate: Integer;
    FChannelModeID: Word;
    FDuration: Extended;

    FTitle   : WideString;
    FArtist  : WideString;
    FComment : WideString;
    FRating  : WideString;
    FCopyright   : WideString;
    FEncoderName : WideString;
    FStreamName  : WideString;

    FExTagList: PList;

    function ReadFieldInteger(const Source: PStream; DataSize: Word): Int64;
    function ReadFieldString(const Source: PStream; DataSize: Word): WideString;

    procedure ReadTagStream(const Source: PStream);
    procedure ReadTagExtended(const Source: PStream);
    procedure ReadTagStandard(const Source: PStream);
    function WriteTagStandard(Stream : PStream) : Cardinal;
    function WriteTagExtended(Stream: PStream): Int64;

    function GetPosition_DataID(Stream : PStream) : Int64;

    procedure ReadObject(const ID: TObjectID; Source: PStream);
    function ReadData(const FileName: string): Boolean;

    function GetChannelMode: string;
    function GetExTag(Index: Integer): PExTag;
    function GetExTagCount: Integer;
    function GetExTagByName(Index: String): PExTag;
    function GetExTagByString(Index: String): String;
    procedure ResetData;
    function GetTagsEx(const Index: TWMATagsEx): string;
    procedure SetTagsEx(const Index: TWMATagsEx; const Value: string);

  public
    destructor Destroy; virtual;

    function ReadFromFile(const FileName: string): Boolean;    // Читает файл и заполняет соответствующие поля
    function SaveToFile(const FileName : String) : Boolean;    // Записывает в файл значения полей
    // Использовать рекомендуется следующим образом:
    // Сначала вызывать ReadFromFile для получения существующих тегов,
    // затем изменять\добавлять\удалять теги, затем записывать SaveToFile.
    function RemoveTags(const FileName: string): Boolean;       // Очищает теги в файле

    property IsValidFile: Boolean read FIsValidFile;            // Признак правильности файла
    property IsTAG: Boolean read FIsTAG;                        // Признак наличия стандартных тегов
    property IsExTAG: Boolean read FIsExTAG;                    // Признак наличия расширенных тегов
    property IsVBR: Boolean read FIsVBR;                        // Признак переменного битрейда

    property ChannelModeID: Word read FChannelModeID;           // Channel mode code
    property ChannelMode: string read GetChannelMode;           // Channel mode name }
    property SampleRate: Integer read FSampleRate;              // Sample rate (hz) }
    property Duration: Extended read FDuration;                 // Duration (seconds) }
    property BitRate: Integer read FBitRate;                    // Bitrate (kbit) }

    property EncoderName: WideString read FEncoderName;         // Название кодера
    property StreamName: WideString read FStreamName;           // Название потока

    property Title: WideString read FTitle write FTitle;        // Стандартные теги, их всего 5.
    property Artist: WideString read FArtist write FArtist;
    property Comment: WideString read FComment write FComment;
    property Rating: WideString read FRating write FRating;
    property Copyright: WideString read FCopyright write FCopyright;

    // Работа с расширенными тегами
    function AddExTag(AName, AValue: String; AExTagType: Integer): Integer;  // Добавляет тег
    procedure SetExTag(AName, AValue: String; AExTagType: Integer);          // Устанавливает значение тега, если он есть, а если нет - добавляет

    function AddTagEx(AName, AValue: String): Integer;  // Добавляет тег
    procedure SetTagEx(AName, AValue: String);          // Устанавливает значение тега, если он есть, а если нет - добавляет
    procedure DelTagEx(Index: Integer);                                      // Удаляет тег

    property ExTagCount: Integer read GetExTagCount;                         // Количество тегов
    property ExTags[Index: Integer]: PExTag read GetExTag;                    // Теги (список)
    property ExTagByName[Index: String]: PExTag read GetExTagByName;         // Теги (по имени)
    property ExTagByNameString[Index: String]: String read GetExTagByString; // Строковое значени тега (по имени)

    property TitleEx: string index tgTitle read GetTagsEx write SetTagsEx;              // Song title
    property ArtistEx: string index tgAuthor read GetTagsEx write SetTagsEx;            // Artist name
    property AlbumEx: string index tgAlbumTitle read GetTagsEx write SetTagsEx;         // Album title
    property TrackEx: string index tgTrack read GetTagsEx write SetTagsEx;              // Track number (string)
    property YearEx: string index tgYear read GetTagsEx write SetTagsEx;                // Release year
    property GenreEx: string index tgGenre read GetTagsEx write SetTagsEx;              // Genre name
    property ComposerEx: string index tgComposer read GetTagsEx write SetTagsEx;        // Composer
    property EncoderEx: string index tgEncoder read GetTagsEx write SetTagsEx;          // Encoder
    property CopyrightEx: string index tgCopyright read GetTagsEx write SetTagsEx;      // (c)
    property LinkEx: string index tgPromotionURL read GetTagsEx write SetTagsEx;        // URL link
    property DescriptionEx: string index tgDescription read GetTagsEx write SetTagsEx;  // Description
    property LyricsEx: string index tgLyrics read GetTagsEx write SetTagsEx;            // Lyrics

  end;

  function NewWMA: PWMA;

implementation

function NewWMA: PWMA;
begin
    New(Result, Create);
    Result.FExTagList:= NewList;
    Result.ResetData;
end;

type
  TRecordID = record Index : Word; Offset : Cardinal; Size : Int64; end;

var
  WMA_CONTENT_DESCRIPTION_ID: TObjectID =
    #51#38#178#117#142#102#207#17#166#217#0#170#0#98#206#108;
  WMA_EXTENDED_CONTENT_DESCRIPTION_ID: TObjectID =
    #64#164#208#210#7#227#210#17#151#240#0#160#201#94#168#80;

const
  WMA_HEADER_ID =
    #48#38#178#117#142#102#207#17#166#217#0#170#0#98#206#108;
  WMA_FILE_PROPERTIES_ID =
    #161#220#171#140#71#169#207#17#142#228#0#192#12#32#83#101;
  WMA_STREAM_PROPERTIES_ID =
    #145#7#220#183#183#169#207#17#142#230#0#192#12#32#83#101;

  WMA_STREAM_DESCRIPTION_ID =
    #64#82#209#134#29#49#208#17#163#164#0#160#201#3#72#246;
//   40 52  D1  86 1D 31  D0 11  A3 A4 00  A0 C9 03 48  F6
  WMA_DATA_ID =
    #54#38#178#117#142#102#207#17#166#217#0#170#0#98#206#108;
//   36 26 B2   75  8E  66  CF 11  A6 D9 00 AA 00 62  CE  6C

  WMA_MAX_STRING_SIZE = 4096;

  WMA_CM_UNKNOWN = 0;                                               { Unknown }
  WMA_CM_MONO = 1;                                                     { Mono }
  WMA_CM_STEREO = 2;                                                 { Stereo }

  WMA_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');

procedure TWMA.ResetData;
var i: Integer;
begin
    FIsValidFile := false; FIsExTag:= False; FIsTAG := false; FIsVBR := false;
    FFileSize := 0; FTagSize := 0;
    FChannelModeID := WMA_CM_UNKNOWN;
    FSampleRate := 0; FDuration := 0; FBitRate := 0;
    FTitle := ''; FArtist := '';
    FComment := '';
    FRating := ''; FCopyright := '';
    FEncoderName := ''; FStreamName := '';
    for i:= FExTagList.Count-1 downto 0 do DelTagEx(i);
end;

function TWMA.GetChannelMode: string;
begin
    Result := WMA_MODE[FChannelModeID];
end;

function TWMA.ReadFromFile(const FileName: string): Boolean;
var Enc: String;
begin
    ResetData;
    Result := ReadData(FileName);

    if Result then begin
      FIsValidFile   := (FMaxBitRate > 0) and
//                      (Data.MaxBitRate < 320000) and
                        ((FChannelModeID = WMA_CM_MONO) or
                        (FChannelModeID = WMA_CM_STEREO)) and
                        (FSampleRate >= 8000) and
                        (FSampleRate <= 96000) and
                        (BitRate > 0) and
                        (BitRate < 400000);

      Enc:= ExTagByNameString[WMATagsEx[tgENCODER]];
      if (Enc <> '') and (FMaxBitRate <> 0) then
        if (Enc[1] <> '9') and (FMaxBitRate < 320000) then
          FDuration := (FFileSize - FTagSize) * 8 / FMaxBitRate
        else
          if (Enc[1] = '9') and (FMaxBitRate < 256000) and (FDuration < 100) then
            FDuration := (FFileSize - FTagSize - FMaxBitRate div 8) / FBitRate - 2;
      if FDuration < 0 then FDuration  := 0;

      FBitRate       := BitRate * 8 div 1000;
      FIsVBR         := (Pos('VBR', string(FStreamName))>0);
    end;
end;

function TWMA.ReadData(const FileName: string): Boolean;
var Source: PStream;
    ID: TObjectID;
    i, ObjectCount, ObjectSize, Position: Integer;
    Size : Cardinal;
begin
    Result:= False;
    try
      Source := NewReadFileStreamW(FileName);
      try
        if NativeUInt(Source.Handle)<=0 then Exit;

        FFileSize := Source.Size;

        Source.Read(ID, SizeOf(ID));
        if ID = WMA_HEADER_ID then begin
          Source.Read(Size, SizeOf(Size));
          Source.Seek(4, spCurrent);
          Source.Read(ObjectCount, SizeOf(ObjectCount));
          Source.Seek(2, spCurrent);

          for i := 1 to ObjectCount do begin
            Position := Source.Position;

            Source.Read(ID, SizeOf(ID));
            Source.Read(ObjectSize, SizeOf(ObjectSize));
            ReadObject(ID, Source);

            Source.Seek(Position + ObjectSize, spBegin);
          end;
        end;
        FTagSize := Size;
        Result := true;
      finally
        Source.Free;
      end;
    except
      Result := false;
    end;
end;

procedure TWMA.ReadObject(const ID: TObjectID; Source: PStream);
begin
    if ID = WMA_FILE_PROPERTIES_ID then begin
      {.Read FileSize from Tag ..}
      Source.Seek(20, spCurrent);
      FFileSize := ReadFieldInteger(Source, 8);
      {.Decode Play Time}
      Source.Seek(16, spCurrent);
      FDuration := (ReadFieldInteger(Source, 8) / 10000000);
      Source.Seek(8, spCurrent);
      FDuration := FDuration - (ReadFieldInteger(Source, 8) / 1000);

  {   Flags :=  (ReadFieldInteger(Source, 4) );
      Flags:= Flags shr 1;                         { Broadcast Flag - not needed }
      Source.Seek(12, spCurrent);
  //  Source.Seek(80, soFromCurrent);              { from previous J.Faul concept seek over all }
      Source.Read(FMaxBitRate, SizeOf(FMaxBitRate));
      FFileSize := Source.Size;
    end;

    if ID = WMA_STREAM_PROPERTIES_ID then begin
      Source.Seek(60, spCurrent);
      Source.Read(FChannelModeID, SizeOf(FChannelModeID));
      Source.Read(FSampleRate, SizeOf(FSampleRate));
      Source.Read(FBitRate, SizeOf(FBitRate));
    end;

    if ID = WMA_CONTENT_DESCRIPTION_ID then begin
      Source.Seek(4, spCurrent);
      FIsTag := True;
      ReadTagStandard(Source);
    end;

    if ID = WMA_EXTENDED_CONTENT_DESCRIPTION_ID then begin
      Source.Seek(4, spCurrent);
      FIsExTag:= True;
      ReadTagExtended(Source);
    end;

    if ID = WMA_STREAM_DESCRIPTION_ID then begin
      Source.Seek(4, spCurrent);
      ReadTagStream(Source);
    end;

    if ID = WMA_DATA_ID then Source.Seek(4, spCurrent);
end;


function TWMA.ReadFieldString(const Source: PStream; DataSize: Word): WideString;
var StringSize: Integer;
    FieldData: array [1..WMA_MAX_STRING_SIZE * 2] of Byte;
begin
    Result := '';
    StringSize := DataSize div 2;
    if StringSize > WMA_MAX_STRING_SIZE then StringSize := WMA_MAX_STRING_SIZE;
    Source.Read(FieldData, StringSize * 2);
    Source.Seek(DataSize - StringSize * 2, spCurrent);
    Result:= WideCharLenToString(PWideChar(@FieldData), StringSize-1);
end;

function TWMA.ReadFieldInteger(const Source: PStream; DataSize: Word): Int64;
var DataBits: Integer;
begin
    Result := 0;
    DataBits := DataSize div 2;
    if DataBits > 8 then DataBits:= 8;                {max int64}
    case DataBits of
      1 : Source.Read(Result, SizeOf(Byte));          {1byte}
      2 : Source.Read(Result, SizeOf(Word));          {2byte}
      4 : Source.Read(Result, SizeOf(Cardinal));      {4byte}
      8 : Source.Read(Result, SizeOf(Int64));         {8byte}
      else result := 0;
    end;
    Source.Seek(DataSize - DataBits, spCurrent);
end;

procedure TWMA.ReadTagStream(const Source: PStream);
var i : Integer;
    FieldCount, DataSize : Word;
    FieldValue: WideString;
begin
    Source.Seek(20, spCurrent);
    Source.Read(FieldCount, SizeOf(FieldCount));
    for I := 1 to FieldCount do begin
      Source.Read(DataSize, SizeOf(DataSize));
      DataSize := DataSize * 2;
      FieldValue := ReadFieldString(Source, DataSize);
      case I of
          1: FEncoderName:= FieldValue;                   {windows media audio v8}
          2: FStreamName:= FieldValue;                    {64 kbps, 44 kHz, stereo}
      end;
    end;
end;

procedure TWMA.ReadTagStandard(const Source: PStream);
var i: Integer;
    FieldSize: array [1..5] of Word;
    FieldValue: WideString;
begin
    Source.Read(FieldSize, SizeOf(FieldSize));
    for i := 1 to 5 do
      if FieldSize[i] > 0 then begin
        FieldValue := ReadFieldString(Source, FieldSize[i]);
        case i of
          1: FTitle:= FieldValue;
          2: FArtist:= FieldValue;
          3: FCopyright:= FieldValue;
          4: FComment:= FieldValue;
          5: FRating:= FieldValue;
        end;
      end;
end;

procedure TWMA.ReadTagExtended(const Source: PStream);
var i, k, FieldCount, DataSize, DataType: Word;
    FieldName: WideString;
begin
    Source.Read(FieldCount, SizeOf(FieldCount));
    for i := 1 to FieldCount do begin
      Source.Read(DataSize, SizeOf(DataSize));
      FieldName := ReadFieldString(Source, DataSize);
      Source.Read(DataType, SizeOf(DataType));
      Source.Read(DataSize, SizeOf(DataSize));

      case DataType of
        0: AddExTag(FieldName, ReadFieldString(Source, DataSize), DataType);
        3: AddExTag(FieldName, Int2Str(ReadFieldInteger(Source, DataSize)), DataType);
        else begin
          k:= AddExTag(FieldName, '', DataType);
          ExTags[k].BinBuff:= AllocMem(DataSize);
          Source.Read(ExTags[k].BinBuff^, DataSize);
        end;
      end;
    end;
end;

destructor TWMA.Destroy;
begin
    ResetData;
    FExTagList.Free;
    inherited;
end;

function TWMA.WriteTagStandard(Stream: PStream): Cardinal;
  procedure Write_Field(Text : widestring);
  var FieldSize: Word;
      FieldData: array [1..WMA_MAX_STRING_SIZE * 2] of Byte;
  begin
      FieldSize:= Min(WMA_MAX_STRING_SIZE, Length(Text)*2 + 2);
      StringToWideChar(Text, PWideChar(@FieldData), FieldSize - 1);
      FieldData[FieldSize-1]:= 0; FieldData[FieldSize]:= 0;
      Stream.Write(FieldData, FieldSize);
  end;
var  FieldSize : array [1 .. 5] of Word;
     Size      : Int64;
begin
    Size := 0;
    FieldSize[1] := Min(WMA_MAX_STRING_SIZE, Length(FTitle)* 2 + 2);
    FieldSize[2] := Min(WMA_MAX_STRING_SIZE, Length(FArtist)* 2 + 2);
    FieldSize[3] := Min(WMA_MAX_STRING_SIZE, Length(FCopyright)*2 + 2);
    FieldSize[4] := Min(WMA_MAX_STRING_SIZE, Length(FComment)*2 + 2);
    FieldSize[5] := Min(WMA_MAX_STRING_SIZE, Length(FRating)*2 + 2);
    Size := 16 + 8 + 5 + 5 + FieldSize[1]+FieldSize[2]+FieldSize[3]+FieldSize[4]+FieldSize[5];

    Stream.Seek(0, spBegin);
    Stream.Write(WMA_CONTENT_DESCRIPTION_ID, 16);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(FieldSize, SizeOf(FieldSize));

    Write_Field(FTitle);
    Write_Field(FArtist);
    Write_Field(FCopyright);
    Write_Field(FComment);
    Write_Field(FRating);
    Result := Stream.Position;
    Stream.Seek(0, spBegin);
end;

function TWMA.WriteTagExtended(Stream: PStream): Int64;
  procedure WriteStr(AName : String);
  var FieldSize: Word;
      FieldData: array [1..WMA_MAX_STRING_SIZE * 2] of Byte;
  begin
      FieldSize:= Min(WMA_MAX_STRING_SIZE, Length(AName)*2 + 2);
      Stream.Write(FieldSize, SizeOf(FieldSize));
      StringToWideChar(AName, PWideChar(@FieldData), FieldSize - 1);
      FieldData[FieldSize-1]:= 0; FieldData[FieldSize]:= 0;
      Stream.Write(FieldData, FieldSize);
  end;
var i: Integer;
    NewObjectSize : Int64;
    IntField, FieldCount: Word;
begin
    FieldCount:= FExTagList.Count;
    Stream.Seek(0, spBegin);
    Stream.Write(WMA_EXTENDED_CONTENT_DESCRIPTION_ID, 16);
    Stream.Write(WMA_EXTENDED_CONTENT_DESCRIPTION_ID, 8);
    Stream.Write(FieldCount, 2);

    for i := 0 to FieldCount-1 do begin
      WriteStr(ExTags[i].Name);
      Stream.Write(ExTags[i].ExTagType, SizeOf(ExTags[i].ExTagType));
      case ExTags[i].ExTagType of
        WM_StrTag:WriteStr(ExTags[i].Value);
        WM_IntTag: begin
          IntField:= 4;
          Stream.Write(IntField, SizeOf(IntField));
          IntField:= Str2Int(ExTags[i].Value);
          Stream.Write(IntField, SizeOf(IntField));
          IntField:= 0;
          Stream.Write(IntField, SizeOf(IntField));
        end;
        WM_Binary: begin
          Stream.Write(ExTags[i].BinSize, SizeOf(ExTags[i].BinSize));
          Stream.Write(ExTags[i].BinBuff, ExTags[i].BinSize);
        end;
      end;
    end;

    {Write Size To OutPut sStrream}
    NewObjectSize := Stream.Position;
    Stream.Seek(16, spBegin);
    Stream.Write(NewObjectSize, SizeOf(NewObjectSize));
    Result:= NewObjectSize; Stream.Seek(0, spBegin);
end;

function TWMA.GetPosition_DataID(Stream: PStream): Int64;
var ID : TObjectID;
    Position : Cardinal;
begin
    Stream.Read(ID, 16);
    if WMA_DATA_ID <> ID then begin
      Stream.Seek(0, spBegin);
      while Stream.Position < Stream.Size-16 do begin                    {Data Head_ID Neni na svem miste, zahajuji hledani}
        Stream.Read(ID, 16);
        if WMA_DATA_ID = ID then Break;
        Position := Stream.Position-15;
        Stream.Seek(Position, spBegin);
        if Position > Stream.Size-15 then begin
          Result := -1; Exit;
        end;
      end;
    end;
    Stream.Seek(-16, spCurrent);
    Result := Stream.Position;
end;

function TWMA.SaveToFile(const FileName: String): Boolean;
var TAG_Stream    : PStream;
    Head_Stream   : PStream;
    Stream        : PStream;
    Data_Buffer   : Pointer;
    ID            : TObjectID;
    Position      : Int64;
    SourceSize,
    SlackSpaceA,
    SlackSpaceB,
    ObjectSizeTAG : Cardinal;
    HeadTAG, DataTAG, SizeTAG, _ExtTAG, _TAG: TRecordID;

  function GetOffsetID(SetID : TObjectID) : TRecordID;
  var  i, Start : Cardinal;
       ID : TObjectID;
       ObjectSize    : Int64;
  begin
      Result.Index := 0; Result.Offset := 30; Result.Size := 0;
      Start := Stream.Position;
      for i:= 1 to HeadTAG.Index do begin
        Position := Stream.Position;
        Stream.Read(ID, SizeOf(ID));
        Stream.Read(ObjectSize, SizeOf(ObjectSize));
        if (ObjectSize < 1) or (ObjectSize > SourceSize) then ;//Application.MessageBox(PChar('The file:'+#13 + PCharW(FileName)), 'Error: Unknown object size  !!!', $00000000 or $00000010);
        if ID = SetID then
        begin
          Result.Index  := i;
          Result.Offset := Position;
          Result.Size   := ObjectSize;
          Break;
        end;
        Stream.Seek(ObjectSize-16-8, spCurrent);
      end;
      Stream.Seek(Start, spBegin);
  end;

begin
    Result := False;
    Data_Buffer := nil;
    try
      Stream := NewReadWriteFileStream(FileName);
      TAG_Stream   := NewMemoryStream;                        {For Tags data}
      Head_Stream  := NewMemoryStream;                        {For Head tag data}
      try
        SourceSize := Stream.Size;
        Stream.Read(ID, SizeOf(ID));
        if ID = WMA_HEADER_ID then begin
          Stream.Read(HeadTAG.Size, SizeOf(HeadTAG.Size));    {Total TAG Size}
          Stream.Read(HeadTAG.Index, SizeOf(SourceSize));     {Total TAG Fields}

          {Sarch for DATA_PART and READ to BUFFER}
          GetMem(Data_Buffer, SourceSize);
          Stream.Seek(HeadTAG.Size, spBegin);
          DataTAG.Offset := GetPosition_DataID(Stream);
          if DataTAG.Offset = 0 then ;//Application.MessageBox(PChar('The file:'+#13 + PCharW(FileName)), 'WMA Data ID Error !!!', $00000000 or $00000010);
          DataTAG.Size := SourceSize - Stream.Position;
          Stream.Read(Data_Buffer^, DataTAG.Size);      {Save DATA Stream}

          {SAVE FILE HEADER}
          Stream.Seek(30, spBegin);                     {Skip first 30b}

          {PROCESSING FIELDS}
          _ExtTAG  := GetOffsetID(WMA_EXTENDED_CONTENT_DESCRIPTION_ID);
          _TAG     := GetOffsetID(WMA_CONTENT_DESCRIPTION_ID);

          if _ExtTAG.Size = 0 then Inc(HeadTAG.Index, 1);
          if _TAG.Size = 0    then Inc(HeadTAG.Index, 1);

          {SAVING FEAUTURES: 1st ExtTAG, slack, 2nd TAG, slack, AudioData}
          SlackSpaceA := 0;
          SlackSpaceB := 0;

          Stream.Seek(0, spBegin);
          Stream2Stream(Head_Stream, Stream, HeadTAG.Size);


          {Save Extend TAG Stream}
          Head_Stream.Seek(_ExtTAG.Offset, spBegin);
          ObjectSizeTAG := WriteTagExtended(TAG_Stream);

          {Search Slack Spaces}
          if _ExtTAG.Offset > _TAG.Offset then begin                       {Tag Is FIRST}
            SlackSpaceA := _ExtTAG.Offset - (_TAG.Offset + _TAG.Size);
            SlackSpaceB := DataTAG.Offset- (_ExtTAG.Offset+_ExtTAG.Size);
            Position    := _TAG.Offset;                                    {Define START Write position}
          end
          else
            if _ExtTAG.Offset < _TAG.Offset then begin               {ExtendTag Is FIRST}
              SlackSpaceA := _TAG.Offset  -(_ExtTAG.Offset + _ExtTAG.Size);
              SlackSpaceB := HeadTAG.Size-(_TAG.Offset + _TAG.Size);
              Position    := _ExtTAG.Offset;
            end
            else ;//Application.MessageBox(PChar('The file:'+#13 + PCharW(FileName)), 'Error: TAG Stream type unsupported !!!', $00000000 or $00000010);

          {WRITE EXTENDED TAG STREAM}
          TAG_Stream.Seek(0, spBegin);
          Stream.Seek(Position, spBegin);
          Stream2Stream(Stream, TAG_Stream, ObjectSizeTAG);

          {WRITE TAG-TAG SLACK SPACE}
          if SlackSpaceA > 16 then begin
            if _ExtTAG.Offset>_TAG.Offset then Head_Stream.Seek(_TAG.Offset+_TAG.Size, spBegin)
            else Head_Stream.Seek(_ExtTAG.Offset+_ExtTAG.Size, spBegin);
            Stream2Stream(Stream, Head_Stream, SlackSpaceA);
          end;

          {WRITE STANDARD TAG STREAM}
          TAG_Stream.Size:= 0; TAG_Stream.Position:= 0;
          ObjectSizeTAG := WriteTagStandard(TAG_Stream);
          Stream2Stream(Stream, TAG_Stream, ObjectSizeTAG);

          {WRITE BEHIND TAGs DATA}
          if SlackSpaceB > 16 then begin
            if _ExtTAG.Offset>_TAG.Offset then Head_Stream.Seek(_ExtTAG.Offset+_ExtTAG.Size, spBegin)
            else
               Head_Stream.Seek(_TAG.Offset+_TAG.Size, spBegin);
             Stream2Stream(Stream, Head_Stream, SlackSpaceB);
          end;

          {SAVE AUDIO DATA STREAM}
          Position := Stream.Position;
          Stream.Write(Data_Buffer^, DataTAG.Size);                      {}
          Stream.Size := Position + DataTAG.Size;
          {Write Tag Size On FileBegin}
          Stream.Seek(16, spBegin);
          Stream.Write(Position, SizeOf(Position));
          {Write Field Count}
          ObjectSizeTAG := HeadTAG.Index;
          Stream.Write(ObjectSizeTAG, SizeOf(ObjectSizeTAG));
          {Zapise Kontrolni Soucet Velikosti Souboru}
          SourceSize := Stream.Size;
          Stream.Seek(30, spBegin);
          SizeTAG := GetOffsetID(WMA_FILE_PROPERTIES_ID);

          Stream.Seek(SizeTAG.Offset+16+4+20, spBegin);{Seek on Position}
          Stream.Write(SourceSize, SizeOf(SourceSize));    {Write New Size}
        end
        else ;//Application.MessageBox(PChar('File:'+#13 + PCharW(FileName)), 'WMA File Error !!!', $00000000 or $00000010);

        Result := True;
      finally
        Stream.Free;
        Head_Stream.Free;
        TAG_Stream.Free;
        if Data_Buffer <> nil then FreeMem(Data_Buffer);
      end;
    except

    end;
end;

function TWMA.RemoveTags(const FileName: string): Boolean;
begin
    ResetData;
    Result := SaveToFile(FileName);
end;

function TWMA.GetExTag(Index: Integer): PExTag;
begin
    Result:= FExTagList.Items[Index];
end;

function TWMA.AddExTag(AName, AValue: String; AExTagType: Integer): Integer;
var _ExTag: PExTag;
begin
    New(_ExTag); _ExTag.Name:= AName; _ExTag.Value:= AValue; _ExTag.ExTagType:= AExTagType; _ExTag.BinBuff:= nil; _ExTag.BinSize:= 0;
    FExTagList.Add(_ExTag);
    Result:= FExTagList.Count-1;
end;

procedure TWMA.DelTagEx(Index: Integer);
var _ExTag: PExTag;
begin
    _ExTag := ExTags[Index];
    FreeMem(_ExTag.BinBuff);
    Finalize(_ExTag);
    Dispose(_ExTag);
    FExTagList.Delete(Index);
end;

function TWMA.GetExTagCount: Integer;
begin
    Result:= FExTagList.Count;
end;

function TWMA.GetExTagByName(Index: String): PExTag;
var i: Integer;
begin
    Result:= nil;
    for i:= 0 to FExTagList.Count-1 do
      if AnsiUpperCase(ExTags[i].Name) = AnsiUpperCase(Index) then begin
        Result:= ExTags[i];
        Break;
      end;
end;

function TWMA.GetExTagByString(Index: String): String;
var _ExTag: PExTag;
begin
    Result:= '';
    _ExTag:= ExTagByName[Index];
    if (_ExTag <> nil) then Result:= _ExTag.Value;
end;

procedure TWMA.SetExTag(AName, AValue: String; AExTagType: Integer);
var _ExTag: PExTag;
begin
    _ExTag:= ExTagByName[AName];
    if _ExTag <> nil then _ExTag.Value:= AValue
    else AddExTag(AName, AValue, AExTagType);
  end;

function TWMA.GetTagsEx(const Index: TWMATagsEx): string;
begin
  Result:= ExTagByNameString[WMATagsEx[Index]];
end;

procedure TWMA.SetTagsEx(const Index: TWMATagsEx; const Value: string);
begin
    SetExTag(WMATagsEx[Index], Value, WM_StrTag);
end;

function TWMA.AddTagEx(AName, AValue: String): Integer;
begin
    Result:= AddExTag(AName, AValue, WM_StrTag);
end;

procedure TWMA.SetTagEx(AName, AValue: String);
begin
    SetExTag(AName, AValue, WM_StrTag);
end;

end.

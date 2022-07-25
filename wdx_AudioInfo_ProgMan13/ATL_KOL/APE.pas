{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TAPE - for manipulating with APE tags                              }
{                                                                             }
{ Copyright (c) 2001,2002 by Jurgen Faul                                      }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.0 (21 April 2002)                                                 }
{   - Reading & writing support for APE 1.0 tags                              }
{   - Reading support for APE 2.0 tags (UTF-8 decoding)                       }
{   - Tag info: title, artist, album, track, year, genre, comment, copyright  }
{                                                                             }
{ Портировано в KOL - Матвеев Дмитрий                                         }
{                                                                             }
{ *************************************************************************** }

// - История -

// Дата: 16.11.2005 Версия: 1.02
// [!] - исправил ошибку при записи APE Teg'ов

// Дата: 18.11.2003 Версия: 1.01
// [!] - исправил ошибку при совместном использовании с ID3v1

// Версия: 1.00
   {Стартовая версия}

unit APE;

interface

uses KOL;

type
  { Class TAPE }
  PAPE = ^TAPE;
  TAPE = object(TObj)
    private
      { Private declarations }
      FExists: Boolean;
      FVersion: Integer;
      FSize: Integer;
      FTitle: string;
      FArtist: string;
      FAlbum: string;
      FTrack: Byte;
      FYear: string;
      FGenre: string;
      FComment: string;
      FCopyright: string;
      FHasCoverArt: Boolean;
      procedure FSetTitle(const NewTitle: string);
      procedure FSetArtist(const NewArtist: string);
      procedure FSetAlbum(const NewAlbum: string);
      procedure FSetTrack(const NewTrack: Byte);
      procedure FSetYear(const NewYear: string);
      procedure FSetGenre(const NewGenre: string);
      procedure FSetComment(const NewComment: string);
      procedure FSetCopyright(const NewCopyright: string);
      procedure InitFields;
    public
      { Public declarations }
      destructor Destroy; virtual;
      procedure ResetData;                                   { Reset all data }
      function ReadFromFile(const FileName: string): Boolean;      { Load tag }
      function RemoveFromFile(const FileName: string): Boolean;  { Delete tag }
      function SaveToFile(const FileName: string): Boolean;        { Save tag }
      property Exists: Boolean read FExists;              { True if tag found }
      property Version: Integer read FVersion;                  { Tag version }
      property Size: Integer read FSize;                     { Total tag size }

      property Title: string read FTitle write FSetTitle;        { Song title }
      property Artist: string read FArtist write FSetArtist;    { Artist name }
      property Album: string read FAlbum write FSetAlbum;       { Album title }
      property Track: Byte read FTrack write FSetTrack;        { Track number }
      property Year: string read FYear write FSetYear;         { Release year }
      property Genre: string read FGenre write FSetGenre;        { Genre name }
      property Comment: string read FComment write FSetComment;     { Comment }
      property Copyright: string read FCopyright write FSetCopyright;

      property HasCoverArt: Boolean read FHasCoverArt;
  end;

  function NewAPE: PAPE;

implementation

const
  { Tag ID }
  ID3V1_ID = 'TAG';                                                   { ID3v1 }
  APE_ID = 'APETAGEX';                                                  { APE }

  { Size constants }
  ID3V1_TAG_SIZE = 128;                                           { ID3v1 tag }
  APE_TAG_FOOTER_SIZE = 32;                                  { APE tag footer }
  APE_TAG_HEADER_SIZE = 32;                                  { APE tag header }

  { First version of APE tag }
  APE_VERSION_1_0 = 1000;

  { Max. number of supported tag fields }
  APE_FIELD_COUNT = 8;

  { Names of supported tag fields }
  APE_FIELD: array [1..APE_FIELD_COUNT] of AnsiString =
    ('Title', 'Artist', 'Album', 'Track', 'Year', 'Genre',
     'Comment', 'Copyright');

type
  { APE tag data - for internal use }
  TagInfo = packed record
    { Real structure of APE footer }
    ID: array [1..8] of AnsiChar;                             { Always "APETAGEX" }
    Version: Integer;                                           { Tag version }
    Size: Integer;                                { Tag size including footer }
    Fields: Integer;                                       { Number of fields }
    Flags: Integer;                                               { Tag flags }
    Reserved: array [1..8] of AnsiChar;                  { Reserved for later use }
    { Extended data }
    DataShift: Byte;                                { Used if ID3v1 tag found }
    FileSize: Integer;                                    { File size (bytes) }
    Field: array [1..APE_FIELD_COUNT] of string;    { Information from fields }
    HasCoverArt: Boolean;
  end;

function NewAPE: PAPE;
begin
    New(Result, Create);
    Result.InitFields;
end;

function StrToIntDef(const S: AnsiString; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(string(S), Result, E);
  if E <> 0 then Result := Default;
end;


{ ********************* Auxiliary functions & procedures ******************** }

function ReadLyricsSize(const Stream: PStream): Integer;
const
  LYRICSBEGIN: AnsiString = 'LYRICSBEGIN';
  LYRICS200:   AnsiString = 'LYRICS200';
var
  StrBegin: array [0..10] of AnsiChar;
  StrEnd: array [0..8] of AnsiChar;
  StrSize: array [0..5] of AnsiChar;
  ASize: Integer;
begin
  Result := 0;

  FillChar(StrEnd, SizeOf(StrEnd), 0);
  FillChar(StrSize, SizeOf(StrSize), 0);

  Stream.Seek(Stream.Size - 128 - 9, spBegin);
  if Stream.Position < 0  then
    Exit;
  Stream.Read(StrEnd, SizeOf(StrEnd));
  if StrEnd <> LYRICS200 then
    Exit;

  Stream.Seek(Stream.Size - 128 - 9 - 6, spBegin);
  if Stream.Position < 0  then
    Exit;
  Stream.Read(StrSize, SizeOf(StrSize));

  ASize := StrToIntDef(StrSize, 0); // size must include 'LYRICSBEGIN'
  if ASize <= 0 then
    Exit;

  Stream.Seek(Stream.Size - 128 - 9 - 6 - ASize, spBegin);
  if Stream.Position < 0  then
    Exit;
  Stream.Read(StrBegin, SizeOf(StrBegin));
  if StrBegin <> LYRICSBEGIN then
    Exit;

  Result := ASize + 9 + 6;
end;

function ReadFooter(const FileName: string; var Tag: TagInfo): Boolean;
var
  SourceFile: PStream;
  TagID: array [1..3] of AnsiChar;
  Transferred: Integer;
begin
  { Load footer from file to variable }
  try
    FillChar(Tag, SizeOf(Tag), 0);
    Result := true;
    { Set read-access and open file }
    SourceFile:= NewReadFileStreamW(FileName);
    try
      Tag.FileSize := SourceFile.Size;
      { Check for existing ID3v1 tag }
      SourceFile.Seek(Tag.FileSize - ID3V1_TAG_SIZE, spBegin);
      SourceFile.Read(TagID, SizeOf(TagID));
      if TagID = ID3V1_ID then
      begin
        Tag.DataShift := ID3V1_TAG_SIZE + ReadLyricsSize(SourceFile);
      end;
      { Read footer data }
      SourceFile.Seek(Tag.FileSize - Tag.DataShift - APE_TAG_FOOTER_SIZE, spBegin);
      Transferred:= SourceFile.Read(Tag, APE_TAG_FOOTER_SIZE);
      if Transferred < APE_TAG_FOOTER_SIZE then Result := false;
    finally
      SourceFile.Free;
    end;
    { if transfer is not complete }
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }
{
function ConvertFromUTF8(const Source: string): string;
var
  Iterator, SourceLength, FChar, NChar: Integer;
begin
  // Convert UTF-8 string to ANSI string
  Result := '';
  Iterator := 0;
  SourceLength := Length(Source);
  while Iterator < SourceLength do
  begin
    Inc(Iterator);
    FChar := Ord(Source[Iterator]);
    if FChar >= $80 then
    begin
      Inc(Iterator);
      if Iterator > SourceLength then break;
      FChar := FChar and $3F;
      if (FChar and $20) <> 0 then
      begin
        FChar := FChar and $1F;
        NChar := Ord(Source[Iterator]);
        if (NChar and $C0) <> $80 then  break;
        FChar := (FChar shl 6) or (NChar and $3F);
        Inc(Iterator);
        if Iterator > SourceLength then break;
      end;
      NChar := Ord(Source[Iterator]);
      if (NChar and $C0) <> $80 then break;
      Result := Result + WideChar((FChar shl 6) or (NChar and $3F));
    end
    else
      Result := Result + WideChar(FChar);
  end;
end;
}
{ --------------------------------------------------------------------------- }

procedure SetTagItem(const FieldName, FieldValue: AnsiString; var Tag: TagInfo);
var
  Iterator: Byte;
  AField: AnsiString;
begin
  AField := UpperCase(FieldName);
  if AField = 'COVER ART (FRONT)' then
    Tag.HasCoverArt := True
  else
  { Set tag item if supported field found }
  for Iterator := 1 to APE_FIELD_COUNT do
    if AField = UpperCase(APE_FIELD[Iterator]) then
      if Tag.Version > APE_VERSION_1_0 then
        Tag.Field[Iterator] := UTF8Decode(FieldValue) //ConvertFromUTF8(FieldValue)
      else
        Tag.Field[Iterator] := FieldValue;
end;

{ --------------------------------------------------------------------------- }

procedure ReadFields(const FileName: string; var Tag: TagInfo);
var
  SourceFile: PStream;
  FieldName: string;
  FieldValue: array [1..250] of AnsiChar;
  NextChar: AnsiChar;
  Iterator, ValueSize, ValuePosition, FieldFlags: Integer;
begin
  try
    { Set read-access, open file }
    SourceFile:= NewReadFileStreamW(FileName);
    try
      SourceFile.Seek(Tag.FileSize - Tag.DataShift - Tag.Size, spBegin);
      { Read all stored fields }
      for Iterator := 1 to Tag.Fields do
      begin
        FillChar(FieldValue, SizeOf(FieldValue), 0);
        SourceFile.Read(ValueSize, SizeOf(ValueSize));
        SourceFile.Read(FieldFlags, SizeOf(FieldFlags));
        FieldName := '';
        repeat
          SourceFile.Read(NextChar, SizeOf(NextChar));
          FieldName := FieldName + NextChar;
        until Ord(NextChar) = 0;
        ValuePosition := SourceFile.Position;
        SourceFile.Read(FieldValue, ValueSize mod SizeOf(FieldValue));
        SetTagItem(Trim(FieldName), Trim(FieldValue), Tag);
        SourceFile.Seek(ValuePosition + ValueSize, spBegin);
      end;
    finally
      SourceFile.Free;
    end;
  except
  end;
end;

{ --------------------------------------------------------------------------- }

function GetTrack(const TrackString: string): Byte;
var
  Index, Value, Code: Integer;
begin
  { Get track from string }
  Index := Pos('/', TrackString);
  if Index = 0 then Val(TrackString, Value, Code)
  else Val(Copy(TrackString, 1, Index - 1), Value, Code);
  if Code = 0 then Result := Value
  else Result := 0;
end;

{ --------------------------------------------------------------------------- }

procedure BuildFooter(var Tag: TagInfo);
var
  Iterator: Integer;
begin
  { Build tag footer }
  Tag.ID := APE_ID;
  Tag.Version := APE_VERSION_1_0;
  Tag.Size := APE_TAG_FOOTER_SIZE;
  for Iterator := 1 to APE_FIELD_COUNT do
    if Tag.Field[Iterator] <> '' then
    begin
      Inc(Tag.Size, Length(APE_FIELD[Iterator] + Tag.Field[Iterator]) + 10);
      Inc(Tag.Fields);
    end;
end;

{ --------------------------------------------------------------------------- }

function AddToFile(const FileName: string; TagData: PStream): Boolean;
var FileData: PStream;
begin
  try
    { Add tag data to file }
    FileData := NewReadWriteFileStream(FileName);
    try
      FileData.Position:= FileData.Size;
      TagData.Position:= 0;
      Stream2Stream(FileData, TagData, TagData.Size);
      Result := true;
    finally
      FileData.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function SaveTag(const FileName: string; Tag: TagInfo): Boolean;
var
  TagData, SourceFile: PStream;
  Iterator, ValueSize, Flags: Integer;
  Buf: Pointer;
  _Tag: TagInfo;
begin
  TagData := NewMemoryStream;
  try
    for Iterator := 1 to APE_FIELD_COUNT do
      if Tag.Field[Iterator] <> '' then
      begin
        ValueSize := Length(Tag.Field[Iterator]) + 1;
        Flags := 0;
        TagData.Write(ValueSize, SizeOf(ValueSize));
        TagData.Write(Flags, SizeOf(Flags));
        TagData.WriteStr(APE_FIELD[Iterator] + #0);
        TagData.WriteStr(Tag.Field[Iterator] + #0);
      end;
    BuildFooter(Tag);
    ReadFooter(FileName, _Tag);
    if _Tag.DataShift>0 then begin
      SourceFile:= NewReadWriteFileStream(FileName);
      SourceFile.Seek(-_Tag.DataShift, spEnd);
      GetMem(Buf, _Tag.DataShift);
      SourceFile.Read(Buf^, _Tag.DataShift);
      SourceFile.Size:= SourceFile.Size - _Tag.DataShift;
      SourceFile.Free;
    end;
    TagData.Write(Tag, APE_TAG_FOOTER_SIZE);
    { Add created tag to file }
    Result := AddToFile(FileName, TagData);
    if _Tag.DataShift>0 then begin
      SourceFile:= NewReadWriteFileStream(FileName);
      SourceFile.Seek(0, spEnd);
      SourceFile.Write(Buf^, _Tag.DataShift);
      FreeMem(Buf);
      SourceFile.Free;
    end;
  finally
    TagData.Free;
  end;
end;

{ ********************** Private functions & procedures ********************* }

procedure TAPE.FSetTitle(const NewTitle: string);
begin
  { Set song title }
  FTitle := Trim(NewTitle);
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.FSetArtist(const NewArtist: string);
begin
  { Set artist name }
  FArtist := Trim(NewArtist);
end;

{ --------------------------------------------------------------------------- }

destructor TAPE.Destroy;
begin
  ResetData;
  inherited;
end;

procedure TAPE.FSetAlbum(const NewAlbum: string);
begin
  { Set album title }
  FAlbum := Trim(NewAlbum);
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.FSetTrack(const NewTrack: Byte);
begin
  { Set track number }
  FTrack := NewTrack;
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.FSetYear(const NewYear: string);
begin
  { Set release year }
  FYear := Trim(NewYear);
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.FSetGenre(const NewGenre: string);
begin
  { Set genre name }
  FGenre := Trim(NewGenre);
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.FSetComment(const NewComment: string);
begin
  { Set comment }
  FComment := Trim(NewComment);
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.FSetCopyright(const NewCopyright: string);
begin
  { Set copyright information }
  FCopyright := Trim(NewCopyright);
end;

{ ********************** Public functions & procedures ********************** }

procedure TAPE.InitFields;
begin
  { Create object }
  ResetData;
end;

{ --------------------------------------------------------------------------- }

procedure TAPE.ResetData;
begin
  { Reset all variables }
  FExists := false;
  FVersion := 0;
  FSize := 0;
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FTrack := 0;
  FYear := '';
  FGenre := '';
  FComment := '';
  FCopyright := '';
  FHasCoverArt := false;
end;

{ --------------------------------------------------------------------------- }

function TAPE.ReadFromFile(const FileName: string): Boolean;
var
  _Tag: TagInfo;
begin
  { Reset data and load footer from file to variable }
  ResetData;
  Result := ReadFooter(FileName, _Tag);
  { Process data if loaded and footer valid }
  if (Result) and (_Tag.ID = APE_ID) then
  begin
    FExists := true;
    { Fill properties with footer data }
    FVersion := _Tag.Version;
    FSize := _Tag.Size;
    { Get information from fields }
    ReadFields(FileName, _Tag);
    FTitle := _Tag.Field[1];
    FArtist := _Tag.Field[2];
    FAlbum := _Tag.Field[3];
    FTrack := GetTrack(_Tag.Field[4]);
    FYear := _Tag.Field[5];
    FGenre := _Tag.Field[6];
    FComment := _Tag.Field[7];
    FCopyright := _Tag.Field[8];
    FHasCoverArt := _Tag.HasCoverArt;
  end;
end;

{ --------------------------------------------------------------------------- }

function TAPE.RemoveFromFile(const FileName: string): Boolean;
var _Tag: TagInfo;
    SourceFile: PStream;
    Buf: Pointer;
begin
  { Remove Tag from file if found }
  Result := True;
  if ReadFooter(FileName, _Tag) then
  try
    if _Tag.ID <> APE_ID then Exit;
    if (_Tag.Flags shr 31) > 0 then Inc(_Tag.Size, APE_TAG_HEADER_SIZE);

    SourceFile:= NewReadWriteFileStream(FileName);
    try
      if _Tag.DataShift>0 then begin
        SourceFile.Seek(-_Tag.DataShift, spEnd);
        GetMem(Buf, _Tag.DataShift);
        SourceFile.Read(Buf^, _Tag.DataShift);
      end;
      SourceFile.Size:= SourceFile.Size - _Tag.DataShift - _Tag.Size;
      if _Tag.DataShift>0 then begin
        SourceFile.Seek(0, spEnd);
        SourceFile.Write(Buf^, _Tag.DataShift);
        FreeMem(Buf);
      end;
    finally
      SourceFile.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function TAPE.SaveToFile(const FileName: string): Boolean;
var
  _Tag: TagInfo;
begin
  { Prepare tag data and save to file }
  FillChar(_Tag, SizeOf(_Tag), 0);
  _Tag.Field[1] := FTitle;
  _Tag.Field[2] := FArtist;
  _Tag.Field[3] := FAlbum;
  if FTrack > 0 then _Tag.Field[4] := Int2Str(FTrack);
  _Tag.Field[5] := FYear;
  _Tag.Field[6] := FGenre;
  _Tag.Field[7] := FComment;
  _Tag.Field[8] := FCopyright;
  { Delete old tag if exists and write new tag }
  Result:= RemoveFromFile(FileName);
  Result:= Result and SaveTag(FileName, _Tag);
end;

end.

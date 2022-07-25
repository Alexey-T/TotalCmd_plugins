{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TID3v1 - for manipulating with ID3v1 tags                             }
{                                                                             }
{ Copyright (c) 2001,2002 by Jurgen Faul                                      }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.0 (25 July 2001)                                                  }
{   - Reading & writing support for ID3v1.x tags                              }
{   - Tag info: title, artist, album, track, year, genre, comment             }
{                                                                             }
{ Портировано в KOL - Матвеев дмитрий                                         }
{                                                                             }
{ *************************************************************************** }

// - История -

// Дата: 18.11.2003 Версия: 1.01
// [!] - исправил ошибку при сохранении в файл.

// Версия: 1.00
   {Стартовая версия}

unit ID3v1;

interface

uses Windows, KOL, ID3Gen;

const
  DEFAULT_GENRE = 255;                              { Index for default genre }

  { Used with VersionID property }
  TAG_VERSION_1_0 = 1;                                { Index for ID3v1.0 tag }
  TAG_VERSION_1_1 = 2;                                { Index for ID3v1.1 tag }

type
  { Used in TID3v1 class }
  String04 = string[4];                          { String with max. 4 symbols }
  String30 = string[30];                        { String with max. 30 symbols }

  PID3v1 = ^TID3v1;
  TID3v1 = object(TObj)
    private
      { Private declarations }
      FExists: Boolean;
      FVersionID: Byte;
      FTitle: String30;
      FArtist: String30;
      FAlbum: String30;
      FYear: String04;
      FComment: String30;
      FTrack: Byte;
      FGenreID: Byte;
      FLyricsSize: Integer;
      procedure FSetTitle(const NewTitle: String30);
      procedure FSetArtist(const NewArtist: String30);
      procedure FSetAlbum(const NewAlbum: String30);
      procedure FSetYear(const NewYear: String04);
      procedure FSetComment(const NewComment: String30);
      procedure FSetTrack(const NewTrack: Byte);
      procedure FSetGenreID(const NewGenreID: Byte);
      function FGetGenre: string;
      procedure InitFields;
      function GetSize: Integer;
      procedure ReadLyrics(const Stream: PStream);
    public
      { Public declarations }
      destructor Destroy; virtual;
      procedure ResetData;                                   { Reset all data }
      function ReadFromFile(const FileName: string): Boolean;      { Load tag }
      function RemoveFromFile(const FileName: string): Boolean;  { Delete tag }
      function SaveToFile(const FileName: string): Boolean;        { Save tag }
      property Exists: Boolean read FExists;              { True if tag found }
      property VersionID: Byte read FVersionID;                { Version code }
      property Title: String30 read FTitle write FSetTitle;      { Song title }
      property Artist: String30 read FArtist write FSetArtist;  { Artist name }
      property Album: String30 read FAlbum write FSetAlbum;      { Album name }
      property Year: String04 read FYear write FSetYear;               { Year }
      property Comment: String30 read FComment write FSetComment;   { Comment }
      property Track: Byte read FTrack write FSetTrack;        { Track number }
      property GenreID: Byte read FGenreID write FSetGenreID;    { Genre code }
      property Genre: string read FGetGenre;                     { Genre name }
      property Size: Integer read GetSize;
      property LyricsSize: Integer read FLyricsSize;
  end;

  function NewID3v1: PID3v1;

implementation

var
  MAX_MUSIC_GENRES: Integer = 0;                       { Max. number of music genres }


function NewID3v1: PID3v1;
begin
    New(Result, Create);
    Result.InitFields;
end;

type
  { Real structure of ID3v1 tag }
  TagRecord = packed record
    Header: array [1..3] of AnsiChar;                { Tag header - must be "TAG" }
    Title: array [1..30] of AnsiChar;                                { Title data }
    Artist: array [1..30] of AnsiChar;                              { Artist data }
    Album: array [1..30] of AnsiChar;                                { Album data }
    Year: array [1..4] of AnsiChar;                                   { Year data }
    Comment: array [1..30] of AnsiChar;                            { Comment data }
    Genre: Byte;                                                 { Genre data }
  end;

{ ********************* Auxiliary functions & procedures ******************** }

function ReadTag(const FileName: string; var TagData: TagRecord): Boolean;
var
  SourceFile: PStream;
begin
  try
    Result := true;
    { Set read-access and open file }
    SourceFile:= NewReadFileStreamW(FileName);
    try
      { Read tag }
      SourceFile.Seek(SourceFile.Size - 128, spBegin);
      SourceFile.Read(TagData, 128);
    finally
      SourceFile.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function FileSetAttr(const FileName: string; Attr: Integer): Integer;
begin
  Result := 0;
  if not SetFileAttributes(PKOLChar(FileName), Attr) then Result := GetLastError;
end;

function RemoveTag(const FileName: string): Boolean;
var
  SourceFile: PStream;
begin
  try
    Result := true;
    { Allow write-access and open file }
    FileSetAttr(FileName, 0);
    SourceFile:= NewReadWriteFileStream(FileName);
    try
      { Delete tag }
      SourceFile.Size:= SourceFile.Size - 128;
    finally
      SourceFile.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function SaveTag(const FileName: string; TagData: TagRecord): Boolean;
var SourceFile: PStream;
begin
  try
    Result := true;
    { Allow write-access and open file }
    FileSetAttr(FileName, 0);
    SourceFile:= NewReadWriteFileStream(FileName);
    try
      { Write tag }
      SourceFile.Seek(0, spEnd);
      SourceFile.Write(TagData, SizeOf(TagData));
    finally
      SourceFile.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetTagVersion(const TagData: TagRecord): Byte;
begin
  Result := TAG_VERSION_1_0;
  { Terms for ID3v1.1 }
  if ((TagData.Comment[29] = #0) and (TagData.Comment[30] <> #0)) or
    ((TagData.Comment[29] = #32) and (TagData.Comment[30] <> #32)) then
    Result := TAG_VERSION_1_1;
end;

{ ********************** Private functions & procedures ********************* }

procedure TID3v1.FSetTitle(const NewTitle: String30);
begin
  FTitle := TrimRight(NewTitle);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.FSetArtist(const NewArtist: String30);
begin
  FArtist := TrimRight(NewArtist);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.FSetAlbum(const NewAlbum: String30);
begin
  FAlbum := TrimRight(NewAlbum);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.FSetYear(const NewYear: String04);
begin
  FYear := TrimRight(NewYear);
end;

function TID3v1.GetSize: Integer;
begin
  if not Exists then
    Exit(0);
  Result := 128 + FLyricsSize;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.FSetComment(const NewComment: String30);
begin
  FComment := TrimRight(NewComment);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.FSetTrack(const NewTrack: Byte);
begin
  FTrack := NewTrack;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.FSetGenreID(const NewGenreID: Byte);
begin
  FGenreID := NewGenreID;
end;

{ --------------------------------------------------------------------------- }

destructor TID3v1.Destroy;
begin
  ResetData;
  inherited;
end;

function TID3v1.FGetGenre: string;
begin
  Result := '';
  { Return an empty string if the current GenreID is not valid }
  // if FGenreID in [0..MAX_MUSIC_GENRES - 1] then Result := MusicGenre[FGenreID];
  // Use below for common genres, defined in ID3Genres.inc
  if FGenreID in [0..MAX_MUSIC_GENRES - 1] then Result := ID3Genres[FGenreID+1];
end;

{ ********************** Public functions & procedures ********************** }

procedure TID3v1.InitFields;
begin
    ResetData;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v1.ResetData;
begin
  FExists := false;
  FVersionID := TAG_VERSION_1_0;
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FYear := '';
  FComment := '';
  FTrack := 0;
  FGenreID := DEFAULT_GENRE;
  FLyricsSize := 0;
end;

{ --------------------------------------------------------------------------- }

function TID3v1.ReadFromFile(const FileName: string): Boolean;

function ReadTag(const FileName: string; var TagData: TagRecord): Boolean;
var
  SourceFile: PStream;
begin
  try
    Result := true;
    { Set read-access and open file }
    SourceFile:= NewReadFileStreamW(FileName);
    try
      { Read tag }
      SourceFile.Seek(SourceFile.Size - 128, spBegin);
      SourceFile.Read(TagData, 128);
      if TagData.Header = 'TAG' then
      begin
        FExists := True;
        ReadLyrics(SourceFile);
      end;
    finally
      SourceFile.Free;
    end;
  except
    { Error }
    Result := false;
  end;
end;

var
  TagData: TagRecord;
begin
  { Reset and load tag data from file to variable }
  ResetData;
  Result := ReadTag(FileName, TagData);
  { Process data if loaded and tag header OK }
  if (Result) and (TagData.Header = 'TAG') then
  begin
    FExists := true;
    FVersionID := GetTagVersion(TagData);
    { Fill properties with tag data }
    FTitle := TrimRight(TagData.Title);
    FArtist := TrimRight(TagData.Artist);
    FAlbum := TrimRight(TagData.Album);
    FYear := TrimRight(TagData.Year);
    if FVersionID = TAG_VERSION_1_0 then
      FComment := TrimRight(TagData.Comment)
    else
    begin
      FComment := TrimRight(Copy(TagData.Comment, 1, 28));
      //FTrack := Ord(TagData.Comment[30]);
      if Ord(TagData.Comment[29]) = 0 then
        FTrack := Byte(TagData.Comment[30]);
    end;
    FGenreID := TagData.Genre;
  end
//  else Result:= False;
end;

function StrToIntDef(const S: AnsiString; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(string(S), Result, E);
  if E <> 0 then Result := Default;
end;

procedure TID3v1.ReadLyrics(const Stream: PStream);
const
  LYRICSBEGIN: AnsiString = 'LYRICSBEGIN';
  LYRICS200:   AnsiString = 'LYRICS200';
var
  StrBegin: array [0..10] of AnsiChar;
  StrEnd: array [0..8] of AnsiChar;
  StrSize: array [0..5] of AnsiChar;
  ASize: Integer;
begin
  if not FExists then
    Exit;

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

  FLyricsSize := ASize + 9 + 6;
end;

{ --------------------------------------------------------------------------- }

function TID3v1.RemoveFromFile(const FileName: string): Boolean;
var
  TagData: TagRecord;
begin
  { Find tag }
  Result := ReadTag(FileName, TagData);
  { Delete tag if loaded and tag header OK }
  if (Result) and (TagData.Header = 'TAG') then Result := RemoveTag(FileName);
end;

{ --------------------------------------------------------------------------- }

function TID3v1.SaveToFile(const FileName: string): Boolean;
var
  TagData: TagRecord;
begin
  { Prepare tag record }
  FillChar(TagData, SizeOf(TagData), 0);
  TagData.Header := 'TAG';
  Move(FTitle[1], TagData.Title, Length(FTitle));
  Move(FArtist[1], TagData.Artist, Length(FArtist));
  Move(FAlbum[1], TagData.Album, Length(FAlbum));
  Move(FYear[1], TagData.Year, Length(FYear));
  Move(FComment[1], TagData.Comment, Length(FComment));
  if FTrack > 0 then
  begin
    TagData.Comment[29] := #0;
    TagData.Comment[30] := AnsiChar(FTrack);
  end;
  TagData.Genre := FGenreID;
  { Delete old tag and write new tag }
  Result := (RemoveFromFile(FileName)) and (SaveTag(FileName, TagData));
end;

{ ************************** Initialize music genres ************************ }

initialization
  MAX_MUSIC_GENRES := Length(ID3Genres);

end.

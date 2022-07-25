unit VGMFile;

interface

uses KOL, SProc;

type
  PVGM = ^TVGM;
  TVGM = object(TObj)
    private
      FSampleCount: Cardinal;
      FValid: Boolean;
      FVersion: string;
      Tags: array [0..10] of string;
      FTagsPresent: Boolean;
    public
      { Public declarations }
      procedure ResetData;
      constructor Create;                                     { Create object }
      destructor Destroy; virtual;
      function ReadFromFile(const FileName: string): Boolean;   { Load header }
      function ReadFromStream(const Stream: PStream): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property SampleCount: Cardinal read FSampleCount;
      property Version: string read FVersion;
      property TagsPresent: Boolean read FTagsPresent;

      { J - Japanese }
      property Track: string read Tags[0];
      property TrackJ: string read Tags[1];
      property Game: string read Tags[2];
      property GameJ: string read Tags[3];
      property System: string read Tags[4];
      property SystemJ: string read Tags[5];
      property Author: string read Tags[6];
      property AuthorJ: string read Tags[7];
      property Date: string read Tags[8];
      property Ripper: string read Tags[9];
      property Note: string read Tags[10];
  end;

  function NewVGM: PVGM;

implementation

function NewVGM: PVGM;
begin
  New(Result, Create);
  Result.ResetData;
end;


{ ********************** Private functions & procedures ********************* }

procedure TVGM.ResetData;
begin
  { Reset all data }
  FValid := false;
  FTagsPresent := False;
  Finalize(Tags);
  Finalize(FVersion);
end;

{ --------------------------------------------------------------------------- }

destructor TVGM.Destroy;
begin
  ResetData;
  inherited;
end;

{ ********************** Public functions & procedures ********************** }

constructor TVGM.Create;
begin
  { Create object }
  inherited;
  ResetData;
end;

{ --------------------------------------------------------------------------- }

function TVGM.ReadFromFile(const FileName: string): Boolean;
var
  SourceFile: PStream;
begin
  Result := False;
  try
    SourceFile:= NewReadFileStreamW(FileName);
    try
      Result := ReadFromStream(SourceFile);
    finally
      SourceFile.Free;
    end;
  except
    Result := False;
  end;
end;

function TVGM.ReadFromStream(const Stream: PStream): Boolean;

  function ParseTags(const ParseChar: Pointer; Size: Cardinal): Boolean;
  var
    A: PWideChar;
    Count, Len, TagCount: Cardinal;
  begin
    Result := False;
    Count := 0;
    TagCount := 0;
    try
      if ParseChar = nil then Exit;
      A := ParseChar;
      while ((Count <= (Size - 2)) and (TagCount <= 10)) do
      begin
        Tags[TagCount] := string(A);
        Len := WStrLen(A);
        Inc(A, Len + 1);
        Inc(Count, Len + 1);
        Inc(TagCount);
      end;
      Result := True;
    except
      Result := False;
    end;
  end;

var
  GD3: array [0..3] of AnsiChar;
  GD3Size: Cardinal;
  P: Pointer;

  Header: packed record
    Magic: array [0..3] of AnsiChar;
    EofOffset: Cardinal;
    Version: array [0..3] of Byte;
    SN76489clock: Cardinal;
    YM2413clock: Cardinal;
    GD3Offset: Cardinal;
    TotalSamples: Cardinal;
    LoopOffset: Cardinal;
    LoopSamples: Cardinal;
    Rate: Cardinal;
  end;
begin
  Result := False;
  P := nil;
  ResetData;

  Stream.Read(Header, SizeOf(Header));

  if Header.Magic <> 'Vgm ' then
    Exit;

  try
    FVersion := IntToStr(Header.Version[1]) + '.' +
      string(Int2Hex(Header.Version[0], 2));

    if Header.GD3Offset > 0 then
    begin
      Stream.Position := Header.GD3Offset + 20;
      if Stream.Position < Stream.Size then
      begin
        Stream.Read(GD3, SizeOf(GD3));
        if GD3 = 'Gd3 ' then
        begin
          Stream.Seek(4, spCurrent);
          Stream.Read(GD3Size, SizeOf(GD3Size));
          if Stream.Position + Int64(GD3Size) <= Stream.Size then
          begin
            FTagsPresent := True;
            P := AllocMem(GD3Size + 2);
            Stream.Read(P^, GD3Size);
            ParseTags(P, GD3Size div 2);
          end;
        end;
      end;
    end;

    Result := True;
    FValid := True;
  finally
    if P <> nil then
      FreeMem(P);
  end;
end;

end.
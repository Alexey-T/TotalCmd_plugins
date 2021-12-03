//********************************************************************************************************************************
//*                                                                                                                              *
//*     MP4 Tag Library 1.0.34.79 © 3delite 2012-2017                                                                            *
//*     See MP4 Tag Library ReadMe.txt for details                                                                               *
//*                                                                                                                              *
//* Two licenses are available for commercial usage of this component:                                                           *
//* Shareware License: €50                                                                                                       *
//* Commercial License: €250                                                                                                     *
//*                                                                                                                              *
//*     http://www.shareit.com/product.html?productid=300548330                                                                  *
//*                                                                                                                              *
//* Using the component in free programs is free.                                                                                *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MP4TagLibrary.html                                         *
//*                                                                                                                              *
//* This component is also available as a part of Tags Library:                                                                  *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/TagsLibrary.html                                           *
//*                                                                                                                              *
//* There is also an ID3v2 Library available at:                                                                                 *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html                                          *
//*                                                                                                                              *
//* and also an APEv2 Library available at:                                                                                      *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/APEv2Library.html                                          *
//*                                                                                                                              *
//* and also an Ogg Vorbis and Opus Tag Library available at:                                                                    *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/OpusTagLibrary.html                                        *
//*                                                                                                                              *
//* a Flac Tag Library available at:                                                                                             *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/FlacTagLibrary.html                                        *
//*                                                                                                                              *
//* an WMA Tag Library available at:                                                                                             *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WMATagLibrary.html                                         *
//*                                                                                                                              *
//* a WAV Tag Library available at:                                                                                              *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WAVTagLibrary.html                                         *
//*                                                                                                                              *
//* an MKV Tag Library available at:                                                                                             *
//*                                                                                                                              *
//*     http://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MKVTagLibrary.html                                         *
//*                                                                                                                              *
//* For other Delphi components see the home page:                                                                               *
//*                                                                                                                              *
//*     http://www.3delite.hu/                                                                                                   *
//*                                                                                                                              *
//* If you have any questions or enquiries please mail: 3delite@3delite.hu                                                       *
//*                                                                                                                              *
//* Good coding! :)                                                                                                              *
//* 3delite                                                                                                                      *
//********************************************************************************************************************************

{.$DEFINE IVAN_LLANAS}

// Changes by Ivan Llanas:
//
// - Added MP4TAGLIBRARY_VERSION_FRIENDLY constant for Trilobite components list.
// - Fixed the Audio information gathering when the audio is not the first trak.
//   GetAudioAttributes assumes that the first trak atom in the header
//   always contains the audio info in an internal mp4a atom. Error. Often the
//   first trak atom is the video one.
// - Added prefix 'Audio' to ChannelCount, Resolution and SampleRate variables.
// - Added VideoWidth and VideoHeight.

// Wanted: FrameRate, VideoColorDepth, VideoBitRate, AudioBitRate.


// Ported to KOL - Dmitry Yudin

unit MP4TagLibrary;

interface

{$IFDEF IOS}
    {$DEFINE MP4TL_MOBILE}
{$ENDIF}

{$IFDEF ANDROID}
    {$DEFINE MP4TL_MOBILE}
{$ENDIF}

Uses
    Windows, KOL, ID3Gen;

{$MINENUMSIZE 4}

Const
    MP4TAGLIBRARY_VERSION = $01003479;
    MP4TAGLIBRARY_VERSION_FRIENDLY = '1.0.34.76 (Ivan Llanas branch)';

Const
    MP4TAGLIBRARY_SUCCESS                       = 0;
    MP4TAGLIBRARY_ERROR                         = $FFFF;
    MP4TAGLIBRARY_ERROR_NO_TAG_FOUND            = 1;
    MP4TAGLIBRARY_ERROR_EMPTY_TAG               = 2;
    MP4TAGLIBRARY_ERROR_EMPTY_FRAMES            = 3;
    MP4TAGLIBRARY_ERROR_OPENING_FILE            = 4;
    MP4TAGLIBRARY_ERROR_READING_FILE            = 5;
    MP4TAGLIBRARY_ERROR_WRITING_FILE            = 6;
    MP4TAGLIBRARY_ERROR_DOESNT_FIT              = 7;
    MP4TAGLIBRARY_ERROR_NOT_SUPPORTED_VERSION   = 8;
    MP4TAGLIBRARY_ERROR_NOT_SUPPORTED_FORMAT    = 9;
    MP4TAGLIBRARY_ERROR_NEED_EXCLUSIVE_ACCESS   = 10;
    MP4TAGLIBRARY_ERROR_UPDATE_stco             = 11;
    MP4TAGLIBRARY_ERROR_UPDATE_co64             = 12;

Const
    MP4TAGLIBRARY_DEFAULT_PADDING_SIZE          = 4096;
    MP4TAGLIBRARY_FAIL_ON_CURRUPT_FILE          = False;

Const
    MAGIC_PNG = $5089;  //* Little endian form
    MAGIC_JPG = $d8ff;  //* Little endian form
    MAGIC_GIF = $4947;  //* Little endian form
    MAGIC_BMP = $4d42;  //* Little endian form

type
    DWord = Cardinal;
    TBytes = array of byte;

    Int64Rec = packed record
      case Integer of
        0: (Lo, Hi: Cardinal);
        1: (Cardinals: array [0..1] of Cardinal);
        2: (Words: array [0..3] of Word);
        3: (Bytes: array [0..7] of Byte);
    end;

type
    TAtomName = Array [0..3] of Byte;

type
    TMP4AudioFormat = (mp4afUnknown, mp4afAAC, mp4afALAC, mpfafAC3);

type
    PMP4Atom = ^TMP4Atom;
    PMP4Atommean = ^TMP4Atommean;
    PMP4Atomname = ^TMP4Atomname;

    TMP4Atommean = object(TObj)
        Data: PStream;
        Parent: PMP4Atom;
        Constructor Create;
        Destructor Destroy; virtual;
        procedure Clear;
        function Write(MP4Stream: PStream): Boolean;
        function Assign(MP4Atommean: PMP4Atommean): Boolean;
        function GetAsText: String;
        function SetAsText(Text: String): Boolean;
    end;

    TMP4Atomname = object(TObj)
        Data: PStream;
        Parent: PMP4Atom;
        Constructor Create;
        Destructor Destroy; virtual;
        procedure Clear;
        function Write(MP4Stream: PStream): Boolean;
        function Assign(MP4Atomname: PMP4Atomname): Boolean;
        function GetAsText: String;
        function SetAsText(Text: String): Boolean;
    end;

    PMP4AtomData = ^TMP4AtomData;
    TMP4AtomData = object(TObj)
        Data: PStream;
        DataType: DWord;
        Reserved: DWord;
        Parent: PMP4Atom;
        Index: Integer;
        Constructor Create;
        Destructor Destroy; virtual;
        function GetAsBytes: TBytes;
        function GetAsText: String;
        function GetAsInteger(out Value: UInt64): Boolean;
        function GetAsInteger8: Byte;
        function GetAsInteger16: Word;
        function GetAsInteger32: DWord;
        function GetAsInteger48(var LowDWord: DWord; var HighWord: Word; out Value: UInt64): Boolean;
        function GetAsInteger64(var LowDWord, HighDWord: DWord; out Value: UInt64): Boolean;
        function GetAsBool: Boolean;
        function GetAsList(List: PStrList): Boolean;
        function SetAsText(Text: String): Boolean;
        function SetAsInteger8(Value: Byte): Boolean;
        function SetAsInteger16(Value: Word): Boolean;
        function SetAsInteger32(Value: DWord): Boolean;
        function SetAsInteger48(Value: UInt64): Boolean; overload;
        function SetAsInteger48(LowDWord: DWord; HighWord: Word): Boolean; overload;
        function SetAsInteger64(Value: UInt64): Boolean; overload;
        function SetAsInteger64(LowDWord, HighDWord: DWord): Boolean; overload;
        function SetAsBool(Value: Boolean): Boolean;
        function SetAsList(List: PStrList): Boolean;
        procedure Clear;
        function Write(MP4Stream: PStream): Boolean;
        function Delete: Boolean;
        function Assign(MP4AtomData: PMP4AtomData): Boolean;
    end;

    PMP4Tag = ^TMP4Tag;

    TMP4Atom = object(TObj)
        ID: TAtomName;
        Size: DWord;
        mean: PMP4Atommean;
        name: PMP4Atomname;
        Datas: Array of PMP4AtomData;
        Flags: DWord;
        Parent: PMP4Tag;
        Index: Integer;
        Constructor Create;
        Destructor Destroy; virtual;
        function AddData: PMP4AtomData;
        function GetAsText: String;
        function GetAsInteger(out Value: UInt64): Boolean;
        function GetAsInteger8: Byte;
        function GetAsInteger16: Word;
        function GetAsInteger32: DWord;
        function GetAsInteger48(var LowDWord: DWord; HiWord: Word; out Value: UInt64): Boolean;
        function GetAsInteger64(var LowDWord, HiDWord: DWord; out Value: UInt64): Boolean;
        function GetAsBool: Boolean;
        function GetAsList(List: PStrList): Boolean;
        function GetAsCommonText(var _name: String; var _mean: String): String;
        function SetAsText(Text: String): Boolean;
        function SetAsInteger8(Value: Byte): Boolean;
        function SetAsInteger16(Value: Word): Boolean;
        function SetAsInteger32(Value: DWord): Boolean;
        function SetAsInteger48(Value: UInt64): Boolean; overload;
        function SetAsInteger48(LowDWord: DWord; HiWord: Word): Boolean; overload;
        function SetAsInteger64(Value: UInt64): Boolean; overload;
        function SetAsInteger64(LowDWord, HiDWord: DWord): Boolean; overload;
        function SetAsBool(Value: Boolean): Boolean;
        function SetAsList(List: PStrList): Boolean;
        function SetAsCommonText(_name: String; _mean: String; Value: String): Boolean;
        function Count: Integer;
        procedure Clear;
        function CalculateAtomSize: Cardinal;
        function Write(MP4Stream: PStream): Boolean;
        procedure Delete;
        function DeleteData(AtomIndex: Integer): Boolean;
        function Deletemean: Boolean;
        function Deletename: Boolean;
        procedure CompactAtomDataList;
        function Assign(MP4Atom: PMP4Atom): Boolean;
    end;

    TMP4Tag = object(TObj)
    private
        FLoaded: Boolean;
        FFileSize: Int64;
        FAtommdatPosition: Int64;
        FAtommdatSize: Int64;
        FPlaytime: Double;
        FAudioChannelCount: Word;
        FAudioResolution: Integer;
        FAudioSampleRate: Cardinal;
        FBitRate: Integer;
        FVideoWidth: Integer;
        FVideoHeight: Integer;
        FAudioFormat: TMP4AudioFormat;
        FAtommoovPosition: Int64;
        FAtommoovSize: Int64;
        FAtomudtaPosition: Int64;
        FAtomudtaSize: Int64;
        FAtommetaPosition: Int64;
        FAtommetaSize: Int64;
        FAtomilstPosition: Int64;
        FAtomilstSize: Int64;
        procedure _GetIvanParameters (MP4Stream : PStream);
    public
        FileName: String;
        Atoms: Array of PMP4Atom;
        Version: Byte;
        Flags: DWord;
        PaddingToWrite: Cardinal;
        Constructor Create;
        Destructor Destroy; virtual;
        function LoadFromFile(MP4FileName: String): Integer;
        function LoadFromStream(MP4Stream: PStream): Integer;
        function SaveToFile(MP4FileName: String; KeepPadding: Boolean = True; UseMemoryForTempDataMaxFileSize: Int64 = 250 * 1024 * 1024 {250 MB}): Integer;
        function SaveToStream(MP4Stream: PStream; KeepPadding: Boolean = True; MP4FileName: String = ''): Integer;
        function AddAtom(AtomName: TAtomName): PMP4Atom; overload;
        function AddAtom(AtomName: String): PMP4Atom; overload;
        function ReadAtom(MP4Stream: PStream; var MP4Atom: PMP4Atom): Boolean;
        function ReadAtomData(MP4Stream: PStream; var MP4AtomData: PMP4AtomData): Boolean;
        function Count: Integer;
        function CoverArtCount: Integer;
        procedure Clear;
        function DeleteAtom(Index: Integer): Boolean; overload;
        function DeleteAtom(AtomName: TAtomName): Boolean; overload;
        function DeleteAtom(AtomName: String): Boolean; overload;
        function DeleteAllAtoms(AtomName: TAtomName): Boolean; overload;
        function DeleteAllAtoms(AtomName: String): Boolean; overload;
        function DeleteAtomCommon(AtomName: String; _name: String; _mean: String): Boolean;
        procedure CompactAtomList;
        function CalculateSize: Int64;
        function FindAtom(AtomName: TAtomName): PMP4Atom; overload;
        function FindAtom(AtomName: String): PMP4Atom; overload;
        function FindAtomCommon(AtomName: TAtomName; _name: String; _mean: String): PMP4Atom; overload;
        function FindAtomCommon(AtomName: String; _name: String; _mean: String): PMP4Atom; overload;
        function GetText(AtomName: TAtomName): String; overload;
        function GetText(AtomName: String): String; overload;
        function GetInteger(AtomName: TAtomName; out Value: UInt64): Boolean; overload;
        function GetInteger(AtomName: String; out Value: UInt64): Boolean; overload;
        function GetInteger8(AtomName: TAtomName): Byte; overload;
        function GetInteger8(AtomName: String): Byte; overload;
        function GetInteger16(AtomName: TAtomName): Word; overload;
        function GetInteger16(AtomName: String): Word; overload;
        function GetInteger32(AtomName: TAtomName): DWord; overload;
        function GetInteger32(AtomName: String): DWord; overload;
        function GetInteger48(AtomName: TAtomName; var LowDWord: DWord; HiWord: Word; out Value: UInt64): Boolean; overload;
        function GetInteger48(AtomName: String; var LowDWord: DWord; HiWord: Word; out Value: UInt64): Boolean; overload;
        function GetInteger64(AtomName: TAtomName; var LowDWord, HiDWord: DWord; out Value: UInt64): Boolean; overload;
        function GetInteger64(AtomName: String; var LowDWord, HiDWord: DWord; out Value: UInt64): Boolean; overload;
        function GetBool(AtomName: TAtomName): Boolean; overload;
        function GetBool(AtomName: String): Boolean; overload;
        function GetList(AtomName: TAtomName; List: PStrList): Boolean; overload;
        function GetList(AtomName: String; List: PStrList): Boolean; overload;
        function GetCommon(_name: String; _mean: String): String;
        function SetText(AtomName: TAtomName; Text: String): Boolean; overload;
        function SetText(AtomName: String; Text: String): Boolean; overload;
        function SetInteger8(AtomName: TAtomName; Value: Byte): Boolean; overload;
        function SetInteger8(AtomName: String; Value: Byte): Boolean; overload;
        function SetInteger16(AtomName: TAtomName; Value: Word): Boolean; overload;
        function SetInteger16(AtomName: String; Value: Word): Boolean; overload;
        function SetInteger32(AtomName: TAtomName; Value: DWord): Boolean; overload;
        function SetInteger32(AtomName: String; Value: DWord): Boolean; overload;
        function SetInteger48(AtomName: TAtomName; Value: UInt64): Boolean; overload;
        function SetInteger48(AtomName: String; Value: UInt64): Boolean; overload;
        function SetInteger48(AtomName: TAtomName; LowDWord: DWord; HighWord: Word): Boolean; overload;
        function SetInteger48(AtomName: String; LowDWord: DWord; HighWord: Word): Boolean; overload;
        function SetInteger64(AtomName: TAtomName; Value: UInt64): Boolean; overload;
        function SetInteger64(AtomName: TAtomName; LowDWord, HighDWord: DWord): Boolean; overload;
        function SetInteger64(AtomName: String; Value: UInt64): Boolean; overload;
        function SetInteger64(AtomName: String; LowDWord, HighDWord: DWord): Boolean; overload;
        function SetBool(AtomName: TAtomName; Value: Boolean): Boolean; overload;
        function SetBool(AtomName: String; Value: Boolean): Boolean; overload;
        function SetList(AtomName: TAtomName; List: PStrList): Boolean; overload;
        function SetList(AtomName: String; List: PStrList): Boolean; overload;
        function SetCommon(_name: String; _mean: String; Value: String): Boolean;
        function GetMediaType: String;
        function SetMediaType(Media: String): Boolean;
        function GetTrack: Word;
        function GetTotalTracks: Word;
        function GetDisc: Word;
        function GetTotalDiscs: Word;
        function SetTrack(Track: Word; TotalTracks: Word): Boolean;
        function SetDisc(Disc: Word; TotalDiscs: Word): Boolean;
        function GetGenre: String;
        function SetGenre(Genre: String): Boolean;
        function GetPurchaseCountry: String;
        function SetPurchaseCountry(Country: String): Boolean;
        function Assign(MP4Tag: PMP4Tag): Boolean;
        function GetMultipleValuesMultipleAtoms(AtomName: TAtomName; List: PStrList): Boolean; overload;
        function GetMultipleValuesMultipleAtoms(AtomName: String; List: PStrList): Boolean; overload;
        procedure SetMultipleValuesCommaSeparated(AtomName: TAtomName; List: PStrList); overload;
        procedure SetMultipleValuesCommaSeparated(AtomName: String; List: PStrList); overload;
        procedure SetMultipleValuesMultipleAtoms(AtomName: TAtomName; List: PStrList); overload;
        procedure SetMultipleValuesMultipleAtoms(AtomName: String; List: PStrList); overload;
        property Loaded: Boolean read FLoaded;
        property FileSize: Int64 read FFileSize;
        property mdatAtomPosition: Int64 read FAtommdatPosition;
        property mdatAtomSize: Int64 read FAtommdatSize;
        property Playtime: Double read FPlaytime;
        property AudioChannelCount: Word read FAudioChannelCount;
        property AudioResolution: Integer read FAudioResolution;
        property AudioSampleRate: Cardinal read FAudioSampleRate;
        property BitRate: Integer read FBitRate;
        property VideoWidth: Integer read FVideoWidth;
        property VideoHeight: Integer read FVideoHeight;
        property AudioFormat: TMP4AudioFormat read FAudioFormat;


        property AtommoovPosition: Int64 read FAtommoovPosition;
        property AtommoovSize: Int64 read FAtommoovSize;
        property AtomudtaPosition: Int64 read FAtomudtaPosition;
        property AtomudtaSize: Int64 read FAtomudtaSize;
        property AtommetaPosition: Int64 read FAtommetaPosition;
        property AtommetaSize: Int64 read FAtommetaSize;
        property AtomilstPosition: Int64 read FAtomilstPosition;
        property AtomilstSize: Int64 read FAtomilstSize;

    end;

    function ReadAtomHeader(MP4Stream: PStream; var AtomName: TAtomName; var AtomSize: Int64; var Is64BitSize: Boolean; FailOnCurrupt: Boolean = True): Boolean;
    function WriteAtomHeader(MP4Stream: PStream; AtomName: TAtomName; AtomSize: Int64): Boolean; overload;
    function WriteAtomHeader(MP4Stream: PStream; AtomName: String; AtomSize: Int64): Boolean; overload;
    function WritePadding(MP4Stream: PStream; PaddingSize: Integer): Integer;
    function MP4mdatAtomLocation(MP4Stream: PStream): Int64;
    function MP4UpdatestcoAtom(MP4Stream: PStream; Offset: Integer): Boolean;
    function MP4Updateco64Atom(MP4Stream: PStream; Offset: Int64): Boolean;
    procedure GetmdatAtom(MP4Stream: PStream; out Position, Size: Int64);
    function GetPlaytime(MP4Stream: PStream): Double;

    //function GetAudioAttributes(MP4Tag: TMP4Tag; MP4Stream: TStream): Boolean;

    function RemoveMP4TagFromFile(FileName: String; KeepPadding: Boolean): Integer;
    function RemoveMP4TagFromStream(Stream: PStream; KeepPadding: Boolean): Integer;

    function ReverseBytes16(AWord: Word): Word; inline;
    function ReverseBytes32(Value: Cardinal): Cardinal; overload; inline;
    function ReverseBytes32(Value: Integer): Integer; overload; inline;
    function ReverseBytes64(const aVal: Int64): Int64; overload; inline;
    function ReverseBytes64(const aVal: UInt64): UInt64; overload; inline;

    function MakeUInt64(LowDWord, HiDWord: DWord): UInt64; inline;
    function LowDWordOfUInt64(Value: UInt64): Cardinal; inline;
    function HighDWordOfUInt64(Value: UInt64): Cardinal; inline;

    function LoWord(L: DWord): Word; inline;
    function HiWord(L: DWord): Word; inline;

    //procedure AnsiStringToPAnsiChar(const Source: String; Dest: PChar; const MaxLength: Integer);

    function GenreToIndex(Genre: String): Integer;

    function MediaTypeToStr(Value: Integer): string;

    function IsSameAtomName(ID: TAtomName; Name: String): Boolean; overload;
    function IsSameAtomName(ID1: TAtomName; ID2: TAtomName): Boolean; overload;

    function StringToAtomName(Name: String; var ID: TAtomName): Boolean;
    function AtomNameToString(ID: TAtomName): String;

    function MP4TagErrorCode2String(ErrorCode: Integer): String;

    function NewMP4Tag: PMP4Tag;

implementation

{$IFDEF POSIX}
Uses
    Posix.UniStd;
    //Posix.StdIO;
{$ENDIF}

var
    MP4AtomDataID: TAtomName;
    MP4AtommeanID: TAtomName;
    MP4AtomnameID: TAtomName;
    MP4TagLibraryDefaultPaddingSize: Integer = MP4TAGLIBRARY_DEFAULT_PADDING_SIZE;
    MP4TagLibraryFailOnCorruptFile: Boolean = MP4TAGLIBRARY_FAIL_ON_CURRUPT_FILE;

function NewMP4Tag: PMP4Tag;
begin
    New(Result, Create);
    Result.Clear;
end;

function ReverseBytes32(Value: Cardinal): Cardinal;
begin
    Result := (Value SHR 24) OR (Value SHL 24) OR ((Value AND $00FF0000) SHR 8) OR ((Value AND $0000FF00) SHL 8);
end;

function ReverseBytes32(Value: Integer): Integer;
begin
    Result := Integer(ReverseBytes32(Cardinal(Value)));
end;

function ReverseBytes64(const aVal: Int64): Int64; inline;
begin
    Int64Rec(Result).Bytes[0] :=  Int64Rec(aVal).Bytes[7];
    Int64Rec(Result).Bytes[1] :=  Int64Rec(aVal).Bytes[6];
    Int64Rec(Result).Bytes[2] :=  Int64Rec(aVal).Bytes[5];
    Int64Rec(Result).Bytes[3] :=  Int64Rec(aVal).Bytes[4];
    Int64Rec(Result).Bytes[4] :=  Int64Rec(aVal).Bytes[3];
    Int64Rec(Result).Bytes[5] :=  Int64Rec(aVal).Bytes[2];
    Int64Rec(Result).Bytes[6] :=  Int64Rec(aVal).Bytes[1];
    Int64Rec(Result).Bytes[7] :=  Int64Rec(aVal).Bytes[0];
end;

function ReverseBytes64(const aVal: UInt64): UInt64;
begin
    Result := UInt64(ReverseBytes64(Int64(aVal)));
end;

function ReverseBytes16(AWord: Word): Word;
begin
    Result := AWord shl 8 or AWord shr 8;
end;

function MakeUInt64(LowDWord, HiDWord: DWord): UInt64;
begin
    Result := LowDWord OR (UInt64(HiDWord) SHL 32);
end;

function LowDWordOfUInt64(Value: UInt64): Cardinal;
begin
    Result := (Value SHL 32) SHR 32;
end;

function HighDWordOfUInt64(Value: UInt64): Cardinal;
begin
    Result := Value SHR 32;
end;

function Min(const B1, B2: Integer): Integer;
begin
    if B1 < B2 then begin
        Result := B1
    end else begin
        Result := B2;
    end;
end;

{
procedure AnsiStringToPAnsiChar(const Source: String; Dest: PChar; const MaxLength: Integer);
begin
    Move(PChar(Source)^, Dest^, Min(MaxLength, Length(Source)));
end;
}

function LoWord(L: DWord): Word;
begin
    Result := Word(L);
end;

function HiWord(L: DWORD): Word;
begin
    Result := L SHR 16;
end;

Constructor TMP4Atommean.Create;
begin
    Inherited;
    Data := NewMemoryStream;
end;

Destructor TMP4Atommean.Destroy;
begin
    Data.Free;
    Data:=nil;
    Inherited;
end;

function TMP4Atommean.GetAsText: String;
var
    i: Integer;
    DataByte: Byte;
    Bytes: TBytes;
begin
    Result := '';
    if Data.Size < 4 then begin
        Exit;
    end;
    Data.Seek(4, spBegin);
    SetLength(Bytes, Data.Size - 4);
    for i := 0 to Data.Size - 1 - 4 do begin
        Data.Read(DataByte, 1);
        Bytes[i] := DataByte;
    end;
    Data.Seek(0, spBegin);
    {$IFDEF FPC}
    SetLength(Result, Length(Bytes) + 1);
    Move(Bytes[0], Result[1], Length(Bytes));
    {$ELSE}
    Result := UTF8Decode(AnsiString(Bytes));
    {$ENDIF}
end;

function TMP4Atommean.SetAsText(Text: String): Boolean;
var
    Zero: DWord;
    Bytes: RawByteString;
begin
    Data.Capacity:=0;
    SetSizeMemStream(Data, 0);
    if Text = '' then begin
        Result := True;
        Exit;
    end;
    {$IFDEF FPC}
    SetLength(Bytes, Length(Text) + 1);
    Move(Text[1], Bytes[0], Length(Text));
    {$ELSE}
    Bytes := UTF8Encode(Text);
    {$ENDIF}
    Zero := 0;
    Data.Write(Zero, 4);
    Data.Write(Bytes[1], Length(Bytes));
    Data.Seek(0, spBegin);
    Result := True;
end;

procedure TMP4Atommean.Clear;
begin
    Data.Capacity:=0;
end;

function TMP4Atommean.Write(MP4Stream: PStream): Boolean;
var
    AtomSize: DWord;
    AtomSizeLE: DWord;
begin
    Result := False;
    try
        if Data.Size > 0 then begin
            AtomSize := Data.Size + 8;
            AtomSizeLE := ReverseBytes32(AtomSize);
            MP4Stream.Write(AtomSizeLE, 4);
            MP4Stream.Write(MP4AtommeanID, 4);
            Data.Seek(0, spBegin);
            Stream2Stream(MP4Stream, Data, Data.Size);
//            MP4Stream.CopyFrom(Data, Data.Size);
            Data.Seek(0, spBegin);
            Result := True;
        end;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function TMP4Atommean.Assign(MP4Atommean: PMP4Atommean): Boolean;
begin
    Clear;
    if MP4Atommean <> nil then begin
        MP4Atommean.Data.Seek(0, spBegin);
        Stream2Stream(Data, MP4Atommean.Data, MP4Atommean.Data.Size);
//        Data.CopyFrom(MP4Atommean.Data, MP4Atommean.Data.Size);
        MP4Atommean.Data.Seek(0, spBegin);
    end;
    Result := True;
end;

Constructor TMP4Atomname.Create;
begin
    Inherited;
    Data := NewMemoryStream;
end;

Destructor TMP4Atomname.Destroy;
begin
    Data.Free;
    Data:=nil;
    Inherited;
end;

function TMP4Atomname.GetAsText: String;
var
    i: Integer;
    DataByte: Byte;
    Bytes: TBytes;
begin
    Result := '';
    if Data.Size < 4 then begin
        Exit;
    end;
    Data.Seek(4, spBegin);
    SetLength(Bytes, Data.Size - 4);
    for i := 0 to Data.Size - 1 - 4 do begin
        Data.Read(DataByte, 1);
        Bytes[i] := DataByte;
    end;
    Data.Seek(0, spBegin);
    {$IFDEF FPC}
    SetLength(Result, Length(Bytes) + 1);
    Move(Bytes[0], Result[1], Length(Bytes));
    {$ELSE}
    Result := UTF8Decode(AnsiString(Bytes));
    {$ENDIF}
end;

function TMP4Atomname.SetAsText(Text: String): Boolean;
var
    Zero: DWord;
    Bytes: RawByteString;
begin
    Data.Capacity:=0;
    if Text = '' then begin
        Result := True;
        Exit;
    end;
    {$IFDEF FPC}
    SetLength(Bytes, Length(Text) + 1);
    Move(Text[1], Bytes[0], Length(Text));
    {$ELSE}
    Bytes := UTF8Encode(Text);
    {$ENDIF}
    Zero := 0;
    Data.Write(Zero, 4);
    Data.Write(Bytes[1], Length(Bytes));
    Data.Seek(0, spBegin);
    Result := True;
end;

procedure TMP4Atomname.Clear;
begin
    Data.Capacity:=0;
end;

function TMP4Atomname.Write(MP4Stream: PStream): Boolean;
var
    AtomSize: DWord;
    AtomSizeLE: DWord;
begin
    Result := False;
    try
        if Data.Size > 0 then begin
            AtomSize := Data.Size + 8;
            AtomSizeLE := ReverseBytes32(AtomSize);
            MP4Stream.Write(AtomSizeLE, 4);
            MP4Stream.Write(MP4AtomnameID, 4);
            Data.Seek(0, spBegin);
            Stream2Stream(MP4Stream, Data, Data.Size);
            //MP4Stream.CopyFrom(Data, Data.Size);
            Data.Seek(0, spBegin);
            Result := True;
        end;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function TMP4Atomname.Assign(MP4Atomname: PMP4Atomname): Boolean;
begin
    Clear;
    if MP4Atomname <> nil then begin
        MP4Atomname.Data.Seek(0, spBegin);
        Stream2Stream(Data, MP4Atomname.Data, MP4Atomname.Data.Size);
        //Data.CopyFrom(MP4Atomname.Data, MP4Atomname.Data.Size);
        MP4Atomname.Data.Seek(0, spBegin);
    end;
    Result := True;
end;

Constructor TMP4AtomData.Create;
begin
    Inherited;
    Data := NewMemoryStream;
end;

Destructor TMP4AtomData.Destroy;
begin
    Data.Free;
    Data:=nil;
    Inherited;
end;

function TMP4AtomData.GetAsBytes: TBytes;
var
    DataDWord: PCardinal;
begin
    Data.Seek(0, spbegin);
    SetLength(Result, Data.Size + 8);
    DataDWord := @Result[0];
    DataDWord^ := DataType;
    DataDWord^ := ReverseBytes32(DataDWord^);
    Inc(DataDWord);
    DataDWord^ := Reserved;
    if Data.Size > 0 then begin
        Inc(DataDWord);
        Data.Read(DataDWord^, Data.Size);
    end;
    Data.Seek(0, spBegin);
end;

function TMP4AtomData.GetAsText: String;
var
    i: Integer;
    DataByte: Byte;
    Bytes: TBytes;
    RB: RawByteString;
begin
    Result := '';
    if DataType <> 1 then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    SetLength(Bytes, Data.Size);
    for i := 0 to Data.Size - 1 do begin
        Data.Read(DataByte, 1);
        Bytes[i] := DataByte;
    end;
    Data.Seek(0, spBegin);
    {$IFDEF FPC}
    SetLength(Result, Length(Bytes) + 1);
    Move(Bytes[0], Result[1], Length(Bytes));
    {$ELSE}
    SetLength(RB, Length(Bytes));
    Move(Bytes[0], RB[1], Length(Bytes));
    Result := UTF8Decode(RB);
    {$ENDIF}
end;

function TMP4AtomData.GetAsInteger(out Value: UInt64): Boolean;
var
    LowDWord: DWord;
    HighDWord: DWord;
    HighWord: Word;
begin
    Result := True;
    Value := 0;
    LowDWord := 0;
    HighDWord := 0;
    HighWord := 0;
    case Data.Size of
        1: Value := GetAsInteger8;
        2: Value := GetAsInteger16;
        4: Value := GetAsInteger32;
        6: Result := GetAsInteger48(LowDWord, HighWord, Value);
        8: Result := GetAsInteger64(LowDWord, HighDWord, Value);
        else Result := False;
    end;
end;

function TMP4AtomData.GetAsInteger8: Byte;
begin
    Result := 0;
    if (DataType <> 0)
    AND (DataType <> 21)
    then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    Data.Read(Result, 1);
    Data.Seek(0, spBegin);
end;

function TMP4AtomData.GetAsList(List: PStrList): Boolean;
var
    DataByte: Byte;
    Bytes: TBytes;
    Name: String;
    Value: String;
    ByteCounter: Integer;
begin
    Result := False;
    List.Clear;
    if DataType <> 1 then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    while Data.Position < Data.Size do begin
        SetLength(Bytes, 0);
        ByteCounter := 0;
        repeat
            Data.Read(DataByte, 1);
            if DataByte = $0D then begin
                Data.Read(DataByte, 1);
                if DataByte = $0A then begin
                    Break;
                end;
            end;
            SetLength(Bytes, Length(Bytes) + 1);
            Bytes[ByteCounter] := DataByte;
            Inc(ByteCounter);
        until Data.Position >= Data.Size;
        {$IFDEF FPC}
        SetLength(Name, Length(Bytes) + 1);
        Move(Bytes[0], Name[1], Length(Bytes));
        {$ELSE}
        Name := UTF8Decode(AnsiString(Bytes));
        {$ENDIF}
        SetLength(Bytes, 0);
        ByteCounter := 0;
        repeat
            Data.Read(DataByte, 1);
            if DataByte = $0D then begin
                Data.Read(DataByte, 1);
                if DataByte = $0A then begin
                    Break;
                end;
            end;
            SetLength(Bytes, Length(Bytes) + 1);
            Bytes[ByteCounter] := DataByte;
            Inc(ByteCounter);
        until Data.Position >= Data.Size;

        {$IFDEF FPC}
        SetLength(Value, Length(Bytes) + 1);
        Move(Bytes[0], Value[1], Length(Bytes));
        {$ELSE}
        Value := UTF8Decode(AnsiString(Bytes));
        {$ENDIF}
        List.Add(Name + '=' + Value);
        Result := True;
    end;
    Data.Seek(0, spBegin);
end;

function TMP4AtomData.GetAsInteger16: Word;
begin
    Result := 0;
    if (DataType <> 0)
    AND (DataType <> 21)
    then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    Data.Read(Result, 2);
    Data.Seek(0, spBegin);
    Result := ReverseBytes16(Result);
end;

function TMP4AtomData.GetAsInteger32: DWord;
begin
    Result := 0;
    if (DataType <> 0)
    AND (DataType <> 21)
    then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    Data.Read(Result, 4);
    Data.Seek(0, spBegin);
    if Data.Size = 4 then begin
        Result := ReverseBytes32(Result);
    end;
end;

function TMP4AtomData.GetAsInteger48(var LowDWord: DWord; var HighWord: Word; out Value: UInt64): Boolean;
begin
    Value := 0;
    LowDWord := 0;
    HighWord := 0;
    Result := (DataType = 0) OR (DataType = 21);
    if (DataType <> 0)
    AND (DataType <> 21)
    then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    Data.Read(HighWord, 2);
    Data.Read(LowDWord, 4);
    Data.Seek(0, spBegin);
    HighWord := ReverseBytes16(HighWord);
    LowDWord := ReverseBytes32(LowDWord);
    Value := MakeUInt64(LowDWord, HighWord);
end;

function TMP4AtomData.GetAsInteger64(var LowDWord, HighDWord: DWord; out Value: UInt64): Boolean;
begin
    Value := 0;
    LowDWord := 0;
    HighDWord := 0;
    Result := (DataType = 0) OR (DataType = 21);
    if (DataType <> 0)
    AND (DataType <> 21)
    then begin
        Exit;
    end;
    Data.Seek(0, spBegin);
    Data.Read(Value, 8);
    Data.Seek(0, spBegin);
    Value := ReverseBytes64(Value);
    HighDWord := HighDWordOfUInt64(Value);
    LowDWord := LowDWordOfUInt64(Value);
end;

function TMP4AtomData.GetAsBool: Boolean;
var
    Value: Byte;
begin
    Value := GetAsInteger8;
    if Value = 0 then begin
        Result := False;
    end else begin
        Result := True;
    end;
end;

function TMP4AtomData.SetAsText(Text: String): Boolean;
var
    Bytes: RawByteString;
begin
    {$IFDEF FPC}
    SetLength(Bytes, Length(Text) + 1);
    Move(Text[1], Bytes[0], Length(Text));
    {$ELSE}
    Bytes := UTF8Encode(Text);
    {$ENDIF}
    Data.Capacity:=0;
    Data.Write(Bytes[1], Length(Bytes));
    Data.Seek(0, spBegin);
    DataType := 1;
    Result := True;
end;

function TMP4AtomData.SetAsInteger8(Value: Byte): Boolean;
begin
    DataType := 0;
    Data.Capacity:=0;
    Data.Write(Value, 1);
    Data.Seek(0, spBegin);
    Result := True;
end;

function TMP4AtomData.SetAsList(List: PStrList): Boolean;
var
    i: Integer;
    DataByte: Byte;
    BytesName: RawByteString;
    BytesValue: RawByteString;
begin
    Data.Capacity:=0;
    for i := 0 to List.Count - 1 do begin
        {$IFDEF FPC}
        SetLength(BytesName, Length(List.Names[i]) + 1);
        Move(List.Names[i][1], BytesName[0], Length(List.Names[i]));
        {$ELSE}
        BytesName := UTF8Encode(List.LineName[i]);
        {$ENDIF}
        {$IFDEF FPC}
        SetLength(BytesValue, Length(List.ValueFromIndex[i]) + 1);
        Move(List.ValueFromIndex[i][1], BytesValue[0], Length(List.ValueFromIndex[i]));
        {$ELSE}
        BytesValue := UTF8Encode(List.LineValue[i]);
        {$ENDIF}
        Data.Write(BytesName[1], Length(BytesName));
        DataByte := $0D;
        Data.Write(DataByte, 1);
        DataByte := $0A;
        Data.Write(DataByte, 1);
        Data.Write(BytesValue[1], Length(BytesValue));
        DataByte := $0D;
        Data.Write(DataByte, 1);
        DataByte := $0A;
        Data.Write(DataByte, 1);
    end;
    Data.Seek(0, spBegin);
    DataType := 1;
    Result := True;
end;

function TMP4AtomData.SetAsInteger16(Value: Word): Boolean;
begin
    DataType := 0;
    Data.Capacity:=0;
    Value := ReverseBytes16(Value);
    Data.Write(Value, 2);
    Data.Seek(0, spBegin);
    Result := True;
end;

function TMP4AtomData.SetAsInteger32(Value: DWord): Boolean;
begin
    DataType := 0;
    Data.Capacity:=0;
    Value := ReverseBytes32(Value);
    Data.Write(Value, 4);
    Data.Seek(0, spBegin);
    Result := True;
end;

function TMP4AtomData.SetAsInteger48(Value: UInt64): Boolean;
var
    LowDWord: DWord;
    HiWord: Word;
begin
    LowDWord := LowDWordOfUInt64(Value);
    HiWord := HighDWordOfUInt64(Value);
    Result := SetAsInteger48(LowDWord, HiWord);
end;

function TMP4AtomData.SetAsInteger48(LowDWord: DWord; HighWord: Word): Boolean;
begin
    DataType := 0;
    Data.Capacity:=0;
    LowDWord := ReverseBytes32(LowDWord);
    HighWord := ReverseBytes16(HighWord);
    Data.Write(HighWord, 2);
    Data.Write(LowDWord, 4);
    Data.Seek(0, spBegin);
    Result := True;
end;

function TMP4AtomData.SetAsInteger64(Value: UInt64): Boolean;
var
    LowDWord: DWord;
    HighDWord: DWord;
begin
    LowDWord := LowDWordOfUInt64(Value);
    HighDWord := HighDWordOfUInt64(Value);
    Result := SetAsInteger64(LowDWord, HighDWord);
end;

function TMP4AtomData.SetAsInteger64(LowDWord, HighDWord: DWord): Boolean;
var
    DataLE: UInt64;
    Value: UInt64;
begin
    DataType := 0;
    Data.Capacity:=0;
    Value := HighDWord;
    Value := Value SHL 32;
    Value := Value OR LowDWord;
    DataLE := ReverseBytes64(Value);
    Data.Write(DataLE, 8);
    Data.Seek(0, spBegin);
    Result := True;
end;

function TMP4AtomData.SetAsBool(Value: Boolean): Boolean;
var
    DataByte: Byte;
begin
    DataByte := Byte(Value);
    Result := SetAsInteger8(DataByte);
end;

procedure TMP4AtomData.Clear;
begin
    DataType := 0;
    Data.Capacity:=0;
end;

function TMP4AtomData.Delete: Boolean;
begin
    Result := Parent.DeleteData(Self.Index);
end;

function TMP4AtomData.Assign(MP4AtomData: PMP4AtomData): Boolean;
begin
    Clear;
    if MP4AtomData <> nil then begin
        DataType := MP4AtomData.DataType;
        Reserved := MP4AtomData.Reserved;
        MP4AtomData.Data.Seek(0, spBegin);
        Stream2Stream(Data, MP4AtomData.Data, MP4AtomData.Data.Size);
        //Data.CopyFrom(MP4AtomData.Data, MP4AtomData.Data.Size);
        MP4AtomData.Data.Seek(0, spBegin);
    end;
    Result := True;
end;

function TMP4AtomData.Write(MP4Stream: PStream): Boolean;
var
    AtomSize: DWord;
    AtomSizeLE: DWord;
    DataTypeLE: DWord;
begin
    Result := False;
    try
        //if Data.Size > 0 then begin
            AtomSize := Data.Size + 16;
            AtomSizeLE := ReverseBytes32(AtomSize);
            MP4Stream.Write(AtomSizeLE, 4);
            MP4Stream.Write(MP4AtomDataID, 4);
            DataTypeLE := ReverseBytes32(DataType);
            MP4Stream.Write(DataTypeLE, 4);
            MP4Stream.Write(Reserved, 4);
            Data.Seek(0, spBegin);
            Stream2Stream(MP4Stream, Data, Data.Size);
//            MP4Stream.CopyFrom(Data, Data.Size);
            Data.Seek(0, spBegin);
            Result := True;
        //end;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

Constructor TMP4Atom.Create;
begin
    Inherited;
    New(mean, Create);
    New(name, Create);
end;

Destructor TMP4Atom.Destroy;
begin
    mean.Free;
    name.Free;
    mean:=nil;
    name:=nil;
    FillChar(ID, SizeOf(ID), 0);
    Inherited;
end;

function TMP4Atom.GetAsText: String;
var
    Value: UInt64;
begin
    Result := '';
    if Datas[0].DataType = 1 then begin
        Result := Datas[0].GetAsText;
    end else begin
        if Datas[0].GetAsInteger(Value) then begin
            Result := Int2Str(Value);
        end;
    end;
end;

function TMP4Atom.GetAsInteger(out Value: UInt64): Boolean;
begin
    Result := Datas[0].GetAsInteger(Value);
end;

function TMP4Atom.GetAsInteger8: Byte;
begin
    Result := Datas[0].GetAsInteger8;
end;

function TMP4Atom.GetAsList(List: PStrList): Boolean;
begin
    Result := Datas[0].GetAsList(List);
end;

function TMP4Atom.GetAsInteger16: Word;
begin
    Result := Datas[0].GetAsInteger16;
end;

function TMP4Atom.GetAsInteger32: DWord;
begin
    Result := Datas[0].GetAsInteger32;
end;

function TMP4Atom.GetAsInteger48(var LowDWord: DWord; HiWord: Word; out Value: UInt64): Boolean;
begin
    Result := Datas[0].GetAsInteger48(LowDWord, HiWord, Value);
end;

function TMP4Atom.GetAsInteger64(var LowDWord, HiDWord: DWord; out Value: UInt64): Boolean;
begin
    Result := Datas[0].GetAsInteger64(LowDWord, HiDWord, Value);
end;

function TMP4Atom.GetAsBool: Boolean;
begin
    Result := Datas[0].GetAsBool;
end;

function TMP4Atom.SetAsText(Text: String): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsText(Text);
end;

function TMP4Atom.SetAsInteger8(Value: Byte): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger8(Value);
end;

function TMP4Atom.SetAsList(List: PStrList): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsList(List);
end;

function TMP4Atom.SetAsInteger16(Value: Word): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger16(Value);
end;

function TMP4Atom.SetAsInteger32(Value: DWord): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger32(Value);
end;

function TMP4Atom.SetAsInteger48(Value: UInt64): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger48(Value);
end;

function TMP4Atom.SetAsInteger48(LowDWord: DWord; HiWord: Word): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger48(LowDWord, HiWord);
end;

function TMP4Atom.SetAsInteger64(Value: UInt64): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger64(Value);
end;

function TMP4Atom.SetAsInteger64(LowDWord, HiDWord: DWord): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsInteger64(LowDWord, HiDWord);
end;

function TMP4Atom.SetAsBool(Value: Boolean): Boolean;
begin
    if Count = 0 then begin
        AddData;
    end;
    Result := Datas[0].SetAsBool(Value);
end;

function TMP4Atom.GetAsCommonText(var _name: String; var _mean: String): String;
begin
    _name := Self.name.GetAsText;
    _mean := Self.mean.GetAsText;
    Result := Self.GetAsText;
end;

function TMP4Atom.SetAsCommonText(_name: String; _mean: String; Value: String): Boolean;
begin
    Result := Self.name.SetAsText(_name)
        AND Self.mean.SetAsText(_mean)
        AND Self.SetAsText(Value);
end;

function TMP4Atom.AddData: PMP4AtomData;
begin
    Result := nil;
    try
        SetLength(Datas, Length(Datas) + 1);
        New(Datas[Length(Datas) - 1], Create);
//        Datas[Length(Datas) - 1] := NewTMP4AtomData.Create;
        Datas[Length(Datas) - 1].Parent := @Self;
        Datas[Length(Datas) - 1].Index := Length(Datas) - 1;
        Result := Datas[Length(Datas) - 1];
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function TMP4Atom.Count: Integer;
begin
    Result := Length(Datas);
end;

procedure TMP4Atom.Clear;
var
    i: Integer;
begin
    for i := 0 to Length(Datas) - 1 do begin
        Datas[i].Clear;
        Datas[i].Free;
        Datas[i]:=nil;
//        FreeAndNil(Datas[i]);
    end;
    SetLength(Datas, 0);
    mean.Clear;
    name.Clear;
end;

function TMP4Atom.CalculateAtomSize: Cardinal;
var
    i: Integer;
begin
    Result := 0;
    if mean.Data.Size > 0 then begin
        Result := Result + mean.Data.Size + 8;
    end;
    if name.Data.Size > 0 then begin
        Result := Result + name.Data.Size + 8;
    end;
    for i := 0 to Length(Datas) - 1 do begin
        //if Datas[i].Data.Size > 0 then begin
            Result := Result + Datas[i].Data.Size + 16;
        //end;
    end;
    Result := Result + 8;
end;

function TMP4Atom.Write(MP4Stream: PStream): Boolean;
var
    AtomSizeLE: DWord;
    i: Integer;
begin
    Result := False;
    try
        AtomSizeLE := ReverseBytes32(CalculateAtomSize);
        //if AtomSizeLE > 0 then begin
            MP4Stream.Write(AtomSizeLE, 4);
            MP4Stream.Write(ID, 4);
            if mean.Data.Size > 0 then begin
                mean.Write(MP4Stream);
            end;
            if name.Data.Size > 0 then begin
                name.Write(MP4Stream);
            end;
            for i := 0 to Count - 1 do begin
                Datas[i].Write(MP4Stream);
            end;
            Result := True;
        //end;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

procedure TMP4Atom.Delete;
begin
    Parent.DeleteAtom(Self.Index);
end;

function TMP4Atom.DeleteData(AtomIndex: Integer): Boolean;
begin
    Result := False;
    if (AtomIndex >= Length(Datas))
    OR (AtomIndex < 0)
    then begin
        Exit;
    end;
//    FreeAndNil(Datas[AtomIndex]);
    Datas[AtomIndex].Free;
    Datas[AtomIndex]:=nil;
    CompactAtomDataList;
    Result := True;
end;

function TMP4Atom.Deletemean: Boolean;
begin
    mean.Clear;
    Result := True;
end;

function TMP4Atom.Deletename: Boolean;
begin
    name.Clear;
    Result := True;
end;

procedure TMP4Atom.CompactAtomDataList;
var
    i: Integer;
    Compacted: Boolean;
begin
    Compacted := False;
    if Datas[Length(Datas) - 1]  = nil then begin
        Compacted := True;
    end else begin
        for i := 0 to Length(Datas) - 2 do begin
            if Datas[i] = nil then begin
                Datas[i] := Datas[i + 1];
                Datas[i].Index := i;
                Datas[i + 1] := nil;
                Compacted := True;
            end;
        end;
    end;
    if Compacted then begin
        SetLength(Datas, Length(Datas) - 1);
    end;
end;

function TMP4Atom.Assign(MP4Atom: PMP4Atom): Boolean;
var
    i: Integer;
begin
    Clear;
    if MP4Atom <> nil then begin
        ID := MP4Atom.ID;
        Flags := MP4Atom.Flags;
        for i := 0 to MP4Atom.Count - 1 do begin
            AddData.Assign(MP4Atom.Datas[i]);
        end;
        mean.Assign(MP4Atom.mean);
        name.Assign(MP4Atom.name);
    end;
    Result := True;
end;

Constructor TMP4Tag.Create;
begin
    Inherited;
    Clear;
    PaddingToWrite := MP4TagLibraryDefaultPaddingSize;
end;

Destructor TMP4Tag.Destroy;
begin
    Clear;
    Inherited;
end;

function TMP4Tag.LoadFromFile(MP4FileName: String): Integer;
var
    MP4Stream: PStream;
//    BufferedStream: TBufferedStream;
begin
    Clear;
    FLoaded := False;
    Self.FileName := MP4FileName;
    try
        MP4Stream := NewReadFileStreamW(MP4FileName);
//        TFileStream.Create(MP4FileName, fmOpenRead OR fmShareDenyWrite);
    except
        Result := MP4TAGLIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;

    try
        Result := LoadFromStream(MP4Stream);
    finally
        MP4Stream.Free;
    end;

    {
    BufferedStream := TBufferedStream.Create(MP4Stream);
    try
        BufferedStream.Seek(0, spBegin);
        Result := LoadFromStream(BufferedStream);
    finally
        FreeAndNil(BufferedStream);
        FreeAndNil(MP4Stream);
    end;
    }
end;

procedure TMP4Tag._GetIvanParameters (MP4Stream : PStream);
var aWidth, aHeight : Word;
    MP4Stream_Size  : Int64;
    AtomName        : TAtomName;
    AtomSize        : Int64;
    Is64BitAtomSize : boolean;

   procedure SkipAtom;
   begin
      if Is64BitAtomSize then MP4Stream.Seek (AtomSize - 16, spCurrent)
      else                    MP4Stream.Seek (AtomSize -  8, spCurrent);
   end;

   procedure ParseTrakForVideoParams;
   begin
      repeat
         ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
         if IsSameAtomName(AtomName, 'tkhd') then
         begin
            MP4Stream.Seek (76, spCurrent);
            MP4Stream.Read (aWidth,  2);
            aWidth := aWidth shl 8 or aWidth shr 8;
            MP4Stream.Read (aHeight, 2);
            MP4Stream.Read (aHeight, 2);
            aHeight := aHeight shl 8 or aHeight shr 8;
            if (FVideoWidth<=aWidth) and (FVideoHeight<=aHeight) then
            begin
               FVideoWidth  := aWidth;
               FVideoHeight := aHeight;
            end;
            Exit;
         end
         else SkipAtom;
      until MP4Stream.Position >= MP4Stream_Size;
   end;

   procedure ParseTrakForAudioParams;
   var _NumberOfDescriptions : Cardinal;
       _AudioChannels        : Word;
       _SampleSize           : Word;
       _SampleRate           : Cardinal;
   begin
      _NumberOfDescriptions := 0;
      _AudioChannels := 0;
      _SampleSize := 0;
      _SampleRate := 0;
      repeat
         ReadAtomHeader (MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
         if IsSameAtomName (AtomName, 'mdia') then
         begin
            repeat
               ReadAtomHeader (MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
               if IsSameAtomName (AtomName, 'minf') then
               begin
                  repeat
                     ReadAtomHeader (MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                     if IsSameAtomName (AtomName, 'stbl') then
                     begin
                        repeat
                           ReadAtomHeader (MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                           if IsSameAtomName (AtomName, 'stsd') then
                           begin
                              MP4Stream.Seek (4, spCurrent);
                              MP4Stream.Read (_NumberOfDescriptions, 4);
                              _NumberOfDescriptions := ReverseBytes32(_NumberOfDescriptions);
                              if _NumberOfDescriptions=1 then
                              begin
                                 repeat
                                    ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                    if IsSameAtomName(AtomName, 'mp4a') then
                                    begin
                                       MP4Stream.Seek($10, spCurrent);
                                       MP4Stream.Read(_AudioChannels, 2);
                                       FAudioChannelCount:= ReverseBytes16(_AudioChannels);
                                       MP4Stream.Read(_SampleSize, 2);
                                       FAudioResolution := ReverseBytes16(_SampleSize);
                                       MP4Stream.Seek(2, spCurrent);
                                       MP4Stream.Read(_SampleRate, 4);
                                       FAudioSampleRate := ReverseBytes32(_SampleRate);
                                       FAudioFormat := mp4afAAC;
                                       Exit;
                                    end else
                                    if IsSameAtomName(AtomName, 'alac') then
                                    begin
                                       MP4Stream.Seek($10, spCurrent);
                                       MP4Stream.Read(_AudioChannels, 2);
                                       FAudioChannelCount:= ReverseBytes16(_AudioChannels);
                                       MP4Stream.Read(_SampleSize, 2);
                                       FAudioResolution := ReverseBytes16(_SampleSize);
                                       MP4Stream.Seek($28, spCurrent);
                                       MP4Stream.Read(_SampleRate, 4);
                                       FAudioSampleRate := ReverseBytes32(_SampleRate);
                                       FAudioFormat := mp4afALAC;
                                       Exit;
                                    end else
                                    if IsSameAtomName(AtomName, 'ac-3') then
                                    begin
                                       MP4Stream.Seek($10, spCurrent);
                                       MP4Stream.Read(_AudioChannels, 2);
                                       FAudioChannelCount:= ReverseBytes16(_AudioChannels);
                                       MP4Stream.Read(_SampleSize, 2);
                                       FAudioResolution := ReverseBytes16(_SampleSize);
                                       MP4Stream.Seek($2, spCurrent);
                                       MP4Stream.Read(_SampleRate, 4);
                                       FAudioSampleRate := ReverseBytes32(_SampleRate);
                                       FAudioFormat := mpfafAC3;
                                       Exit;
                                    end
                                    else SkipAtom;
                                 until (MP4Stream.Position >= MP4Stream_Size) OR (MP4Stream.Position + AtomSize >= MP4Stream_Size);
                              end;
                           end
                           else SkipAtom;
                        until (MP4Stream.Position >= MP4Stream_Size) OR (MP4Stream.Position + AtomSize >= MP4Stream_Size);
                     end
                     else SkipAtom;
                  until (MP4Stream.Position >= MP4Stream_Size) OR (MP4Stream.Position + AtomSize >= MP4Stream_Size);
               end
               else SkipAtom;
            until (MP4Stream.Position >= MP4Stream_Size) OR (MP4Stream.Position + AtomSize >= MP4Stream_Size);
         end
         else SkipAtom;
      until MP4Stream.Position >= MP4Stream_Size;
   end;

var SavedPos           : Int64;
    oldAtomSize        : Int64;
    oldIs64BitAtomSize : boolean;

   procedure SaveState;
   begin
      oldAtomSize        := AtomSize;
      oldIs64BitAtomSize := Is64BitAtomSize;
      SavedPos           := MP4Stream.Position;
   end;

   procedure RestoreState;
   begin
      MP4Stream.Position := SavedPos;
      AtomSize           := oldAtomSize;
      Is64BitAtomSize    := oldIs64BitAtomSize;
   end;

begin
   FVideoWidth     := 0;
   FVideoHeight    := 0;
   MP4Stream_Size := MP4Stream.Size;
   try
      repeat
         ReadAtomHeader (MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
         if IsSameAtomName (AtomName, 'moov') then
         begin
            repeat
               ReadAtomHeader (MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
               if IsSameAtomName(AtomName, 'trak') then
               begin
                  SaveState;
                  ParseTrakForVideoParams; // Parse the trak for video params.
                  RestoreState;            // Restore state to parse for audio params.
                  ParseTrakForAudioParams; // Parse the trak for video params.
                  RestoreState;            // Restore state to skip correctly this trak and check the next one if exists.
               end;
               SkipAtom;
            until (MP4Stream.Position >= MP4Stream_Size)
            OR (AtomSize = 0)
            OR ((AtomName[0] = 0) AND (AtomName[1] = 0) AND (AtomName[2] = 0) AND (AtomName[3] = 0));
         end
         else SkipAtom;
      until (MP4Stream.Position >= MP4Stream_Size) OR (AtomSize <= 0);
   except
   end;
end;

function TMP4Tag.LoadFromStream(MP4Stream: PStream): Integer;
var
    AtomName: TAtomName;
    AtomSize: Int64;
    ilstAtomSize: Int64;
    ilstAtomPosition: Int64;
    NewAtom: PMP4Atom;
    moovAtomSize: Int64;
    Is64BitAtomSize: Boolean;
    moovOffset: Int64;
begin
    Clear;
    FLoaded := False;
    Self.FileName := '';
    try
        Result := MP4TAGLIBRARY_ERROR_NO_TAG_FOUND;
        try
            ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize, False);
        except
            //* Will except if not an MP4 file and MP4TagLibraryFailOnCorruptFile is True
        end;
        if NOT IsSameAtomName(AtomName, 'ftyp') then begin
            Result := MP4TAGLIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
            Exit;
        end;
        FFileSize := MP4Stream.Size;
        //* Get mdat atom size
        GetmdatAtom(MP4Stream, FAtommdatPosition, FAtommdatSize);
        //* Get play time
        FPlaytime := GetPlayTime(MP4Stream);
{$IFNDEF IVAN_LLANAS}
        // No. This function assumes that the first trak atom in the header
        // always contains the audio info in an internal mp4a atom. Error.
        // Often the first trak atom is the video one.
        //* Get audio attributes
        //GetAudioAttributes(Self, MP4Stream);
{$ENDIF}
        if FPlayTime <> 0 then begin
            FBitRate := Trunc((FAtommdatSize / FPlayTime / 125) + 0.5);
        end;
        //* Continue loading
        MP4Stream.Seek(AtomSize - 8, spCurrent);
        moovOffset := MP4Stream.Position;
        _GetIvanParameters (MP4Stream); // Get Video parameters. Get Audio parameters correctly.
        MP4Stream.Position := moovOffset;
        // Continue gathering the atoms for the file metadata.
        repeat
            ReadAtomHeader(MP4Stream, AtomName, moovAtomSize, Is64BitAtomSize);
            if IsSameAtomName(AtomName, 'moov') then begin
                if Is64BitAtomSize then begin
                    FAtommoovPosition := MP4Stream.Position - 16;
                end else begin
                    FAtommoovPosition := MP4Stream.Position - 8;
                end;
                FAtommoovSize := moovAtomSize;
                repeat
                    ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                    if IsSameAtomName(AtomName, 'udta') then begin
                        if Is64BitAtomSize then begin
                            FAtomudtaPosition := MP4Stream.Position - 16;
                        end else begin
                            FAtomudtaPosition := MP4Stream.Position - 8;
                        end;
                        FAtomudtaSize := AtomSize;
                        repeat
                            ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                            if IsSameAtomName(AtomName, 'meta') then begin
                                if Is64BitAtomSize then begin
                                    FAtommetaPosition := MP4Stream.Position - 16;
                                end else begin
                                    FAtommetaPosition := MP4Stream.Position - 8;
                                end;
                                FAtommetaSize := AtomSize;
                                MP4Stream.Read(Version, 1);
                                MP4Stream.Read(Flags, 3);
                                repeat
                                    ReadAtomHeader(MP4Stream, AtomName, ilstAtomSize, Is64BitAtomSize);
                                    if IsSameAtomName(AtomName, 'ilst') then begin
                                        if Is64BitAtomSize then begin
                                            ilstAtomPosition := MP4Stream.Position - 16;
                                            FAtomilstPosition := MP4Stream.Position - 16;
                                        end else begin
                                            FAtomilstPosition := MP4Stream.Position - 8;
                                            ilstAtomPosition := MP4Stream.Position - 8;
                                        end;
                                        FAtomilstSize := ilstAtomSize;
                                        while MP4Stream.Position < ilstAtomPosition + ilstAtomSize do begin
                                            NewAtom := AddAtom('');
                                            ReadAtom(MP4Stream, NewAtom);
                                            Result := MP4TAGLIBRARY_SUCCESS;
                                            FLoaded := True;
                                        end;
                                        //Break;
                                        Exit;
                                    end else begin
                                        if Is64BitAtomSize then begin
                                            MP4Stream.Seek(ilstAtomSize - 16, spCurrent);
                                        end else begin
                                            MP4Stream.Seek(ilstAtomSize - 8, spCurrent);
                                        end;
                                    end;
                                until (MP4Stream.Position >= MP4Stream.Size)
                                OR (MP4Stream.Position + ilstAtomSize >= MP4Stream.Size);
                            end else begin
                                if Is64BitAtomSize then begin
                                    MP4Stream.Seek(AtomSize - 16, spCurrent);
                                end else begin
                                    MP4Stream.Seek(AtomSize - 8, spCurrent);
                                end;
                            end;
                        until MP4Stream.Position >= MP4Stream.Size;
                    end else begin
                        if Is64BitAtomSize then begin
                            MP4Stream.Seek(AtomSize - 16, spCurrent);
                        end else begin
                            MP4Stream.Seek(AtomSize - 8, spCurrent);
                        end;
                    end;
                until MP4Stream.Position >= MP4Stream.Size;
            end else begin
                if Is64BitAtomSize then begin
                    MP4Stream.Seek(moovAtomSize - 16, spCurrent);
                end else begin
                    MP4Stream.Seek(moovAtomSize - 8, spCurrent);
                end;
            end;
        until (MP4Stream.Position >= MP4Stream.Size)
        OR (moovAtomSize = 0);
    except
        Result := MP4TAGLIBRARY_ERROR_READING_FILE
    end;
end;

function ReadAtomHeader(MP4Stream: PStream; var AtomName: TAtomName; var AtomSize: Int64; var Is64BitSize: Boolean; FailOnCurrupt: Boolean = True): Boolean;
var
    AtomSize32LE: DWord;
begin
    Result := False;
    if MP4Stream.Position >= MP4Stream.Size then begin
        Exit;
    end;
    Is64BitSize := False;
    FillChar(AtomName, SizeOf(AtomName), 0);
    AtomSize32LE := 0;
    AtomSize := 0;
    MP4Stream.Read(AtomSize32LE, 4);
    MP4Stream.Read(AtomName, 4);
    AtomSize := ReverseBytes32(AtomSize32LE);
    //* 64 bit
    if AtomSize = 1 then begin
        MP4Stream.Read(AtomSize, 8);
        AtomSize := ReverseBytes64(AtomSize);
        Is64BitSize := True;
    end;
    if FailOnCurrupt
    OR MP4TagLibraryFailOnCorruptFile
    then begin
        if (AtomSize < 8)
        OR (AtomSize > MP4Stream.Size - MP4Stream.Position + 8)
        then begin
            //raise Exception.Create('Corrupted MP4 file. Atom name: ' + AtomNameToString(AtomName));
        end;
    end;
    Result := True;
end;

function WriteAtomHeader(MP4Stream: PStream; AtomName: TAtomName; AtomSize: Int64): Boolean;
var
    AtomSize32: DWord;
    AtomSize32LE: DWord;
    AtomSize64: UInt64;
begin
    Result := False;
    try
        //* 32 bit
        if AtomSize <= High(Cardinal) then begin
            AtomSize32 := Cardinal(AtomSize);
            AtomSize32LE := ReverseBytes32(AtomSize32);
            MP4Stream.Write(AtomSize32LE, 4);
            MP4Stream.Write(AtomName, 4);
        //* 64 bit
        end else begin
            AtomSize32LE := ReverseBytes32(1);
            MP4Stream.Write(AtomSize32LE, 4);
            MP4Stream.Write(AtomName, 4);
            AtomSize64 := ReverseBytes64(AtomSize);
            MP4Stream.Write(AtomSize64, 8);
        end;
        Result := True;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function WriteAtomHeader(MP4Stream: PStream; AtomName: String; AtomSize: Int64): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := WriteAtomHeader(MP4Stream, AtomID, AtomSize);
end;

function TMP4Tag.ReadAtom(MP4Stream: PStream; var MP4Atom: PMP4Atom): Boolean;
var
    AtomSize: DWord;
    AtomName: TAtomName;
    AtomData: PMP4AtomData;
    AtomPosition: Int64;
begin
    Result := False;
    try
        MP4Stream.Read(AtomSize, 4);
        MP4Stream.Read(AtomName, 4);
        MP4Atom.Size := ReverseBytes32(AtomSize);
        MP4Atom.ID := AtomName;
        AtomPosition := MP4Stream.Position - 8;
        while MP4Stream.Position < AtomPosition + MP4Atom.Size do begin
            MP4Stream.Read(AtomSize, 4);
            MP4Stream.Read(AtomName, 4);
            AtomSize := ReverseBytes32(AtomSize);
            if IsSameAtomName(AtomName, 'mean') then begin
                if AtomSize > 8 then begin
                    Stream2Stream(MP4Atom.mean.Data, MP4Stream, AtomSize - 8);
//                    MP4Atom.mean.Data.CopyFrom(MP4Stream, AtomSize - 8);
                end;
            end else begin
                if IsSameAtomName(AtomName, 'name') then begin
                    if AtomSize > 8 then begin
                      Stream2Stream(MP4Atom.name.Data, MP4Stream, AtomSize - 8);
//                        MP4Atom.name.Data.CopyFrom(MP4Stream, AtomSize - 8);
                    end;
                end else begin
                    if IsSameAtomName(AtomName, 'data') then begin
                        MP4Stream.Seek(- 8, spCurrent);
                        AtomData := MP4Atom.AddData;
                        ReadAtomData(MP4Stream, AtomData);
                    end else begin
                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                    end;
                end;
            end;
            Result := True;
        end;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function TMP4Tag.ReadAtomData(MP4Stream: PStream; var MP4AtomData: PMP4AtomData): Boolean;
var
    AtomSize: DWord;
    AtomName: TAtomName;
    DataType: DWord;
begin
    Result := False;
    try
        MP4Stream.Read(AtomSize, 4);
        MP4Stream.Read(AtomName, 4);
        AtomSize := ReverseBytes32(AtomSize);
        if IsSameAtomName(AtomName, 'data') then begin
            MP4Stream.Read(DataType, 4);
            MP4AtomData.DataType := ReverseBytes32(DataType);
            MP4Stream.Read(MP4AtomData.Reserved, 4);
            if AtomSize > 16 then begin
                Stream2Stream(MP4AtomData.Data,MP4Stream, AtomSize - 16);
//                MP4AtomData.Data.CopyFrom(MP4Stream, AtomSize - 16);
                MP4AtomData.Data.Seek(0, spBegin);
            end;
            Result := True;
        end else begin
            MP4Stream.Seek(AtomSize - 16, spCurrent);
        end;
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function TMP4Tag.AddAtom(AtomName: TAtomName): PMP4Atom;
begin
    Result := nil;
    try
        SetLength(Atoms, Length(Atoms) + 1);
        New(Atoms[Length(Atoms) - 1], Create);
        Atoms[Length(Atoms) - 1].ID := AtomName;
        Atoms[Length(Atoms) - 1].Parent := @Self;
        Atoms[Length(Atoms) - 1].Index := Length(Atoms) - 1;
        Result := Atoms[Length(Atoms) - 1];
    except
//        On E: exception do begin
            //*
//        end;
    end;
end;

function TMP4Tag.AddAtom(AtomName: String): PMP4Atom;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := AddAtom(AtomID);
end;

function TMP4Tag.Count: Integer;
begin
    Result := Length(Atoms);
end;

procedure TMP4Tag.Clear;
var
    i: Integer;
begin
    for i := 0 to Length(Atoms) - 1 do begin
        Atoms[i].Clear;
        Atoms[i].Free;
        Atoms[i]:=nil;
//        FreeAndNil(Atoms[i]);
    end;
    SetLength(Atoms, 0);
    Version := 0;
    Flags := 0;
    FLoaded := False;
    FFileSize := 0;
    FAtommdatPosition := 0;
    FAtommdatSize := 0;
    FAtommoovPosition := 0;
    FAtommoovSize := 0;
    FAtomudtaPosition := 0;
    FAtomudtaSize := 0;
    FAtommetaPosition := 0;
    FAtommetaSize := 0;
    FAtomilstPosition := 0;
    FAtomilstSize := 0;
    FAudioChannelCount := 0;
    FAudioResolution := 0;
    FAudioSampleRate := 0;
    FPlaytime := 0;
    FVideoWidth := 0;
    FVideoHeight := 0;
    FAudioFormat := mp4afUnknown;
end;

function TMP4Tag.DeleteAtom(Index: Integer): Boolean;
begin
    Result := False;
    if (Index >= Length(Atoms))
    OR (Index < 0)
    then begin
        Exit;
    end;
//    FreeAndNil(Atoms[Index]);
    Atoms[Index].Free;
    Atoms[Index]:=nil;
    CompactAtomList;
    Result := True;
end;

function TMP4Tag.DeleteAtom(AtomName: TAtomName): Boolean;
var
    Atom: PMP4Atom;
begin
    Result := False;
    Atom := FindAtom(AtomName);
    if Assigned(Atom) then begin
        Atom.Delete;
        Result := True;
    end;
end;

function TMP4Tag.DeleteAtom(AtomName: String): Boolean;
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    Result := DeleteAtom(ID);
end;

function TMP4Tag.DeleteAtomCommon(AtomName: String; _name: String; _mean: String): Boolean;
var
    Atom: PMP4Atom;
begin
    Result := False;
    Atom := FindAtomCommon(AtomName, _name, _mean);
    if Assigned(Atom) then begin
        Atom.Delete;
        Result := True;
    end;
end;

procedure TMP4Tag.CompactAtomList;
var
    i: Integer;
    Compacted: Boolean;
begin
    Compacted := False;
    if Atoms[Length(Atoms) - 1]  = nil then begin
        Compacted := True;
    end else begin
        for i := 0 to Length(Atoms) - 2 do begin
            if Atoms[i] = nil then begin
                Atoms[i] := Atoms[i + 1];
                Atoms[i].Index := i;
                Atoms[i + 1] := nil;
                Compacted := True;
            end;
        end;
    end;
    if Compacted then begin
        SetLength(Atoms, Length(Atoms) - 1);
    end;
end;

function TMP4Tag.CalculateSize: Int64;
var
    i: Integer;
begin
    Result := 0;
    for i := 0 to Count - 1 do begin
        Result := Result + Atoms[i].CalculateAtomSize;
    end;
    if Result > 0  then begin
        Inc(Result, 8);
    end;
    if Result > High(Cardinal) then begin
        Inc(Result, 8);
    end;
end;

function TMP4Tag.SaveToFile(MP4FileName: String; KeepPadding: Boolean = True; UseMemoryForTempDataMaxFileSize: Int64 = 250 * 1024 * 1024 {250 MB}): Integer;
var
    MP4Stream: PStream;
begin
    try
        if NOT WFileExists(MP4FileName) then begin
            MP4Stream := NewWriteFileStream(MP4FileName);
        end else begin
            MP4Stream := NewWriteFileStream(MP4FileName);
        end;
//        BufferedStream := TBufferedStream.Create(MP4Stream);
//        BufferedStream.Seek(0, spBegin);
        try
            if MP4Stream.Size > UseMemoryForTempDataMaxFileSize then begin
                //* Temporary data in memory
                Result := SaveToStream(MP4Stream, KeepPadding, '');
            end else begin
                //* Temporary data to temp files
                Result := SaveToStream(MP4Stream, KeepPadding, MP4FileName);
            end;
        finally
            MP4Stream.Free;
            MP4Stream:=nil;
        end;
    except
        Result := MP4TAGLIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
end;

function TMP4Tag.SaveToStream(MP4Stream: PStream; KeepPadding: Boolean = True; MP4FileName: String = ''): Integer;
var
    //MP4Stream: TFileStream;
    AtomName: TAtomName;
    AtomSize: Int64;
    moovAtomSize: Int64;
    moovAtomPosition: Int64;
    udtaAtomSize: Int64;
    udtaAtomPosition: Int64;
    metaAtomSize: Int64;
    metaAtomPosition: Int64;
    freeAtomSize: Int64;
    i: Integer;
    NewTagSize: Int64;
    StreamRest: PStream;
    moovAtomRest: PStream;
    udtaAtomRest: PStream;
    metaAtomRest: PStream;
    StreamRestFileName: String;
    moovAtomRestFileName: String;
    udtaAtomRestFileName: String;
    metaAtomRestFileName: String;
    mdatPreviousLocation: Int64;
    mdatNewLocation: Int64;
    AvailableSpace: UInt64;
    NeededSpace: UInt64;
    PaddingNeededToWrite: Integer;
    moovProcessingFinished: Boolean;
    Temp: DWord;
    Is64BitAtomSize: Boolean;
    moovIs64BitAtomSize: Boolean;
begin

    //Result := MP4TAGLIBRARY_ERROR;
    NewTagSize := CalculateSize;
    if NewTagSize = 0 then begin
        NewTagSize := 8;
    end;
    FLoaded := False;
    Self.FileName := MP4FileName;
    Flags := 0;
    moovAtomSize := 0;
    moovAtomPosition := 0;
    AvailableSpace := 0;
    moovIs64BitAtomSize := False;
    try
        //* When creating new file add a fake ftyp
        MP4Stream.Seek(0, spBegin);
        if MP4Stream.Size = 0 then begin
            WriteAtomHeader(MP4Stream, 'ftyp', 8);
        end;
        //* Working with file
        if FileName <> '' then begin
            StreamRestFileName := ChangeFileExt(FileName, '.rest.tmp');
            moovAtomRestFileName := ChangeFileExt(FileName, '.moovAtom.tmp');
            udtaAtomRestFileName := ChangeFileExt(FileName, '.udtaAtom.tmp');
            metaAtomRestFileName := ChangeFileExt(FileName, '.metaAtom.tmp');
            StreamRest := NewWriteFileStream(StreamRestFileName);
            moovAtomRest := NewWriteFileStream(moovAtomRestFileName);
            udtaAtomRest := NewWriteFileStream(udtaAtomRestFileName);
            metaAtomRest := NewWriteFileStream(metaAtomRestFileName);
        //* Working in memory
        end else begin
            StreamRest := NewMemoryStream;
            moovAtomRest := NewMemoryStream;
            udtaAtomRest := NewMemoryStream;
            metaAtomRest := NewMemoryStream;
        end;
        MP4Stream.Seek(0, spBegin);
        mdatPreviousLocation := MP4mdatAtomLocation(MP4Stream);
        MP4Stream.Seek(0, spBegin);
        //* Locate moov atom and calc free atoms after it (moovAtomPosition is used for where to write the new tag, if free atom is before moov atom then use that)
        moovProcessingFinished := False;
        repeat
            ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
            if IsSameAtomName(AtomName, 'moov') then begin
                moovAtomSize := AtomSize;
                moovIs64BitAtomSize := Is64BitAtomSize;
                if Is64BitAtomSize then begin
                    Inc(AvailableSpace, AtomSize);
                    if moovAtomPosition = 0 then begin
                        moovAtomPosition := MP4Stream.Position - 16;
                    end;
                end else begin
                    Inc(AvailableSpace, AtomSize);
                    if moovAtomPosition = 0 then begin
                        moovAtomPosition := MP4Stream.Position - 8;
                    end;
                end;
                if moovAtomSize > High(Cardinal) then begin
                    MP4Stream.Seek(moovAtomSize - 16, spCurrent);
                end else begin
                    MP4Stream.Seek(moovAtomSize - 8, spCurrent);
                end;
                if ((AtomSize < 8 + 1) AND (NOT Is64BitAtomSize))
                OR ((AtomSize < 16 + 1) AND (Is64BitAtomSize))
                then begin
                    Continue;
                end;
                repeat
                    ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                    if IsSameAtomName(AtomName, 'free') then begin
                        if Is64BitAtomSize then begin
                            Inc(AvailableSpace, AtomSize);
                            if moovAtomPosition = 0 then begin
                                moovAtomPosition := MP4Stream.Position - 16;
                            end;
                        end else begin
                            Inc(AvailableSpace, AtomSize);
                            if moovAtomPosition = 0 then begin
                                moovAtomPosition := MP4Stream.Position - 8;
                            end;
                        end;
                        if Is64BitAtomSize then begin
                            MP4Stream.Seek(AtomSize - 16, spCurrent);
                        end else begin
                            MP4Stream.Seek(AtomSize - 8, spCurrent);
                        end;
                    end else begin
                        moovProcessingFinished := True;
                    end;
                until NOT IsSameAtomName(AtomName, 'free')
                OR (MP4Stream.Position >= MP4Stream.Size)
                OR moovProcessingFinished;
            end else begin
                if Is64BitAtomSize then begin
                    MP4Stream.Seek(AtomSize - 16, spCurrent);
                end else begin
                    MP4Stream.Seek(AtomSize - 8, spCurrent);
                end;
            end;
        until (MP4Stream.Position >= MP4Stream.Size)
        //OR ((moovAtomPosition > 0) AND (MP4Stream.Position >= moovAtomPosition + moovAtomSize))
        OR moovProcessingFinished
        OR (AtomSize = 0);
        //* Load the content of the moov atom
        if (moovAtomPosition > 0)
        AND (((moovAtomSize > 8 + 1) AND (NOT moovIs64BitAtomSize)) OR ((moovAtomSize > 16 + 1) AND (moovIs64BitAtomSize)))
        then begin
            if moovIs64BitAtomSize then begin
                MP4Stream.Seek(moovAtomPosition + 16, spBegin);
            end else begin
                MP4Stream.Seek(moovAtomPosition + 8, spBegin);
            end;
            //* Process all moov sub-atoms
            repeat
                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                if IsSameAtomName(AtomName, 'udta') then begin
                    udtaAtomSize := AtomSize;
                    if Is64BitAtomSize then begin
                        udtaAtomPosition := MP4Stream.Position - 16;
                    end else begin
                        udtaAtomPosition := MP4Stream.Position - 8;
                    end;
                    if ((AtomSize < 8 + 1) AND (NOT Is64BitAtomSize))
                    OR ((AtomSize < 16 + 1) AND (Is64BitAtomSize))
                    then begin
                        Continue;
                    end;
                    repeat
                        ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                        if IsSameAtomName(AtomName, 'meta') then begin
                            metaAtomSize := AtomSize;
                            if Is64BitAtomSize then begin
                                metaAtomPosition := MP4Stream.Position - 16;
                            end else begin
                                metaAtomPosition := MP4Stream.Position - 8;
                            end;
                            if ((AtomSize >= 8 + 4) AND (NOT Is64BitAtomSize))
                            OR ((AtomSize >= 16 + 4) AND (Is64BitAtomSize))
                            then begin
                                MP4Stream.Read(Temp, 1);
                                MP4Stream.Read(Temp, 3);
                            end;
                            if ((AtomSize <= 8 + 4) AND (NOT Is64BitAtomSize))
                            OR ((AtomSize <= 16 + 4) AND (Is64BitAtomSize))
                            then begin
                                Continue;
                            end;
                            repeat
                                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                if IsSameAtomName(AtomName, 'ilst') then begin
                                    //ilstAtomSize := AtomSize;
                                    if Is64BitAtomSize then begin
                                        MP4Stream.Seek(AtomSize - 16, spCurrent);
                                    end else begin
                                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                                    end;
                                end else if NOT IsSameAtomName(AtomName, 'free') then begin
                                    if Is64BitAtomSize then begin
                                        MP4Stream.Seek(- 16, spCurrent);
                                    end else begin
                                        MP4Stream.Seek(- 8, spCurrent);
                                    end;
                                    Stream2Stream(metaAtomRest, MP4Stream, AtomSize);
                                    //metaAtomRest.CopyFrom(MP4Stream, AtomSize);
                                end else begin
                                    if Is64BitAtomSize then begin
                                        MP4Stream.Seek(AtomSize - 16, spCurrent);
                                    end else begin
                                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                                    end;
                                end;
                            until (MP4Stream.Position >= MP4Stream.Size)
                            OR (MP4Stream.Position >= metaAtomPosition + metaAtomSize);
                        end else if NOT IsSameAtomName(AtomName, 'free') then begin
                            if Is64BitAtomSize then begin
                                MP4Stream.Seek(- 16, spCurrent);
                            end else begin
                                MP4Stream.Seek(- 8, spCurrent);
                            end;
                            Stream2Stream(udtaAtomRest, MP4Stream, AtomSize);
                            //udtaAtomRest.CopyFrom(MP4Stream, AtomSize);
                        end else begin
                            if Is64BitAtomSize then begin
                                MP4Stream.Seek(AtomSize - 16, spCurrent);
                            end else begin
                                MP4Stream.Seek(AtomSize - 8, spCurrent);
                            end;
                        end;
                    until (MP4Stream.Position >= MP4Stream.Size)
                    OR (MP4Stream.Position >= udtaAtomPosition + udtaAtomSize);
                end else if NOT IsSameAtomName(AtomName, 'free') then begin
                    if Is64BitAtomSize then begin
                        MP4Stream.Seek(- 16, spCurrent);
                    end else begin
                        MP4Stream.Seek(- 8, spCurrent);
                    end;
                    Stream2Stream(moovAtomRest, MP4Stream, AtomSize);
                    //moovAtomRest.CopyFrom(MP4Stream, AtomSize);
                end else begin
                    if Is64BitAtomSize then begin
                        MP4Stream.Seek(AtomSize - 16, spCurrent);
                    end else begin
                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                    end;
                end;
            until (MP4Stream.Position >= MP4Stream.Size)
            OR (MP4Stream.Position >= moovAtomPosition + moovAtomSize);
        end;
        //* Calculate needed space
        NeededSpace := NewTagSize;
        //* meta
        if NeededSpace + metaAtomRest.Size + 8 + 4 > High(Cardinal) then begin
            Inc(NeededSpace, metaAtomRest.Size + 16 + 4); //* + 4 bytes for version/flags
        end else begin
            Inc(NeededSpace, metaAtomRest.Size + 8 + 4); //* + 4 bytes for version/flags
        end;
        //* udta
        if NeededSpace + udtaAtomRest.Size + 8 > High(Cardinal) then begin
            Inc(NeededSpace, udtaAtomRest.Size + 16);
        end else begin
            Inc(NeededSpace, udtaAtomRest.Size + 8);
        end;
        //* moov
        if NeededSpace + moovAtomRest.Size + 8 > High(Cardinal) then begin
            Inc(NeededSpace, moovAtomRest.Size + 16);
        end else begin
            Inc(NeededSpace, moovAtomRest.Size + 8);
        end;
        //* Check if tags fit
        if (AvailableSpace = NeededSpace)
        AND KeepPadding
        then begin
            PaddingNeededToWrite := 0;
        //* Fits
        end else if (AvailableSpace > NeededSpace + 8 + 1)
        AND KeepPadding
        then begin
            PaddingNeededToWrite := AvailableSpace - NeededSpace;
        //* Doesn't fit
        end else begin
            PaddingNeededToWrite := Self.PaddingToWrite;
            //* Copy everything after moov atom except free atoms following moov atom
            MP4Stream.Seek(moovAtomPosition + AvailableSpace, spBegin);
            if MP4Stream.Size <> MP4Stream.Position then begin
                Stream2Stream(StreamRest, MP4Stream, MP4Stream.Size - MP4Stream.Position);
                //StreamRest.CopyFrom(MP4Stream, MP4Stream.Size - MP4Stream.Position);
            end;
        end;
        //* Write the new atoms
        if moovAtomPosition <> 0 then begin
            MP4Stream.Seek(moovAtomPosition, spBegin);
        end else begin
            MP4Stream.Seek(0, spEnd);
        end;
        //* Write moov
        if NeededSpace + PaddingNeededToWrite > High(Cardinal) then begin
            WriteAtomHeader(MP4Stream, 'moov', NeededSpace + PaddingNeededToWrite);
        end else begin
            WriteAtomHeader(MP4Stream, 'moov', NeededSpace + PaddingNeededToWrite);
        end;
        Stream2Stream(MP4Stream, moovAtomRest, 0);
//        MP4Stream.CopyFrom(moovAtomRest, 0);
        //* Write udta
        if NeededSpace - moovAtomRest.Size + PaddingNeededToWrite - 8 > High(Cardinal) then begin
            WriteAtomHeader(MP4Stream, 'udta', NeededSpace - moovAtomRest.Size + PaddingNeededToWrite - 16);
        end else begin
            WriteAtomHeader(MP4Stream, 'udta', NeededSpace - moovAtomRest.Size + PaddingNeededToWrite - 8);
        end;
        Stream2Stream(MP4Stream, udtaAtomRest, 0);
//        MP4Stream.CopyFrom(udtaAtomRest, 0);
        //* Write meta
        if NeededSpace - moovAtomRest.Size - udtaAtomRest.Size + PaddingNeededToWrite - 8 - 8 > High(Cardinal) then begin
            WriteAtomHeader(MP4Stream, 'meta', NeededSpace - moovAtomRest.Size - udtaAtomRest.Size + PaddingNeededToWrite - 16 - 16);
        end else begin
            WriteAtomHeader(MP4Stream, 'meta', NeededSpace - moovAtomRest.Size - udtaAtomRest.Size + PaddingNeededToWrite - 8 - 8);
        end;
        //* TODO: Reverse bytes ?
        MP4Stream.Write(Self.Version, 1);
        MP4Stream.Write(Self.Flags, 3);
        Stream2Stream(MP4Stream, metaAtomRest, 0);
        //MP4Stream.CopyFrom(metaAtomRest, 0);
        //* ilst finally
        WriteAtomHeader(MP4Stream, 'ilst', NewTagSize);
        //* Write the new tags
        for i := 0 to Count - 1 do begin
            Atoms[i].Write(MP4Stream);
        end;
        //* Write the padding
        if PaddingNeededToWrite > 0 then begin
            freeAtomSize := PaddingNeededToWrite;
            WriteAtomHeader(MP4Stream, 'free', freeAtomSize);
            if freeAtomSize > High(Cardinal) then begin
                WritePadding(MP4Stream, PaddingNeededToWrite - 16);
            end else begin
                WritePadding(MP4Stream, PaddingNeededToWrite - 8);
            end;
        end;
        //* Copy file rest
        if StreamRest.Size > 0 then begin
            //* Truncate file
            MP4Stream.Size := MP4Stream.Position;
            //* Copy rest
            Stream2Stream(MP4Stream, StreamRest, 0);
            //MP4Stream.CopyFrom(StreamRest, 0);
        end;
        //* Check and update stco/co64 atom
        MP4Stream.Seek(0, spBegin);
        mdatNewLocation := MP4mdatAtomLocation(MP4Stream);
        MP4Stream.Seek(0, spBegin);
        if mdatNewLocation - mdatPreviousLocation <> 0 then begin
            MP4Stream.Seek(0, spBegin);
            if NOT MP4UpdatestcoAtom(MP4Stream, mdatNewLocation - mdatPreviousLocation) then begin
                Result := MP4TAGLIBRARY_ERROR_UPDATE_stco;
                Exit;
            end;
            MP4Stream.Seek(0, spBegin);
            if NOT MP4Updateco64Atom(MP4Stream, mdatNewLocation - mdatPreviousLocation) then begin
                Result := MP4TAGLIBRARY_ERROR_UPDATE_co64;
                Exit;
            end;
        end;
        Result := MP4TAGLIBRARY_SUCCESS;
    finally
        StreamRest.Free;
        moovAtomRest.Free;
        udtaAtomRest.Free;
        metaAtomRest.Free;
        StreamRest:=nil;
        moovAtomRest:=nil;
        udtaAtomRest:=nil;
        metaAtomRest:=nil;
        DeleteFileW(PWideChar(StreamRestFileName));
        DeleteFileW(PWideChar(moovAtomRestFileName));
        DeleteFileW(PWideChar(udtaAtomRestFileName));
        DeleteFileW(PWideChar(metaAtomRestFileName));
    end;

end;

function TMP4Tag.FindAtom(AtomName: TAtomName): PMP4Atom;
var
    i: Integer;
begin
    Result := nil;
    for i := 0 to Count - 1 do begin
        if IsSameAtomName(Atoms[i].ID, AtomName) then begin
            Result := Atoms[i];
            Exit;
        end;
    end;
end;

function TMP4Tag.FindAtom(AtomName: String): PMP4Atom;
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    Result := FindAtom(ID);
end;

function TMP4Tag.FindAtomCommon(AtomName: TAtomName; _name: String; _mean: String): PMP4Atom;
var
    i: Integer;
    _nameValue: String;
    _meanValue: String;
begin
    Result := nil;
    for i := 0 to Count - 1 do begin
        if IsSameAtomName(Atoms[i].ID, AtomName) then begin
            Atoms[i].GetAsCommonText(_nameValue, _meanValue);
            if (AnsiCompareStrNoCase(_nameValue, _name) = 0)
            AND (AnsiCompareStrNoCase(_meanValue, _mean) = 0)
            then begin
                Result := Atoms[i];
                Exit;
            end;
        end;
    end;
end;

function TMP4Tag.FindAtomCommon(AtomName: String; _name: String; _mean: String): PMP4Atom;
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    Result := FindAtomCommon(ID, _name, _mean);
end;

function TMP4Tag.CoverArtCount: Integer;
begin
    Result := 0;
    if Assigned(FindAtom('covr')) then begin
        Result := Length(FindAtom('covr').Datas);
    end;
end;

function TMP4Tag.GetText(AtomName: TAtomName): String;
var
    MP4Atom: PMP4Atom;
begin
    Result := '';
    MP4Atom := FindAtom(AtomName);
    if Assigned(MP4Atom) then begin
        Result := MP4Atom.GetAsText;
    end;
end;

function TMP4Tag.GetText(AtomName: String): String;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetText(AtomID);
end;

function TMP4Tag.GetInteger(AtomName: TAtomName; out Value: UInt64): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    Result := Assigned(MP4Atom) and MP4Atom.GetAsInteger(Value);
end;

function TMP4Tag.GetInteger(AtomName: String; out Value: UInt64): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetInteger(AtomID, Value);
end;

function TMP4Tag.GetInteger8(AtomName: TAtomName): Byte;
var
    MP4Atom: PMP4Atom;
begin
    Result := 0;
    MP4Atom := FindAtom(AtomName);
    if Assigned(MP4Atom) then begin
        Result := MP4Atom.GetAsInteger8;
    end;
end;

function TMP4Tag.GetInteger8(AtomName: String): Byte;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetInteger8(AtomID);
end;

function TMP4Tag.GetList(AtomName: TAtomName; List: PStrList): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    Result := False;
    MP4Atom := FindAtom(AtomName);
    if Assigned(MP4Atom) then begin
        Result := MP4Atom.GetAsList(List);
    end;
end;

function TMP4Tag.GetList(AtomName: String; List: PStrList): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetList(AtomID, List);
end;

function TMP4Tag.GetCommon(_name: String; _mean: String): String;
var
    Atom: PMP4Atom;
begin
    Result := '';
    Atom := FindAtomCommon('----', _name, _mean);
    if Assigned(Atom) then begin
        Result := Atom.GetAsText;
    end;
end;

function TMP4Tag.GetInteger16(AtomName: TAtomName): Word;
var
    MP4Atom: PMP4Atom;
begin
    Result := 0;
    MP4Atom := FindAtom(AtomName);
    if Assigned(MP4Atom) then begin
        Result := MP4Atom.GetAsInteger16;
    end;
end;

function TMP4Tag.GetInteger16(AtomName: String): Word;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetInteger16(AtomID);
end;

function TMP4Tag.GetInteger32(AtomName: TAtomName): DWord;
var
    MP4Atom: PMP4Atom;
begin
    Result := 0;
    MP4Atom := FindAtom(AtomName);
    if Assigned(MP4Atom) then begin
        Result := MP4Atom.GetAsInteger32;
    end;
end;

function TMP4Tag.GetInteger32(AtomName: String): DWord;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetInteger32(AtomID);
end;

function TMP4Tag.GetInteger48(AtomName: TAtomName; var LowDWord: DWord; HiWord: Word; out Value: UInt64): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    Result := Assigned(MP4Atom) and MP4Atom.GetAsInteger48(LowDWord, HiWord, Value);
end;

function TMP4Tag.GetInteger48(AtomName: String; var LowDWord: DWord; HiWord: Word; out Value: UInt64): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetInteger48(AtomID, LowDWord, HiWord, Value);
end;

function TMP4Tag.GetInteger64(AtomName: TAtomName; var LowDWord, HiDWord: DWord; out Value: UInt64): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    Result := Assigned(MP4Atom) and MP4Atom.GetAsInteger64(LowDWord, HiDWord, Value);
end;

function TMP4Tag.GetInteger64(AtomName: String; var LowDWord, HiDWord: DWord; out Value: UInt64): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetInteger64(AtomID, LowDWord, HiDWord, Value);
end;

function TMP4Tag.GetBool(AtomName: TAtomName): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    Result := False;
    MP4Atom := FindAtom(AtomName);
    if Assigned(MP4Atom) then begin
        Result := MP4Atom.GetAsBool;
    end;
end;

function TMP4Tag.GetBool(AtomName: String): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := GetBool(AtomID);
end;

function TMP4Tag.SetText(AtomName: TAtomName; Text: String): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if Text <> '' then begin
        if NOT Assigned(MP4Atom) then begin
            MP4Atom := AddAtom(AtomName);
        end;
        Result := MP4Atom.SetAsText(Text);
    end else begin
        if Assigned(MP4Atom) then begin
            DeleteAtom(MP4Atom.Index);
        end;
        Result := True;
    end;
end;

function TMP4Tag.SetText(AtomName: String; Text: String): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetText(AtomID, Text);
end;

function TMP4Tag.SetInteger8(AtomName: TAtomName; Value: Byte): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger8(Value);
end;

function TMP4Tag.SetInteger8(AtomName: String; Value: Byte): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger8(AtomID, Value);
end;

function TMP4Tag.SetList(AtomName: TAtomName; List: PStrList): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsList(List);
end;

function TMP4Tag.SetList(AtomName: String; List: PStrList): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetList(AtomID, List);
end;

function TMP4Tag.SetCommon(_name: String; _mean: String; Value: String): Boolean;
var
    Atom: PMP4Atom;
begin
    Atom := FindAtomCommon('----', _name, _mean);
    if NOT Assigned(Atom) then begin
        Atom := AddAtom('----');
    end;
    Result := Atom.SetAsCommonText(_name, _mean, Value);
end;

function TMP4Tag.SetInteger16(AtomName: TAtomName; Value: Word): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger16(Value);
end;

function TMP4Tag.SetInteger16(AtomName: String; Value: Word): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger16(AtomID, Value);
end;

function TMP4Tag.SetInteger32(AtomName: TAtomName; Value: DWord): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger32(Value);
end;

function TMP4Tag.SetInteger32(AtomName: String; Value: DWord): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger32(AtomID, Value);
end;

function TMP4Tag.SetInteger48(AtomName: TAtomName; Value: UInt64): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger48(Value);
end;

function TMP4Tag.SetInteger48(AtomName: String; Value: UInt64): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger48(AtomID, Value);
end;

function TMP4Tag.SetInteger48(AtomName: TAtomName; LowDWord: DWord; HighWord: Word): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger48(LowDWord, HighWord);
end;

function TMP4Tag.SetInteger48(AtomName: String; LowDWord: DWord; HighWord: Word): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger48(AtomID, LowDWord, HighWord);
end;

function TMP4Tag.SetInteger64(AtomName: TAtomName; Value: UInt64): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger64(Value);
end;

function TMP4Tag.SetInteger64(AtomName: String; Value: UInt64): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger64(AtomID, Value);
end;

function TMP4Tag.SetInteger64(AtomName: TAtomName; LowDWord, HighDWord: DWord): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsInteger64(LowDWord, HighDWord);
end;

function TMP4Tag.SetInteger64(AtomName: String; LowDWord, HighDWord: DWord): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetInteger64(AtomID, LowDWord, HighDWord);
end;

function TMP4Tag.SetBool(AtomName: TAtomName; Value: Boolean): Boolean;
var
    MP4Atom: PMP4Atom;
begin
    MP4Atom := FindAtom(AtomName);
    if NOT Assigned(MP4Atom) then begin
        MP4Atom := AddAtom(AtomName);
    end;
    Result := MP4Atom.SetAsBool(Value);
end;

function TMP4Tag.SetBool(AtomName: String; Value: Boolean): Boolean;
var
    AtomID: TAtomName;
begin
    StringToAtomName(AtomName, AtomID);
    Result := SetBool(AtomID, Value);
end;

function MediaTypeToStr(Value: Integer): string;
begin
    Result := '';
    case Value of
        0: Result := 'Movie';
        1: Result := 'Music';
        2: Result := 'Audiobook';
        6: Result := 'Music Video';
        9: Result := 'Movie';
        10: Result := 'TV Show';
        11: Result := 'Booklet';
        14: Result := 'Ringtone';
    end;
end;

function TMP4Tag.GetMediaType: String;
begin
    if FindAtom('stik') <> nil then
        Result := MediaTypeToStr(GetInteger16('stik'))
    else
        Result := '';
end;

function TMP4Tag.SetMediaType(Media: String): Boolean;
begin
    Result := False;
    if Media = 'Movie' then begin
        Result := SetInteger16('stik', 9);
    end;
    if Media = 'Music' then begin
        Result := SetInteger16('stik', 1);
    end;
    if Media = 'Audiobook' then begin
        Result := SetInteger16('stik', 2);
    end;
    if Media = 'Music Video' then begin
        Result := SetInteger16('stik', 6);
    end;
    if Media = 'TV Show' then begin
        Result := SetInteger16('stik', 10);
    end;
    if Media = 'Booklet' then begin
        Result := SetInteger16('stik', 11);
    end;
    if Media = 'Ringtone' then begin
        Result := SetInteger16('stik', 14);
    end;
end;

function TMP4Tag.GetTrack: Word;
var
    LowDWord: DWord;
    HighWord: Word;
    Value: UInt64;
begin
    Result := 0;
    LowDWord := 0;
    HighWord := 0;
    if GetInteger48('trkn', LowDWord, HighWord, Value) then begin
        Result := HiWord(LowDWord);
    end;
end;

function TMP4Tag.GetTotalTracks: Word;
var
    LowDWord: DWord;
    HighWord: Word;
    Value: UInt64;
begin
    Result := 0;
    LowDWord := 0;
    HighWord := 0;
    if GetInteger48('trkn', LowDWord, HighWord, Value) then begin
        Result := LoWord(LowDWord);
    end;
end;

function TMP4Tag.GetDisc: Word;
var
    LowDWord: DWord;
    HighWord: Word;
    Value: UInt64;
begin
    Result := 0;
    LowDWord := 0;
    HighWord := 0;
    if GetInteger48('disk', LowDWord, HighWord, Value) then begin
        Result := HiWord(LowDWord);
    end;
end;

function TMP4Tag.GetTotalDiscs: Word;
var
    LowDWord: DWord;
    HighWord: Word;
    Value: UInt64;
begin
    Result := 0;
    LowDWord := 0;
    HighWord := 0;
    if GetInteger48('disk', LowDWord, HighWord, Value) then begin
        Result := LoWord(LowDWord);
    end;
end;

function TMP4Tag.SetTrack(Track: Word; TotalTracks: Word): Boolean;
var
    LowDWord: DWord;
    HighDWord: DWord;
    Atom: PMP4Atom;
begin
    if (Track = 0)
    AND (TotalTracks = 0)
    then begin
        Atom := FindAtom('trkn');
        if Assigned(Atom) then begin
            DeleteAtom(Atom.Index);
        end;
        Result := True;
    end else begin
        LowDWord := TotalTracks SHL 16;
        HighDWord := Track;
        Result := SetInteger64('trkn', LowDWord, HighDWord);
    end;
end;

function TMP4Tag.SetDisc(Disc: Word; TotalDiscs: Word): Boolean;
var
    Value: DWord;
    Atom: PMP4Atom;
begin
    if (Disc = 0)
    AND (TotalDiscs = 0)
    then begin
        Atom := FindAtom('disk');
        if Assigned(Atom) then begin
            DeleteAtom(Atom.Index);
        end;
        Result := True;
    end else begin
        Value := (Disc SHL 16) + TotalDiscs;
        Result := SetInteger48('disk', Value, 0);
    end;
end;

function WritePadding(MP4Stream: PStream; PaddingSize: Integer): Integer;
var
    Data: TBytes;
begin
    try
        SetLength(Data, PaddingSize);
        MP4Stream.Write(Data[0], Length(Data));
        Result := MP4TAGLIBRARY_SUCCESS;
    except
        Result := MP4TAGLIBRARY_ERROR_WRITING_FILE;
    end;
end;

function RemoveMP4TagFromFile(FileName: String; KeepPadding: Boolean): Integer;
var
    MP4Tag: PMP4Tag;
begin
    if NOT FileExists(FileName) then begin
        Result := MP4TAGLIBRARY_ERROR_OPENING_FILE;
        Exit;
    end else begin
        New(MP4Tag, Create);
//        MP4Tag := TMP4Tag.Create;
        try
            Result := MP4Tag.SaveToFile(FileName, KeepPadding);
        finally
            MP4Tag.Free;
            MP4Tag:=nil;
//            FreeAndNil(MP4Tag);
        end;
    end;
end;

function RemoveMP4TagFromStream(Stream: PStream; KeepPadding: Boolean): Integer;
var
    MP4Tag: PMP4Tag;
begin
    if Stream.Size = 0 then begin
        Result := MP4TAGLIBRARY_ERROR_OPENING_FILE;
        Exit;
    end else begin
        New(MP4Tag, Create);
//        MP4Tag := TMP4Tag.Create;
        try
            Result := MP4Tag.SaveToStream(Stream, KeepPadding);
        finally
            MP4Tag.Free;
            MP4Tag:=nil;

//            FreeAndNil(MP4Tag);
        end;
    end;
end;

function MP4mdatAtomLocation(MP4Stream: PStream): Int64;
var
    Size: Int64;
begin
    GetmdatAtom(MP4Stream, Result, Size);
end;

function MP4UpdatestcoAtom(MP4Stream: PStream; Offset: Integer): Boolean;
var
    AtomName: TAtomName;
    //AtomSize: Int64;
    moovAtomSize: Int64;
    moovAtomPosition: Int64;
    trakAtomSize: Int64;
    trakAtomPosition: Int64;
    mdiaAtomSize: Int64;
    mdiaAtomPosition: Int64;
    minfAtomSize: Int64;
    minfAtomPosition: Int64;
    stblAtomSize: Int64;
    stblAtomPosition: Int64;
    stcoAtomSize: Int64;
    stcoAtomPosition: Int64;
    Version: Byte;
    Flags: DWord;
    NumberOfOffsets: Cardinal;
    OffsetValue: Integer;//DWord;
    i: Cardinal;
    moovIs64BitAtomSize: Boolean;
    trakIs64BitAtomSize: Boolean;
    mdiaIs64BitAtomSize: Boolean;
    minfIs64BitAtomSize: Boolean;
    stblIs64BitAtomSize: Boolean;
    stcoIs64BitAtomSize: Boolean;
begin
    Result := True;
    try
        Version := 0;
        Flags := 0;
        NumberOfOffsets := 0;
        repeat
            ReadAtomHeader(MP4Stream, AtomName, moovAtomSize, moovIs64BitAtomSize);
            if IsSameAtomName(AtomName, 'moov') then begin
                if moovIs64BitAtomSize then begin
                    moovAtomPosition := MP4Stream.Position - 16;
                end else begin
                    moovAtomPosition := MP4Stream.Position - 8;
                end;
                repeat
                    ReadAtomHeader(MP4Stream, AtomName, trakAtomSize, trakIs64BitAtomSize);
                    if IsSameAtomName(AtomName, 'trak') then begin
                        if trakIs64BitAtomSize then begin
                            trakAtomPosition := MP4Stream.Position - 16;
                        end else begin
                            trakAtomPosition := MP4Stream.Position - 8;
                        end;
                        repeat
                            ReadAtomHeader(MP4Stream, AtomName, mdiaAtomSize, mdiaIs64BitAtomSize);
                            if IsSameAtomName(AtomName, 'mdia') then begin
                                if mdiaIs64BitAtomSize then begin
                                    mdiaAtomPosition := MP4Stream.Position - 16;
                                end else begin
                                    mdiaAtomPosition := MP4Stream.Position - 8;
                                end;
                                repeat
                                    ReadAtomHeader(MP4Stream, AtomName, minfAtomSize, minfIs64BitAtomSize);
                                    if IsSameAtomName(AtomName, 'minf') then begin
                                        if minfIs64BitAtomSize then begin
                                            minfAtomPosition := MP4Stream.Position - 16;
                                        end else begin
                                            minfAtomPosition := MP4Stream.Position - 8;
                                        end;
                                        repeat
                                            ReadAtomHeader(MP4Stream, AtomName, stblAtomSize, stblIs64BitAtomSize);
                                            if IsSameAtomName(AtomName, 'stbl') then begin
                                                if stblIs64BitAtomSize then begin
                                                    stblAtomPosition := MP4Stream.Position - 16;
                                                end else begin
                                                    stblAtomPosition := MP4Stream.Position - 8;
                                                end;
                                                repeat
                                                    ReadAtomHeader(MP4Stream, AtomName, stcoAtomSize, stcoIs64BitAtomSize);
                                                    if IsSameAtomName(AtomName, 'stco') then begin
                                                        Result := False;
                                                        if stcoIs64BitAtomSize then begin
                                                            stcoAtomPosition := MP4Stream.Position - 16;
                                                        end else begin
                                                            stcoAtomPosition := MP4Stream.Position - 8;
                                                        end;
                                                        MP4Stream.Read(Version, 1);
                                                        MP4Stream.Read(Flags, 3);
                                                        MP4Stream.Read(NumberOfOffsets, 4);
                                                        NumberOfOffsets := ReverseBytes32(NumberOfOffsets);
                                                        i := 0;
                                                        while MP4Stream.Position < stcoAtomPosition + stcoAtomSize do begin
                                                            MP4Stream.Read(OffsetValue, 4);
                                                            OffsetValue := ReverseBytes32(OffsetValue);
                                                            OffsetValue := OffsetValue + Offset;
                                                            OffsetValue := ReverseBytes32(OffsetValue);
                                                            MP4Stream.Seek(- 4, spCurrent);
                                                            MP4Stream.Write(OffsetValue, 4);
                                                            Inc(i);
                                                        end;
                                                        if i = NumberOfOffsets then begin
                                                            Result := True;
                                                        end;
                                                    end else begin
                                                        if stcoIs64BitAtomSize then begin
                                                            MP4Stream.Seek(stcoAtomSize - 16, spCurrent);
                                                        end else begin
                                                            MP4Stream.Seek(stcoAtomSize - 8, spCurrent);
                                                        end;
                                                    end;
                                                 until (MP4Stream.Position >= MP4Stream.Size)
                                                OR (MP4Stream.Position >= stblAtomPosition + stblAtomSize);
                                            end else begin
                                                if stblIs64BitAtomSize then begin
                                                    MP4Stream.Seek(stblAtomSize - 16, spCurrent);
                                                end else begin
                                                    MP4Stream.Seek(stblAtomSize - 8, spCurrent);
                                                end;
                                            end;
                                        until (MP4Stream.Position >= MP4Stream.Size)
                                        OR (MP4Stream.Position >= minfAtomPosition + minfAtomSize);
                                    end else begin
                                        if minfIs64BitAtomSize then begin
                                            MP4Stream.Seek(minfAtomSize - 16, spCurrent);
                                        end else begin
                                            MP4Stream.Seek(minfAtomSize - 8, spCurrent);
                                        end;
                                    end;
                                until (MP4Stream.Position >= MP4Stream.Size)
                                OR (MP4Stream.Position >= mdiaAtomPosition + mdiaAtomSize);
                            end else begin
                                if mdiaIs64BitAtomSize then begin
                                    MP4Stream.Seek(mdiaAtomSize - 16, spCurrent);
                                end else begin
                                    MP4Stream.Seek(mdiaAtomSize - 8, spCurrent);
                                end;
                            end;
                        until (MP4Stream.Position >= MP4Stream.Size)
                        OR (MP4Stream.Position >= trakAtomPosition + trakAtomSize);
                    end else begin
                        if trakIs64BitAtomSize then begin
                            MP4Stream.Seek(trakAtomSize - 16, spCurrent);
                        end else begin
                            MP4Stream.Seek(trakAtomSize - 8, spCurrent);
                        end;
                    end;
                until (MP4Stream.Position >= MP4Stream.Size)
                OR (MP4Stream.Position >= moovAtomPosition + moovAtomSize);
            end else begin
                if moovIs64BitAtomSize then begin
                    MP4Stream.Seek(moovAtomSize - 16, spCurrent);
                end else begin
                    MP4Stream.Seek(moovAtomSize - 8, spCurrent);
                end;
            end;
        until (MP4Stream.Position >= MP4Stream.Size)
        OR (moovAtomSize = 0);
    except
        Result := False;
    end;
end;

function MP4Updateco64Atom(MP4Stream: PStream; Offset: Int64): Boolean;
var
    AtomName: TAtomName;
    //AtomSize: Int64;
    moovAtomSize: Int64;
    moovAtomPosition: Int64;
    trakAtomSize: Int64;
    trakAtomPosition: Int64;
    mdiaAtomSize: Int64;
    mdiaAtomPosition: Int64;
    minfAtomSize: Int64;
    minfAtomPosition: Int64;
    stblAtomSize: Int64;
    stblAtomPosition: Int64;
    co64AtomSize: Int64;
    co64AtomPosition: Int64;
    Version: Byte;
    Flags: DWord;
    NumberOfOffsets: Cardinal;
    OffsetValue: UInt64;//DWord;
    i: Cardinal;
    moovIs64BitAtomSize: Boolean;
    trakIs64BitAtomSize: Boolean;
    mdiaIs64BitAtomSize: Boolean;
    minfIs64BitAtomSize: Boolean;
    stblIs64BitAtomSize: Boolean;
    co64Is64BitAtomSize: Boolean;
begin
    Result := True;
    try
        Version := 0;
        Flags := 0;
        NumberOfOffsets := 0;
        repeat
            ReadAtomHeader(MP4Stream, AtomName, moovAtomSize, moovIs64BitAtomSize);
            if IsSameAtomName(AtomName, 'moov') then begin
                if moovIs64BitAtomSize then begin
                    moovAtomPosition := MP4Stream.Position - 16;
                end else begin
                    moovAtomPosition := MP4Stream.Position - 8;
                end;
                repeat
                    ReadAtomHeader(MP4Stream, AtomName, trakAtomSize, trakIs64BitAtomSize);
                    if IsSameAtomName(AtomName, 'trak') then begin
                        if trakIs64BitAtomSize then begin
                            trakAtomPosition := MP4Stream.Position - 16;
                        end else begin
                            trakAtomPosition := MP4Stream.Position - 8;
                        end;
                        repeat
                            ReadAtomHeader(MP4Stream, AtomName, mdiaAtomSize, mdiaIs64BitAtomSize);
                            if IsSameAtomName(AtomName, 'mdia') then begin
                                if mdiaIs64BitAtomSize then begin
                                    mdiaAtomPosition := MP4Stream.Position - 16;
                                end else begin
                                    mdiaAtomPosition := MP4Stream.Position - 8;
                                end;
                                repeat
                                    ReadAtomHeader(MP4Stream, AtomName, minfAtomSize, minfIs64BitAtomSize);
                                    if IsSameAtomName(AtomName, 'minf') then begin
                                        if minfIs64BitAtomSize then begin
                                            minfAtomPosition := MP4Stream.Position - 16;
                                        end else begin
                                            minfAtomPosition := MP4Stream.Position - 8;
                                        end;
                                        repeat
                                            ReadAtomHeader(MP4Stream, AtomName, stblAtomSize, stblIs64BitAtomSize);
                                            if IsSameAtomName(AtomName, 'stbl') then begin
                                                if stblIs64BitAtomSize then begin
                                                    stblAtomPosition := MP4Stream.Position - 16;
                                                end else begin
                                                    stblAtomPosition := MP4Stream.Position - 8;
                                                end;
                                                repeat
                                                    ReadAtomHeader(MP4Stream, AtomName, co64AtomSize, co64Is64BitAtomSize);
                                                    if IsSameAtomName(AtomName, 'co64') then begin
                                                        Result := False;
                                                        if co64Is64BitAtomSize then begin
                                                            co64AtomPosition := MP4Stream.Position - 16;
                                                        end else begin
                                                            co64AtomPosition := MP4Stream.Position - 8;
                                                        end;
                                                        MP4Stream.Read(Version, 1);
                                                        MP4Stream.Read(Flags, 3);
                                                        MP4Stream.Read(NumberOfOffsets, 4);
                                                        NumberOfOffsets := ReverseBytes32(NumberOfOffsets);
                                                        i := 0;
                                                        while MP4Stream.Position < co64AtomPosition + co64AtomSize do begin
                                                            MP4Stream.Read(OffsetValue, 8);
                                                            OffsetValue := ReverseBytes64(OffsetValue);
                                                            OffsetValue := OffsetValue + Offset;
                                                            OffsetValue := ReverseBytes64(OffsetValue);
                                                            MP4Stream.Seek(- 8, spCurrent);
                                                            MP4Stream.Write(OffsetValue, 8);
                                                            Inc(i);
                                                        end;
                                                        if i = NumberOfOffsets then begin
                                                            Result := True;
                                                        end;
                                                    end else begin
                                                        if co64Is64BitAtomSize then begin
                                                            MP4Stream.Seek(co64AtomSize - 16, spCurrent);
                                                        end else begin
                                                            MP4Stream.Seek(co64AtomSize - 8, spCurrent);
                                                        end;
                                                    end;
                                                 until (MP4Stream.Position >= MP4Stream.Size)
                                                OR (MP4Stream.Position >= stblAtomPosition + stblAtomSize);
                                            end else begin
                                                if stblIs64BitAtomSize then begin
                                                    MP4Stream.Seek(stblAtomSize - 16, spCurrent);
                                                end else begin
                                                    MP4Stream.Seek(stblAtomSize - 8, spCurrent);
                                                end;
                                            end;
                                        until (MP4Stream.Position >= MP4Stream.Size)
                                        OR (MP4Stream.Position >= minfAtomPosition + minfAtomSize);
                                    end else begin
                                        if minfIs64BitAtomSize then begin
                                            MP4Stream.Seek(minfAtomSize - 16, spCurrent);
                                        end else begin
                                            MP4Stream.Seek(minfAtomSize - 8, spCurrent);
                                        end;
                                    end;
                                until (MP4Stream.Position >= MP4Stream.Size)
                                OR (MP4Stream.Position >= mdiaAtomPosition + mdiaAtomSize);
                            end else begin
                                if mdiaIs64BitAtomSize then begin
                                    MP4Stream.Seek(mdiaAtomSize - 16, spCurrent);
                                end else begin
                                    MP4Stream.Seek(mdiaAtomSize - 8, spCurrent);
                                end;
                            end;
                        until (MP4Stream.Position >= MP4Stream.Size)
                        OR (MP4Stream.Position >= trakAtomPosition + trakAtomSize);
                    end else begin
                        if trakIs64BitAtomSize then begin
                            MP4Stream.Seek(trakAtomSize - 16, spCurrent);
                        end else begin
                            MP4Stream.Seek(trakAtomSize - 8, spCurrent);
                        end;
                    end;
                until (MP4Stream.Position >= MP4Stream.Size)
                OR (MP4Stream.Position >= moovAtomPosition + moovAtomSize);
            end else begin
                if moovIs64BitAtomSize then begin
                    MP4Stream.Seek(moovAtomSize - 16, spCurrent);
                end else begin
                    MP4Stream.Seek(moovAtomSize - 8, spCurrent);
                end;
            end;
        until (MP4Stream.Position >= MP4Stream.Size)
        OR (moovAtomSize = 0);
    except
        Result := False;
    end;
end;

procedure GetmdatAtom(MP4Stream: PStream; out Position, Size: Int64);
var
    PreviousPosition: Int64;
    AtomName: TAtomName;
    AtomSize: Int64;
    Is64BitAtomSize: Boolean;
begin
    Position := - 1;
    Size := 0;
    try
        PreviousPosition := MP4Stream.Position;
        try
            MP4Stream.Seek(0, spBegin);
            repeat
                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize, False);
                if Is64BitAtomSize then begin
                    if IsSameAtomName(AtomName, 'mdat') then begin
                        Position := MP4Stream.Position - 16;
                        Size := AtomSize;
                        Exit;
                    end else begin
                        MP4Stream.Seek(AtomSize - 16, spCurrent);
                    end;
                end else begin
                    if IsSameAtomName(AtomName, 'mdat') then begin
                        Position := MP4Stream.Position - 8;
                        Size := AtomSize;
                        Exit;
                    end else begin
                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                    end;
                end;
            until MP4Stream.Position >= MP4Stream.Size;
        finally
            MP4Stream.Seek(PreviousPosition, spBegin);
        end;
    except
        Position := - 1;
        Size := 0;
    end;
end;

function GetPlayTime(MP4Stream: PStream): Double;
var
    AtomName: TAtomName;
    AtomSize: Int64;
    Version: Byte;
    TimeScale: Cardinal;
    Duration4: Cardinal;
    Duration8: UInt64;
    moovAtomSize: Int64;
    Is64BitAtomSize: Boolean;
    PreviousPosition: Int64;
begin
    Result := 0;
    PreviousPosition := MP4Stream.Position;
    try
        try
            MP4Stream.Seek(0, spBegin);
            try
                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize, False);
            except
                //* Will except if not an MP4 file and MP4TagLibraryFailOnCorruptFile is True
            end;
            if NOT IsSameAtomName(AtomName, 'ftyp') then begin
                Exit;
            end;
            //* Continue loading
            MP4Stream.Seek(AtomSize - 8, spCurrent);
            repeat
                ReadAtomHeader(MP4Stream, AtomName, moovAtomSize, Is64BitAtomSize);
                if IsSameAtomName(AtomName, 'moov') then begin
                    repeat
                        ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                        if IsSameAtomName(AtomName, 'mvhd') then begin
                            MP4Stream.Read(Version, 1);
                            MP4Stream.Seek(3, spCurrent);
                            if Version = 1 then begin
                                MP4Stream.Seek(8, spCurrent);
                                MP4Stream.Seek(8, spCurrent);
                                MP4Stream.Read(TimeScale, 4);
                                TimeScale := ReverseBytes32(TimeScale);
                                MP4Stream.Read(Duration8, 8);
                                Duration8 := ReverseBytes64(Duration8);
                                if (TimeScale <> 0)
                                AND (Duration8 <> 0)
                                then begin
                                    Result := (Duration8 / TimeScale);
                                end;
                            end else begin
                                MP4Stream.Seek(4, spCurrent);
                                MP4Stream.Seek(4, spCurrent);
                                MP4Stream.Read(TimeScale, 4);
                                TimeScale := ReverseBytes32(TimeScale);
                                MP4Stream.Read(Duration4, 4);
                                Duration4 := ReverseBytes32(Duration4);
                                if (TimeScale <> 0)
                                AND (Duration4 <> 0)
                                then begin
                                    Result := (Duration4 / TimeScale);
                                end;
                            end;
                            Exit;
                        end else begin
                            if Is64BitAtomSize then begin
                                MP4Stream.Seek(AtomSize - 16, spCurrent);
                            end else begin
                                MP4Stream.Seek(AtomSize - 8, spCurrent);
                            end;
                        end;
                    until MP4Stream.Position >= MP4Stream.Size;
                end else begin
                    if Is64BitAtomSize then begin
                        MP4Stream.Seek(moovAtomSize - 16, spCurrent);
                    end else begin
                        MP4Stream.Seek(moovAtomSize - 8, spCurrent);
                    end;
                end;
            until (MP4Stream.Position >= MP4Stream.Size)
            OR (moovAtomSize = 0);
        except
            Result := 0;
        end;
    finally
        MP4Stream.Seek(PreviousPosition, spBegin);
    end;
end;

(*
function GetAudioAttributes(MP4Tag: TMP4Tag; MP4Stream: TStream): Boolean;
var
    AtomName: TAtomName;
    AtomSize: Int64;
    moovAtomSize: Int64;
    Is64BitAtomSize: Boolean;
    PreviousPosition: Int64;
    NumberOfDescriptions: Cardinal;
    AudioChannels: Word;
    SampleSize: Word;
    SampleRate: Cardinal;
begin
    Result := False;
    MP4Tag.Resolution := 0;
    MP4Tag.SampleRate := 0;
    PreviousPosition := MP4Stream.Position;
    try
        try
            MP4Stream.Seek(0, spBegin);
            try
                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize, False);
            except
                //* Will except if not an MP4 file and MP4TagLibraryFailOnCorruptFile is True
            end;
            if NOT IsSameAtomName(AtomName, 'ftyp') then begin
                Exit;
            end;
            //* Continue loading
            MP4Stream.Seek(AtomSize - 8, spCurrent);
            repeat
                ReadAtomHeader(MP4Stream, AtomName, moovAtomSize, Is64BitAtomSize);
                if IsSameAtomName(AtomName, 'moov') then begin
                    repeat
                        ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                        if IsSameAtomName(AtomName, 'trak') then begin
                            repeat
                                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                if IsSameAtomName(AtomName, 'mdia') then begin
                                    repeat
                                        ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                        if IsSameAtomName(AtomName, 'minf') then begin
                                            repeat
                                                ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                                if IsSameAtomName(AtomName, 'stbl') then begin
                                                    repeat
                                                        ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                                        if IsSameAtomName(AtomName, 'stsd') then begin
                                                            MP4Stream.Seek(4, spCurrent);
                                                            MP4Stream.Read2(NumberOfDescriptions, 4);
                                                            NumberOfDescriptions := ReverseBytes32(NumberOfDescriptions);
                                                            if NumberOfDescriptions = 1 then begin
                                                                repeat
                                                                    ReadAtomHeader(MP4Stream, AtomName, AtomSize, Is64BitAtomSize);
                                                                    if IsSameAtomName(AtomName, 'mp4a') then begin
                                                                        MP4Stream.Seek($10, spCurrent);
                                                                        MP4Stream.Read2(AudioChannels, 2);
                                                                        MP4Tag.ChannelCount := ReverseBytes16(AudioChannels);
                                                                        MP4Stream.Read2(SampleSize, 2);
                                                                        MP4Tag.Resolution := ReverseBytes16(SampleSize);
                                                                        MP4Stream.Seek(2, spCurrent);
                                                                        MP4Stream.Read2(SampleRate, 4);
                                                                        MP4Tag.SampleRate := ReverseBytes32(SampleRate);
                                                                        Exit;
                                                                    end else begin
                                                                        if Is64BitAtomSize then begin
                                                                            MP4Stream.Seek(AtomSize - 16, spCurrent);
                                                                        end else begin
                                                                            MP4Stream.Seek(AtomSize - 8, spCurrent);
                                                                        end;
                                                                    end;
                                                                until (MP4Stream.Position >= MP4Stream.Size)
                                                                OR (MP4Stream.Position + AtomSize >= MP4Stream.Size);
                                                            end;
                                                        end else begin
                                                            if Is64BitAtomSize then begin
                                                                MP4Stream.Seek(AtomSize - 16, spCurrent);
                                                            end else begin
                                                                MP4Stream.Seek(AtomSize - 8, spCurrent);
                                                            end;
                                                        end;
                                                    until (MP4Stream.Position >= MP4Stream.Size)
                                                    OR (MP4Stream.Position + AtomSize >= MP4Stream.Size);
                                                end else begin
                                                    if Is64BitAtomSize then begin
                                                        MP4Stream.Seek(AtomSize - 16, spCurrent);
                                                    end else begin
                                                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                                                    end;
                                                end;
                                            until (MP4Stream.Position >= MP4Stream.Size)
                                            OR (MP4Stream.Position + AtomSize >= MP4Stream.Size);
                                        end else begin
                                            if Is64BitAtomSize then begin
                                                MP4Stream.Seek(AtomSize - 16, spCurrent);
                                            end else begin
                                                MP4Stream.Seek(AtomSize - 8, spCurrent);
                                            end;
                                        end;
                                    until (MP4Stream.Position >= MP4Stream.Size)
                                    OR (MP4Stream.Position + AtomSize >= MP4Stream.Size);
                                end else begin
                                    if Is64BitAtomSize then begin
                                        MP4Stream.Seek(AtomSize - 16, spCurrent);
                                    end else begin
                                        MP4Stream.Seek(AtomSize - 8, spCurrent);
                                    end;
                                end;
                            until MP4Stream.Position >= MP4Stream.Size;
                        end else begin
                            if Is64BitAtomSize then begin
                                MP4Stream.Seek(AtomSize - 16, spCurrent);
                            end else begin
                                MP4Stream.Seek(AtomSize - 8, spCurrent);
                            end;
                        end;
                    until MP4Stream.Position >= MP4Stream.Size;
                end else begin
                    if Is64BitAtomSize then begin
                        MP4Stream.Seek(moovAtomSize - 16, spCurrent);
                    end else begin
                        MP4Stream.Seek(moovAtomSize - 8, spCurrent);
                    end;
                end;
            until (MP4Stream.Position >= MP4Stream.Size)
            OR (moovAtomSize = 0);
        except
            Result := False
        end;
    finally
        MP4Stream.Seek(PreviousPosition, spBegin);
    end;
end;
*)

function GenreToIndex(Genre: String): Integer;
var
    i: Integer;
    GenreText: String;
begin
    Result := - 1;
    GenreText := UpperCase(Genre);
    for i := 1 to Length(ID3Genres) - 1 do begin
        if UpperCase(ID3Genres[i]) = GenreText then begin
            Result := i;
            Exit;
        end;
    end;
end;

function TMP4Tag.GetGenre: String;
var
    GenreIndex: Integer;
    GenreString: String;
begin
    Result := '';
    GenreString := '';
    GenreIndex := GetInteger16('gnre');
    if (GenreIndex >= Low(ID3Genres))
    AND (GenreIndex <= High(ID3Genres))
    then begin
        GenreString := ID3Genres[GenreIndex];
    end;
    if GenreString = '' then begin
        GenreString := GetText('©gen');
    end;
    Result := GenreString;
end;

function TMP4Tag.SetGenre(Genre: String): Boolean;
var
    GenreIndex: Integer;
begin
    DeleteAllAtoms('gnre');
    DeleteAllAtoms('©gen');
    if Genre = '' then begin
        Result := True;
        Exit;
    end;
    GenreIndex := GenreToIndex(Genre);
    if GenreIndex > - 1 then begin
        Result := SetInteger16('gnre', GenreIndex);
    end else begin
        Result := SetText('©gen', Genre);
    end;
end;

function TMP4Tag.GetPurchaseCountry: String;
var
    Value: UInt64;
begin
    Result := '';
    if not GetInteger('sfID', Value) then
        Exit;
    case Value of
        143460: Result := 'Australia';
        143445: Result := 'Austria';
        143446: Result := 'Belgium';
        143455: Result := 'Canada';
        143458: Result := 'Denmark';
        143447: Result := 'Finland';
        143442: Result := 'France';
        143443: Result := 'Germany';
        143448: Result := 'Greece';
        143449: Result := 'Ireland';
        143450: Result := 'Italy';
        143462: Result := 'Japan';
        143451: Result := 'Luxembourg';
        143452: Result := 'Netherlands';
        143461: Result := 'New Zealand';
        143457: Result := 'Norway';
        143453: Result := 'Portugal';
        143454: Result := 'Spain';
        143456: Result := 'Sweden';
        143459: Result := 'Switzerland';
        143444: Result := 'United Kingdom';
        143441: Result := 'United States';
        else begin
            if Value <> 0 then begin
                Result := Int2Str(Value);
            end;
        end;
    end;
end;

function TMP4Tag.SetPurchaseCountry(Country: String): Boolean;
var
    Value: Integer;
begin
    Value := 0;
    if Country = 'Australia' then begin
        Value := 143460;
    end;
    if Country = 'Austria' then begin
        Value := 143445;
    end;
    if Country = 'Belgium' then begin
        Value := 143446;
    end;
    if Country = 'Canada' then begin
        Value := 143455;
    end;
    if Country = 'Denmark' then begin
        Value := 143458;
    end;
    if Country = 'Finland' then begin
        Value := 143447;
    end;
    if Country = 'France' then begin
        Value := 143442;
    end;
    if Country = 'Germany' then begin
        Value := 143443;
    end;
    if Country = 'Greece' then begin
        Value := 143448;
    end;
    if Country = 'Ireland' then begin
        Value := 143449;
    end;
    if Country = 'Italy' then begin
        Value := 143450;
    end;
    if Country = 'Japan' then begin
        Value := 143462;
    end;
    if Country = 'Luxembourg' then begin
        Value := 143451;
    end;
    if Country = 'Netherlands' then begin
        Value := 143452;
    end;
    if Country = 'New Zealand' then begin
        Value := 143461;
    end;
    if Country = 'Norway' then begin
        Value := 143457;
    end;
    if Country = 'Portugal' then begin
        Value := 143453;
    end;
    if Country = 'Spain' then begin
        Value := 143454;
    end;
    if Country = 'Sweden' then begin
        Value := 143456;
    end;
    if Country = 'Switzerland' then begin
        Value := 143459;
    end;
    if Country = 'United Kingdom' then begin
        Value := 143444;
    end;
    if Country = 'United States' then begin
        Value := 143441;
    end;
    if Value = 0 then begin
        Value := Str2Int(Country);
    end;
    Result := SetInteger32('sfID', Value);
end;

function TMP4Tag.Assign(MP4Tag: PMP4Tag): Boolean;
var
    i: Integer;
begin
    Clear;
    if MP4Tag <> nil then begin
        FileName := MP4Tag.FileName;
        FLoaded := MP4Tag.Loaded;
        Version := MP4Tag.Version;
        Flags := MP4Tag.Flags;
        PaddingToWrite := MP4Tag.PaddingToWrite;
        for i := 0 to MP4Tag.Count - 1 do begin
            AddAtom(MP4Tag.Atoms[i].ID).Assign(MP4Tag.Atoms[i]);
        end;
    end;
    Result := True;
end;

function TMP4Tag.GetMultipleValuesMultipleAtoms(AtomName: String; List: PStrList): Boolean;
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    Result := GetMultipleValuesMultipleAtoms(ID, List);
end;

function TMP4Tag.GetMultipleValuesMultipleAtoms(AtomName: TAtomName; List: PStrList): Boolean;
var
    i: Integer;
begin
    List.clear;
    for i := 0 to Count - 1 do begin
        if IsSameAtomName(Atoms[i].ID, AtomName) then begin
            List.Add(Atoms[i].GetAsText);
        end;
    end;
    Result := List.count > 0;
end;

procedure TMP4Tag.SetMultipleValuesCommaSeparated(AtomName: String; List: PStrList);
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    SetMultipleValuesCommaSeparated(ID, List);
end;

procedure TMP4Tag.SetMultipleValuesCommaSeparated(AtomName: TAtomName; List: PStrList);
var
    i: Integer;
    Text: String;
begin
    for i := Count - 1 downto 0 do begin
        if IsSameAtomName(Atoms[i].ID, AtomName) then begin
            DeleteAtom(i);
        end;
    end;
    for i := 0 to List.count - 1 do begin
        if i < List.count - 1 then begin
            Text := List.Items[i] + ', ';
        end else begin
            Text := List.Items[i];
        end;
    end;
    AddAtom(AtomName).SetAsText(Text);
end;

procedure TMP4Tag.SetMultipleValuesMultipleAtoms(AtomName: String; List: PStrList);
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    SetMultipleValuesMultipleAtoms(ID, List);
end;

procedure TMP4Tag.SetMultipleValuesMultipleAtoms(AtomName: TAtomName; List: PStrList);
var
    i: Integer;
begin
    for i := Count - 1 downto 0 do begin
        if IsSameAtomName(Atoms[i].ID, AtomName) then begin
            DeleteAtom(i);
        end;
    end;
    for i := 0 to List.count - 1 do begin
        AddAtom(AtomName).SetAsText(List.Items[i]);
    end;
end;

function TMP4Tag.DeleteAllAtoms(AtomName: TAtomName): Boolean;
var
    i: Integer;
begin
    Result := False;
    for i := High(Atoms) downto 0 do begin
        if IsSameAtomName(Atoms[i].ID, AtomName) then begin
            DeleteAtom(i);
            Result := True;
        end;
    end;
end;

function TMP4Tag.DeleteAllAtoms(AtomName: String): Boolean;
var
    ID: TAtomName;
begin
    StringToAtomName(AtomName, ID);
    Result := DeleteAllAtoms(ID);
end;

function IsSameAtomName(ID: TAtomName; Name: String): Boolean;
begin
    {$IFDEF MP4TL_MOBILE}
    if (ID[0] = Ord(Name[0]))
    AND (ID[1] = Ord(Name[1]))
    AND (ID[2] = Ord(Name[2]))
    AND (ID[3] = Ord(Name[3]))
    {$ELSE}
    if (ID[0] = Ord(Name[1]))
    AND (ID[1] = Ord(Name[2]))
    AND (ID[2] = Ord(Name[3]))
    AND (ID[3] = Ord(Name[4]))
    {$ENDIF}
    then begin
        Result := True;
    end else begin
        Result := False;
    end;
end;

function IsSameAtomName(ID1: TAtomName; ID2: TAtomName): Boolean;
begin
    if (ID1[0] = ID2[0])
    AND (ID1[1] = ID2[1])
    AND (ID1[2] = ID2[2])
    AND (ID1[3] = ID2[3])
    then begin
        Result := True;
    end else begin
        Result := False;
    end;
end;

function StringToAtomName(Name: String; var ID: TAtomName): Boolean;
begin
    FillChar(ID, SizeOf(ID), 0);
    {$IFDEF MP4TL_MOBILE}
    if Length(Name) > 0 then begin
        ID[0] := Ord(Name[0]);
    end;
    if Length(Name) > 1 then begin
        ID[1] := Ord(Name[1]);
    end;
    if Length(Name) > 2 then begin
        ID[2] := Ord(Name[2]);
    end;
    if Length(Name) > 3 then begin
        ID[3] := Ord(Name[3]);
    end;
    {$ELSE}
    if Length(Name) > 0 then begin
        ID[0] := Ord(Name[1]);
    end;
    if Length(Name) > 1 then begin
        ID[1] := Ord(Name[2]);
    end;
    if Length(Name) > 2 then begin
        ID[2] := Ord(Name[3]);
    end;
    if Length(Name) > 3 then begin
        ID[3] := Ord(Name[4]);
    end;
    {$ENDIF}
    Result := True;
end;

function AtomNameToString(ID: TAtomName): String;
begin
    Result := Char(ID[0]) + Char(ID[1]) + Char(ID[2]) +Char(ID[3]);
end;

function MP4TagErrorCode2String(ErrorCode: Integer): String;
begin
    Result := 'Unknown error code.';
    case ErrorCode of
        MP4TAGLIBRARY_SUCCESS: Result := 'Success.';
        MP4TAGLIBRARY_ERROR: Result := 'Unknown error occured.';
        MP4TAGLIBRARY_ERROR_NO_TAG_FOUND: Result := 'No MP4 tag found.';
        MP4TAGLIBRARY_ERROR_EMPTY_TAG: Result := 'MP4 tag is empty.';
        MP4TAGLIBRARY_ERROR_EMPTY_FRAMES: Result := 'MP4 tag contains only empty frames.';
        MP4TAGLIBRARY_ERROR_OPENING_FILE: Result := 'Error opening file.';
        MP4TAGLIBRARY_ERROR_READING_FILE: Result := 'Error reading file.';
        MP4TAGLIBRARY_ERROR_WRITING_FILE: Result := 'Error writing file.';
        MP4TAGLIBRARY_ERROR_DOESNT_FIT: Result := 'Error: MP4 tag doesn''t fit into the file.';
        MP4TAGLIBRARY_ERROR_NOT_SUPPORTED_VERSION: Result := 'Error: not supported MP4 version.';
        MP4TAGLIBRARY_ERROR_NOT_SUPPORTED_FORMAT: Result := 'Error: not supported file format.';
        MP4TAGLIBRARY_ERROR_NEED_EXCLUSIVE_ACCESS: Result := 'Error: file is locked. Need exclusive access to write MP4 tag to this file.';
        MP4TAGLIBRARY_ERROR_UPDATE_stco: Result := 'Error: updating MP4 ''stco'' atom.';
        MP4TAGLIBRARY_ERROR_UPDATE_co64: Result := 'Error: updating MP4 ''co64'' atom.';
    end;
end;

Initialization

    MP4AtomDataID[0] := Ord('d');
    MP4AtomDataID[1] := Ord('a');
    MP4AtomDataID[2] := Ord('t');
    MP4AtomDataID[3] := Ord('a');

    MP4AtommeanID[0] := Ord('m');
    MP4AtommeanID[1] := Ord('e');
    MP4AtommeanID[2] := Ord('a');
    MP4AtommeanID[3] := Ord('n');

    MP4AtomnameID[0] := Ord('n');
    MP4AtomnameID[1] := Ord('a');
    MP4AtomnameID[2] := Ord('m');
    MP4AtomnameID[3] := Ord('e');

end.

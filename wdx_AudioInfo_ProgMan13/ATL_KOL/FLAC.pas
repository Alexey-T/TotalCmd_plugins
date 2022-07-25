{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TFLACfile - for manipulating with FLAC file information               }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.4 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.3 (13 August 2004) by jtclipper                                   }
{   - unit rewritten, VorbisComment is obsolete now                           }
{                                                                             }
{ Version 1.2 (23 June 2004) by sundance                                      }
{   - Check for ID3 tags (although not supported)                             }
{   - Don't parse for other FLAC metablocks if FLAC header is missing         }
{                                                                             }
{ Version 1.1 (6 July 2003) by Erik                                           }
{   - Class: Vorbis comments (native comment to FLAC files) added             }
{                                                                             }
{ Version 1.0 (13 August 2002)                                                }
{   - Info: channels, sample rate, bits/sample, file size, duration, ratio    }
{   - Class TID3v1: reading & writing support for ID3v1 tags                  }
{   - Class TID3v2: reading & writing support for ID3v2 tags                  }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }

unit FLAC;

interface

uses
  Windows, KOL, ID3v1, ID3v2;

const
  META_STREAMINFO      = 0;
  META_PADDING         = 1;
  META_APPLICATION     = 2;
  META_SEEKTABLE       = 3;
  META_VORBIS_COMMENT  = 4;
  META_CUESHEET        = 5;
  META_IMAGE           = 6; // just guess, need more research

type
  TFields = (fTITLE, fVERSION, fALBUM, fARTIST, fTRACKNUMBER, fORGANIZATION, fDESCRIPTION,
             fDATE, fGENRE, fLOCATION, fCOPYRIGHT, fISRC, fPERFORMER, fLICENSE, fCONTACT, fCOMMENT,
             fCOMPOSER, fLANGUAGE, fURL, fENCODER, fTONES, fSTYLES, fMOOD, fSITUATION, fRATING,
             fQUALITY, fTEMPO, fTYPE, fLYRICS);

  TFieldNames = array [TFields] of AnsiString;


  TFlacHeader = record
    StreamMarker: array[1..4] of AnsiChar; //should always be 'fLaC'
    MetaDataBlockHeader: array[1..4] of Byte;
    Info: array[1..18] of Byte;
    MD5Sum: array[1..16] of Byte;
  end;

  TMetaData = record
    MetaDataBlockHeader: array[1..4] of Byte;
    Data: PStream;
  end;

  PFLAC = ^TFLAC;
  TFLAC = object(TObj)
  private
    FFieldValues: TFieldNames;

    FHeader: TFlacHeader;
    FFileName: WideString;
    FPaddingIndex: integer;
    FPaddingLast: boolean;
    FPaddingFragments: boolean;
    FVorbisIndex: integer;
    FPadding: integer;
    FVCOffset: integer;
    FAudioOffset: integer;
    FChannels: byte;
    FSampleRate: integer;
    FBitsPerSample: byte;
    FBitrate: integer;
    FFileLength: Int64;
    FSamples: Int64;

    aMetaBlockOther: array of TMetaData;

    // tag data
    FVendor: string;
    FTagSize: integer;
    FExists: boolean;
    FTrack: string;
    FGenres: string;

    FID3v1: PID3v1;
    FID3v2: PID3v2;
    fHasCoverArt: Boolean;

    function FGetHasLyrics: boolean;

    procedure FResetData( const bHeaderInfo, bTagFields :boolean );
    function FIsValid: Boolean;
    function FGetDuration: Double;
    function FGetRatio: Double;
    function FGetChannelMode: string;

    function GetInfo(const sFile: WideString; bSetTags: boolean ): boolean;
    procedure AddMetaDataOther( aMetaHeader: array of Byte; stream: PStream; const iBlocklength,iIndex: integer );
    procedure ReadTag(const Source: PStream; bSetTagFields: boolean );
    function RebuildFile( const sFile: WideString; VorbisBlock: PStream ): Boolean;

    procedure InitTag(TagStr: String);
    function GetField(const Index: TFields): string;
    procedure SetField(const Index: TFields; const Value: string);
//    function DecodeUTF8(const Source: string): WideString;
//    function EncodeUTF8(const Source: WideString): string;

  public
    aExtraFields: array of array of string;

    constructor Create;
    destructor Destroy; virtual;

    function ReadFromFile( const sFile: WideString ): boolean;
    function SaveToFile( const sFile: WideString): boolean;
    function RemoveFromFile( const sFile: WideString ):boolean;
    procedure AddExtraField(const sID, sValue: string);

    property ID3v1: PID3v1 read FID3v1;                    { ID3v1 tag data }
    property ID3v2: PID3v2 read FID3v2;                    { ID3v2 tag data }

    property Channels: Byte read FChannels;                     // Number of channels
    property SampleRate: Integer read FSampleRate;              // Sample rate (hz)
    property BitsPerSample: Byte read FBitsPerSample;           // Bits per sample
    property FileLength: Int64 read FFileLength;              // File length (bytes)
    property Samples: Int64 read FSamples;                      // Number of samples
    property Valid: Boolean read FIsValid;                      // True if header valid
    property Duration: Double read FGetDuration;                // Duration (seconds)
    property Ratio: Double read FGetRatio;                      // Compression ratio (%)
    property Bitrate: integer read FBitrate;
    property ChannelMode: string read FGetChannelMode;
    property Exists: boolean read FExists;

    property FileName: WideString read FFileName;
    property AudioOffset: integer read FAudioOffset;           //offset of audio data
    property HasLyrics: boolean read FGetHasLyrics;

    property Vendor: String read FVendor write FVendor;
    property Title: string index fTITLE read GetField write SetField;        { Song title }
    property Artist: string index fARTIST read GetField write SetField;    { Artist name }
    property Album: string index fALBUM read GetField write SetField;       { Album title }
    property Track: string index fTRACKNUMBER read GetField write SetField;        { Track number }
    property Year: string index fDATE read GetField write SetField;         { Release year }
    property Genre: string index fGENRE  read GetField write SetField;        { Genre name }
    property Genres: string read FGenres;                                  { Genres in one string }
    property Comment: string index fCOMMENT read GetField write SetField;     { Comment }
    property Copyright: string index fCOPYRIGHT read GetField write SetField;   { (c) }

    property Composer: string index fCOMPOSER read GetField write SetField;   { (c) }
    property Language: string index fLANGUAGE read GetField write SetField;   { (c) }

    property Link: string index fURL read GetField write SetField;   { (c) }
    property Encoder: string index fENCODER read GetField write SetField;   { (c) }
    property Lyrics: string index fLYRICS read GetField write SetField;   { (c) }
    property Performer: string index fPERFORMER read GetField write SetField;   { (c) }
    property License: string index fLICENSE read GetField write SetField;   { (c) }
    property Organization: string index fORGANIZATION read GetField write SetField;   { (c) }
    property Description: string index fDESCRIPTION read GetField write SetField;   { (c) }
    property Location: string index fLOCATION read GetField write SetField;   { (c) }
    property Contact: string index fCONTACT read GetField write SetField;   { (c) }
    property ISRC: string index fISRC read GetField write SetField;   { (c) }

    property HasCoverArt: Boolean read fHasCoverArt;
{
      Tones := '';
      Styles := '';
      Mood := '';
      Situation := '';
      Rating := '';
      Quality := '';
      Tempo := '';
      Type := '';
}
  end;

function NewFLAC: PFLAC;

const FieldNames: TFieldNames = (
                  'TITLE', 'VERSION', 'ALBUM', 'ARTIST', 'TRACKNUMBER',
                  'ORGANIZATION', 'DESCRIPTION', 'DATE', 'GENRE', 'LOCATION',
                  'COPYRIGHT', 'ISRC', 'PERFORMER','LICENSE', 'CONTACT',
                  'COMMENT', 'COMPOSER', 'LANGUAGE', 'URL', 'ENCODER', 'TONES',
                  'STYLES', 'MOOD', 'SITUATION', 'RATING', 'QUALITY', 'TEMPO',
                  'TYPE', 'LYRICS');

implementation

uses SProc;

(* -------------------------------------------------------------------------- *)

function NewFLAC: PFLAC;
begin
    New(Result, Create);
    Result.FResetData(true, true);
end;

procedure TFLAC.FResetData( const bHeaderInfo, bTagFields :boolean );
var
  i: integer;
begin

   if bHeaderInfo then begin
      Finalize(FHeader);
      FFileName := '';
      FPadding := 0;
      FPaddingLast := false;
      FPaddingFragments := false;
      FChannels := 0;
      FSampleRate := 0;
      FBitsPerSample := 0;
      FFileLength := 0;
      FSamples := 0;
      FVorbisIndex := 0;
      FPaddingIndex := 0;
      FVCOffset := 0;
      FAudioOffset := 0;
      fHasCoverArt := false;

      for i := 0 to Length( aMetaBlockOther ) - 1 do
        aMetaBlockOther[ i ].Data.Free;
      Finalize(aMetaBlockOther);
   end;

   //tag data
   if bTagFields then begin
      FVendor := '';
      FTagSize := 0;
      FExists := false;

      Title := '';
      Artist := '';
      Album := '';
      Year := '';
      Genre := '';
      Comment := '';
      //extra
{
      Tones := '';
      Styles := '';
      Mood := '';
      Situation := '';
      Rating := '';
      Quality := '';
      Tempo := '';
      Type := '';
}
      //
      Composer := '';
      Language := '';
      Copyright := '';
      Link := '';
      Encoder := '';
      Lyrics := '';
      Performer := '';
      License := '';
      Organization := '';
      Description := '';
      Location := '';
      Contact := '';
      ISRC := '';
      FGenres := '';
      Finalize(aExtraFields);
  end;
end;

(* -------------------------------------------------------------------------- *)
// Check for right FLAC file data
function TFLAC.FIsValid: Boolean;
begin
  result := (FHeader.StreamMarker = 'fLaC') and
            (FChannels > 0) and
            (FSampleRate > 0) and
            (FBitsPerSample > 0) and
            (FSamples > 0);
end;

(* -------------------------------------------------------------------------- *)

function TFLAC.FGetDuration: Double;
begin
  if (FIsValid) and (FSampleRate > 0) then begin
     result := FSamples / FSampleRate
  end else begin
     result := 0;
  end;
end;

(* -------------------------------------------------------------------------- *)
//   Get compression ratio
function TFLAC.FGetRatio: Double;
begin
  if FIsValid then begin
     result := (FFileLength - FAudioOffset) / (FSamples * FChannels * FBitsPerSample / 8) * 100
  end else begin
     result := 0;
  end;
end;

(* -------------------------------------------------------------------------- *)
//   Get channel mode
function TFLAC.FGetChannelMode: string;
begin
  if FIsValid then begin
     case FChannels of
      1 : result := 'Mono';
      2 : result := 'Stereo';
      else result := 'Multi Channel';
     end;
  end else begin
     result := '';
  end;
end;

(* -------------------------------------------------------------------------- *)

function TFLAC.FGetHasLyrics: boolean;
begin
  result := ( Trim( Lyrics ) <> '' );
end;

(* -------------------------------------------------------------------------- *)

constructor TFLAC.Create;
begin
  inherited;
  FID3v1 := NewID3v1;
  FID3v2 := NewID3v2;
  FResetData( true, true );
end;

destructor TFLAC.Destroy;
begin
  FResetData( true, true );
  Finalize(FFieldValues);
  FID3v2.Free;
  FID3v1.Free;
  inherited;
end;

(* -------------------------------------------------------------------------- *)

function TFLAC.ReadFromFile( const sFile: WideString ): boolean;
begin
  FResetData( false, true );
  result := GetInfo( sFile, true );
end;

(* -------------------------------------------------------------------------- *)

function TFLAC.GetInfo(const sFile: WideString; bSetTags: boolean ): boolean;
var
  SourceFile: PStream;
  aMetaDataBlockHeader: array[1..4] of byte;
  iBlockLength, iMetaType, iIndex: integer;
  bPaddingFound: boolean;
begin

  result := true;
  bPaddingFound := false;
  FResetData( true, false );
  SourceFile := nil;
  try
    { Read data from ID3 tags }
    FID3v2.ReadFromFile(sFile);

    // Set read-access and open file
    SourceFile := NewReadFileStreamW(sFile);//OpenRead or fmShareDenyWrite);
    FFileLength := SourceFile.Size;
    FFileName := sFile;

    { Seek past the ID3v2 tag, if there is one }
    if FID3v2.Exists then begin
      SourceFile.Seek(FID3v2.Size, spBegin)
    end;

    // Read header data
    FillChar( FHeader, SizeOf(FHeader), 0 );
    SourceFile.Read( FHeader, SizeOf(FHeader) );

    // Process data if loaded and header valid
    if FHeader.StreamMarker = 'fLaC' then begin

       with FHeader do begin
         FChannels      := ( Info[13] shr 1 and $7 + 1 );
         FSampleRate    := ( Info[11] shl 12 or Info[12] shl 4 or Info[13] shr 4 );
         FBitsPerSample := ( Info[13] and 1 shl 4 or Info[14] shr 4 + 1 );
         FSamples       := ( Info[15] shl 24 or Info[16] shl 16 or Info[17] shl 8 or Info[18] );
       end;



       if (FHeader.MetaDataBlockHeader[1] and $80) <> 0 then exit; //no metadata blocks exist
       iIndex := 0;
       repeat // read more metadata blocks if available

          SourceFile.Read( aMetaDataBlockHeader, 4 );

          iIndex := iIndex + 1; // metadatablock index
          iBlockLength := (aMetaDataBlockHeader[2] shl 16 or aMetaDataBlockHeader[3] shl 8 or aMetaDataBlockHeader[4]); //decode length
          if iBlockLength <= 0 then exit; // can it be 0 ?

          iMetaType := (aMetaDataBlockHeader[1] and $7F); // decode metablock type


          if iMetaType = META_VORBIS_COMMENT then begin  // read vorbis block
             FVCOffset := SourceFile.Position;
             FTagSize := iBlockLength;
             FVorbisIndex := iIndex;
             ReadTag(SourceFile, bSetTags); // set up fields
          end else if (iMetaType = META_PADDING) and not bPaddingFound then begin // we have padding block
             FPadding := iBlockLength;                                            // if we find more skip & put them in metablock array
             FPaddingLast := ((aMetaDataBlockHeader[1] and $80) <> 0);
             FPaddingIndex := iIndex;
             bPaddingFound := true;
             SourceFile.Seek(FPadding, spCurrent); // advance into file till next block or audio data start
          end else begin // all other
             if iMetaType <= 5 then begin // is it a valid metablock ?
                if (iMetaType = META_PADDING) then begin // set flag for fragmented padding blocks
                   FPaddingFragments := true;
                end;
                AddMetaDataOther(aMetaDataBlockHeader, SourceFile, iBlocklength, iIndex);
             end else begin
                if iMetaType = META_IMAGE then
                  fHasCoverArt := True;
                SourceFile.Seek(iBlockLength, spCurrent); // unknown block, skip to next one
                // why stop here? after unknown frame can be one we need
//              FSamples := 0; //ops...
//              exit;
             end;
          end;

       until ((aMetaDataBlockHeader[1] and $80) <> 0); // until is last flag ( first bit = 1 )

    end;
  finally
    if FIsValid then begin
       FAudioOffset := SourceFile.Position;  // we need that to rebuild the file if nedeed
       FBitrate := Round( ( ( FFileLength - FAudioOffset ) / 1000 ) * 8 / FGetDuration ); //time to calculate average bitrate
    end else begin
       result := false;
    end;
    SourceFile.Free;
    SourceFile:=nil;
  end;

end;

(* -------------------------------------------------------------------------- *)

procedure TFLAC.AddMetaDataOther( aMetaHeader: array of Byte; stream: PStream; const iBlocklength,iIndex: integer );
var
  iMetaLen: integer;
begin
  // enlarge array
  iMetaLen := Length( aMetaBlockOther ) + 1;
  SetLength( aMetaBlockOther, iMetaLen );
  // save header
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[1] := aMetaHeader[0];
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[2] := aMetaHeader[1];
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[3] := aMetaHeader[2];
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[4] := aMetaHeader[3];
  // save content in a stream
  aMetaBlockOther[ iMetaLen - 1 ].Data := NewMemoryStream;
  aMetaBlockOther[ iMetaLen - 1 ].Data.Position := 0;
//  aMetaBlockOther[ iMetaLen - 1 ].Data.CopyFrom( stream, iBlocklength );
  Stream2Stream(aMetaBlockOther[ iMetaLen - 1 ].Data, stream, iBlocklength );
end;

(* -------------------------------------------------------------------------- *)

procedure TFLAC.InitTag(TagStr: String);
var i: TFields;
    N: String;
begin
    N:= UpperCase(KOL.ParseW(TagStr, '='));

    if N = 'GENRE' then
      if FGenres <> '' then
        FGenres := FGenres + '; ' + TagStr
      else
        FGenres := TagStr;

    for i:= Low(TFields) to High(TFields) do
       if FieldNames[i] = N then begin
         FFieldValues[i]:= TagStr;
         Exit;
       end;
    AddExtraField( N, Copy(TagStr, Length(N)+1, Length(TagStr)));
end;

procedure TFLAC.SetField(const Index: TFields; const Value: string);
begin
    FFieldValues[index]:= Value;
end;

function TFLAC.GetField(const Index: TFields): string;
begin
    Result:= FFieldValues[index];
end;

procedure TFLAC.ReadTag(const Source: PStream; bSetTagFields: boolean );
var
  i, iCount, iSize, iSepPos: Integer;
  Data: AnsiString;//array of AnsiChar;
  sFieldData: string;
begin
  Source.Read( iSize, SizeOf( iSize ) ); // vendor
  SetLength( Data, iSize );
  Source.Read( Data[1], iSize ); // [ 0 ]
  FVendor := UTF8Decode(Data);

  Source.Read( iCount, SizeOf( iCount ) ); //fieldcount

  FExists := ( iCount > 0 );

  for i := 0 to iCount - 1 do begin
      Finalize(Data);
      Source.Read( iSize, SizeOf( iSize ) );
      SetLength( Data , iSize );
      Source.Read(Data[1], iSize );

      if not bSetTagFields then Continue; // if we don't want to re asign fields we skip
      InitTag(UTF8Decode(Data));
  end;

  Finalize(Data);
end;

(* -------------------------------------------------------------------------- *)

procedure TFLAC.AddExtraField(const sID, sValue: string);
var
  iExtraLen: integer;
begin
  iExtraLen := Length( aExtraFields ) + 1;
  SetLength( aExtraFields, iExtraLen );
  SetLength( aExtraFields[ iExtraLen - 1 ], 2 );

  aExtraFields[ iExtraLen - 1, 0 ] := sID;
  aExtraFields[ iExtraLen - 1, 1 ] := sValue;
end;

(* -------------------------------------------------------------------------- *)

function TFLAC.SaveToFile( const sFile: WideString): boolean;
var
  i, iFieldCount, iSize: Integer;
  VorbisBlock, _Tag: PStream;
  k: TFields;

  procedure _WriteTagBuff( sID, sData: string );
  var
    sTmp: AnsiString;
    iTmp: integer;
  begin
    if sData <> '' then  begin
       sTmp := sID + '=' + UTF8Encode( sData );
       iTmp := Length( sTmp );
       _Tag.Write( iTmp, SizeOf( iTmp ) );
       _Tag.WriteStr( sTmp );
       iFieldCount := iFieldCount + 1;
    end;
  end;

begin

  try
    result := false;

    _Tag := NewMemoryStream;
    VorbisBlock := NewMemoryStream;
    if not GetInfo( sFile, false ) then exit; //reload all except tag fields

    iFieldCount := 0;

    for k := Low(TFields) to High(TFields) do
    begin
      _WriteTagBuff(FieldNames[k], FFieldValues[k]);
    end;

    for i := 0 to Length( aExtraFields ) - 1 do begin
       if Trim( aExtraFields[ i, 0 ] ) <> '' then _WriteTagBuff( aExtraFields[ i, 0 ], aExtraFields[ i, 1 ] );
    end;

    // Write vendor info and number of fields
    if FVendor = '' then FVendor := 'reference libFLAC 1.1.0 20030126'; // guess it
    iSize := Length( FVendor );
    VorbisBlock.Write( iSize, SizeOf( iSize ) );
    VorbisBlock.WriteStr( AnsiString(FVendor) );
    VorbisBlock.Write( iFieldCount, SizeOf( iFieldCount ) );

//    VorbisBlock.CopyFrom( Tag, 0 ); // All tag data is here now
    Stream2Stream(VorbisBlock, _Tag, 0);
    VorbisBlock.Position := 0;

    result := RebuildFile( sFile, VorbisBlock );
    FExists := result and (_Tag.Size > 0 );


  finally
    _Tag.Free;
    VorbisBlock.Free;
    _Tag:=nil;
    VorbisBlock:=nil;
  end;

end;

(* -------------------------------------------------------------------------- *)

function TFLAC.RemoveFromFile( const sFile: WideString ):boolean;
begin
  FResetData( false, true );
  result := SaveToFile( sFile );
  if FExists then FExists := not result;
end;

(* -------------------------------------------------------------------------- *)
// saves metablocks back to the file
// always tries to rebuild header so padding exists after comment block and no more than 1 padding block exists
function TFLAC.RebuildFile( const sFile: WideString; VorbisBlock: PStream ): Boolean;
var
  Source, Destination: PStream;
  i, iFileAge, iNewPadding, iMetaCount, iExtraPadding: Integer;
  BufferName, sTmp: string;
  MetaDataBlockHeader: array[1..4] of Byte;
  oldHeader: TFlacHeader;
  MetaBlocks: PStream;
  bRebuild, bRearange: boolean;
begin

  result := false;
  bRearange := false;
  iExtraPadding := 0;
  if (not WFileExists(sFile)) or (FileSetAttr(sFile, 0) <> 0) then exit;

  try
    iFileAge := 0;
    { TODO : Fix }
//    if bTAG_PreserveDate then iFileAge := FileAgeW( sFile );

    // re arrange other metadata in case of
    // 1. padding block is not aligned after vorbis comment
    // 2. insufficient padding - rearange upon file rebuild
    // 3. fragmented padding blocks
    iMetaCount := Length( aMetaBlockOther );
    if (FPaddingIndex <> FVorbisIndex + 1) or (FPadding <= VorbisBlock.Size - FTagSize ) or FPaddingFragments then begin
       MetaBlocks := NewMemoryStream;//TMemoryStream.Create;
       for i := 0 to iMetaCount - 1 do begin
           aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ] := ( aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ] and $7f ); // not last

           if aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ] = META_PADDING then begin
              iExtraPadding := iExtraPadding + aMetaBlockOther[ i ].Data.Size + 4; // add padding size plus 4 bytes of header block
           end else begin
              aMetaBlockOther[ i ].Data.Position := 0;
              MetaBlocks.Write( aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ], 4 );
//              MetaBlocks.CopyFrom( aMetaBlockOther[ i ].Data, 0 );
              Stream2Stream(MetaBlocks, aMetaBlockOther[ i ].Data, 0);
           end;

       end;
       MetaBlocks.Position := 0;
       bRearange := true;
    end;

    // set up file
    if (FPadding <= VorbisBlock.Size - FTagSize ) then begin // no room rebuild the file from scratch
       bRebuild := true;
       BufferName := sFile + '~';
       Source := NewReadFileStreamW( sFile ); // Set read-only and open old file, and create new
       Destination := NewWriteFileStream( BufferName );
       Source.Read( oldHeader, sizeof( oldHeader ) );
       oldHeader.MetaDataBlockHeader[ 1 ] := (oldHeader.MetaDataBlockHeader[ 1 ] and $7f ); //just in case no metadata existed
       Destination.Write( oldHeader, Sizeof( oldHeader ) );
//       Destination.CopyFrom( MetaBlocks, 0 );
       Stream2Stream(Destination, MetaBlocks, 0);
    end else begin
       bRebuild := false;
       Source := nil;
       Destination := NewWriteFileStream( sFile ); // Set write-access and open file
       if bRearange then begin
          Destination.Seek( SizeOf( FHeader ), spBegin );
//          Destination.CopyFrom( MetaBlocks, 0 );
          Stream2Stream(Destination, MetaBlocks, 0);
       end else begin
          Destination.Seek( FVCOffset - 4, spBegin );
       end;
    end;

    // finally write vorbis block
    MetaDataBlockHeader[1] := META_VORBIS_COMMENT;
    MetaDataBlockHeader[2] := Byte(( VorbisBlock.Size shr 16 ) and 255 );
    MetaDataBlockHeader[3] := Byte(( VorbisBlock.Size shr 8 ) and 255 );
    MetaDataBlockHeader[4] := Byte( VorbisBlock.Size and 255 );
    Destination.Write( MetaDataBlockHeader[ 1 ], SizeOf( MetaDataBlockHeader ) );
//    Destination.CopyFrom( VorbisBlock, VorbisBlock.Size );
    Stream2Stream(Destination, VorbisBlock, VorbisBlock.Size);

    // and add padding
    if FPaddingLast or bRearange then begin
       MetaDataBlockHeader[1] := META_PADDING or $80;
    end else begin
       MetaDataBlockHeader[1] := META_PADDING;
    end;
    if bRebuild then begin
       iNewPadding := 4096; // why not...
    end else begin
       if FTagSize > VorbisBlock.Size then begin // tag got smaller increase padding
          iNewPadding := (FPadding + FTagSize - VorbisBlock.Size) + iExtraPadding;
       end else begin // tag got bigger shrink padding
          iNewPadding := (FPadding - VorbisBlock.Size + FTagSize ) + iExtraPadding;
       end;
    end;
    MetaDataBlockHeader[2] := Byte(( iNewPadding shr 16 ) and 255 );
    MetaDataBlockHeader[3] := Byte(( iNewPadding shr 8 ) and 255 );
    MetaDataBlockHeader[4] := Byte( iNewPadding and 255 );
    Destination.Write(MetaDataBlockHeader[ 1 ], 4);
    if (FPadding <> iNewPadding) or bRearange then begin // fill the block with zeros
       { TODO : Fix }
       //sTmp := DupeString( #0, iNewPadding );
       Destination.Write( sTmp[1], iNewPadding );
    end;

    // finish
    if bRebuild then begin // time to put back the audio data...
       Source.Seek( FAudioOffset, spBegin );
//       Destination.CopyFrom( Source, Source.Size - FAudioOffset );
       Stream2Stream(Destination, Source, Source.Size - FAudioOffset);
       Source.Free;
       Destination.Free;
       if ( DeleteFileW( PWideChar(sFile) ) ) and ( MoveFileW( PWideChar(BufferName), PWideChar(sFile) ) ) then begin //Replace old file and delete temporary file
          result := true
       end else
       begin
         result := false;
//          raise Exception.Create('');
       end;
    end else begin
       result := true;
       Destination.Free;
    end;

    // post save tasks
    { TODO : Fix }
//    if bTAG_PreserveDate then WideFileSetDate( sFile, iFileAge );
    if bRearange then
    begin
      MetaBlocks.Free;
      MetaBlocks:=nil;
    end;

  except
    // Access error
    if WFileExists( BufferName ) then DeleteFileW( PWideChar(BufferName) );
  end;
end;

end.


{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TMonkey - for manipulating with Monkey's Audio file information       }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.6 (11 April 2004) by Gambit                                       }
{   - Added Ratio property again                                              }
{                                                                             }
{ Version 1.5 (22 August 2003) by MaDah                                       }
{   - Added support for Monkey's Audio 3.98                                   }
{   - Added/changed/removed some stuff                                        }
{                                                                             }
{ Version 1.4 (29 July 2002)                                                  }
{   - Correction for calculating of duration                                  }
{                                                                             }
{ Version 1.1 (11 September 2001)                                             }
{   - Added property Samples                                                  }
{   - Removed WAV header information                                          }
{                                                                             }
{ Version 1.0 (7 September 2001)                                              }
{   - Support for Monkey's Audio files                                        }
{   - Class TID3v1: reading & writing support for ID3v1 tags                  }
{   - Class TID3v2: reading & writing support for ID3v2 tags                  }
{   - Class TAPEtag: reading & writing support for APE tags                   }
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
{ Портировано в KOL - Матвеев Дмитрий                                         }
{                                                                             }
{ *************************************************************************** }

// - История -

// Дата: 16.11.2005 Версия: 1.05
// [+] - Поддержка версий >= 3.98

// Дата: 18.11.2003 Версия: 1.01
// [*] - Исправил поддержку APE-тегов.
//       Для старых версий файлов можно использовать ID3v1 и ID3v2.

// Версия: 1.00
   {Стартовая версия}

unit Monkey;

interface

uses KOL, ID3v1, ID3v2, APE;

const
   { Compression level codes }
   MONKEY_COMPRESSION_FAST       = 1000;  { Fast (poor) }
   MONKEY_COMPRESSION_NORMAL     = 2000;  { Normal (good) }
   MONKEY_COMPRESSION_HIGH       = 3000;  { High (very good) }
   MONKEY_COMPRESSION_EXTRA_HIGH = 4000;  { Extra high (best) }
   MONKEY_COMPRESSION_INSANE     = 5000;  { Insane }
   MONKEY_COMPRESSION_BRAINDEAD  = 6000;  { BrainDead }
   { Compression level names }
   MONKEY_COMPRESSION: array [0..6] of string =
    ('Unknown', 'Fast', 'Normal', 'High', 'Extra High', 'Insane', 'BrainDead');

   { Format flags, only for Monkey's Audio <= 3.97 }
   MONKEY_FLAG_8_BIT          = 1;  // Audio 8-bit
   MONKEY_FLAG_CRC            = 2;  // New CRC32 error detection
   MONKEY_FLAG_PEAK_LEVEL     = 4;  // Peak level stored
   MONKEY_FLAG_24_BIT         = 8;  // Audio 24-bit
   MONKEY_FLAG_SEEK_ELEMENTS  = 16; // Number of seek elements stored
   MONKEY_FLAG_WAV_NOT_STORED = 32; // WAV header not stored

type
   { Real structure of Monkey's Audio header }
   // Common header for all versions
   TMAC = array[0..3] of AnsiChar;
   TAPE_Header = packed record
      cID: TMAC;                       // should equal 'MAC '
      nVersion : WORD;                 // version number * 1000 (3.81 = 3810)
   end;
   // Old header for <= 3.97
   TAPE_Header_Old = packed record
      nCompressionLevel: word;         // the compression level
      nFormatFlags: word;              // any format flags (for future use)
      nChannels: word;                 // the number of channels (1 or 2)
      nSampleRate: longword;           // the sample rate (typically 44100)
      nHeaderBytes: longword;          // the bytes after the MAC header that compose the WAV header
      nTerminatingBytes: longword;     // the bytes after that raw data (for extended info)
      nTotalFrames: longword;          // the number of frames in the file
      nFinalFrameBlocks: longword;     // the number of samples in the final frame
      nInt: integer;
   end;
   // New header for >= 3.98
   TAPE_Header_New = packed record
      nCompressionLevel : word;     // the compression level (see defines I.E. COMPRESSION_LEVEL_FAST)
      nFormatFlags      : word;   // any format flags (for future use) Note: NOT the same flags as the old header!
      nBlocksPerFrame   : longword;  // the number of audio blocks in one frame
      nFinalFrameBlocks : longword;  // the number of audio blocks in the final frame
      nTotalFrames      : longword;  // the total number of frames
      nBitsPerSample    : word;   // the bits per sample (typically 16)
      nChannels         : word;   // the number of channels (1 or 2)
      nSampleRate       : longword;  // the sample rate (typically 44100)
   end;

  PMonkey = ^TMonkey;
  TMonkey = object(TObj)
  private
    APE_CommonHeader: TAPE_Header;
    APE_Header: TAPE_Header_New;

    FValid :boolean;

    FPeakLevel: longword;
    FPeakLevelRatio: double;
    FTotalSamples: int64;
    FBitrate: double;
    FDuration: double;
    FCompressionModeStr: string;

    // FormatFlags, only used with Monkey's <= 3.97
    FHasPeakLevel: boolean;
    FHasSeekElements: boolean;
    FWavNotStored: boolean;
    // Tagging
    FID3v1: PID3v1;
    FID3v2: PID3v2;
    FAPE: PAPE;

    FFileSize: LongWord;

    procedure ResetData;

    function GetRatio: Double;
    procedure InitFields;
    function GetChannelMode: String;
    function GetVersionStr: string;

  public
    destructor Destroy; virtual;

    function ReadFromFile(const FileName: string): Boolean;   { Load header }

    property FileSize: LongWord read FFileSize;

    property ID: TMAC read APE_CommonHeader.cID;
    property Version: Word read APE_CommonHeader.nVersion;

    property CompressionLevel: word read APE_Header.nCompressionLevel;
    property SampleRate: LongWord read APE_Header.nSamplerate;
    property Channels: Word read APE_Header.nChannels;
    property FormatFlags: Word read APE_Header.nFormatFlags;
    property Bits: Word read APE_Header.nBitsPerSample;

    property VersionStr: string read GetVersionStr;


    property Valid: boolean read FValid;
    property ChannelMode: String read GetChannelMode;
    property Bitrate: double read FBitrate;
    property Duration: double  read FDuration;
    property PeakLevel: longword read FPeakLevel;
    property PeakLevelRatio: double  read FPeakLevelRatio;
    property TotalSamples: int64    read FTotalSamples;
    property CompressionModeStr: string  read FCompressionModeStr;
    // FormatFlags, only used with Monkey's <= 3.97
    property HasPeakLevel: boolean read FHasPeakLevel;
    property HasSeekElements: boolean read FHasSeekElements;
    property WavNotStored: boolean read FWavNotStored;
    // Tagging
    property ID3v1: PID3v1 read FID3v1;                    { ID3v1 tag data }
    property ID3v2: PID3v2 read FID3v2;                    { ID3v2 tag data }
    property APEtag: PAPE read FAPE;                   { APE tag data }

    property Ratio: Double read GetRatio;          { Compression ratio (%) }
  end;

  function NewMonkey: PMonkey;

implementation

function NewMonkey: PMonkey;
begin
    New(Result, Create);
    Result.InitFields;
end;

type
   { Real structure of Monkey's Audio header }

   // data descriptor for >= 3.98
   APE_DESCRIPTOR = packed record
    padded : Word;                   // padding/reserved (always empty)
    nDescriptorBytes,              // the number of descriptor bytes (allows later expansion of this header)
    nHeaderBytes,               // the number of header APE_HEADER bytes
    nSeekTableBytes,              // the number of bytes of the seek table
    nHeaderDataBytes,              // the number of header data bytes (from original file)
    nAPEFrameDataBytes,           // the number of bytes of APE frame data
    nAPEFrameDataBytesHigh,          // the high order number of APE frame data bytes
    nTerminatingDataBytes : longword; // the terminating data of the file (not including tag data)
    cFileMD5 : array[0..15] of Byte; // the MD5 hash of the file (see notes for usage... it's a littly tricky)
   end;

{ ********************** Private functions & procedures ********************* }

procedure TMonkey.ResetData;
begin
    FValid             := False;
    FillChar(APE_CommonHeader, SizeOf(TAPE_Header), 0);
    FillChar(APE_Header, SizeOf(TAPE_Header_New), 0);

    FPeakLevel:= 0;
    FPeakLevelRatio      := 0.0;
    FTotalSamples        := 0;
    FBitrate          := 0.0;
    FDuration        := 0.0;
    FCompressionModeStr  := '';
    FHasPeakLevel        := false;
    FHasSeekElements     := false;
    FWavNotStored        := false;
    FFileSize          := 0;
    FID3v1.ResetData;
    FID3v2.ResetData;
    FAPE.ResetData;
end;

{ ********************** Public functions & procedures ********************** }

{ --------------------------------------------------------------------------- }

destructor TMonkey.Destroy;
begin
  { Destroy object }
  ResetData;
  FID3v1.Free;
  FID3v2.Free;
  FAPE.Free;
  inherited;
end;

{ --------------------------------------------------------------------------- }

function TMonkey.ReadFromFile(const FileName: string): Boolean;
var
   f : PStream;
   APE_OLD        : TAPE_HEADER_OLD; // old header   <= 3.97
   APE_DESC       : APE_DESCRIPTOR; // extra header >= 3.98
   BlocksPerFrame : integer;
   LoadSuccess    : boolean;
   TagSize        : integer;
begin
   Result := FALSE;
   ResetData;

   FID3v2.ReadFromFile(FileName);
   FID3v1.ReadFromFile(FileName);
   FAPE.ReadFromFile(FileName);

   TagSize := 0;
   if FID3v1.Exists  then inc(TagSize, 128);
   if FID3v2.Exists  then inc(TagSize, FID3v2.Size);
   if FAPE.Exists then inc(TagSize, FAPE.Size);

   LoadSuccess := FALSE;
   f:=nil;
   try
      try
         f:= NewReadFileStreamW(FileName);
         FFileSize := f.Size;
         // seek past id3v2-tag
         if FID3v2.Exists then begin
            f.Seek(FID3v2.Size, spBegin);
         end;
         // Read APE Format Header
         FillChar(APE_CommonHeader, SizeOf(TAPE_Header), 0);
         if (f.Read(APE_CommonHeader, SizeOf(TAPE_Header)) = SizeOf(TAPE_Header)) and ( StrLComp(@APE_CommonHeader.cID[0],'MAC ',4)=0) then begin

            // Load New Monkey's Audio Header for version >= 3.98
            if APE_CommonHeader.nVersion >= 3980 then begin
               FillChar(APE_DESC, SizeOf(APE_DESC), 0);

               if (f.Read(APE_DESC, SizeOf(APE_DESC)) = SizeOf(APE_DESC)) then begin

                  // seek past description header
                  if APE_DESC.nDescriptorBytes <> 52 then f.Seek(APE_DESC.nDescriptorBytes - 52, spCurrent);
                  // load new ape_header
                  if APE_DESC.nHeaderBytes > SizeOf(TAPE_Header_New) then APE_DESC.nHeaderBytes := SizeOf(TAPE_Header_New);
                  FillChar(APE_Header, SizeOf(TAPE_Header_New), 0);
                  if (longword(f.Read(APE_Header, APE_DESC.nHeaderBytes)) = APE_DESC.nHeaderBytes ) then begin
                     // based on MAC SDK 3.98a1 (APEinfo.h)
                     //.....
                     // calculate total uncompressed samples
                     if APE_Header.nTotalFrames>0 then begin
                        FTotalSamples     := Int64(APE_Header.nBlocksPerFrame) *
                                             Int64(APE_Header.nTotalFrames-1) +
                                             Int64(APE_Header.nFinalFrameBlocks);
                     end;
                     LoadSuccess := TRUE;
                  end;
               end;
            end else begin
               // Old Monkey <= 3.97
               FillChar(APE_OLD, SizeOf(APE_OLD), 0);
               if (f.Read(APE_OLD, SizeOf(APE_OLD)) = SizeOf(APE_OLD) ) then begin
                  APE_Header.nCompressionLevel  := APE_OLD.nCompressionLevel;
                  APE_Header.nSampleRate       := APE_OLD.nSampleRate;
                  APE_Header.nChannels         := APE_OLD.nChannels;
                  APE_Header.nFormatFlags      := APE_OLD.nFormatFlags;

                  APE_Header.nBitsPerSample := 16;
                  if APE_OLD.nFormatFlags and MONKEY_FLAG_8_BIT  <>0 then APE_Header.nBitsPerSample :=  8;
                  if APE_OLD.nFormatFlags and MONKEY_FLAG_24_BIT <>0 then APE_Header.nBitsPerSample := 24;

                  FHasSeekElements  := APE_OLD.nFormatFlags and MONKEY_FLAG_PEAK_LEVEL    <>0;
                  FWavNotStored     := APE_OLD.nFormatFlags and MONKEY_FLAG_SEEK_ELEMENTS <>0;
                  FHasPeakLevel     := APE_OLD.nFormatFlags and MONKEY_FLAG_WAV_NOT_STORED<>0;
                  if FHasPeakLevel then begin
                     FPeakLevel        := APE_OLD.nInt;
                     FPeakLevelRatio   := (FPeakLevel / (1 shl APE_Header.nBitsPerSample) / 2.0) * 100.0;
                  end;
                  // based on MAC_SDK_397 (APEinfo.cpp)
                  if (APE_CommonHeader.nVersion >= 3950) then
                     BlocksPerFrame := 73728 * 4
                  else if (APE_CommonHeader.nVersion >= 3900) or ((APE_CommonHeader.nVersion >= 3800) and (APE_OLD.nCompressionLevel = MONKEY_COMPRESSION_EXTRA_HIGH)) then
                     BlocksPerFrame := 73728
                  else
                     BlocksPerFrame := 9216;
                  // calculate total uncompressed samples

                  if APE_OLD.nTotalFrames>0 then begin
                     FTotalSamples :=  Int64(APE_OLD.nTotalFrames-1) *
                                       Int64(BlocksPerFrame) +
                                       Int64(APE_OLD.nFinalFrameBlocks);
                  end;
                  LoadSuccess := TRUE;
               end;
            end;

            if LoadSuccess then begin
               // compression profile name
               if ((APE_Header.nCompressionLevel mod 1000) = 0) and (APE_Header.nCompressionLevel<=6000) then begin
                  FCompressionModeStr := MONKEY_COMPRESSION[APE_Header.nCompressionLevel div 1000];
               end else begin
                  FCompressionModeStr := Int2Str(APE_Header.nCompressionLevel);
               end;
               // length
               if APE_Header.nSampleRate>0 then FDuration := FTotalSamples / APE_Header.nSampleRate;
               // average bitrate
               if FDuration>0 then FBitrate := (FFileSize - Int64(TagSize))*8.0 / (FDuration/1000.0);
               // some extra sanity checks
               FValid   := (APE_Header.nBitsPerSample>0) and (APE_Header.nSampleRate>0) and (FTotalSamples>0) and (APE_Header.nChannels>0);
               Result   := FValid;
            end;

         end;
      finally
         f.free;
      end;
   except
   end;
end;
   
{ --------------------------------------------------------------------------- }

function TMonkey.GetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    Result := FFileSize / (FTotalSamples * (APE_Header.nChannels * APE_Header.nBitsPerSample / 8) + 44) * 100
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

procedure TMonkey.InitFields;
begin
  FID3v1:= NewID3v1;
  FID3v2:= NewID3v2;
  FAPE:= NewAPE;
  ResetData;
end;

function TMonkey.GetChannelMode: String;
const MONKEY_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');
begin
    Result:= MONKEY_MODE[APE_Header.nChannels];
end;

function TMonkey.GetVersionStr: string;
begin
    Str(APE_CommonHeader.nVersion / 1000 : 4 : 2, Result);
end;

end.

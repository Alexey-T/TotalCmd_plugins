{ TMidiFile2

 The component for MIDI File Reader.
 You can get various information of MIDI file such as format type, number of tracks, playback
  length and others, with this component.

  note) The source units of Tnt Delphi UNICODE Controls are needed if you compile this unit
        with Delphi 2007 or earlier version.

 Author : Silhwan Hyun   (e-mail addr : hyunsh@hanafos.com)

 Contibutors
   Emil Weiss : Test & Advice for update/debug

 #### Copyright Notice ####
 Copyright 2011 - 2015 Silhwan Hyun, All Rights Reserved

   This Program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.

   This Program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

  Revision History
 --------------------------
  Ver 0.9.7     11 Oct 2015
   - Added procedure InvalidateMidiData
   - Changed procedure ClearList to avoid potential list index error
   - Fixed getting errorneous value of function Tick2TimePos for the case 1st tempo event
     does not start at tick pos 0.
   - (for type 0 MIDI files) Split single track into multiple tracks, separating each into
     a separate track.
   - Added property Notes in TMidiTrack class, which holds the number of notes.

  Ver 0.9.6.2   5 Apr 2014
   - Fix package conflict with Tnt Unicode Controls VCL
    ( The referenced unit TntClasses is substituted with TntCollection )

  Ver 0.9.6.1   2 Jun 2013
   - Bug fix at getting FLyricsTrack

  Ver 0.9.6     2 May 2013
   - Added property TimeSinature

  Ver 0.9.5.1  11 Feb 2013
   - Some addtions to accept non-standard Karaoke files and RIFF MIDI Files(*.RMI)
   - Bug fix : The value of FIsValid is not set to false if exception occurs at executing
      function "GetInfo".

  Ver 0.9.5    10 Jun 2012
   - Modifications to read the lyrics recorded with MIDI Karaoke file type 1(Soft Karaoke) format.
   - Added property IsSoftKaraoke which shows whether opened MIDI file is MIDI Karaoke file type 1
     (Soft Karaoke) or not.

  Ver 0.9.4    07 Apr 2012
   - Added property LyricsTrack
   - Added property SyncLyrics

  Ver 0.9.3    27 Aug 2011
   - Fixed bug : incorrect result of function Time2TickPos
   - Added property Lyrics ( = whole lyrics in plain text)
     note) You also can get the lyrics with time information from property RawLyrics of TMidiTrack.

  Ver 0.9.2    09 Jul 2011
   - Fixed incorrect result of function Time2TickPos
   - Added function Tick2TimePos

  Ver 0.9.1    26 Jun 2011
   - Replaced data interface function "ReadMidiFile" with component "TMidiFile".

  Ver 0.9.0    05 Jun 2011
   - Initial release      (function "ReadMidiFile" is used for data interface)
'----------------------------------------------------------------------------}

unit MidiFile2;

interface

uses Windows, KOL;

type
  {$IFNDEF UNICODE}
  UnicodeString = WideString;
  {$ENDIF}

  TMidiEvent = record
    Event: Byte;
    Data1: Byte;
    Data2: Byte;
    Msg: AnsiString;
    Positon: LongWord;
  end;
  PMidiEvent = ^TMidiEvent;

  TTempoData = record
    TickPos: LongWord;
    TimePos: LongWord;
    Tempo: LongWord;
  end;
  PTempoData = ^TTempoData;

  TTimeSig = record
    Numerator: byte;   // numerator
    Denominator: byte; // denominator
    Metronome: byte;   // metronome pulse
    Notes: byte;       // The number of 32nd notes per 24 MIDI clock signals
  end;

  TRawLyric = record
     Position: Integer;
     Lyric: AnsiString;
  end;
  TRawLyrics = array of TRawLyric;

  PMidiTrack = ^TMidiTrack;
  TMidiTrack = object(TObj)
  private
    FEventList: PList;
    FFoundKaraokeId: boolean;
    FFoundKaraokeId2: boolean;
    FRawLyrics: TRawLyrics;
    FActive: Boolean;
    FEndOfTrack: Boolean;  // True : playing position reached the end of track
    FTrackName: AnsiString;
    FTrackKeyword: AnsiString;
    FCopyright: AnsiString;
    FInstrument: AnsiString;
    FNotes: integer;
    FPlayPos: Integer;
  protected
    function GetEventCount: Integer;
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure AddEvent(Event: PMidiEvent);
    function GetEvent(Index: Integer): PMidiEvent;
   // function GetChannels(Index: Integer): Boolean;
  public
    property Active: Boolean read FActive write FActive;
    property RawLyrics: TRawLyrics read FRawLyrics;
    property EndOfTrack: Boolean read FEndOfTrack write FEndOfTrack;
    property PlayPos: Integer read FPlayPos write FPlayPos;
    property TrackKeyword: AnsiString read FTrackKeyword;
    property TrackName: AnsiString read FTrackName;
    property Copyright: AnsiString read FCopyright;
    property Instrument: AnsiString read FInstrument;
    property Notes: integer read FNotes;  // Number of notes  (* added at ver 0.9.7)
    property EventCount: Integer read GetEventCount;
  end;

  TMidiFileInfo = record
    FileSize: Int64;    { File size (bytes) }
    Format: Word;       { 0: single-track, 1: multiple tracks, synchronous, 2: multiple tracks, asynchronous }
    Tracks: Word;
    Tempos: Word;
    TrackList: PList;
    TempoList: PList;
    TimeSig: TTimeSig;
    TickUnit: Word;      { 0: Ticks per quarter note, 1: Ticks per second }
    Ticks: Word;         { Ticks per quarter note or Ticks per second }
    PlayTime: LongWord;  { Play length in mili seconds }
    PlayTicks: LongWord; { Play length in ticks }
  end;

  PMidiFile2 = ^TMidiFile2;
  TMidiFile2 = object(TObj)
  private
    FIsValid: Boolean;
    FFormat: Word;
    FTimeSignature: TTimeSig;
    FIsSoftKaraoke: boolean;
    FTickUnit: Word;
    FTicksPerQuarter: Word;
    FTrackList: PList;
    FTempoList: PList;
    FTrackCount: word;
    FTempoCount: word;
    FDuration: LongWord;
    FPlayTicks: LongWord;
    FLyricsTrack: Integer;
    FLyrics: AnsiString;
    procedure ClearList;
    function  GetSyncLyrics: TRawLyrics;
  public
    constructor Create;
    destructor Destroy; virtual;
    function ReadFromFile(const FileName: WideString): Boolean; { Load data }
    procedure Clear;
    procedure InvalidateMidiData;                   { Clear loaded data }
    property Valid: Boolean read FIsValid;          { True if file valid }
    property Format: Word read FFormat;             { 0: single-track, 1: multiple tracks, synchronous, 2: multiple tracks, asynchronous }
    property TimeSignature: TTimeSig read FTimeSignature; { Get Time Signature }
    property IsSoftKaraoke: boolean read FIsSoftKaraoke;  { True if Soft Karaoke file }
    property TickUnit: Word read FTickUnit;         { 0: Ticks per quarter note, 1: Ticks per second }
    property TicksPerQuarter: Word read FTicksPerQuarter;
    property TrackCount: word read FTrackCount;     { Number of tracks }
    property TempoCount: word read FTempoCount;     { Number of tempos }
    function GetTrack(Index: Integer): PMidiTrack;  { Gets an item of TMidiTrack }
    function GetTempo(Index: Integer): PTempoData;  { Gets an item of tempo data }
    property Duration: LongWord read FDuration;     { Play length in milliseconds }
    property PlayTicks: LongWord read FPlayTicks;   { Play length in ticks }
    property LyricsTrack: Integer read FLyricsTrack;{ The track where lyrics is recorded }
    property Lyrics: AnsiString read FLyrics;       { The plain lyrics text }
    property SyncLyrics: TRawLyrics read GetSyncLyrics; { The sync lyrics data }
    function Time2TickPos(TimeVal: LongWord): LongWord; { Get position in ticks from position in time(milliseconds) }
    function Tick2TimePos(TickVal: LongWord): LongWord; { Get position in time(milliseconds) from position in ticks }
  end;

// function ReadMidiFile(const FileName: UnicodeString; var Info: TMidiFile2Info): Boolean;

function NewMIDI: PMidiFile2;

implementation

const
  RIFF_ID: AnsiString = 'RIFF';
  MIDI_ID: AnsiString = 'MThd';
  TRACK_ID: AnsiString = 'MTrk';
 // End_Of_Track = $FF2F00;
  DefaultTempo = 500000;  // 500000 microseconds per quarter note = 0.5 second per quarter note.

type
  TFileID = array[1..4] of AnsiChar;

  TTempoMap = array of TTempoData;

{ TMidiTrack }

function NewMidi: PMidiFile2;
begin
  New(Result, Create);
end;

function NewMidiTrack: PMidiTrack;
begin
  New(Result, Create);
end;

constructor TMidiTrack.Create;
begin
  inherited;
  FEventList := NewList;
  FActive := true;
  FEndOfTrack := False;
  FTrackName := '';
  FTrackKeyword := '';
  FCopyright := '';
  FInstrument := '';
  FNotes := 0;
  FPlayPos := 0;
end;

destructor TMidiTrack.Destroy;
var
  i: integer;
begin
  Finalize(FTrackName);
  Finalize(FTrackKeyword);
  Finalize(FCopyright);
  Finalize(FInstrument);
  for i := 0 to FEventList.Count - 1 do
  begin
    Finalize(PMidiEvent(FEventList^.Items[i])^);
    Dispose(PMidiEvent(FEventList^.Items[i]));
  end;
  FEventList.Free;
  Finalize(FRawLyrics);
  SetLength(FRawLyrics, 0);
  inherited;
end;

procedure TMidiTrack.AddEvent(Event: PMidiEvent);
var
  N: Integer;
begin
  if (Event^.Event = $FF) then
  begin
    case Event^.Data1 of
    // ** MIDI Karaoke file type 1 (Soft Karaoke) uses data type code $1 for recording lyrics
    // ** instead of data type code $5.
    // ** The identifier of this file type is text "@KMIDI KARAOKE FILE" as the first text at position 0.
    // ** (Applied at ver 0.9.5)
    // ** Some additions to accept non-standard Karaoke files. (ver 0.9.5.1)
      $1: begin
           if FTrackKeyword = '' then
             if (Event^.Positon = 0) and (Event^.Msg = '@KMIDI KARAOKE FILE') then
               FFoundKaraokeId := true
             else
           // The name of Track 1 = "Soft Karaoke", Track 2 = "Words"
               if FFoundKaraokeId2 and (FTrackName = 'Words') then
                 FFoundKaraokeId := true;

          // For very suspicious karaoke file
           if not FFoundKaraokeId then
             if FTrackName = 'Words' then
               if High(FRawLyrics) = -1 then
                 if pos('@L', FTrackKeyword) <> 0 then
                   if pos('@T', FTrackKeyword) <> 0 then
                     FFoundKaraokeId := true;

           FTrackKeyword := FTrackKeyword + Event^.Msg;
           if FFoundKaraokeId and (Event^.Positon > 0) then
             if (FTrackName = 'Words') then
             begin
               N := High(FRawLyrics) + 1;  // M : the number of recored lyrics
               SetLength(FRawLyrics, N + 1);
               FRawLyrics[N].Position := Event^.Positon;
             // Replace preceding '\' or '/' with chr(13).
               if Event^.Msg <> '' then
                 if (Event^.Msg[1] = '\') or (Event^.Msg[1] = '/') then
                   Event^.Msg := chr(13) + copy(Event^.Msg, 2, length(Event^.Msg) - 1);
               FRawLyrics[N].Lyric := Event^.Msg;
             // Remove the first preceding '\' character.
               if (N = 0) and (Event^.Msg <> '') then
                 if FRawLyrics[N].Lyric[1] = chr(13) then
                 begin
                   FRawLyrics[N].Lyric := copy(FRawLyrics[N].Lyric, 2, length(FRawLyrics[N].Lyric) - 1);
                   Event^.Msg := FRawLyrics[N].Lyric;
                 end;
             end;
         end;
      $2: FCopyright := FCopyright + Event^.Msg;
      $3: FTrackName := FTrackName + Event^.Msg;
      $4: FInstrument := FInstrument + Event^.Msg;
      $5: begin    // **
           N := High(FRawLyrics) + 1;  // N : the number of recored lyrics
           SetLength(FRawLyrics, N + 1);
           FRawLyrics[N].Position := Event^.Positon;
           FRawLyrics[N].Lyric := Event^.Msg;
         end;
    end;
  end else
  begin
   { case Event^.iEvent of
      $B0..$BF, $C0..$CF: // control change, program change
        FChannels[Event^.iEvent and $F] := True;
    end; }
  end;

  FEventList.Add(Event);
end;

function TMidiTrack.GetEvent(Index: Integer): PMidiEvent;
begin
  if (Index >= 0) and (Index < FEventList.Count) then
    Result := PMidiEvent(FEventList^.Items[Index]) else
    Result := nil;
end;

function TMidiTrack.GetEventCount: Integer;
begin
  Result := FEventList.Count;
end;


{function TMidiTrack.GetChannels(Index: Integer): Boolean;
begin
  Result := FChannels[Index];
end; }

function GetInfo(const FileName: UnicodeString; var Info: TMidiFileInfo): Boolean;
var
  SourceFile, FileStream: PStream;

  FileID: TFileID;
  TrackID: TFileID;
  buf: array[1..4] of byte;
  TrackDataSize: integer;
  I, N1: integer;
  DeltaTime: LongWord;
  Tempo: LongWord;
  Len: word;
  EndOfTrack: boolean;
  DataBytes: integer;

  Elapsed: LongWord;
  ElapsedTime: array of LongWord;
  MaxTime: LongWord;
  Play_Time: double;

  Status, RunningStatus: byte;
  SysExContinue: boolean;
  NextValue: byte;
  str: AnsiString;

  MidiTrack: PMidiTrack;
  FoundKaraokeId, FoundKaraokeId2: boolean;
  TempoCounter: word;
  TempoMap: TTempoMap;
  PTempo: PTempoData;
  pEvent1: PMidiEvent;

 function GetDelta(var Len_: word): LongWord;
 var
   m: LongWord;
   n: integer;
   b: byte;
   LastByte: boolean;
 begin
   m := 0;
   Len_ := 0;
   LastByte := false;

   for n := 0 to 4 do
   begin
    if SourceFile.Position >= (SourceFile.Size - 1) then
      break;
    SourceFile.Read(b, 1);
   { if (b = $FF) then
      break; }
    inc(Len_);
    if (b and $80) = 0 then
      LastByte := true;

    m := (m shl 7) + (b and $7f);
    if LastByte then
      break;
   end;

  result := m;
 end;

 procedure AddDeltaTime(DeltaTime_: LongWord; Track_: byte; Adding: boolean; var Elapsed_: LongWord);
 begin
   if DeltaTime_ = 0 then
     exit;

   if Adding then
   begin  // Add
     if Info.Format = 0 then
       Elapsed_ := Elapsed_ + DeltaTime_
     else begin
       ElapsedTime[Track_] := ElapsedTime[Track_] + DeltaTime_;
       Elapsed_ := ElapsedTime[Track_];
     end;
   end else
   begin  // Substract
     if Info.Format = 0 then
       Elapsed_ := Elapsed_ - DeltaTime_
     else begin
       ElapsedTime[Track_] := ElapsedTime[Track_] - DeltaTime_;
       Elapsed_ := ElapsedTime[Track_];
     end;
   end;
 end;

 procedure SaveChannelEvents(Status_: byte; Elapsed_: LongWord);
 var
   MessageType: byte;
   ChanData: array[1..2] of byte;
   pEvent: PMidiEvent;
 begin
   MessageType := Status_ and $F0;
   New(pEvent); // New event
   pEvent^.Event := Status_;
   pEvent^.Positon := Elapsed_;

 // $80 : Note off,  $90 : Note on,  $A0 : Note After touch,  $B0 : Control change
 // $C0 : Program change,  $D0 : Channel after touch,  $E0 : Pitch wheel change
   case MessageType of
     $80, $90, $A0, $B0, $E0 : begin
             SourceFile.Read(ChanData, 2);
             pEvent^.Data1 := ChanData[1];
             pEvent^.Data2 := ChanData[2];
             if (MessageType = $90) and (pEvent^.Data2 > 0) then
               MidiTrack.FNotes := MidiTrack.FNotes + 1;
           end;

     $C0, $D0 : begin
             SourceFile.Read(ChanData, 1);
             pEvent^.Data1 := ChanData[1];
           end;
   end;

   MidiTrack.AddEvent(pEvent);
 end;

 procedure SaveMetaEvent(Status_: byte; Elapsed_: LongWord);
 var
   DataBytes_: word;
   DataType: byte;
   Len2: word;
   pEvent: PMidiEvent;
   str2: AnsiString;
 begin
   { DataType  Description
        0      Set track¡¯s sequence #
      $01      User Text
      $02      Copyright info.
      $03      Track name
      $04      Instrument Names
      $05      Lyric
      $06      Marker
      $07      Cue Point
      $08      Program Name
      $09      Device Name
      $20      MIDI channel prefix assignment
      $21      MIDI port
      $51      Set tempo (microseconds per quarter note)
      $54      SMPTE Offset
      $58      Time signature
      $59      Key signature
      $7f      Sequencer specific
     end; }

   SourceFile.Read(DataType, 1);  // Read type code of Meta event
   DataBytes_ := GetDelta(Len2);  // Read the length of data
   SetLength(str2, DataBytes_);
   SourceFile.Read(str2[1], DataBytes_); // Read text
   if DataType = $2f then        // End of Track ?
     EndOfTrack := true
   else if DataType = $51 then  // Set tempo
   begin
     Tempo := (byte(str2[1]) shl 16) + (byte(str2[2]) shl 8) + byte(str2[3]);
     if TempoCounter <> 0 then
       if TempoMap[TempoCounter-1].TickPos = Elapsed then
         exit;

     // **** Added at v0.9.7
     // Added to fix getting errorneous value of function Tick2TimePos for the case
     // that the 1st tempo event does not start at tick pos 0.
     if (TempoCounter = 0) and (Elapsed > 0) then
     begin
       inc(TempoCounter);
       SetLength(TempoMap, TempoCounter);
       TempoMap[TempoCounter-1].TickPos := 0;
       TempoMap[TempoCounter-1].Tempo := DefaultTempo;
     end;

     inc(TempoCounter);
     SetLength(TempoMap, TempoCounter);
     TempoMap[TempoCounter-1].TickPos := Elapsed;
     TempoMap[TempoCounter-1].Tempo := Tempo;
   end
   else if DataType = $58 then   // Set Time signature
   begin
   // default value : Numerator = 4,  Denominator = 4
     if (DataBytes_ >= 2) and (DataBytes_ <= 4) then
     begin
       Info.TimeSig.Numerator := byte(str2[1]);
       Info.TimeSig.Denominator := 1 shl byte(str2[2]);  // power of 2
       if DataBytes_ >= 3 then
         Info.TimeSig.Metronome := byte(str2[3]);
       if DataBytes_ >= 4 then
         Info.TimeSig.Notes := byte(str2[4]);
     end
   end;

   New(pEvent);
   pEvent^.Event := Status_;
   pEvent^.Positon := Elapsed_;
   pEvent^.Data1 := DataType;
   pEvent^.Msg := str2;
   MidiTrack.AddEvent(pEvent);
 end;


label Raise_Exception;

begin
  { Get info from file }
  Result := false;
  SourceFile := nil;

  FileStream := NewReadFileStreamW(FileName);
  try
    if FileStream.Size <= (1024 * 1024) then
    begin
      SourceFile := NewMemoryStream;
      Stream2Stream(SourceFile, FileStream, FileStream.Size);
      SourceFile.Position := 0;
    end else
      SourceFile := FileStream;

    Info.FileSize := SourceFile.Size;
    SourceFile.Read(FileID, 4); // Read File ID. (should be 'MThd')

  // ** Added sentences for RIFF file at Ver 0.9.5.1
    // if (formatIdentifier == "RIFF")
		//		{
					// RMID file as documented: http://www.midi.org/about-midi/rp29spec(rmid).pdf
					// RIFF Chunk size
		//			binaryReader.SkipBytes(4);

		//			if (binaryReader.ReadFixedLengthString(8) != "RMIDdata")
		//				throw new InvalidDataException("Expected 'RMIDdata' identifier after RIFF header.");

					// MIDI size
		//			binaryReader.SkipBytes(4);

					// Actual MIDI header should now begin
		//			formatIdentifier = binaryReader.ReadFixedLengthString(4);
		//		}

    if FileID = RIFF_ID then
    begin
      SourceFile.Seek($14, spBegin);
      SourceFile.Read(FileID, 4);
    end;

    if FileID <> MIDI_ID then
      goto Raise_Exception;//('File has invalid MIDI Header ID.'#13#10#13#10 + ' => ' + FileName);

    SourceFile.Read(buf, 4);    // Read Header Length (should be $00000006)
    if (buf[1] <> 0) or (buf[2] <> 0) or (buf[3] <> 0) or (buf[4] <> 6) then
      goto Raise_Exception;//('File has invalid MIDI Header length.'#13#10#13#10 + ' => ' + FileName);

    SourceFile.Read(buf, 2);    // Read Format Type ($00 ~ $02)
    Info.Format := buf[1] * 256 + buf[2];

    SourceFile.Read(buf, 2);    // Read number of tracks (1 ~ 65,535)
    Info.Tracks := buf[1] * 256 + buf[2];

    SourceFile.Read(buf, 2);    // Read Time Division
    Info.TickUnit := buf[1] shr 7;  // Take MSB of buf[1]
    if Info.TickUnit = 0 then   // 0: Ticks per quarter note
      Info.Ticks := buf[1] * 256 + buf[2]
    else                        // none 0: Ticks per frame
      Info.Ticks := (128 - (buf[1] and $7f)) * buf[2];

    N1 := Info.Tracks;
    Info.TrackList := NewList;
    SetLength(ElapsedTime, Info.Tracks);
    for I := 1 to Info.Tracks do
      ElapsedTime[I-1] := 0;
    Tempo:= DefaultTempo;
    TempoCounter := 0;
    FoundKaraokeId := false;
    FoundKaraokeId2 := false;

    for I := 0 to (N1 - 1) do
    begin
      SourceFile.Read(TrackID, 4); // Read Track ID. (should be 'MTrk')
      if TrackID <> TRACK_ID then
        goto Raise_Exception; //('File has invalid MIDI Track ID.'#13#10#13#10 + ' => ' + FileName);

      Elapsed := 0;
      MidiTrack := NewMidiTrack;
      if FoundKaraokeId then
        MidiTrack.FFoundKaraokeId := true;
      if FoundKaraokeId2 then
        MidiTrack.FFoundKaraokeId2 := true;
      Info.TrackList.Add(MidiTrack); // Add to track list

      SourceFile.Read(buf, 4);     // Read Track size
      TrackDataSize := (buf[1] shl 24) + (buf[2] shl 16) + (buf[3] shl 8) + buf[4];

    //  Info.TrackNames[I] := '';
      EndOfTrack := false;

      RunningStatus := 0;     // the running status byte
      SysExContinue := false; // whether we're in a multi-segment system exclusive message

      repeat
        DeltaTime := GetDelta(Len);
        SourceFile.Read(NextValue, 1);   // Read Status Byte
       // Are we continuing a sys ex?  If so, the next value should be $F7
        if (SysExContinue and (NextValue <> $F7)) then
          goto Raise_Exception; //(Format('Expected to find a system exclusive continue byte at,'#13#10#13#10 + '%d', [SourceFile.Position]));
       // Are we in running status?  Determine whether we're running and what the current status byte is.
        if ((NextValue and $80) = 0) then
        begin
          if (RunningStatus = 0) then
            goto Raise_Exception; //(Format('Status byte required for running status at,'#13#10#13#10 + '%d', [SourceFile.Position]));
       // Keep the last iteration's status byte, and now we're in running mode
          SourceFile.Position := SourceFile.Position - 1;   // backward to 1 byte.
          Status := RunningStatus;
        end else
        begin
        // Running status is only implemented for Voice Category messages (ie, Status is $80 to $EF).
          if (NextValue >= $80) and (NextValue <= $EF) then
            RunningStatus := NextValue
        // System Common Category messages (ie, Status of 0xF0 to 0xF7) cancel any running status.
        // note) RealTime Category messages (ie, Status of 0xF8 to 0xFF) do not effect running status in any way.
          else if (NextValue >= $F0) and (NextValue <= $F7) then
            RunningStatus := 0;
          Status := NextValue;
        end;

        AddDeltaTime(DeltaTime, I, true{Adding}, Elapsed);
        if (Status >= $80) and (Status <= $EF) then   // MIDI Channel Event ?
        begin
        // Handle channel events
          SaveChannelEvents(Status, Elapsed);
        end
        else if (Status = $FF) then                   // Meta Event ?
        begin
         // Handle meta events
          SaveMetaEvent(Status, Elapsed);
          if EndOfTrack then   // The DeltaTime at "End of Track" event should not be accounted.
            AddDeltaTime(DeltaTime, I, false{Adding}, Elapsed);
        end
        else if (Status >= $F8) and (Status <= $FE) then  // System Real-Time Message ?
        begin
          New(pEvent1);
          pEvent1^.Event := Status;
          pEvent1^.Positon := Elapsed;
          MidiTrack.AddEvent(pEvent1);
        end
        else if (Status >= $F1) and (Status <= $F6) then  // System Common Message ?
        begin
          New(pEvent1);
          pEvent1^.Event := Status;
          pEvent1^.Positon := Elapsed;
          if (Status = $F1) or (Status = $F3) then
          begin
            SourceFile.Read(buf, 1);
            pEvent1^.Data1 := buf[1];
          end else
          if (Status = $F2) then
          begin
            SourceFile.Read(buf, 2);
            pEvent1^.Data1 := buf[1];
            pEvent1^.Data2 := buf[2];
          end;
          MidiTrack.AddEvent(pEvent1);
        end
        else if (Status = $F0) or (Status = $F7) then   // System Exclusive Message ?
        begin
          DataBytes := GetDelta(Len);    // Read the length of data
          if DataBytes = 0 then   // ** for the case there is no data.
            Continue;
          SetLength(str, DataBytes);
          SourceFile.Read(str[1], DataBytes); // Read data
          if (Status = $F0) then
            if (ord(str[DataBytes]) <> $F7) then  // multi-segment Message ?
              SysExContinue := true;
          if (Status = $F7) then                  // Message continuations ?
            if (ord(str[DataBytes]) = $F7) then   // Found end marker ?
              SysExContinue := false;

          New(pEvent1);
          pEvent1^.Event := Status;
          pEvent1^.Positon := Elapsed;
          pEvent1^.Msg := str;
          MidiTrack.AddEvent(pEvent1);
        end;
      until EndOfTrack or (SourceFile.Position = (SourceFile.Size - 1));

      if SourceFile.Position = (SourceFile.Size - 1) then
        break;

      if MidiTrack.FFoundKaraokeId then
        FoundKaraokeId := true
      else if (I = 1) and (MidiTrack.TrackName = 'Soft Karaoke') then
        FoundKaraokeId2 := true;
    end;   // end of "for I := 1 to N do"

    if Info.Format = 0 then
      MaxTime := Elapsed
    else begin
      MaxTime := ElapsedTime[0];
      for I := 1 to (Info.TrackList.Count - 1) do
        if ElapsedTime[I] > MaxTime then
           MaxTime := ElapsedTime[I];
    end;

    Info.PlayTicks := MaxTime;
    if Info.TickUnit = 0 then     {0: Ticks per quarter note, 1: Ticks per second}
    begin
      if TempoCounter = 0 then
        Play_Time := (MaxTime / Info.Ticks) * (Tempo / 1000.0)
      else if TempoCounter = 1 then
        Play_Time := (MaxTime / Info.Ticks) * (TempoMap[0].Tempo / 1000.0)
      else begin
        if TempoMap[0].TickPos = 0 then
        begin
          Play_Time := 0;
          TempoMap[0].TimePos := 0
        end else begin
          Play_Time := ((TempoMap[0].TickPos) / Info.Ticks) * (DefaultTempo / 1000.0);
          TempoMap[0].TimePos := round(Play_Time);
        end;
        for I := 0 to TempoCounter - 2 do
        begin
          Play_Time := Play_Time
                     + ((TempoMap[I+1].TickPos - TempoMap[I].TickPos) / Info.Ticks) * (TempoMap[I].Tempo / 1000.0);
          TempoMap[I+1].TimePos := round(Play_Time);
        end;
        if MaxTime > TempoMap[TempoCounter - 1].TickPos then
          Play_Time := Play_Time
                     + ((MaxTime - TempoMap[TempoCounter - 1].TickPos) / Info.Ticks) * (TempoMap[TempoCounter - 1].Tempo / 1000.0);
      end;
      Info.PlayTime := round(Play_Time);
    end else
      Info.PlayTime := round((MaxTime / Info.Ticks) * 1000.0);

    Info.TempoList := NewList;
    if TempoCounter = 0 then
    begin
      Info.Tempos := 1;
      new(PTempo);
      PTempo^.TickPos := 0;
      PTempo^.TimePos := 0;
      PTempo^.Tempo := Tempo;
      Info.TempoList.Add(PTempo);
    end else
    begin
      Info.Tempos := TempoCounter;
      for I := 0 to (TempoCounter - 1) do
      begin
        new(PTempo);
        PTempo^.TickPos := TempoMap[I].TickPos;
        PTempo^.TimePos := TempoMap[I].TimePos;
        PTempo^.Tempo := TempoMap[I].Tempo;
        Info.TempoList.Add(PTempo);
      end;
    end;

    Result := true;
    Exit;

    Raise_Exception:
    begin
      Result := False;
    end;
  finally
    if (SourceFile <> FileStream) and (SourceFile <> nil) then
      SourceFile.Free;
    FileStream.Free;
    SetLength(ElapsedTime, 0);
    Finalize(str);
    SetLength(str, 0);
    SetLength(TempoMap, 0);
  end;

end;


{ function ReadMidiFile(const FileName: UnicodeString; var Info: TMidiFile2Info): Boolean;
begin
  result := GetInfo(FileName, Info);
end; }


constructor TMidiFile2.Create;
begin
  inherited;
  FTrackList := NewList;
  FTempoList := NewList;
  FLyricsTrack := -1;
end;

destructor TMidiFile2.Destroy;
begin
  Clear;
  FTrackList.Free;
  FTempoList.Free;
  inherited;
end;

procedure TMidiFile2.Clear;
begin
  Finalize(FLyrics);
  InvalidateMidiData;
end;

procedure TMidiFile2.ClearList;
var
  i: Integer;
begin
  FTrackCount := 0;
  FTempoCount := 0;

  if FTrackList.Count > 0 then
  begin
    for i := 0 to FTrackList.Count - 1 do
      PMidiTrack(FTrackList.Items[i]).Free;
    FTrackList.Clear;
  end;

  if FTempoList.Count > 0 then
  begin
    for i := 0 to FTempoList.Count - 1 do
      Dispose(PTempoData(FTempoList.Items[i]));
    FTempoList.Clear;
  end;
end;

//** Split single track into multiple tracks, separating each
// * channel into a separate track.
function SplitChannels(orgtrack: PMidiTrack): PList;
var
  firstTrack: PMidiTrack;
  newTrack: PMidiTrack;
  foundchannel: boolean;
  event: PMidiEvent;
  event2: PMidiEvent;
  aEvent: PMidiEvent;
  i, n: integer;
begin
  result := NewList;

  firstTrack := NewMidiTrack;
  result^.Add(firstTrack);   // first track is used to save non-channel event
  for i := 0 to orgTrack.EventCount - 1 do
  begin
    event := orgTrack.GetEvent(i);
    New(aEvent);
    aEvent^.Event := event^.Event;
    aEvent^.Positon := event^.Positon;
    aEvent^.Data1 := event^.Data1;
    aEvent^.Data2 := event^.Data2;
    aEvent^.Msg := event^.Msg;
    if (event^.Event > $EF) then  // not a channel event ?
    begin
      // save to track 0
      firstTrack.AddEvent(aEvent);
      foundchannel := true;
    end else
    begin
      foundchannel := false;
      for n := 1 to result.Count - 1 do
      begin
        event2 := PMidiTrack(result^.Items[n]).GetEvent(0);
        if (event^.Event and $0F) = (event2^.Event and $0F) then  // same channel nunmber ?
        begin
          foundchannel := true;
          PMidiTrack(result^.Items[n]).AddEvent(aEvent);
          break;
        end;
      end;
    end;
    if (not foundchannel) then
    begin
      newTrack := NewMidiTrack;
      newTrack.AddEvent(aEvent);
      result.Add(newTrack);
    end;
  end;

  if High(orgtrack.FRawLyrics) <> - 1 then
  begin
    SetLength(firstTrack.FRawLyrics, High(orgtrack.FRawLyrics) + 1);
    for n := 0 to High(orgtrack.FRawLyrics) do
      firstTrack.FRawLyrics[n] := orgtrack.FRawLyrics[n];
  end else
  // Remove first track if it contains no events then adjust FTrackCount value
  if firstTrack.FEventList.Count = 0 then
  begin
    firstTrack.Free;
    result^.Delete(0);
  end;

  // Adjust each track's FNotes value
  for i := 0 to result.Count - 1 do
  begin
    for N := 0 to PMidiTrack(result^.Items[i]).EventCount - 1 do
    begin
      event := PMidiTrack(result^.Items[i]).GetEvent(N);
      if ((event^.Event and $F0) = $90) and (event^.Data2 > 0) then
        PMidiTrack(result^.Items[i]).FNotes := PMidiTrack(result^.Items[i]).FNotes + 1;
    end;
  end;

  orgTrack.Free;
end;

function TMidiFile2.ReadFromFile(const FileName: WideString): Boolean;
var
  FileInfo: TMidiFileInfo;
  I, N: integer;
  _LyricsTrack: integer;
begin
  try
    InvalidateMidiData;
    FileInfo.TrackList := nil;
    FileInfo.TempoList := nil;
    FileInfo.TimeSig.Numerator := 4;
    FileInfo.TimeSig.Denominator := 4;
    FileInfo.TimeSig.Metronome := 0;   // ** Check if this item is non zero
    FileInfo.TimeSig.Notes := 8;
    FIsValid := GetInfo(FileName, FileInfo);
  except
  // We need to reset FIsValid.
    FIsValid := false;
  end;

  if FIsValid then
  begin
    FFormat := FileInfo.Format;
    FTickUnit := FileInfo.TickUnit;
    FTicksPerQuarter := FileInfo.Ticks;
    ClearList;
    FTrackCount := FileInfo.Tracks;
    FTempoCount := FileInfo.Tempos;
    FTimeSignature := FileInfo.TimeSig;
    _LyricsTrack := -1;
    FLyrics := '';

    FIsSoftKaraoke := false;
    if FileInfo.TrackList.Count = 1 then  // type 0 MIDI file  ?
    begin
      FTrackList.Free;  // FTrackList will be recreated
      FTrackList := SplitChannels(FileInfo.TrackList^.Items[0]);
      FTrackCount := FTrackList.Count;
      if High(PMidiTrack(FTrackList^.Items[0]).FRawLyrics) <> - 1 then
        _LyricsTrack := 0;
    end else
    for I := 0 to FTrackCount - 1 do
    begin
      FTrackList.Add(FileInfo.TrackList^.Items[I]);
      if PMidiTrack(FileInfo.TrackList^.Items[I]).FFoundKaraokeId then
        FIsSoftKaraoke := true;
      if High(PMidiTrack(FileInfo.TrackList^.Items[I]).FRawLyrics) <> - 1 then
      // Some karaoke files have several text tracks.
      // ** Changed at 2012-06-18
        if _LyricsTrack = -1 then
          _LyricsTrack := I
        else
          if High(PMidiTrack(FileInfo.TrackList^.Items[I]).FRawLyrics) >
             High(PMidiTrack(FileInfo.TrackList^.Items[_LyricsTrack]).FRawLyrics) then
            _LyricsTrack := I;
    end;

    FLyricsTrack := _LyricsTrack;
    if FLyricsTrack <> -1 then
    begin
      N := High(PMidiTrack(FileInfo.TrackList^.Items[FLyricsTrack]).FRawLyrics);
      for I := 0 to N do
        FLyrics := FLyrics + PMidiTrack(FileInfo.TrackList^.Items[FLyricsTrack]).FRawLyrics[I].Lyric;
     // ** Soft Karaoke files uses '\' for clear screen and '/' for new line.
     { if FIsSoftKaraoke then
      begin
        FLyrics := ReplaceStr(FLyrics, '\', chr(13));
        FLyrics := ReplaceStr(FLyrics, '/', chr(13));
      end; }
    end;

    for I := 0 to FTempoCount - 1 do
      FTempoList.Add(FileInfo.TempoList^.Items[I]);
    FDuration := FileInfo.PlayTime;
    FPlayTicks := FileInfo.PlayTicks;
    FIsValid := (FTrackCount > 0) and (FDuration > 0);
    result := true;
  end else
  begin
    FIsValid := false;
    result := false;
    if FileInfo.TrackList <> nil then
      if FileInfo.TrackList.Count > 0 then
      begin
        for i := 0 to FileInfo.TrackList.Count - 1 do
          PMidiTrack(FileInfo.TrackList.Items[i]).Free;
        FTrackCount := 0;
      end;
    if FileInfo.TempoList <> nil then
      if FileInfo.TempoList.Count > 0 then
      begin
        for i := 0 to FileInfo.TempoList.Count - 1 do
          Dispose(PTempoData(FileInfo.TempoList.Items[i]));
        FTempoCount := 0;
      end;
  end;

  if FileInfo.TrackList <> nil then
  begin
    FileInfo.TrackList.Clear;
    FileInfo.TrackList.Free;
  end;
  if FileInfo.TempoList <> nil then
  begin
    FileInfo.TempoList.Clear;
    FileInfo.TempoList.Free;
  end;
end;

procedure TMidiFile2.InvalidateMidiData;
begin
  FLyricsTrack := -1;
  ClearList;
  FIsValid := false;
end;

function TMidiFile2.GetTrack(Index: Integer): PMidiTrack;
begin
  if (Index >= 0) and (Index < FTrackList.Count) then
    Result := PMidiTrack(FTrackList^.Items[Index]) else
    Result := nil;
end;

function TMidiFile2.GetTempo(Index: Integer): PTempoData;
begin
  if (Index >= 0) and (Index < FTempoList.Count) then
    Result := PTempoData(FTempoList^.Items[Index]) else
    Result := nil;
end;

function TMidiFile2.Time2TickPos(TimeVal: LongWord): LongWord;  // milliseconds -> ticks
var
  I, K: integer;
  Residue: LongWord;
  NumQuarter: double;
begin
  if not FIsValid then
  begin
    result := 0;
    exit;
  end;

  if TimeVal = 0 then
  begin
    result := 0;
    exit;
  end;

 { if Val > FDuration then
  begin
    result := FPlayTicks;
    exit;
  end; }

  if FTickUnit = 1 then     {0: Ticks per quarter note, 1: Ticks per second}
  begin
    result := round(TimeVal * FTicksPerQuarter / 1000.0);
    exit;
  end;

  if FTempoCount = 1 then
  begin
    if PTempoData(FTempoList^.Items[0])^.TimePos = 0 then
      result := round(TimeVal * FTicksPerQuarter / PTempoData(FTempoList^.Items[0])^.Tempo * 1000.0)
    else if TimeVal < PTempoData(FTempoList^.Items[0])^.TimePos then
      result := round(TimeVal * FTicksPerQuarter / DefaultTempo * 1000.0)
    else begin
      Residue := TimeVal - PTempoData(FTempoList^.Items[0])^.TimePos;
      NumQuarter := (Residue * 1000.0) / PTempoData(FTempoList^.Items[0])^.Tempo;
      result := round(PTempoData(FTempoList^.Items[0])^.TickPos + NumQuarter * FTicksPerQuarter);
    end;

    exit;
  end;

  K := (FTempoCount - 1);
  for I := 1 to (FTempoCount - 1) do
    if PTempoData(FTempoList^.Items[I])^.TimePos > TimeVal then
    begin
      K := I - 1;
      break;
    end;

  if (K = 0) and (TimeVal < PTempoData(FTempoList^.Items[K])^.TimePos) then  // ** Added
    NumQuarter := (TimeVal * 1000.0) / DefaultTempo
  else begin
    Residue := TimeVal - PTempoData(FTempoList^.Items[K])^.TimePos;
    NumQuarter := (Residue * 1000.0) / PTempoData(FTempoList^.Items[K])^.Tempo;
  end;
  result := round(PTempoData(FTempoList^.Items[K])^.TickPos + NumQuarter * FTicksPerQuarter);
end;

function TMidiFile2.Tick2TimePos(TickVal: LongWord): LongWord;  // ticks -> milliseconds
var
  I, K: integer;
begin
  if not FIsValid then
  begin
    result := 0;
    exit;
  end;

  if TickVal = 0 then
  begin
    result := 0;
    exit;
  end;

  if FTickUnit = 0 then     {0: Ticks per quarter note, 1: Ticks per second}
  begin
    K := FTempoCount - 1;
    for I := (FTempoCount - 1) downto 0 do
    begin
      if TickVal < PTempoData(FTempoList^.Items[I])^.TickPos then
        dec(K);
    end;

    if K < 0 then
      result := round((TickVal / FTicksPerQuarter) * (DefaultTempo / 1000.0))
    else
      result := round(PTempoData(FTempoList^.Items[K])^.TimePos
              + ((TickVal - PTempoData(FTempoList^.Items[K])^.TickPos) / FTicksPerQuarter) * (PTempoData(FTempoList^.Items[K])^.Tempo / 1000.0));

  end else
    result := round((TickVal / FTicksPerQuarter) * 1000.0);

end;

function TMidiFile2.GetSyncLyrics: TRawLyrics;
begin
  if FLyricsTrack <> -1 then
    result := PMidiTrack(FTrackList^.Items[FLyricsTrack]).FRawLyrics
  else
    SetLength(result, 0);
end;

end.

unit SConvert;

interface

uses
  Windows, SysUtils,
  LConvEncoding;

type
  TMyCodepage = (
    cpUnknown,
    cpANSI,
    cpOEM,
    cpUTF8,
    cpUTF16,
    cpUTF16BE,
    cpUTF16LE,
    cpRTF
    );

const
  cMyCodepageNames: array[TMyCodepage] of string = (
    '',
    'ANSI',
    'OEM',
    'UTF8',
    'UTF16',
    'UTF16BE',
    'UTF16LE',
    'RTF'
    );


function Conv_WideData(const S: string; IsBE: boolean): Widestring;
function Conv_WideData_Detect(const S: string): Widestring;
function Conv_RTF(const Value: string): string;

function Conv_AnyCodepage(const S: string; CP: TMyCodepage): string;
function CodepageStringToCodepageId(const S: string): TMyCodepage;


implementation 

function CodepageStringToCodepageId(const S: string): TMyCodepage;
var
  i: TMyCodepage;
begin
  Result:= cpUnknown;
  for i in TMyCodepage do
    if S=cMyCodepageNames[i] then Exit(i);
end;

function Conv_AnsiToUtf8(const SA: string): string;
begin
  {$ifdef windows}
  case Windows.GetACP of
    1250: Result:= CP1250ToUTF8(SA);
    1251: Result:= CP1251ToUTF8(SA);
    1252: Result:= CP1252ToUTF8(SA);
    1253: Result:= CP1253ToUTF8(SA);
    1254: Result:= CP1254ToUTF8(SA);
    1255: Result:= CP1255ToUTF8(SA);
    1256: Result:= CP1256ToUTF8(SA);
    1257: Result:= CP1257ToUTF8(SA);
    1258: Result:= CP1258ToUTF8(SA);
    437: Result:= CP437ToUTF8(SA);
    else Result:= CP1250ToUTF8(SA);
  end;
  {$else}
  Result:= CP1250ToUTF8(SA);
  {$endif}
end;

function Conv_OemToUtf8(const SA: string): string;
begin
  {$ifdef windows}
  case Windows.GetACP of
    1250: Result:= CP852ToUTF8(SA);
    1251: Result:= CP866ToUTF8(SA);
    1252: Result:= CP850ToUTF8(SA);
    //1253: Result:= CP737ToUTF8(SA);
    //1255: Result:= CP862ToUTF8(SA);
    //1256: Result:= CP720ToUTF8(SA);
    //1257: Result:= CP775ToUTF8(SA);
    else Result:= CP437ToUTF8(SA);
  end;
  {$else}
  Result:= CP437ToUTF8(SA);
  {$endif}
end;
 
 
function Conv_AnyCodepage(const S: string; CP: TMyCodepage): string;
begin
  Result:= '';
  case CP of
    cpANSI:
      Result:= Conv_AnsiToUtf8(S);
    cpOEM:
      Result:= Conv_OemToUtf8(S);
    cpUTF8:
      Result:= S;
    cpUTF16:
      Result:= UTF8Encode(Conv_WideData_Detect(S));
    cpUTF16BE:
      Result:= UTF8Encode(Conv_WideData(S, true));
    cpUTF16LE:
      Result:= UTF8Encode(Conv_WideData(S, false));
    cpRTF:
      Result:= UTF8Encode(Conv_RTF(S));
  end;
end;

//-----------------------------------------------------------
function SetStringW(Buffer: PChar; BufSize: Integer; SwapBytes: Boolean): WideString;
var
  P: PChar;
  i, j: Integer;
  ch: char;
begin
  Result := '';
  if BufSize < 2 then Exit;

  SetLength(Result, BufSize div 2);
  Move(Buffer^, Result[1], Length(Result) * 2);

  if SwapBytes then
  begin
    P := @Result[1];
    for i := 1 to Length(Result) do
    begin
      j := (i - 1) * 2;
      ch := P[j];
      P[j] := P[j + 1];
      P[j + 1] := ch;
    end;
  end;
end;


function Conv_WideString(const S: WideString): string;
var
  i: integer;
begin
  SetLength(Result, Length(S));
  for i:= 1 to Length(S) do
    Result[i]:= Char(S[i]);
end;

function Conv_WideData(const S: string; IsBE: boolean): Widestring;
begin
  Result:= SetStringW(PChar(S), Length(S), IsBE);
end;

function Conv_WideData_Detect(const S: string): Widestring;
var
  IsBE: boolean;
begin
  IsBE:= (Length(S)>=2) and (S[1] = #$FE) and (S[2] = #$FF);
  Result:= Conv_WideData(S, IsBE);
end;

//-----------------------------------------------------------
function Conv_UsingCodepage(const S, CP: string): string;
var 
  i: integer; 
begin 
  Result:= ''; 
  for i := 1 to Length(S) do
  begin 
    if Ord(S[i]) < $80 then
      Result:= Result + S[i]
    else
      Result := Result + CP[Ord(S[i]) - $80]; 
  end;
end; 


//-----------------------------------------------------------
{
(C) Alex Demchenko(alex@ritlabs.com)
}

function HexToInt(Value: String): LongWord;
const
  HexStr: String = '0123456789abcdef';
var
  i: Word;
begin
  Result := 0;
  if Value = '' then Exit;
  for i := 1 to Length(Value) do
    Inc(Result, (Pos(Value[i], HexStr) - 1) shl ((Length(Value) - i) shl 2));
end;

{Convert RTF enabled text to plain.}
function Conv_RTF(const Value: string): string;
var
  i: Word;
  tag: Boolean;
  st: String;
begin
  Result := ''; tag := False; st := '';
  if Value = '' then Exit;
  if Copy(Value, 0, 6) <> '{\rtf1' then
  begin
    Result := Value;
    Exit;
  end;
  for i := 1 to Length(Value) do
  begin
    if Value[i] in ['\', '}', '{'] then
      tag := True;
    if Value[i + 1] in ['\', '}', '{'] then
    begin
      tag := False;
      if st <> '' then
      begin
        if st = 'par' then Result := Result + #13#10
        else if (st[1] = '''') and (Length(st) >= 3) then
        begin
          Delete(st, 1, 1);
          Result := Result + Chr(HexToInt(Copy(st, 0, 2))) + Copy(st, 3, Length(st) - 2);
        end
        else if ((Pos(' ', st) > 0) or ((Copy(st, 0, 3) = 'par') and (st <> 'pard'))) and (st[Length(st)] <> ';') then
        begin
          while (Pos(#13, st) > 0) do Delete(st, Pos(#13, st), 1);
          while (Pos(#10, st) > 0) do Delete(st, Pos(#10, st), 1);
          if Copy(st, 0, 3) = 'par' then
            Result := Result + #13#10 + Copy(st, 4, Length(st) - 3)
          else
            Result := Result + Copy(st, Pos(' ', st) + 1, Length(st) - Pos(' ', st));
        end;
      end;
      st := '';
    end;
    if tag then
      st := st + Value[i + 1];
  end;
end;


end. 

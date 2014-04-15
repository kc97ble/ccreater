unit token_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, multi_update, basic_database;

resourcestring
  rsTokenCBT = 'Token đã được bật';
  rsTokenAngBTT = 'Token đang bị tắt';

type

  { TToken }

  TToken = class (TPersistent, IBasicDatabase)
  public
    Initial, GenNumber, GenTime, Max, Total, MinInterval: Integer;
    InitialSet, GenNumberSet, GenTimeSet, MaxSet, TotalSet, MinIntervalSet: Boolean;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings);
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;
  end;

implementation

{ TToken }

procedure TToken.ExportToStream(List: TStrings);
begin
  YamlWriteIntegerIf(List, '', '',
    [InitialSet, GenNumberSet, GenTimeSet, MaxSet, TotalSet, MinIntervalSet],
    ['token_initial', 'token_gen_number', 'token_gen_time', 'token_max', 'token_total', 'token_min_interval'],
    [Initial, GenNumber, GenTime, Max, Total, MinInterval]);
end;

function TToken.IsValid(Message: TStrings): Boolean;
begin
  Result := True;
end;

procedure TToken.SaveToStream(List: TIniFile; const Section: String);
begin
  WriteBool(List, Section,
  ['InitialSet','GenNumberSet','GenTimeSet','MaxSet','TotalSet','MinIntervalSet'],
  [InitialSet, GenNumberSet, GenTimeSet, MaxSet, TotalSet, MinIntervalSet]);
  WriteInteger(List, Section,
  ['Initial', 'GenNumber', 'GenTime', 'Max', 'Total', 'MinInterval'],
  [Initial, GenNumber, GenTime, Max, Total, MinInterval]);
end;

procedure TToken.LoadFromStream(List: TIniFile; const Section: String);
begin
  ReadBool(List, Section,
  ['InitialSet','GenNumberSet','GenTimeSet','MaxSet','TotalSet','MinIntervalSet'],
  [@InitialSet, @GenNumberSet, @GenTimeSet, @MaxSet, @TotalSet, @MinIntervalSet],false);
  ReadInteger(List, Section,
  ['Initial', 'GenNumber', 'GenTime', 'Max', 'Total', 'MinInterval'],
  [@Initial, @GenNumber, @GenTime, @Max, @Total, @MinInterval],0);
end;

function TToken.Introduction: String;
begin
  if Self.InitialSet then
    Result := rsTokenCBT
  else
    Result := rsTokenAngBTT;
end;

end.


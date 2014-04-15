unit limit_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, basic_database, multi_update;

resourcestring
  rsNPDLN = ', nộp %d lần';
  rsDGiYMTLNNP = ', %d giây một lần nộp';
  rsTestDLN = ', test %d lần';
  rsDGiYMTLNTest = ', %d giây một lần test';
  rsKhNgCGiIHNNP = 'Không có giới hạn nộp bài';
  rsGiIHN = 'Giới hạn%s';

type

  { TLimit }

  TLimit = class (TPersistent, IBasicDatabase)
  public
    MSN, MUTN: Integer; // MaxSubmissionNumber, MaxUsetTestNumber
    MSI, MUTI: Integer; // MinSubmissionInterval, MinUserTestInterval
    MSNSet, MUTNSet: Boolean; // MaxSubmissionNumberSet, MaxUsetTestNumberSet
    MSISet, MUTISet: Boolean; // MinSubmissionIntervalSet, MinUserTestIntervalSet
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings);
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;
    procedure Assign(Source: TPersistent); override;
  end;

implementation

{ TLimit }

procedure TLimit.SaveToStream(List: TIniFile; const Section: String);
begin
  WriteBool(List, Section,
    ['MSNSet', 'MUTNSet', 'MSISet', 'MUTISet'],
    [MSNSet, MUTNSet, MSISet, MUTISet]);
  WriteInteger(List, Section,
    ['MSN', 'MUTN', 'MSI', 'MUTI'],
    [MSN, MUTN, MSI, MUTI]);
end;

procedure TLimit.LoadFromStream(List: TIniFile; const Section: String);
begin
  ReadBool(List, Section,
    ['MSNSet', 'MUTNSet', 'MSISet', 'MUTISet'],
    [@MSNSet, @MUTNSet, @MSISet, @MUTISet], False);
  ReadInteger(List, Section,
    ['MSN', 'MUTN', 'MSI', 'MUTI'],
    [@MSN, @MUTN, @MSI, @MUTI], 0);
end;

procedure TLimit.ExportToStream(List: TStrings);
begin
  YamlWriteIntegerIf(List, '', '',
    [MSNSet, MUTNSet, MSISet, MUTISet],
    ['max_submission_number', 'max_user_test_number', 'min_submission_interval', 'min_user_test_interval'],
    [MSN, MUTN, MSI, MUTI]);
end;
function TLimit.IsValid(Message: TStrings): Boolean;
begin
  Result := True;
end;

function TLimit.Introduction: String;
begin
  Result := '';
  if Self.MSNSet then
    Result += Format(rsNPDLN, [MSN]);
  if Self.MSISet then
    Result += Format(rsDGiYMTLNNP, [MSI]);
  if Self.MUTNSet then
    Result += Format(rsTestDLN, [MUTN]);
  if Self.MUTISet then
    Result += Format(rsDGiYMTLNTest, [MUTI]);
  if Result = '' then
    Result := rsKhNgCGiIHNNP
  else
    Result := Format(rsGiIHN, [Result]);
end;

procedure TLimit.Assign(Source: TPersistent);
var A: TLimit;
begin
  if Source is TLimit then
  begin
    A := Source as TLimit;
    WriteToVariable(
      [@MSNSet, @MUTNSet, @MSISet, @MUTISet, @MSN, @MUTN, @MSI, @MUTI],
      [A.MSNSet, A.MUTNSet, A.MSISet, A.MUTISet, A.MSN, A.MUTN, A.MSI, A.MUTI]);
  end
  else inherited Assign(Source);
end;

end.


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
  rsProblemSS = 'Problem "%s": "%s"';
  rsProblemNameS = 'Problem name should be a valid identifier';
  rsTimeLimitSho = 'Time limit should be strictly positive';
  rsMemoryLimitS = 'Memory limit should be stricly positive';
  rsStatementFil = 'Statement file not found';
  rsInputListAnd = 'Input list and output list has different size';
  rsInputListSho = 'Input list should not be empty';
  rsFileNotFound = 'File not found: %s';
  rsTokenError = 'Token error';
  rsLimitationEr = 'Limitation error';
  rsSubtaskListE = 'Subtask list error';
  rsMaxSubmissio = 'Max submission must be positive';
  rsMaxUserTestM = 'Max user test must be positive';
  rsMinSubmissio = 'Min submission interval must be positive';
  rsMinUserTestI = 'Min user test interval must be positive';
  rsLimitS = 'Limit: "%s"';

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
var
  ErrorCount: Integer=0;

 procedure ErrorIf(B: Boolean; S: String);
 begin
   if B then ErrorCount += 1 else exit;
   if S<>'' then Message.Add(Format(rsLimitS, [S]));
 end;

begin
  if MSNSet then ErrorIf(MSN<=0, rsMaxSubmissio);
  if MUTNSet then ErrorIf(MUTN<=0, rsMaxUserTestM);
//  if MSISet then ErrorIf(MSI<=0, rsMinSubmissio);
//  if MUTISet then ErrorIf(MUTI<=0, rsMinUserTestI);
  Result := ErrorCount=0;
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


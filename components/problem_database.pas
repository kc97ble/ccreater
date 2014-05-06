unit problem_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Math, FileUtil,
  basic_database, token_database, limit_database, multi_update, subtask_database;

const
  DefaultProblemName='problem';
  DefaultProblemTitle='Problem';

type

  { TProblem }

  TProblem = class (IBasicDatabase)
    Name, Title, StatementFile, Checker: String;
    MemLimit, TimeLimit: Integer;
    InputList, OutputList: TStringList; //
    InputStreamFile, OutputStreamFile, PublicTestCase: String;
    Token: TToken; //
    Limit: TLimit; //
    SubtaskList: TSubtaskList; //
    OnStep: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings); unimplemented;
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;
    function Execute(Dir: String; Message: TStrings): Boolean;
    function Estimate: Integer;
  end;


implementation


{ TProblem }

function TProblem.IsValid(Message: TStrings): Boolean;
var ErrorCount: Integer = 0;

  procedure ErrorIf(B: Boolean; S: String);
  begin
    if B then ErrorCount += 1 else exit;
    if S<>'' then Message.Add(Format(rsProblemSS, [Name, S]));
  end;

var
  S: String;
begin
  ErrorIf(not IsValidIdent(Name), rsProblemNameS);
  ErrorIf(TimeLimit<=0, rsTimeLimitSho);
  ErrorIf(MemLimit<=0, rsMemoryLimitS);
  ErrorIf(not FileExists(StatementFile) and not FileExists(DefaultStatementFile), rsStatementFil);
  ErrorIf(InputList.Count <> OutputList.Count, rsInputListAnd);
  ErrorIf(InputList.Count=0, rsInputListSho);
  for S in InputList do
    ErrorIf(not FileExists(S), Format(rsFileNotFound, [S]));
  for S in OutputList do
    ErrorIf(not FileExists(S), Format(rsFileNotFound, [S]));
  ErrorIf(not Token.IsValid(Message), rsTokenError);
  ErrorIf(not Limit.IsValid(Message), rsLimitationEr);
  ErrorIf(not SubtaskList.IsValid(Message), rsSubtaskListE);
  Result := ErrorCount=0;
end;

operator ** (A, B: String) C: String;
  begin C := ConcatPaths([A,B]); end;

function TProblem.Estimate: Integer; // how many file will be written
begin
  Result := 2 + InputList.Count + OutputList.Count;
end;

function TProblem.Execute(Dir: String; Message: TStrings): Boolean;
var
  List: TStrings;
  i: Integer;
procedure Step(B: Boolean=true);
  begin if Assigned(OnStep) then OnStep(Self); end;
begin
  Result := True;
  List := TStringList.Create;
  try
    ExportToStream(List);
    List.SaveToFile(Dir**(Name+'.yaml')); Step;

    { Task directory }
    ForceDirectories(Dir**Name);
    ForceDirectories(Dir**Name**'testo');
    ForceDirectories(Dir**Name**'input');
    ForceDirectories(Dir**Name**'output');
    if StatementFile='' then
      Step(CopyFile(DefaultStatementFile, Dir**Name**'testo'**'testo.pdf'))
    else
      Step(CopyFile(StatementFile, Dir**Name**'testo'**'testo.pdf'));
    if Checker<>'' then begin
      ForceDirectory(Dir**Name**'cor');
      CopyFile(Checker, Dir**Name**'cor'**'correttore');
    end else begin
      DeleteFile(Dir**Name**'cor'**'correttore');
      RemoveDir(Dir**Name**'cor');
    end;

    for i := 0 to InputList.Count-1 do
    Step(CopyFile(InputList[i], Dir**Name**'input'**('input'+IntToStr(i)+'.txt')));
    for i := 0 to OutputList.Count-1 do
    Step(CopyFile(OutputList[i], Dir**Name**'output'**('output'+IntToStr(i)+'.txt')));
    Result := Result and SubtaskList.Execute(Dir**Name, Message);
  except
    on E: Exception do
    begin Message.Add(E.Message); Result := False; end;
  end;
  List.Free;
end;

procedure TProblem.SaveToStream(List: TIniFile; const Section: String);
begin
  WriteString(List, Section,
    ['Name', 'Title', 'StatementFile','InputStreamFile','OutputStreamFile', 'PublicTestCase', 'Checker'],
    [Name, Title, StatementFile, InputStreamFile, OutputStreamFile, PublicTestCase, Checker]);
  WriteInteger(List, Section,
    ['MemLimit', 'TimeLimit'],
    [MemLimit, TimeLimit]);
  WriteStrings(List, Section,
    ['InputList', 'OutputList'],
    [InputList, OutputList]);
  Token.SaveToStream(List, Section+'.Token');
  Limit.SaveToStream(List, Section+'.Limit');
  SubtaskList.SaveToStream(List, Section+'.SubtaskList');
end;

procedure TProblem.LoadFromStream(List: TIniFile; const Section: String);
begin
  ReadString(List, Section,
    ['Name', 'Title', 'StatementFile','InputStreamFile','OutputStreamFile','PublicTestCase', 'Checker'],
    [@Name, @Title, @StatementFile, @InputStreamFile, @OutputStreamFile, @PublicTestCase, @Checker], '');
  ReadInteger(List, Section,
    ['MemLimit', 'TimeLimit'],
    [@MemLimit, @TimeLimit], 0);
  ReadStrings(List, Section,
    ['InputList', 'OutputList'],
    [InputList, OutputList], '');
  Token.LoadFromStream(List, Section+'.Token');
  Limit.LoadFromStream(List, Section+'.Limit');
  SubtaskList.LoadFromStream(List, Section+'.SubtaskList');
end;

{$WARNING 'outputonly', 'total_value', are not supported }
procedure TProblem.ExportToStream(List: TStrings);
begin
  YamlWriteStringIf(List, '', '',
    [true, true, PublicTestCase<>'', true, true],
    ['nome_breve', 'nome', 'risultati', 'infile', 'outfile'],
    [Name, Title, PublicTestCase, InputStreamFile, OutputStreamFile]);
  YamlWriteIntegerIf(List, '', '',
    [true, true, true],
    ['n_input', 'timeout', 'memlimit'],
    [InputList.Count, TimeLimit, MemLimit]);
  Limit.ExportToStream(List);
  Token.ExportToStream(List);
end;

constructor TProblem.Create;
begin
  InputList := TStringList.Create;
  OutputList := TStringList.Create;
  Token := TToken.Create;
  Limit := TLimit.Create;
  SubtaskList := TSubtaskList.Create;

  Name:=DefaultProblemName;
  Title:=DefaultProblemTitle;
  MemLimit:=256;
  TimeLimit:=1;
  OnStep:=nil;
end;

destructor TProblem.Destroy;
begin
  InputList.Free;
  OutputList.Free;
  Token.Free;
  Limit.Free;
  SubtaskList.Free;
  inherited Destroy;
end;

function TProblem.Introduction: String;
begin
  Result := Format('%s (%s) %ds, %dMB, %d test',
    [Name, Title, TimeLimit, MemLimit, InputList.Count]);
end;

end.


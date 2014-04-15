unit contest_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, contnrs, dateutils,
  multi_update, basic_database, token_database, limit_database,
  problem_database, user_database;

const
  DefaultContestName = 'contest';
  DefaultContestTitle = 'Untitled Contest';

resourcestring
  rsKThiBTUVOSKT = 'Kì thi bắt đầu vào %s, kết thúc vào %s';
  rsChAQuyNhThIG = 'Chưa quy định thời gian cho kì thi';
  rsExportDirect = 'Export directory not found';
  rsExportDirect2 = 'Export directory name must be the same with contest name';
  rsContestNameI = 'Contest name is an invalid identifier';
  rsRequireAtLea = 'Require at least one user';
  rsFoundDErrorS = 'Found %d error(s)';
  rsContestIntro = 'Contest introduction';
  rsContestSS = 'Contest "%s": "%s"';

type

  { TContest }

  TContest = class (IBasicDatabase)
  private
    FProblemListIntroduction: TStrings; //
    FUserListIntroduction: TStrings; //
  public
    Name, Title, ExportDir: String;
    ProblemList, UserList: TObjectList; //
    StartTimeSet: Boolean;
    StartTime, StopTime: Integer;
    Token: TToken; //
    Limit: TLimit; //
    OnStep: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings);
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;

    function Execute(Dir: String; Message: TStrings): Boolean;
    function GetProblemListIntroduction: TStrings;
    function GetTimeIntroduction: String;
    function GetUserListIntroduction: TStrings;
    function Estimate: Integer;
  end;

implementation

{ TContest }

function TContest.GetProblemListIntroduction: TStrings;
var Problem: TProblem;
begin
  FProblemListIntroduction.Clear;
  for Pointer(Problem) in ProblemList do
  FProblemListIntroduction.Add(Problem.Introduction);
  Result := FProblemListIntroduction;
end;

function TContest.GetTimeIntroduction: String;
begin
  if StartTimeSet then
    Result := Format(rsKThiBTUVOSKT,
      [DateTimeToStr(UnixToDateTime(StartTime)),
      DateTimeToStr(UnixToDateTime(StopTime))])
  else
    Result := rsChAQuyNhThIG;
end;

function TContest.GetUserListIntroduction: TStrings;
var User: TUser;
begin
  FUserListIntroduction.Clear;
  for Pointer(User) in UserList do
  FUserListIntroduction.Add(User.Introduction);
  Result := FUserListIntroduction;
end;

constructor TContest.Create;
begin
  Token := TToken.Create;
  Limit := TLimit.Create;
  FProblemListIntroduction := TStringList.Create;
  FUserListIntroduction := TStringList.Create;
  ProblemList := TObjectList.Create;
  UserList := TObjectList.Create;
  Name := DefaultContestName;
  Title := DefaultContestTitle;
  OnStep:=nil;
end;

destructor TContest.Destroy;
begin
  Token.Free;
  Limit.Free;
  FProblemListIntroduction.Free;
  FUserListIntroduction.Free;
  ProblemList.Free;
  UserList.Free;
end;

procedure TContest.SaveToStream(List: TIniFile; const Section: String);
var
  i: Integer;
begin
  WriteString(List, Section,
    ['Name', 'Title', 'ExportDir'],
    [Name, Title, ExportDir]);
  WriteInteger(List, Section,
    ['ProblemList.Count', 'UserList.Count'],
    [ProblemList.Count, UserList.Count]);
  for i := 0 to ProblemList.Count-1 do
    TProblem(ProblemList[i]).SaveToStream(List, Section+'.Problem.'+IntToStr(i));
  for i := 0 to UserList.Count-1 do
    TUser(UserList[i]).SaveToStream(List, Section+'.User.'+IntToStr(i));
  WriteBool(List, Section,
    ['StartTimeSet'],
    [StartTimeSet]);
  WriteInteger(List, Section,
    ['StartTime', 'StopTime'],
    [StartTime, StopTime]);
  Token.SaveToStream(List, Section+'.Token');
  Limit.SaveToStream(List, Section+'.Limit');
end;

procedure TContest.LoadFromStream(List: TIniFile; const Section: String);
var
  ProblemListCount, UserListCount: Integer;
  i: Integer;
begin
  ReadString(List, Section,
    ['Name', 'Title', 'ExportDir'],
    [@Name, @Title, @ExportDir], '');
  ReadInteger(List, Section,
    ['ProblemList.Count', 'UserList.Count'],
    [@ProblemListCount, @UserListCount], 0);

  ProblemList.Clear;
  while ProblemList.Count < ProblemListCount do
    ProblemList.Add(TProblem.Create);
  UserList.Clear;
  while UserList.Count < UserListCount do
    UserList.Add(TUser.Create);

  for i := 0 to ProblemList.Count-1 do
    TProblem(ProblemList[i]).LoadFromStream(List, Section+'.Problem.'+IntToStr(i));
  for i := 0 to UserList.Count-1 do
    TUser(UserList[i]).LoadFromStream(List, Section+'.User.'+IntToStr(i));
  ReadBool(List, Section,
    ['StartTimeSet'],
    [@StartTimeSet], False);
  ReadInteger(List, Section,
    ['StartTime', 'StopTime'],
    [@StartTime, @StopTime], 0);
  Token.LoadFromStream(List, Section+'.Token');
  Limit.LoadFromStream(List, Section+'.Limit');
end;

function TContest.Estimate: Integer; // how many file will be written
var Problem: TProblem;
begin
  Result := 1;
  for Pointer(Problem) in ProblemList do
  Result += Problem.Estimate;
end;

function TContest.Execute(Dir: String; Message: TStrings): Boolean;

  procedure Step(B: Boolean=true);
  begin
    if Assigned(OnStep) then
    OnStep(Self);
  end;

var
  List: TStringList;
  Problem: TProblem;
begin
  Result := True;
  List := TStringList.Create;
  try
    ExportToStream(List);
    List.SaveToFile(ConcatPaths([Dir, 'contest.yaml'])); Step;
    for Pointer(Problem) in ProblemList do
    begin
      Problem.OnStep:=OnStep;
      Problem.Execute(Dir, Message);
    end;
  except
    On E: Exception do
    begin Message.Add(E.Message); Result := False; end;
  end;
  List.Free;
end;

{
function TContest.Execute: Boolean; experimental;
var
  List: TStringList;
  Problem: TProblem;
  User: TUser;
  Dir: String;

  procedure Log(S: String);
  begin
    Message.Add('Fatal: '+S);
    Result := False;
  end;

begin
  if not Compile then exit(False);
  try
    Dir := ExportDir;
    List := TStringList.Create;
    List.Add('nome_breve: "'+Name+'"');
    List.Add('nome: "'+Title+'"');

    List.Add('problemi:');
    for Pointer(Problem) in ProblemList do
    List.Add('  - "'+Problem.Name+'"');
    List.Add('utenti:');
    for Pointer(User) in UserList do
    begin
      List.Add('  - username: "'+User.Name+'"');
      List.Add('    password: "'+User.Password+'"');
      if User.IP <> '' then List.Add('    ip: '+User.IP);
    end;
    List.SaveToFile(Dir**'contest.yaml');
    List.Free;
    // Problems
    for Pointer(Problem) in ProblemList do
    Problem.Execute(Dir);
    Result := True;
  except
    On E: Exception do
    Log(E.Message);
  end;
end;
}
{ WARNING: This won't check validity }
procedure TContest.ExportToStream(List: TStrings);

  procedure AddString(Key, Value: String);
    begin List.Add(Key+': "'+Value+'"'); end;
  procedure AddIntegerIf(B: Boolean; Key: String; Value: Integer);
    begin if B then List.Add(Key+': '+IntToStr(Value)); end;

var
  Problem: TProblem;
  User: TUser;
begin
  AddString('nome_breve', Name);
  AddString('nome', Title);
  AddIntegerIf(StartTimeSet, 'inizio', StartTime);
  AddIntegerIf(StartTimeSet, 'fine', StopTime);
  Token.ExportToStream(List);
  Limit.ExportToStream(List);

  List.Add('problemi:');
  for Pointer(Problem) in ProblemList do
    List.Add('  - "'+Problem.Name+'"');
  List.Add('utenti:');
  for Pointer(User) in UserList do
    User.ExportToStream(List);
end;

function TContest.IsValid(Message: TStrings): Boolean;
var ErrorCount: Integer=0;

  procedure ErrorIf(B: Boolean; S: String);
  begin
    if B then ErrorCount += 1 else exit;
    if S<>'' then Message.Add(Format(rsContestSS, [Name, S]));
  end;

var
  Problem: TProblem;
  User: TUser;
begin
  ErrorIf(not DirectoryExists(ExportDir), rsExportDirect);
  ErrorIf(not IsValidIdent(Name), rsContestNameI);
  ErrorIf(ExtractFileName(ExportDir)<>Name, rsExportDirect2);
  ErrorIf(UserList.Count=0, rsRequireAtLea);
  for Pointer(Problem) in ProblemList do
  ErrorIf(not Problem.IsValid(Message), '');
  for Pointer(User) in UserList do
  ErrorIf(not User.IsValid(Message), '');
  ErrorIf(not Result, Format(rsFoundDErrorS, [ErrorCount]));
  Result := ErrorCount=0;
end;

function TContest.Introduction: String;
begin
  Result := rsContestIntro;
end;

end.


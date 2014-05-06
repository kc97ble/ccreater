unit subtask_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, IniFiles, basic_database, multi_update;

resourcestring
  rsSubtaskIntro = 'Subtask introduction';
  rsSubtaskListS = 'Subtask list: "%s"';
  rsSubtaskInter = 'Subtask intervals must be continuous';
  rsLengthOfSubt = 'Length of subtask intervals must be positive';
  rsSomeTestNotA = 'Some test not appeared';
  rsSubtaskModeE = 'Subtask mode enabled';
  rsSubtaskModeD = 'Subtask mode disabled';
  DefaultStatementFile = 'languages/almost_blank_page.pdf';
  rsSumScoreOfSu = 'Sum score of subtasks must be exactly 100.0';

type

  { TSubtask }

  TSubtask = class (IBasicDatabase)
    Head, Tail, Score: Integer;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings);
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;
  end;

  { TSubtaskList }

  TSubtaskList = class (IBasicDatabase)
  private
    FList: TObjectList; //
    function GetCount: Integer;
    function GetSubtask(Index: Integer): TSubtask;
    procedure SetSubtask(Index: Integer; AValue: TSubtask);
    procedure SetCount(NewCount: Integer);
  public
    OwnerProblemListCount: Integer;
    Enabled: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings);
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;
    function Execute(Dir: String; Message: TStrings): Boolean;
    property Subtask[Index: Integer]: TSubtask read GetSubtask write SetSubtask; default;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

{ TSubtask }

procedure TSubtask.SaveToStream(List: TIniFile; const Section: String);
begin
  WriteInteger(List, Section,
    ['Head', 'Tail', 'Score'],
    [Head, Tail, Score]);
end;

procedure TSubtask.LoadFromStream(List: TIniFile; const Section: String);
begin
  ReadInteger(List, Section,
    ['Head', 'Tail', 'Score'],
    [@Head, @Tail, @Score], 0);
end;

procedure TSubtask.ExportToStream(List: TStrings);
var i: Integer;
begin
  List.Add(Format('# ST: %d', [Score]));
  for i := Head to Tail do
  List.Add(IntToStr(i));
end;

function TSubtask.IsValid(Message: TStrings): Boolean;
begin
  Result := (0<=Head) and (Head<=Tail);
end;

function TSubtask.Introduction: String;
begin
  Result := rsSubtaskIntro;
end;

{ TSubtaskList }

function TSubtaskList.GetSubtask(Index: Integer): TSubtask;
  begin Result := TSubtask(FList.Items[Index]); end;
function TSubtaskList.GetCount: Integer;
  begin Result := FList.Count; end;
procedure TSubtaskList.SetSubtask(Index: Integer; AValue: TSubtask);
  begin FList.Items[Index] := AValue; end;
procedure TSubtaskList.SetCount(NewCount: Integer);
  begin while FList.Count<NewCount do FList.Add(TSubtask.Create); FList.Count := NewCount; end;

constructor TSubtaskList.Create;
begin
  FList := TObjectList.Create;
  Enabled := False;
end;

destructor TSubtaskList.Destroy;
begin
  FList.Free;
end;

procedure TSubtaskList.SaveToStream(List: TIniFile; const Section: String);
var i: Integer;
begin
  WriteInteger(List, Section, ['FList.Count'], [FList.Count]);
  WriteBool(List, Section, ['Enabled'], [Enabled]);
  for i := 0 to FList.Count-1 do
  Subtask[i].SaveToStream(List, Section+'.'+IntToStr(i));
end;

procedure TSubtaskList.LoadFromStream(List: TIniFile; const Section: String);
var
  FListCount: Integer;
  i: Integer;
begin
  ReadInteger(List, Section, ['FList.Count'], [@FListCount], 0);
  ReadBool(List, Section, ['Enabled'], [@Enabled], False);
  SetCount(FListCount);
  for i := 0 to FList.Count-1 do
  Subtask[i].LoadFromStream(List, Section+'.'+IntToStr(i));
end;

procedure TSubtaskList.ExportToStream(List: TStrings);
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
  Subtask[i].ExportToStream(List);
end;

function TSubtaskList.IsValid(Message: TStrings): Boolean;
var
  i, Pos: Integer;
  ErrorCount: Integer=0;
  SumScore: Integer=0;

  procedure ErrorIf(B: Boolean; S: String);
  begin
    if B then ErrorCount += 1 else exit;
    if S<>'' then Message.Add(Format(rsSubtaskListS, [S]));
  end;

begin
  if not Enabled then exit(True);
  Pos := 0;
  for i := 0 to Count-1 do
  begin
    ErrorIf(Subtask[i].Head<>Pos, rsSubtaskInter);
    ErrorIf(Subtask[i].Tail<Pos, rsLengthOfSubt);
    Pos := Subtask[i].Tail+1;
    SumScore += Subtask[i].Score;
  end;
  ErrorIf(SumScore<>100, rsSumScoreOfSu);
  //ErrorIf(Pos <> OwnerProblemListCount, rsSomeTestNotA);
  for i := 0 to Count-1 do
  ErrorIf(not Subtask[i].IsValid(Message), '');
  Result := ErrorCount=0;
end;

function TSubtaskList.Introduction: String;
begin
  if Enabled then
    Result := rsSubtaskModeE
  else
    Result := rsSubtaskModeD;
end;

operator ** (A, B: String) C: String;
  begin C := ConcatPaths([A,B]); end;

function TSubtaskList.Execute(Dir: String; Message: TStrings): Boolean;
var List : TStrings;
begin
  Result := True;
  if not Enabled then
  begin
    DeleteFile(Dir**'gen'**'GEN');
    RemoveDir(Dir**'gen');
  end
  else begin
    List := TStringList.Create;
    try
      ExportToStream(List);
      ForceDirectories(Dir**'gen');
      List.SaveToFile(Dir**'gen'**'GEN');
    except
      on E: Exception do
      begin Message.Add(E.Message); Result := False; end;
    end;
  end;
end;

end.

unit database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, contnrs, FileUtil, Math, dateutils,
  multi_update, user_database, token_database, limit_database, problem_database,
  contest_database, workspace_database;

type
  TUser = user_database.TUser;
  TToken = token_database.TToken;
  TLimit = limit_database.TLimit;
  TProblem = problem_database.TProblem;
  TContest = contest_database.TContest;

procedure SaveContestToFile(FileName: String; Contest: TContest);
procedure LoadContestFromFile(FileName: String; Contest: TContest);

implementation

operator ** (A, B: String) C: String;
  begin C := ConcatPaths([A,B]); end;
procedure AddValue(List: TStringList; Key: String; Value: String);
  begin List.Add(Key+': "'+Value+'"'); end;
procedure AddValue(List: TStringList; Key: String; Value: Integer);
  begin List.Add(Key+': '+IntToStr(Value)); end;

procedure SaveContestToFile(FileName: String; Contest: TContest);
var List: TIniFile;
begin
  Workspace.RecentFileList.Add(FileName);
  List := TIniFile.Create(FileName);
  Contest.SaveToStream(List, 'Contest');
  List.Free;
end;

procedure LoadContestFromFile(FileName: String; Contest: TContest);
var List: TIniFile;
begin
  Workspace.RecentFileList.Add(FileName);
  List := TIniFile.Create(FileName);
  Contest.LoadFromStream(List, 'Contest');
  List.Free;
end;

end.



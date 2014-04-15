unit user_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, multi_update, basic_database;

const
  DefaultUserName='username';
  DefaultUserPassword='password';

resourcestring
  rsUserNameIsAn = 'User name is an invalid identifier';
  rsPasswordIsAn = 'Password is an empty string';
  rsInTheUserSS = 'In the user "%s": %s';

type

  { TUser }

  TUser = class (IBasicDatabase)
    Name, Password, IP: String;
    FirstName, LastName: String;
    Hidden: Boolean;
    constructor Create;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure ExportToStream(List: TStrings);
    function IsValid(Message: TStrings): Boolean;
    function Introduction: String;
  end;

implementation

{ TUser }

constructor TUser.Create;
begin
  Name:=DefaultUserName;
  Password:=DefaultUserPassword;
end;

procedure TUser.SaveToStream(List: TIniFile; const Section: String);
begin
  WriteString(List, Section,
    ['Name', 'Password', 'IP', 'FirstName', 'LastName'],
    [Name, Password, IP, FirstName, LastName]);
  WriteBool(List, Section,
    ['Hidden'],
    [Hidden]);
end;

procedure TUser.LoadFromStream(List: TIniFile; const Section: String);
begin
  ReadString(List, Section,
    ['Name', 'Password', 'IP', 'FirstName', 'LastName'],
    [@Name, @Password, @IP, @FirstName, @LastName], '');
  ReadBool(List, Section,
    ['Hidden'],
    [@Hidden], False);
end;

procedure TUser.ExportToStream(List: TStrings);
begin
  YamlWriteStringIf(List, '  - ', '    ',
    [true, true, FirstName<>'', LastName<>'', IP<>'', Hidden],
    ['username', 'password', 'nome', 'cognome', 'ip', 'fake'],
    [Name, Password, FirstName, LastName, IP, 'True']);
end;

function TUser.IsValid(Message: TStrings): Boolean;

  procedure ErrorIf(B: Boolean; S: String);
  begin
    if not B then exit;
    Result := False;
    Message.Add(Format(rsInTheUserSS, [Name, S]));
  end;

begin
  Result := True;
  ErrorIf(not IsValidIdent(Name), rsUserNameIsAn);
  ErrorIf(Password = '', rsPasswordIsAn);
end;

function TUser.Introduction: String;
begin
  Result := Format('%s (%s %s)', [Name, FirstName, LastName]);
end;

end.


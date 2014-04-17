unit workspace_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ActnList, Menus, StdActns, IniFiles, multi_update;
type

  { TRecentFileList }

  TRecentFileList = class (TComponent)
  private
    FRecentMenuItem: TMenuItem;
    procedure SetRecentMenuItem(AValue: TMenuItem);
  public
    MaxRecentFile: Integer;
    FList: TStrings; //
    OnClick: TNotifyEvent;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Add(FileName: String);
    procedure SaveToMenu(Item: TMenuItem);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure SaveToStream(List: TIniFile; const Section: String);
    property RecentMenuItem: TMenuItem read FRecentMenuItem write SetRecentMenuItem;
  end;

  { TWorkspace }

  TWorkspace = class (TComponent)
  public
    RecentFileList: TRecentFileList;
    procedure AfterConstruction; override;
    procedure SaveToStream(List: TIniFile; const Section: String);
    procedure LoadFromStream(List: TIniFile; const Section: String);
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
  end;

var
  Workspace: TWorkspace;

implementation

{ TWorkspace }

procedure TWorkspace.AfterConstruction;
begin
  inherited AfterConstruction;
  RecentFileList := TRecentFileList.Create(Self);
end;

procedure TWorkspace.SaveToStream(List: TIniFile; const Section: String);
begin
  RecentFileList.SaveToStream(List, Section+'.RecentFileList');
end;

procedure TWorkspace.LoadFromStream(List: TIniFile; const Section: String);
begin
  RecentFileList.LoadFromStream(List, Section+'.RecentFileList');
end;

procedure TWorkspace.SaveToFile(FileName: String);
var List: TIniFile;
begin
  List := TIniFile.Create(FileName);
  try
    SaveToStream(List, 'Workspace');
    finally List.Free;
  end;
end;

procedure TWorkspace.LoadFromFile(FileName: String);
var List: TIniFile;
begin
  List := TIniFile.Create(FileName);
  try
    LoadFromStream(List, 'Workspace');
    finally List.Free;
  end;
end;

{ TRecentFileList }

procedure TRecentFileList.SetRecentMenuItem(AValue: TMenuItem);
begin
  if FRecentMenuItem=AValue then Exit;
  FRecentMenuItem:=AValue;
  SaveToMenu(FRecentMenuItem);
end;

procedure TRecentFileList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TStringList.Create;
  MaxRecentFile:=15;
end;

procedure TRecentFileList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FList.Destroy;
end;

procedure TRecentFileList.Add(FileName: String);
var i: Integer;
begin
  for i := FList.Count-1 downto 0 do
  if FList[i]=FileName then FList.Delete(i);
  FList.Insert(0, FileName);
  if FList.Count>MaxRecentFile then
  FList.Delete(FList.Count-1);
  SaveToMenu(FRecentMenuItem);
end;

procedure TRecentFileList.SaveToMenu(Item: TMenuItem);
var
  AItem: TMenuItem;
  i: Integer;
begin
  if Item=nil then exit;
  Item.Clear;
  for i := 0 to FList.Count-1 do
  begin
    AItem := TMenuItem.Create(Self);
    AItem.Caption:=FList[i];
    AItem.OnClick:=OnClick;
    Item.Add(AItem);
  end;
end;

procedure TRecentFileList.LoadFromStream(List: TIniFile; const Section: String);
begin
  ReadInteger(List, Section, ['MaxRecentFile'], [@MaxRecentFile], 15);
  ReadStrings2(List, Section, 'FList', FList);
end;

procedure TRecentFileList.SaveToStream(List: TIniFile; const Section: String);
begin
  WriteInteger(List, Section, ['MaxRecentFile'], [MaxRecentFile]);
  WriteStrings2(List, Section, 'FList', FList);
end;

initialization
  Workspace := TWorkspace.Create(nil);
  if FileExists('ccreater.ini') then
  Workspace.LoadFromFile('ccreater.ini');
finalization
  Workspace.SaveToFile('ccreater.ini');
  Workspace.Free;
end.

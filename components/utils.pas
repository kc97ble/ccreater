unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Dialogs, Forms, Controls;

type
  CharSet = set of char;
  TFPGMap_String_Integer = specialize TFPGMap<String, Integer>;
  TFPGMap_String_PString=specialize TFPGMap<String, PString>;
  TFileDialogClass = class of TFileDialog;

  { TDialogProperty }

  TDialogProperty = class
    History: TStrings;
    InitialDir: String;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToDialog(Dialog: TFileDialog);
    procedure LoadFromDialog(Dialog: TFileDialog);
    procedure CreateDialog(DialogClass: TFileDialogClass; out Dialog);
    procedure FreeDialog(Dialog: TFileDialog);
  end;

function GetPath(const Location: String): String;
operator ** (A, B: String) C: String;
procedure Swap(var A, B: String);
procedure Swap(var A, B: Integer);
function Maximize(var A: Integer; B: Integer): Boolean;
procedure SafelySort(L: TStringList);
function AddSearchResult(L: TStringList; Dir: string;
  Recursive: boolean): boolean;
function ExecuteOpenDialog(out S: String): Boolean;

var
  LastDialogProperty: TDialogProperty;

implementation

{ File name routine }
function GetPath(const Location: String): String;
begin
  if length(Location)=0 then exit('');
  if Location[Length(Location)] in AllowDirectorySeparators then exit(Location)
  else exit(Location+DirectorySeparator);
end;

operator ** (A, B: String) C: String;
begin
  C := ConcatPaths([A,B]);
end;

{ Other }
procedure Swap(var A, B: String);
var Z : String;
begin
  Z:=A; A:=B; B:=Z;
end;

procedure Swap(var A, B: Integer);
var C: Integer;
begin
  C:=A; A:=B; B:=C;
end;

function Maximize(var A: Integer; B: Integer): Boolean;
begin
  Result := A<B;
  if Result then A:=B;
end;

function SafelyCompare(List: TStringList; i, j: Integer): Integer;
begin
  if List[i]=List[j] then exit(0)
  else if List[i]<List[j] then exit(-1)
  else exit(1);
end;

procedure SafelySort(L: TStringList);
begin
  L.CustomSort(@SafelyCompare);
end;

function AddSearchResult(L: TStringList; Dir: string;
  Recursive: boolean): boolean;
var Info: TSearchRec;
    Path: String;
begin
  Path := Dir**'/';
  if FindFirst(Path+'*', faAnyFile, Info)=0 then
  repeat
    if Info.Attr and faDirectory = 0 then
    L.Add(Path+Info.Name)
    else if Recursive and (Info.Name[1]<>'.') then
    AddSearchResult(L, Path+Info.Name, true);
  until FindNext(Info) <> 0;
  FindClose(Info);
  Result := true;
end;

function ExecuteOpenDialog(out S: String): Boolean;
var Dialog: TOpenDialog;
begin
  LastDialogProperty.CreateDialog(TOpenDialog, Dialog);
  try
    Result := Dialog.Execute;
    if Result then S := Dialog.FileName;
    finally LastDialogProperty.FreeDialog(Dialog);
  end;
end;

{ TDialogProperty }

constructor TDialogProperty.Create;
begin
  History := TStringList.Create;
end;

destructor TDialogProperty.Destroy;
begin
  History.Free;
  inherited Destroy;
end;

procedure TDialogProperty.SaveToDialog(Dialog: TFileDialog);
begin
  Dialog.InitialDir:=InitialDir;
  Dialog.HistoryList.Assign(History);
end;

procedure TDialogProperty.LoadFromDialog(Dialog: TFileDialog);
begin
  InitialDir := Dialog.InitialDir;
  History.Assign(Dialog.HistoryList);
end;

procedure TDialogProperty.CreateDialog(DialogClass: TFileDialogClass; out Dialog);
begin
  TFileDialog(Dialog) := DialogClass.Create(nil);
  SaveToDialog(TFileDialog(Dialog));
end;

procedure TDialogProperty.FreeDialog(Dialog: TFileDialog);
begin
  LoadFromDialog(Dialog);
  Dialog.Free;
end;

initialization
  LastDialogProperty := TDialogProperty.Create;
end.


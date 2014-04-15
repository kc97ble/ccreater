unit test_filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, utils;

type

  TEvaluateStrStr = function (A, B: String): Integer of object;
  TEvaluateStrStrStrStr = function (A, B, C, D: String): Integer of object;

  { TTestFilter }

  TTestFilter = class(TObject)
  public
    Files, Inputs, Outputs : TStringList;
    function LoadFromDir(Dir: string; Recursive: Boolean): boolean;
    function CopyToDir(Dir: String; AFormat, BFormat: String): boolean;
    constructor Create;
    destructor Destroy; override;
    procedure CustomSort(Evaluate: TEvaluateStrStr);
    procedure CustomSort(Evaluate: TEvaluateStrStrStrStr);
    procedure Clear;
  private
    procedure ExtractTwoMaxValue(Map: TFPGMap_String_Integer; var AStr, BStr: String);
    function GeneralStr(const s: String): String;
    function GeneralStrProbablityCompare(A, B: String): Integer;
    function VariantStr(s: String): String;
    procedure Classify(L, A, B: TStringList);
  public
    procedure RemoveInvalidPairs(A, B: TStringList);
  end;

function AddSearchResult(L: TStringList; Dir: string;
  Recursive: boolean): boolean;

implementation

function AddSearchResult(L: TStringList; Dir: string;
  Recursive: boolean): boolean;
var Info: TSearchRec;
    Path: String;
begin
  Path := GetPath(Dir);
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

{ TTestFilter }

function TTestFilter.LoadFromDir(Dir: string; Recursive: Boolean): boolean;
begin
  Files.Clear;
  Inputs.Clear;
  Outputs.Clear;
  AddSearchResult(Files, Dir, Recursive);
  Classify(Files, Inputs, Outputs);
  RemoveInvalidPairs(Inputs, Outputs);
  Result := true;
end;

function TTestFilter.CopyToDir(Dir: String; AFormat, BFormat: String): boolean;
var Path : String;
    i : integer;
begin
  Result := true;
  Path := GetPath(Dir);
  ForceDirectories(Path);
  for i := 0 to Inputs.Count-1 do
  begin
    if CopyFile(Inputs[i], Path+Format(AFormat, [i])) = False then
      Result := False;
    if CopyFile(Outputs[i], Path+Format(BFormat, [i])) = False then
      Result := False;
  end;
end;

procedure TTestFilter.CustomSort(Evaluate: TEvaluateStrStr);
var i, j: Integer;
begin
  for i := 0 to Inputs.Count-1-1 do
  for j := i+1 to Inputs.Count-1 do
  if Evaluate(Inputs[i], Outputs[i]) > Evaluate(Inputs[j], Outputs[j]) then
  begin
    Inputs.Exchange(i,j);
    Outputs.Exchange(i,j);
  end;
end;

procedure TTestFilter.CustomSort(Evaluate: TEvaluateStrStrStrStr);
var i, j: Integer;
begin
  for i := 0 to Inputs.Count-1-1 do
  for j := i+1 to Inputs.Count-1 do
  if Evaluate(Inputs[i], Outputs[i], Inputs[j], Outputs[j]) > 0 then
  begin
    Inputs.Exchange(i,j);
    Outputs.Exchange(i,j);
  end;
end;


procedure TTestFilter.Clear;
begin
  Files.Clear;
  Inputs.Clear;
  Outputs.Clear;
end;

constructor TTestFilter.Create;
begin
  Files := TStringList.Create;
  Inputs := TStringList.Create;
  Outputs := TStringList.Create;
end;

destructor TTestFilter.Destroy;
begin
  FreeAndNil(Files);
  FreeAndNil(Inputs);
  FreeAndNil(Outputs);
  inherited Destroy;
end;

function TTestFilter.GeneralStr(const s: String): String;
var c: Char;
begin
  Result := '';
  for c in s do
  if c in ['a'..'z', 'A'..'Z'] then
  Result += LowerCase(c);
end;

function TTestFilter.VariantStr(s: String): String;
var c: Char;
begin
  Result := '';
  for c in s do
  if not (c in ['a'..'z', 'A'..'Z']) then
  Result += LowerCase(c);
end;

function TTestFilter.GeneralStrProbablityCompare(A, B: String): Integer;
var c: Char;
    AValue: Integer = 0;
    BValue: Integer = 0;
begin
  for c in A do if c in ['i','I'] then AValue += 100;
  for c in B do if c in ['i','I'] then BValue += 100;

  if AValue < BValue then exit(-1)
  else if AValue = BValue then exit(0)
  else exit(1);
end;

procedure TTestFilter.ExtractTwoMaxValue(Map: TFPGMap_String_Integer; var AStr, BStr: String);
var i: Integer;
    ACnt, BCnt: Integer;
begin
  AStr:=''; BStr:=''; ACnt:=0; BCnt:=0;

  for i := 0 to Map.Count-1 do
  if Maximize(BCnt, Map.Data[i]) then
  begin
    BStr := Map.Keys[i];
    if ACnt<BCnt then
    begin
      Swap(ACnt, BCnt);
      Swap(AStr, BStr);
    end;
  end;
end;

procedure TTestFilter.Classify(L, A, B: TStringList);
var Map: TFPGMap_String_Integer;
    AStr, BStr: String;
    s, gs : String;
begin
  A.Clear;
  B.Clear;
  if L.Count=0 then exit;
  Map:=TFPGMap_String_Integer.Create;

  for s in L do
  begin
    gs := GeneralStr(s);
    if Map.IndexOf(gs)=-1 then
    Map.Add(gs, 0);
    Map[gs] := Map[gs] + 1;
  end;

  ExtractTwoMaxValue(Map, AStr, BStr);
  if GeneralStrProbablityCompare(AStr, BStr) < 0 then Swap(AStr, BStr);

  for s in L do
  begin
    if GeneralStr(s)=AStr then A.Add(s);
    if GeneralStr(s)=BStr then B.Add(s);
  end;
  Map.Free;
end;

procedure TTestFilter.RemoveInvalidPairs(A, B: TStringList);
var
  i, j : integer;
  A1, B1: TStringList;

  function Sign(x: Integer): Integer;
  begin
    if x=0 then Result := 0
    else if x<0 then Result := -1
    else Result := 1;
  end;

begin
  A1 := TStringList.Create;
  B1 := TStringList.Create;
  try
    SafelySort(A);
    SafelySort(B);
    i := 0;
    j := 0;
    while (i<A.Count) and (j<B.Count) do
    case Sign(CompareStr(VariantStr(A[i]), VariantStr(B[j]))) of
      0 : begin A1.Add(A[i]); B1.Add(B[j]); Inc(i); Inc(j); end;
      -1 : Inc(i);
      1 : Inc(j);
    end;
    A.Assign(A1);
    B.Assign(B1);
  finally
    A1.Free;
    B1.Free;
  end;
end;

end.



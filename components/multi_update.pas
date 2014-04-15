unit multi_update;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Spin, StdCtrls, IniFiles, ActnList, Controls;

// TControl and ordinary types
procedure WriteInteger(Key: array of TSpinEdit; Value: array of Integer); deprecated;
procedure ReadInteger(Key: array of TSpinEdit; Value: array of PInteger); deprecated;
procedure WriteBool(Key: array of TCheckBox; Value: array of Boolean);
procedure ReadBool(Key: array of TCheckBox; Value: array of PBoolean);
procedure WriteString(Key: array of TEdit; Value: array of String);
procedure ReadString(Key: array of TEdit; Value: array of PString);
procedure AssignBool(Value: Boolean; Dest: array of PBoolean);
procedure AssignBool(Value: Boolean; Dest: array of TAction);

procedure WriteToControls(Key: array of TControl; Value: array of const);
procedure ReadFromControls(Key: array of TControl; Value: array of Pointer);
procedure WriteToPersistent(Key: array of TPersistent; Value: array of TPersistent);
procedure ReadFromPersistent(Key: array of TPersistent; Value: array of TPersistent);
procedure WriteToVariable(Key: array of Pointer; Value: array of const);
procedure ReadFromVariable(Key: array of const; Value: array of Pointer); unimplemented;

// TIniFile and ordinary types
procedure WriteBool(List: TIniFile; Section: String; Key: array of String;
                          const Value: array of Boolean);
procedure ReadBool(List: TIniFile; Section: String; Key: array of String;
                          Value: array of PBoolean; DefaultValue: Boolean);
procedure WriteInteger(List: TIniFile; Section: String; Key: array of String;
                          const Value: array of Integer);
procedure ReadInteger(List: TIniFile; Section: String; Key: array of String;
                          Value: array of PInteger; DefaultValue: Integer);
procedure WriteString(List: TIniFile; Section: String; Key: array of String;
                          const Value: array of String);
procedure ReadString(List: TIniFile; Section: String; Key: array of String;
                          Value: array of PString; DefaultValue: String);
procedure WriteStrings(List: TIniFile; Section: String; Key: array of String;
                          const Value: array of TStrings);
procedure ReadStrings(List: TIniFile; Section: String; Key: array of String;
                          Value: array of TStrings; DefaultValue: String);

// Yaml Strings and ordinary types
procedure YamlWriteIntegerIf(List: TStrings; FirstIndent, SecondIndent: String;
  Condition: array of Boolean; Key: array of String; Value: array of Integer);
procedure YamlWriteStringIf(List: TStrings; FirstIndent, SecondIndent: String;
  Condition: array of Boolean; Key: array of String; Value: array of String);


implementation

// Control updates

procedure WriteInteger(Key: array of TSpinEdit; Value: array of Integer);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Key[i].Value:=Value[i];
end;

procedure ReadInteger(Key: array of TSpinEdit; Value: array of PInteger);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Value[i]^:=Key[i].Value;
end;

procedure WriteBool(Key: array of TCheckBox; Value: array of Boolean);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Key[i].Checked:=Value[i];
end;

procedure ReadBool(Key: array of TCheckBox; Value: array of PBoolean);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Value[i]^:=Key[i].Checked;
end;

procedure WriteString(Key: array of TEdit; Value: array of String);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Key[i].Text:=Value[i];
end;

procedure ReadString(Key: array of TEdit; Value: array of PString);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Value[i]^:=Key[i].Text;
end;

procedure AssignBool(Value: Boolean; Dest: array of PBoolean);
var i: Integer;
begin
  for i := 0 to High(Dest) do
  Dest[i]^:=Value;
end;

procedure AssignBool(Value: Boolean; Dest: array of TAction);
var i: Integer;
begin
  for i := 0 to High(Dest) do
  Dest[i].Enabled:=Value;
end;

procedure WriteToControls(Key: array of TControl; Value: array of const);
var i: Integer;
begin
  for i := 0 to High(Key) do
  begin
    if Key[i] is TEdit then
      case Value[i].VType of
        vtAnsiString: TEdit(Key[i]).Text:=AnsiString(Value[i].VAnsiString); // usually
        vtPChar: TEdit(Key[i]).Text:=PChar(Value[i].VPChar);
        vtString: TEdit(Key[i]).Text:=String(Value[i].VString^);
      end
    else if Key[i] is TCheckBox then
      TCheckBox(Key[i]).Checked:=Value[i].VBoolean
    else if Key[i] is TSpinEdit then
      TSpinEdit(Key[i]).Value:=Value[i].VInteger
    else
      raise Exception.Create(Key[i].Name);
  end;
end;

procedure ReadFromControls(Key: array of TControl; Value: array of Pointer);
var i: Integer;
begin
  for i := 0 to High(Key) do
  begin
    if Key[i] is TEdit then
      PAnsiString(Value[i])^ := TEdit(Key[i]).Text
    else if Key[i] is TCheckBox then
      PBoolean(Value[i])^ := TCheckBox(Key[i]).Checked
    else if Key[i] is TSpinEdit then
      PInteger(Value[i])^ := TSpinEdit(Key[i]).Value
    else
      raise Exception.Create(Key[i].Name);
  end;
end;

procedure WriteToPersistent(Key: array of TPersistent;
  Value: array of TPersistent);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Key[i].Assign(Value[i]);
end;

procedure ReadFromPersistent(Key: array of TPersistent;
  Value: array of TPersistent);
var i: Integer;
begin
  for i := 0 to High(Key) do
  Value[i].Assign(Key[i]);
end;

procedure WriteToVariable(Key: array of Pointer; Value: array of const);
var
  i: Integer;
begin
  for i := 0 to High(Key) do
  case Value[i].VType of
    vtInteger: PInteger(Key[i])^ := Value[i].VInteger;
    vtBoolean: PBoolean(Key[i])^ := Value[i].VBoolean;
    vtAnsiString: PAnsiString(Key[i])^ := AnsiString(Value[i].VAnsiString);
  else
    raise Exception.Create('WriteToVariable incompleted');
  end;
end;

procedure ReadFromVariable(Key: array of const; Value: array of Pointer);
begin

end;


// TIniFile updates

procedure WriteBool(List: TIniFile; Section: String; Key: array of String;
  const Value: array of Boolean);
var i: Integer;
begin
  for i := Low(Key) to High(Key) do
  List.WriteBool(Section, Key[i], Value[i]);
end;

procedure WriteInteger(List: TIniFile; Section: String; Key: array of String;
  const Value: array of Integer);
var i: Integer;
begin
  for i := Low(Key) to High(Key) do
  List.WriteInteger(Section, Key[i], Value[i]);
end;

procedure WriteString(List: TIniFile; Section: String; Key: array of String;
  const Value: array of String);
var i: Integer;
begin
  for i := Low(Key) to High(Key) do
  List.WriteString(Section, Key[i], Value[i]);
end;

procedure ReadString(List: TIniFile; Section: String; Key: array of String;
  Value: array of PString; DefaultValue: String);
var i: Integer;
begin
  List.StripQuotes:=False;
  for i := Low(Key) to High(Key) do
  Value[i]^ := List.ReadString(Section, Key[i], DefaultValue);
end;

procedure WriteStrings(List: TIniFile; Section: String; Key: array of String;
  const Value: array of TStrings);
var i: Integer;
begin
  List.StripQuotes:=False;
  for i := Low(Key) to High(Key) do
  List.WriteString(Section, Key[i], Value[i].CommaText);
end;

procedure ReadStrings(List: TIniFile; Section: String; Key: array of String;
  Value: array of TStrings; DefaultValue: String);
var i: Integer;
begin
  for i := Low(Key) to High(Key) do
  Value[i].CommaText := List.ReadString(Section, Key[i], DefaultValue);
end;

procedure YamlWriteIntegerIf(List: TStrings; FirstIndent, SecondIndent: String;
  Condition: array of Boolean; Key: array of String; Value: array of Integer);
var i: Integer;
begin
  if Condition[0] then List.Add(FirstIndent+Key[0]+': '+IntToStr(Value[0]));
  for i := 1 to high(Condition) do
  if Condition[i] then List.Add(SecondIndent+Key[i]+': '+IntToStr(Value[i]));
end;

procedure YamlWriteStringIf(List: TStrings; FirstIndent, SecondIndent: String;
  Condition: array of Boolean; Key: array of String; Value: array of String);
var i: Integer;
begin
  if Condition[0] then List.Add(FirstIndent+Key[0]+': "'+Value[0]+'"');
  for i := 1 to high(Condition) do
  if Condition[i] then List.Add(SecondIndent+Key[i]+': "'+Value[i]+'"');
end;

procedure ReadBool(List: TIniFile; Section: String; Key: array of String;
  Value: array of PBoolean; DefaultValue: Boolean);
var i: Integer;
begin
  for i := Low(Key) to High(Key) do
  Value[i]^ := List.ReadBool(Section, Key[i], DefaultValue);
end;

procedure ReadInteger(List: TIniFile; Section: String; Key: array of String;
  Value: array of PInteger; DefaultValue: Integer);
var i: Integer;
begin
  for i := Low(Key) to High(Key) do
  Value[i]^ := List.ReadInteger(Section, Key[i], DefaultValue);
end;

end.


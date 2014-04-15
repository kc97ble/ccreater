unit subtask_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ButtonPanel, subtask_database, multi_update;

type

  { TSubtaskEditor }

  TSubtaskEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    SubtaskEnabledCheck: TCheckBox;
    StringGrid1: TStringGrid;
  private
    { private declarations }
  public
    function Execute(SubtaskList: TSubtaskList): TModalResult;
    procedure LoadControls(SubtaskList: TSubtaskList);
    procedure SaveControls(SubtaskList: TSubtaskList);
    function Introduction: String;
    class function DefaultExecute(Sender: TObject): TModalResult;
  end;

implementation

{$R *.lfm}

{ TSubtaskEditor }

function TSubtaskEditor.Execute(SubtaskList: TSubtaskList): TModalResult;
begin
  LoadControls(SubtaskList);
  Result := ShowModal;
  if Result = mrOK then
  SaveControls(SubtaskList);
end;

procedure TSubtaskEditor.LoadControls(SubtaskList: TSubtaskList);
var
  i: Integer;
begin
  WriteBool([SubtaskEnabledCheck], [SubtaskList.Enabled]);
  StringGrid1.RowCount := SubtaskList.Count + 1;
  for i := 0 to SubtaskList.Count-1 do
  with SubtaskList[i], StringGrid1 do
  begin
    Cells[1,i+1] := IntToStr(Head);
    Cells[2,i+1] := IntToStr(Tail);
    Cells[3,i+1] := IntToStr(Score);
  end;
end;

procedure TSubtaskEditor.SaveControls(SubtaskList: TSubtaskList);
var
  i: Integer;
begin
  ReadBool([SubtaskEnabledCheck], [@SubtaskList.Enabled]);
  SubtaskList.Count := StringGrid1.RowCount - 1;
  for i := 0 to SubtaskList.Count-1 do
  with SubtaskList[i], StringGrid1 do
  begin
    Head := StrToInt(Cells[1,i+1]);
    Tail := StrToInt(Cells[2,i+1]);
    Score := StrToInt(Cells[3,i+1]);
  end;
end;

function TSubtaskEditor.Introduction: String;
begin
  if SubtaskEnabledCheck.Checked then
    Result := 'Subtask mode is enabled'
  else
    Result := 'Subtask mode is disabled';
end;

class function TSubtaskEditor.DefaultExecute(Sender: TObject): TModalResult;
var
  Form: TSubtaskEditor;
begin
  Form := TSubtaskEditor.Create(nil);
  try
    Result := Form.Execute(Sender as TSubtaskList);
    finally Form.Free;
  end;
end;

end.


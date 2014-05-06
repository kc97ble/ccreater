unit properties_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Spin,
  EditBtn, StdCtrls, ButtonPanel, ExtCtrls, database, multi_update, utils;

resourcestring
  rsNameTitleTim = '#,Name,Title,"Time limit","Mem limit","Statement file","'
    +'Input stream","Output stream","Public testcase"';

type

  { TPropertiesEditor }

  TPropertiesEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    ComboBox1: TComboBox;
    FileNameEdit1: TFileNameEdit;
    Label2: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Splitter1: TSplitter;
    StringGrid1: TStringGrid;
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEdit1EditingDone(Sender: TObject);
    procedure FileNameEdit1Enter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
  private
    FContest: TContest;
    procedure AdjustColWidth;
  public
    function Execute(Contest: TContest): TModalResult;
    procedure LoadControls(Contest: TContest);
    procedure SaveControls(Contest: TContest);
    class function DefaultExecute(Contest: TContest): TModalResult;
  end;

implementation

{$R *.lfm}

{ TPropertiesEditor }

procedure TPropertiesEditor.StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);

  function PublicAllTestCase(n: Integer): String;
  var i: Integer;
  begin
    if n=0 then exit('');
    Result := '0';
    for i := 1 to n-1 do
    Result += ','+IntToStr(i);
  end;

begin
  if aCol=3 then
  begin
    SpinEdit1.BoundsRect := StringGrid1.CellRect(aCol, aRow);
    SpinEdit1.Value:=StrToInt(StringGrid1.Cells[aCol, aRow]);
    Editor := SpinEdit1;
  end;
  if aCol=4 then
  begin
    SpinEdit2.BoundsRect := StringGrid1.CellRect(aCol, aRow);
    SpinEdit2.Value:=StrToInt(StringGrid1.Cells[aCol, aRow]);
    Editor := SpinEdit2;
  end;
  if aCol=5 then
  begin
    FileNameEdit1.BoundsRect := StringGrid1.CellRect(aCol, aRow);
    FileNameEdit1.FileName := StringGrid1.Cells[aCol, aRow];
    Editor := FileNameEdit1;
  end;
  if aCol=6 then
  with ComboBox1 do begin
    BoundsRect := StringGrid1.CellRect(aCol, aRow);
    Text:=StringGrid1.Cells[aCol, aRow];
    Items.CommaText:=Format('"%:s.in","%:s.inp","input.txt"', [StringGrid1.Cells[1,aRow]]);
    Editor := ComboBox1;
  end;
  if aCol=7 then
  with ComboBox1 do begin
    BoundsRect := StringGrid1.CellRect(aCol, aRow);
    Text:=StringGrid1.Cells[aCol, aRow];
    Items.CommaText:=Format('"%:s.out","output.txt"', [StringGrid1.Cells[1,aRow]]);
    Editor := ComboBox1;
  end;
  if aCol=8 then
  with ComboBox1 do begin
    BoundsRect := StringGrid1.CellRect(aCol, aRow);
    Text:=StringGrid1.Cells[aCol, aRow];
    Items.Clear;
    Items.Add(PublicAllTestCase(TProblem(FContest.ProblemList[aRow-1]).InputList.Count));
    Items.Add('');
    Editor := ComboBox1;
  end;
end;

procedure TPropertiesEditor.AdjustColWidth;
var
  EachWidth: Integer;
  Index: Integer;
  i: Integer;
begin
  with StringGrid1 do EachWidth:=(Width-10)-ColWidths[0]-ColWidths[1];
  Index := ListBox1.ItemIndex;

  if Index<2 then // view all column
    with StringGrid1 do
    begin
      EachWidth:=EachWidth div (ColCount-2);
      if EachWidth<50 then EachWidth:=100;
      for i := 2 to ColCount-1 do
      ColWidths[i]:=EachWidth;
    end
  else // view one column
    with StringGrid1 do
    for i := 2 to ColCount-1 do
    if i=Index then ColWidths[i] := EachWidth-20
    else ColWidths[i]:=0;
end;

function TPropertiesEditor.Execute(Contest: TContest): TModalResult;
begin
  FContest := Contest;
  LoadControls(Contest);
  Result := ShowModal;
  if Result=mrOK then SaveControls(Contest);
end;

procedure TPropertiesEditor.LoadControls(Contest: TContest);
var
  i: Integer;
begin
  StringGrid1.RowCount:=Contest.ProblemList.Count+1;
  for i := 0 to Contest.ProblemList.Count-1 do
  with StringGrid1, TProblem(Contest.ProblemList[i]) do
  begin
    Cells[1,i+1] := Name;
    Cells[2,i+1] := Title;
    Cells[3,i+1] := IntToStr(TimeLimit);
    Cells[4,i+1] := IntToStr(MemLimit);
    Cells[5,i+1] := StatementFile;
    Cells[6,i+1] := InputStreamFile;
    Cells[7,i+1] := OutputStreamFile;
    Cells[8,i+1] := PublicTestCase;
  end;
  Caption:=Format('%s (%s) - Properties Editor', [Contest.Title, Contest.Name]);
end;

procedure TPropertiesEditor.SaveControls(Contest: TContest);
var
  i: Integer;
begin
  for i := 0 to Contest.ProblemList.Count-1 do
  with StringGrid1, TProblem(Contest.ProblemList[i]) do
  begin
    Name                := Cells[1,i+1];
    Title               := Cells[2,i+1];
    TimeLimit := StrToInt(Cells[3,i+1]);
    MemLimit  := StrToInt(Cells[4,i+1]);
    StatementFile       := Cells[5,i+1];
    InputStreamFile     := Cells[6,i+1];
    OutputStreamFile    := Cells[7,i+1];
    PublicTestCase      := Cells[8,i+1];
  end;
end;

class function TPropertiesEditor.DefaultExecute(Contest: TContest
  ): TModalResult;
var Form: TPropertiesEditor;
begin
  Form := TPropertiesEditor.Create(nil);
  try
    Result := Form.Execute(Contest);
    finally Form.Free;
  end;
end;

procedure TPropertiesEditor.FileNameEdit1EditingDone(Sender: TObject);
begin
  if Sender is TCustomEdit then with StringGrid1 do
  Cells[Col, Row] := TCustomEdit(Sender).Text;
  if Sender is TCustomComboBox then with StringGrid1 do
  Cells[Col, Row] := TCustomComboBox(Sender).Text;
  if Sender is TFileNameEdit then
  LastDialogProperty.InitialDir:=TFileNameEdit(Sender).InitialDir;
end;

procedure TPropertiesEditor.FileNameEdit1AcceptFileName(Sender: TObject;
  var Value: String);
begin
  LastDialogProperty.InitialDir := FileNameEdit1.InitialDir;
end;

procedure TPropertiesEditor.FileNameEdit1Enter(Sender: TObject);
begin
  FileNameEdit1.InitialDir := LastDialogProperty.InitialDir;
end;

procedure TPropertiesEditor.FormCreate(Sender: TObject);
begin
  StringGrid1.Rows[0].CommaText:=rsNameTitleTim;
  ListBox1.Items.Assign(StringGrid1.Rows[0]);
end;

procedure TPropertiesEditor.FormResize(Sender: TObject);
begin
  AdjustColWidth;
end;

procedure TPropertiesEditor.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  AdjustColWidth;
  StringGrid1.EditorMode:=False;
end;

end.


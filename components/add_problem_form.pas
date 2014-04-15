unit add_problem_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ActnList, ButtonPanel, RegExpr,
  utils, regex_option_form, test_filter;

type

  { TAddProblemForm }

  TAddProblemForm = class(TForm)
    actInvalidate: TAction;
    actSortNumber: TAction;
    actSortSize: TAction;
    actSortAlphabet: TAction;
    actSimpleLoad: TAction;
    actRegexLoad: TAction;
    ButtonPanel1: TButtonPanel;
    SearchDirEdit: TEdit;
    SearchDirLabel: TLabel;
    ActionList: TActionList;

    GroupBox1ofAddProblem: TGroupBox;
    ImageList1: TImageList;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure actInvalidateExecute(Sender: TObject);
    procedure actSimpleLoadExecute(Sender: TObject);
    procedure actRegexLoadExecute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure actSortAlphabetExecute(Sender: TObject);
    procedure actSortNumberExecute(Sender: TObject);
    procedure actSortSizeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    function CustomEvaluate(A, B: String): Integer;
    function CustomEvaluate2(A, B, C, D: String): Integer;
    function GetInputs: TStringList;
    function GetOutputs: TStringList;
    function GetSearchDir: String;
    procedure SetSearchDir(AValue: String);
  public
    TestFilter: TTestFilter;
    function Execute(ASearchDir: String): TModalResult;
    property SearchDir: String read GetSearchDir write SetSearchDir;
    property Inputs: TStringList read GetInputs;
    property Outputs: TStringList read GetOutputs;
  end;

implementation

{$R *.lfm}

{ TAddProblemForm }

procedure TAddProblemForm.FormCreate(Sender: TObject);
begin
  TestFilter := TTestFilter.Create;
end;

procedure TAddProblemForm.FormDestroy(Sender: TObject);
begin
  TestFilter.Free;
end;

procedure TAddProblemForm.ListBox1SelectionChange(Sender: TObject; User: boolean
  );
var AListBox, BListBox: TListBox;
begin
  if not User then exit;
  if Sender is TListBox then
  begin
    AListBox := Sender as TListBox;

    if AListBox=ListBox1 then
      BListBox:=ListBox2
    else
      BListBox:=ListBox1;

    if AListBox.ItemIndex<BListBox.Count then
      BListBox.ItemIndex:=AListBox.ItemIndex
    else
      BListBox.ItemIndex:=-1;
  end;
end;

function TAddProblemForm.GetInputs: TStringList;
begin
  Result := TestFilter.Inputs;
end;

function TAddProblemForm.GetOutputs: TStringList;
begin
  Result := TestFilter.Outputs;
end;

procedure TAddProblemForm.actSimpleLoadExecute(Sender: TObject);
begin
  TestFilter.LoadFromDir(SearchDir, true);
  actSortNumber.Execute; // Almost people like this
  actInvalidate.Execute;
end;

procedure TAddProblemForm.actInvalidateExecute(Sender: TObject);
var s: String;
begin
  ListBox1.Clear;
  ListBox2.Clear;
  for s in Inputs do
  ListBox1.Items.Add(ExtractRelativepath(SearchDir, s));
  for s in Outputs do
  ListBox2.Items.Add(ExtractRelativepath(SearchDir, s));
  Application.ProcessMessages;
end;

procedure TAddProblemForm.actRegexLoadExecute(Sender: TObject);
var
  ARegExpr, BRegExpr: TRegExpr;
  s: String;
  Form: TRegexOptionForm;
  List: TStringList;
begin
  Form := TRegexOptionForm.Create(Self);
  List := TStringList.Create;
  if Form.Execute('in', 'out') = mrOK then
  begin
    ARegExpr:=TRegExpr.Create;
    BRegExpr:=TRegExpr.Create;

    ARegExpr.Expression := Form.AExpr;
    BRegExpr.Expression := Form.BExpr;
    AddSearchResult(List, SearchDir, true);

    Inputs.Clear;
    Outputs.Clear;
    for s in List do
    begin
      if ARegExpr.Exec(s) then
        Inputs.Add(s)
      else if BRegExpr.Exec(s) then
        Outputs.Add(s);
    end;
    TestFilter.RemoveInvalidPairs(Inputs, Outputs);

    SafelySort(Inputs);
    SafelySort(Outputs);
    actInvalidate.Execute;

    ARegExpr.Free;
    BRegExpr.Free;
  end;
  List.Free;
  Form.Free;
end;

procedure TAddProblemForm.Action3Execute(Sender: TObject);
begin

end;

procedure TAddProblemForm.actSortAlphabetExecute(Sender: TObject);
begin
  Inputs.Sort;
  Outputs.Sort;
  actInvalidate.Execute;
end;

function TAddProblemForm.CustomEvaluate2(A, B, C, D: String): Integer;
begin
  Result := (Length(A)+Length(B)) - (Length(C)+Length(D));
  if Result=0 then Result := CompareStr(A+B, C+D);
end;

procedure TAddProblemForm.actSortNumberExecute(Sender: TObject);
begin
  TestFilter.CustomSort(@CustomEvaluate2);
  actInvalidate.Execute;
end;

function TAddProblemForm.CustomEvaluate(A, B: String): Integer;
begin
  Result := FileSize(A) + FileSize(B);
end;

procedure TAddProblemForm.actSortSizeExecute(Sender: TObject);
begin
  TestFilter.CustomSort(@CustomEvaluate);
  actInvalidate.Execute;
end;

procedure TAddProblemForm.SetSearchDir(AValue: String);
begin
  SearchDirEdit.Text:=AValue;
end;

function TAddProblemForm.GetSearchDir: String;
begin
  Result := SearchDirEdit.Text;
end;

function TAddProblemForm.Execute(ASearchDir: String) : TModalResult;
begin
  SearchDir:=ASearchDir;
  actSimpleLoad.Execute;
  Result := ShowModal;
  Hide;
end;

end.


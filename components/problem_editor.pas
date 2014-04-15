unit problem_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, Buttons, ExtCtrls, ActnList, PairSplitter, ButtonPanel,
  add_problem_form, utils, database, problem_database, multi_update,
  token_editor, limit_editor, subtask_editor;

type

  { TProblemEditor }

  TProblemEditor = class(TForm)
    actAdd: TAction;
    actEditLimit: TAction;
    actEditToken: TAction;
    actEditSubtask: TAction;
    actRem: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    SubtaskListButton: TButton;
    ButtonPanel1: TButtonPanel;
    SubtaskListLabel: TLabel;
    StandardStreamCheck: TCheckBox;
    TokenButton: TButton;
    LimitButton: TButton;
    Button2: TButton;
    StatementButton: TButton;
    PublicTestCaseEdit: TEdit;
    InputStreamEdit: TEdit;
    OutputStreamEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LimitLabel: TLabel;
    NameEdit: TEdit;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Splitter1: TSplitter;
    TabSheet3: TTabSheet;
    TimeEdit: TSpinEdit;
    TitleEdit: TEdit;
    StatementEdit: TEdit;
    NameLabel: TLabel;
    TitleLabel: TLabel;
    TimeLabel: TLabel;
    MemLabel: TLabel;
    StatementLabel: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    MemEdit: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TokenLabel: TLabel;
    procedure actAddExecute(Sender: TObject);
    procedure actEditLimitExecute(Sender: TObject);
    procedure actEditSubtaskExecute(Sender: TObject);
    procedure actEditTokenExecute(Sender: TObject);
    procedure actRemExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure StatementButtonClick(Sender: TObject);
  private
    FInputOutputIntroduction: TStrings;
    FProblem: TProblem;
    Inputs, Outputs: TStringList;
    FTokenEditor: TTokenEditor; //
    FLimitEditor: TLimitEditor; //
    FSubtaskEditor: TSubtaskEditor; //
    function GetInputOutputIntroduction: TStrings;
    function PublicAllTestCase: String;
    procedure UpdateControl(Sender: TObject);
  public
    function Execute(Problem: TProblem; IsNew: Boolean): TModalResult;
    procedure LoadControls(Problem: TProblem);
    procedure SaveControls(Problem: TProblem);
    procedure InvalidateControls;
    property InputOutputIntroduction: TStrings read GetInputOutputIntroduction;
  end;

implementation

{$R *.lfm}

{ TProblemEditor }

procedure TProblemEditor.actAddExecute(Sender: TObject);
var
  Form: TAddProblemForm;
  Dialog: TSelectDirectoryDialog;
begin
  Form := TAddProblemForm.Create(Nil);
  Dialog := TSelectDirectoryDialog.Create(nil);
  LastDialogProperty.SaveToDialog(Dialog);
  try
    if Dialog.Execute then
    if Form.Execute(Dialog.FileName)=mrOK then
    begin
      Inputs.AddStrings(Form.Inputs);
      Outputs.AddStrings(Form.Outputs);
      if (NameEdit.Text=DefaultProblemName)
      and (TitleEdit.Text=DefaultProblemTitle) then
      begin
        NameEdit.Text:=LowerCase(ExtractFileName(Form.SearchDir));
        TitleEdit.Text:=ExtractFileName(Form.SearchDir);
      end;
      InvalidateControls;
      UpdateControl(ListBox1);
    end;
  finally
    LastDialogProperty.FreeDialog(Dialog);
    Form.Free;
  end;
end;

procedure TProblemEditor.actEditLimitExecute(Sender: TObject);
begin
  if FLimitEditor.ShowModal <> mrOK then
    FLimitEditor.LoadControls(FProblem.Limit)
  else
    InvalidateControls;
end;

procedure TProblemEditor.actEditSubtaskExecute(Sender: TObject);
begin
  if FSubtaskEditor.ShowModal = mrOK then
    InvalidateControls
  else
    FSubtaskEditor.LoadControls(FProblem.SubtaskList);
end;

procedure TProblemEditor.actEditTokenExecute(Sender: TObject);
begin
  if FTokenEditor.ShowModal <> mrOK then
    FTokenEditor.LoadControls(FProblem.Token)
  else
    InvalidateControls;
end;

procedure TProblemEditor.actRemExecute(Sender: TObject);
var i: Integer;
begin
  Inputs.BeginUpdate;
  Outputs.BeginUpdate;
  for i := ListBox1.Count-1 downto 0 do
  if ListBox1.Selected[i] then
  begin
    Inputs.Delete(i);
    Outputs.Delete(i);
  end;
  Inputs.EndUpdate;
  Outputs.EndUpdate;
  UpdateControl(ListBox1);
  InvalidateControls;
end;

procedure TProblemEditor.FormCreate(Sender: TObject);
begin
  Inputs := TStringList.Create;
  Outputs := TStringList.Create;
  FInputOutputIntroduction := TStringList.Create;
  FTokenEditor := TTokenEditor.Create(Self);
  FLimitEditor := TLimitEditor.Create(Self);
  FSubtaskEditor := TSubtaskEditor.Create(Self);
end;

procedure TProblemEditor.FormDestroy(Sender: TObject);
begin
  Inputs.Free;
  Outputs.Free;
  FInputOutputIntroduction.Free;
end;

procedure TProblemEditor.NameEditChange(Sender: TObject);
begin
  InvalidateControls;
end;

procedure TProblemEditor.StatementButtonClick(Sender: TObject);
var Dialog: TOpenDialog;
begin
  LastDialogProperty.CreateDialog(TOpenDialog, Dialog);
  try
    if Dialog.Execute then
    StatementEdit.Text:=Dialog.FileName;
  finally
    LastDialogProperty.FreeDialog(Dialog);
  end;
end;

function TProblemEditor.Execute(Problem: TProblem; IsNew: Boolean): TModalResult;
begin
  FProblem := Problem;
  LoadControls(Problem);
  if IsNew then actAdd.Execute;
  Result := ShowModal;
  if Result=mrOK then SaveControls(Problem);
end;

function TProblemEditor.PublicAllTestCase: String;
var i: Integer;
begin
  if FProblem.InputList.Count=0 then exit('');
  Result := '0';
  for i := 1 to FProblem.InputList.Count-1 do
  Result += ','+IntToStr(i);
end;

function TProblemEditor.GetInputOutputIntroduction: TStrings;

  function f(A, B: String): String;
  begin
    Result := Format('%s, %s', [ExtractRelativepath(B, A),
      ExtractRelativepath(A, B)]);
  end;

var i: Integer;
begin
  FInputOutputIntroduction.Clear;
  for i := 0 to Inputs.Count-1 do
  FInputOutputIntroduction.Add(f(Inputs[i], Outputs[i]));
  Result := FInputOutputIntroduction;
end;

procedure TProblemEditor.LoadControls(Problem: TProblem);
begin
  with Problem do WriteToControls(
    [NameEdit, TitleEdit, TimeEdit, MemEdit, StatementEdit, InputStreamEdit, OutputStreamEdit, PublicTestCaseEdit],
    [Name, Title, TimeLimit, MemLimit, StatementFile, InputStreamFile, OutputStreamFile, PublicTestCase]);
  with Problem do WriteToPersistent(
    [Inputs, Outputs],
    [InputList, OutputList]);
  FTokenEditor.LoadControls(Problem.Token);
  FLimitEditor.LoadControls(Problem.Limit);
  FSubtaskEditor.LoadControls(Problem.SubtaskList);
  { Load constrains }
  with Problem do begin
    StandardStreamCheck.Checked:=(InputStreamFile='') and (OutputStreamFile='');
    if PublicTestCase = '' then
      RadioButton2.Checked:=True
    else if PublicTestCase = PublicAllTestCase then
      RadioButton1.Checked:=True
    else
      RadioButton3.Checked := True;
  end;
  InvalidateControls;
  UpdateControl(ListBox1);
end;

procedure TProblemEditor.SaveControls(Problem: TProblem);
begin
  with Problem do ReadFromControls(
    [NameEdit, TitleEdit, TimeEdit, MemEdit, StatementEdit, InputStreamEdit, OutputStreamEdit, PublicTestCaseEdit],
    [@Name, @Title, @TimeLimit, @MemLimit, @StatementFile, @InputStreamFile, @OutputStreamFile, @PublicTestCase]);
  with Problem do ReadFromPersistent(
    [Inputs, Outputs],
    [InputList, OutputList]);
  FTokenEditor.SaveControls(Problem.Token);
  FLimitEditor.SaveControls(Problem.Limit);
  FSubtaskEditor.SaveControls(Problem.SubtaskList);
  { Save contrains }
  if StandardStreamCheck.Checked then
  begin
    Problem.InputStreamFile:='';
    Problem.OutputStreamFile:='';
  end;
  if RadioButton1.Checked then
    Problem.PublicTestCase:=PublicAllTestCase;
  if RadioButton2.Checked then
    Problem.PublicTestCase:='';
  InvalidateControls;
end;

procedure TProblemEditor.UpdateControl(Sender: TObject);
begin
  if Sender=ListBox1 then
    ListBox1.Items.Assign(InputOutputIntroduction)
  else
    raise Exception.Create('TProblemEditor.UpdateControl(Sender: TObject) not yet completed');
end;

procedure TProblemEditor.InvalidateControls;
begin
  LimitLabel.Caption:=FLimitEditor.Introduction;
  TokenLabel.Caption:=FTokenEditor.Introduction;
  SubtaskListLabel.Caption:=FSubtaskEditor.Introduction;
  Caption:=Format('%s (%s)', [TitleEdit.Text, NameEdit.Text]);
  PublicTestCaseEdit.Enabled:=RadioButton3.Checked;
  InputStreamEdit.Enabled:=not StandardStreamCheck.Checked;
  OutputStreamEdit.Enabled:=not StandardStreamCheck.Checked;
end;

end.


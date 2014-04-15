unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ActnList, Menus, StdActns, DefaultTranslator,
  database, problem_editor, edit_user_form,
  utils, token_editor, time_editor, limit_editor, multi_update, properties_editor;

resourcestring
  rsXuTKThiThNhC = 'Xuất kì thi thành công';
  rsGPLIKhiAngXu = 'Gặp lỗi khi đang xuất kì thi';
  rsKiMTraNIDung = 'Kiểm tra nội dung thành công';
  rsPhTHiNLITron = 'Phát hiện lỗi trong kì thi';
  rsFileHasChang = 'File has changed. Save?';
  rsBNMuNXoHTCCB = 'Bạn muốn xoá hết các bài?';
  rsBNMuNXoHTCCU = 'Bạn muốn xoá hết các user?';
  rsCCreaterV000 = 'CCreater v0.000000|Phần mềm tạo kì thi cho CMS|'
    +'Người đóng góp|Nguyễn Tiến Trung Kiên';
  CurrentLanguage = 'Some language';

type

  { TContestEditorForm }

  TContestEditorForm = class(TForm)
    actAddProblem, actEditProblem, actCompile, actExecute, actAbout, actAddUser,
      actExitFile, actEditToken, actSaveFile, actOpenFile, actRemUser,
      actRemProblem, actSaveAsFile, actClearUser, actEditUser, actClearProblem,
      actEditLimit, actEditTime: TAction;
      actImportUser: TAction;
      actEditProperties: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    AddProblemButton, RemUserButton, RemProblemButton, EditProblemButton,
      AddUserButton, ClearProblemButton, ClearUserButton,
      Button7, Button8, Button9: TButton;
      ProgressBar1: TProgressBar;
      EditAllProblemButton: TButton;
      Button1: TButton;
      Label2: TLabel;
      ImageList1: TImageList;
      MenuItem24: TMenuItem;
      MenuItem23: TMenuItem;
      MenuItem22: TMenuItem;
      MenuItem20: TMenuItem;
    MenuItem21, MenuItem19, MenuItem18, MenuItem17: TMenuItem;
    EditUserButton, Button11, Button12, Button13: TButton;
    ExportDirEdit: TEdit;
    ExportDirLabel, Label1, TimeIntroductionLabel, LimitIntroductionLabel: TLabel;
    ListBox2, UserListBox: TListBox;
    ToolButton16: TToolButton;
    ToolButton15: TToolButton;
    ToolButton14: TToolButton;
    ToolButton13: TToolButton;
    ToolButton12: TToolButton;
    ToolButton11: TToolButton;
    ToolButton10: TToolButton;
    ToolButton9: TToolButton;
    ToolButton8: TToolButton;
    ToolButton7: TToolButton;
    ToolButton6: TToolButton;
    ToolButton5: TToolButton;
    ToolButton4: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    ToolButton1: TToolButton;
    ToolBar1: TToolBar;
    MenuItem15, MenuItem16: TMenuItem;
    NameEdit: TEdit;
    Panel3: TPanel;
    ContestGeneralTab: TTabSheet;
    TitleEdit: TEdit;
    NameLabel, TitleLabel: TLabel;
    ProblemListBox: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1, MenuItem10, MenuItem11, MenuItem12, MenuItem13, MenuItem14,
      MenuItem2, MenuItem3, MenuItem4, MenuItem5, MenuItem6, MenuItem7,
      MenuItem8, MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Panel1, Panel2: TPanel;
    ProblemTab, ExportTab, UserTab: TTabSheet;
    TokenIntroductionLabel: TLabel;
    procedure actAboutExecute(Sender: TObject);
    procedure actAddProblemExecute(Sender: TObject);
    procedure actAddUserExecute(Sender: TObject);
    procedure actClearProblemExecute(Sender: TObject);
    procedure actClearUserExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actEditLimitExecute(Sender: TObject);
    procedure actEditProblemExecute(Sender: TObject);
    procedure actEditPropertiesExecute(Sender: TObject);
    procedure actEditTimeExecute(Sender: TObject);
    procedure actEditTokenExecute(Sender: TObject);
    procedure actEditUserExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject); experimental;
    procedure actExitFileExecute(Sender: TObject);
    procedure actImportUserExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRemProblemExecute(Sender: TObject);
    procedure actRemUserExecute(Sender: TObject);
    procedure actSaveAsFileExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ComponentChange(Sender: TObject); experimental;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    ControlsModified: Boolean;
    FileName: String;
    Contest: TContest;
    function CanCloseFile: Boolean;
    function CanOpenFile: Boolean;
    function CanSaveAsFile: Boolean;
    function CanSaveFile: Boolean;
    procedure ContestStep(Sender: TObject);
    procedure InvalidateControls;
    procedure UpdateCaption;
    procedure UpdateControl(Sender: TObject);
  public
    procedure LoadControls;
    procedure SaveControls;
  end;

var
  ContestEditorForm: TContestEditorForm;

implementation

{$R *.lfm}

{ TContestEditorForm }

procedure TContestEditorForm.Button9Click(Sender: TObject);
var Dialog: TSelectDirectoryDialog;
begin
  Dialog := TSelectDirectoryDialog.Create(nil);
  LastDialogProperty.SaveToDialog(Dialog);
  try
    if Dialog.Execute then
    ExportDirEdit.Text:=Dialog.FileName;
  finally
    LastDialogProperty.FreeDialog(Dialog);
  end;
end;

procedure TContestEditorForm.ComponentChange(Sender: TObject); experimental;
begin
  if not ControlsModified then
  begin
    ControlsModified:=true;
    UpdateCaption;
  end;
  if (Sender=NameEdit) or (Sender=TitleEdit) then UpdateCaption;
end;

function TContestEditorForm.CanSaveAsFile: Boolean;
var Dialog: TSaveDialog;
begin
  Result := False;
  LastDialogProperty.CreateDialog(TSaveDialog, Dialog);
  try
    if Dialog.Execute then
    begin
      SaveControls;
      SaveContestToFile(Dialog.FileName, Contest);
      FileName:=Dialog.FileName;
      ControlsModified:=False;
      InvalidateControls;
      Result := True;
    end;
  finally
    LastDialogProperty.FreeDialog(Dialog);
  end;
end;

function TContestEditorForm.CanSaveFile: Boolean;
begin
  if FileName='' then
    Result := CanSaveAsFile
  else begin
    SaveContestToFile(FileName, Contest);
    ControlsModified:=false;
    InvalidateControls;
    Result := True;
  end;
end;

function TContestEditorForm.CanCloseFile: Boolean;
begin
  if ControlsModified then
    case MessageDlg(rsFileHasChang, mtConfirmation, mbYesNoCancel, 0) of
      mrYes: Result := CanSaveFile;
      mrNo: Result := True;
    else
      Result := False;
    end
  else Result := True;
end;

procedure TContestEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=CanCloseFile;
end;

procedure TContestEditorForm.actRemProblemExecute(Sender: TObject);
var i: Integer;
begin
  for i := ProblemListBox.Count-1 downto 0 do
  if ProblemListBox.Selected[i] then
  begin
    ComponentChange(Sender);
    Contest.ProblemList.Delete(i);
  end;
  UpdateControl(ProblemListBox);
end;

procedure TContestEditorForm.actRemUserExecute(Sender: TObject);
var i: Integer;
begin
  for i := UserListBox.Count-1 downto 0 do
  if UserListBox.Selected[i] then
  begin
    ComponentChange(Sender);
    Contest.UserList.Delete(i);
  end;
  UpdateControl(UserListBox);
end;

procedure TContestEditorForm.actSaveAsFileExecute(Sender: TObject);
begin
  CanSaveAsFile;
end;

procedure TContestEditorForm.actSaveFileExecute(Sender: TObject);
begin
  CanSaveFile;
end;

procedure TContestEditorForm.actEditProblemExecute(Sender: TObject);
var
  Index: Integer;
  Form: TProblemEditor;
begin
  Index := ProblemListBox.ItemIndex;
  if Index=-1 then exit;
  Form := TProblemEditor.Create(nil);
  try
    if Form.Execute(Contest.ProblemList[Index] as TProblem, false)=mrOK then
    begin
      ComponentChange(Sender);
      UpdateControl(ProblemListBox);
    end;
  finally
    Form.Free;
  end;
end;

procedure TContestEditorForm.actEditPropertiesExecute(Sender: TObject);
var Form: TPropertiesEditor;
begin
  Form := TPropertiesEditor.Create(nil);
  try
    if Form.Execute(Contest) = mrOK then
    begin
      UpdateControl(ProblemListBox);
      InvalidateControls;
      ComponentChange(Sender);
    end;
  finally
    Form.Free;
  end;
end;

procedure TContestEditorForm.actEditTimeExecute(Sender: TObject);
var Form: TTimeEditor;
begin
  Form := TTimeEditor.Create(nil);
  try
    if Form.Execute(Contest)=mrOK then
    begin
      InvalidateControls;
      ComponentChange(Sender);
    end;
  finally
    Form.Free;
  end;
end;

procedure TContestEditorForm.actEditTokenExecute(Sender: TObject);
var Form: TTokenEditor;
begin
  Form := TTokenEditor.Create(nil);
  try
    if Form.Execute(Contest.Token) = mrOK then
    begin
      InvalidateControls;
      ComponentChange(Sender);
    end;
  finally
    Form.Free;
  end;
end;

procedure TContestEditorForm.actEditUserExecute(Sender: TObject);
var
  Index: Integer;
  Form: TEditUserForm;
begin
  Index := UserListBox.ItemIndex;
  if Index=-1 then exit;
  Form := TEditUserForm.Create(nil);
  try
    if Form.Execute(Contest.UserList[Index] as TUser) = mrOK then
    begin
      UpdateControl(UserListBox);
      ComponentChange(Sender);
    end;
  finally
    Form.Free;
  end;
end;

procedure TContestEditorForm.ContestStep(Sender: TObject);
begin
  ProgressBar1.Position:=ProgressBar1.Position+1;
  Application.ProcessMessages;
end;

procedure TContestEditorForm.actExecuteExecute(Sender: TObject);
begin
  if Contest.ExportDir='' then Button9Click(Sender);
  with ProgressBar1 do
  begin
    Max:=Contest.Estimate;
    Position:=0;
    Show;
  end;
  Contest.OnStep:=@ContestStep;
  if Contest.Execute(Contest.ExportDir, ListBox2.Items)
    then ShowMessage(rsXuTKThiThNhC)
    else ShowMessage(rsGPLIKhiAngXu);
  PageControl1.PageIndex:=3;
  ProgressBar1.Hide;
end;

procedure TContestEditorForm.actExitFileExecute(Sender: TObject);
begin
  Close;
end;

procedure TContestEditorForm.actImportUserExecute(Sender: TObject);
var
  AFileName: String;
  AContest: TContest;
begin
  if ExecuteOpenDialog(AFileName) then
  begin
    AContest := TContest.Create;
    try
      LoadContestFromFile(AFileName, AContest);
      AContest.UserList.OwnsObjects:=False;
      Contest.UserList.AddList(AContest.UserList);
      UpdateControl(UserListBox);
      ComponentChange(Sender);
      finally AContest.Free;
    end;
  end;
end;

function TContestEditorForm.CanOpenFile: Boolean;
var Dialog: TOpenDialog;
begin
  Result := False;
  Dialog := TOpenDialog.Create(nil);
  LastDialogProperty.SaveToDialog(Dialog);
  try
    if Dialog.Execute then
    begin
      LoadContestFromFile(Dialog.FileName, Contest);
      LoadControls;
      FileName:=Dialog.FileName;
      ControlsModified:=False;
      UpdateCaption;
      Result := True;
    end;
  finally
    LastDialogProperty.FreeDialog(Dialog);
  end;
end;

procedure TContestEditorForm.actOpenFileExecute(Sender: TObject);
begin
  CanOpenFile;
end;

procedure TContestEditorForm.actCompileExecute(Sender: TObject);
begin
  SaveControls;
  if Contest.ExportDir='' then Button9Click(Sender);
  ListBox2.Items.Clear;
  if Contest.IsValid(ListBox2.Items)
    then ShowMessage(rsKiMTraNIDung)
    else ShowMessage(rsPhTHiNLITron);
  PageControl1.PageIndex:=3;
end;

procedure TContestEditorForm.actEditLimitExecute(Sender: TObject);
var Form: TLimitEditor;
begin
  Form := TLimitEditor.Create(nil);
  try
    if Form.Execute(Contest.Limit) = mrOK then
    begin
      InvalidateControls;
      ComponentChange(Sender);
    end;
  finally
    Form.Free;
  end;
end;

procedure TContestEditorForm.actAddProblemExecute(Sender: TObject);
var
  Form: TProblemEditor;
  NewProblem: TProblem;
  Added: Boolean = false;
begin
  Form := TProblemEditor.Create(Nil);
  NewProblem := TProblem.Create;
  try
    if Form.Execute(NewProblem, true)=mrOK then
    begin
      Contest.ProblemList.Add(NewProblem);
      Added := true;
      UpdateControl(ProblemListBox);
      ComponentChange(Sender);
    end;
  finally
    if not Added then
    NewProblem.Free;
    Form.Free;
  end;
end;

procedure TContestEditorForm.actAddUserExecute(Sender: TObject);
var
  NewUser: TUser;
  Form: TEditUserForm;
  OK: Boolean;
begin
  Form := TEditUserForm.Create(Nil);
  NewUser := TUser.Create;
  try
    OK := Form.Execute(NewUser) = mrOK;
    if OK then
    begin
      ComponentChange(Sender);
      Contest.UserList.Add(NewUser);
      UpdateControl(UserListBox);
    end;
  finally
    Form.Free;
    if not OK then NewUser.Free;
  end;
end;

procedure TContestEditorForm.actClearProblemExecute(Sender: TObject);
begin
  if MessageDlg(rsBNMuNXoHTCCB, mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    ComponentChange(Sender);
    Contest.ProblemList.Clear;
    UpdateControl(ProblemListBox);
  end;
end;

procedure TContestEditorForm.actClearUserExecute(Sender: TObject);
begin
  if MessageDlg(rsBNMuNXoHTCCU, mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    Contest.UserList.Clear;
    UpdateControl(ListBox2);
    ComponentChange(Sender);
  end;
end;

procedure TContestEditorForm.actAboutExecute(Sender: TObject);
begin
  ShowMessage(StringReplace(rsCCreaterV000, '|', LineEnding, [rfReplaceAll]));
end;

procedure TContestEditorForm.FormCreate(Sender: TObject);
begin
  Contest:=TContest.Create;
  LoadControls;
  ControlsModified:=False;
  UpdateCaption;
end;

procedure TContestEditorForm.FormDestroy(Sender: TObject);
begin
  Contest.Free;
end;

procedure TContestEditorForm.Label2Click(Sender: TObject);
begin
  if CurrentLanguage='vi' then
    SetDefaultLang('en')
  else if CurrentLanguage='en' then
    SetDefaultLang('vi');
end;

procedure TContestEditorForm.PageControl1Change(Sender: TObject);
begin
  InvalidateControls;
end;

procedure TContestEditorForm.UpdateControl(Sender: TObject);
begin
  if Sender=ProblemListBox then
    ProblemListBox.Items.Assign(Contest.GetProblemListIntroduction)
  else if Sender=UserListBox then
    UserListBox.Items.Assign(Contest.GetUserListIntroduction)
  else if Sender=Self then
    begin
      UpdateControl(ProblemListBox);
      UpdateControl(UserListBox);
    end
  else
    ShowMessage('UpdateControl ('+TComponent(Sender).Name+') not implemented');
end;

procedure TContestEditorForm.InvalidateControls;
begin
  UpdateCaption;
  TimeIntroductionLabel.Caption:=Contest.GetTimeIntroduction;
  LimitIntroductionLabel.Caption:=Contest.Limit.Introduction;
  TokenIntroductionLabel.Caption:=Contest.Token.Introduction;
  AssignBool(PageControl1.PageIndex=0, [actAddProblem, actRemProblem, actEditProblem]);
  AssignBool(PageControl1.PageIndex=1, [actAddUser, actRemUser, actEditUser]);
end;

procedure TContestEditorForm.SaveControls;
begin
  Contest.Name:=NameEdit.Text;
  Contest.Title:=TitleEdit.Text;
  Contest.ExportDir:=ExportDirEdit.Text;
end;

procedure TContestEditorForm.LoadControls;
begin
  NameEdit.Text      := Contest.Name;
  TitleEdit.Text     := Contest.Title;
  ExportDirEdit.Text := Contest.ExportDir;
  InvalidateControls;
  UpdateControl(Self);
end;

procedure TContestEditorForm.UpdateCaption;

  function ifthen(Value: Boolean; A, B: String): String;
  begin
    if Value then Result := A
    else Result := B;
  end;

begin
  Caption := Format('CCreater v0.000000 - %s (%s)',
    [ifthen(ControlsModified, '*', '') + TitleEdit.Text, NameEdit.Text]);
end;

end.


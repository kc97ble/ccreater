unit limit_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ButtonPanel, database, multi_update;

resourcestring
  rsNPTIADLN = 'Nộp tối đa %d lần';
  rsTGiIHN = 'Đã đặt giới hạn';
  rsKhNgCGiIHN = 'Không có giới hạn';

type

  { TLimitEditor }

  TLimitEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    MSNEdit: TSpinEdit;
    MSNLabel: TCheckBox;
    MUTNEdit: TSpinEdit;
    MUTNLabel: TCheckBox;
    MSIEdit: TSpinEdit;
    MSILabel: TCheckBox;
    MUTIEdit: TSpinEdit;
    MUTILabel: TCheckBox;
    procedure MSNLabelChange(Sender: TObject);
  private
    procedure InvalidateControls;
  public
    function Execute(Limit: TLimit): TModalResult;
    procedure SaveControls(Limit: TLimit);
    procedure LoadControls(Limit: TLimit);
    function Introduction: String;
  end;

implementation

{$R *.lfm}

{ TLimitEditor }

procedure TLimitEditor.MSNLabelChange(Sender: TObject);
begin
  InvalidateControls;
end;

procedure TLimitEditor.InvalidateControls;
begin
  MSNEdit.Enabled:=MSNLabel.Checked;
  MUTNEdit.Enabled:=MUTNLabel.Checked;
  MSIEdit.Enabled:=MSILabel.Checked;
  MUTIEdit.Enabled:=MUTILabel.Checked;
end;

function TLimitEditor.Execute(Limit: TLimit): TModalResult;
begin
  LoadControls(Limit);
  InvalidateControls;
  Result := ShowModal;
  if Result=mrOK then
  SaveControls(Limit);
  Hide;
end;

procedure TLimitEditor.SaveControls(Limit: TLimit);
begin
  with Limit do ReadBool(
    [MSNLabel, MUTNLabel, MSILabel, MUTILabel],
    [@MSNSet, @MUTNSET, @MSISet, @MUTISet]);
  with Limit do ReadInteger(
    [MSNEdit, MUTNEdit, MSIEdit, MUTIEdit],
    [@MSN, @MUTN, @MSI, @MUTI]);
end;

procedure TLimitEditor.LoadControls(Limit: TLimit);
begin
  with Limit do WriteBool(
    [MSNLabel, MUTNLabel, MSILabel, MUTILabel],
    [MSNSet, MUTNSET, MSISet, MUTISet]);
  with Limit do WriteInteger(
    [MSNEdit, MUTNEdit, MSIEdit, MUTIEdit],
    [MSN, MUTN, MSI, MUTI]);
  InvalidateControls;
end;

function TLimitEditor.Introduction: String;
begin
  if MSNLabel.Checked then
    Result := Format(rsNPTIADLN, [MSNEdit.Value])
  else if MUTNLabel.Checked or MSILabel.Checked or MUTILabel.Checked then
    Result := rsTGiIHN
  else
    Result := rsKhNgCGiIHN;
end;

end.


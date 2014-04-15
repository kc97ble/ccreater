unit token_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Spin, StdCtrls, ButtonPanel, database, utils, multi_update;

resourcestring
  rsTokenAngBT = 'Token đang bật';
  rsTokenAngTT = 'Token đang tắt';

type

  { TTokenEditor }

  TTokenEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    InitialLabel: TCheckBox;
    GenNumberLabel: TCheckBox;
    GenTimeLabel: TCheckBox;
    TokenInfiniteCheck: TCheckBox;
    TokenEnabledCheck: TCheckBox;
    PageControl3: TPageControl;
    MaxEdit: TSpinEdit;
    InitialEdit: TSpinEdit;
    MinIntervalEdit: TSpinEdit;
    TotalEdit: TSpinEdit;
    GenNumberEdit: TSpinEdit;
    GenTimeEdit: TSpinEdit;
    TokenAdvancedTab: TTabSheet;
    TokenGeneralTab: TTabSheet;
    MaxLabel: TCheckBox;
    MinIntervalLabel: TCheckBox;
    TotalLabel: TCheckBox;
    procedure TokenEnabledCheckChange(Sender: TObject);
  private
    procedure InvalidateControls;
  public
    function Execute(Token: TToken): TModalResult;
    procedure LoadControls(Token: TToken);
    procedure SaveControls(Token: TToken);
    function Introduction: String;
  end;

implementation

{$R *.lfm}

{ TTokenEditor }

procedure TTokenEditor.InvalidateControls;
begin
  PageControl3.Enabled:=TokenEnabledCheck.Checked;
  InitialLabel.Checked:=InitialLabel.Enabled;
  InitialEdit.Enabled:=InitialLabel.Checked;
  MinIntervalEdit.Enabled:=MinIntervalLabel.Checked;
  GenNumberEdit.Enabled:=GenNumberLabel.Checked;
  GenTimeEdit.Enabled:=GenTimeLabel.Checked;
  TotalEdit.Enabled:=TotalLabel.Checked;
  MaxEdit.Enabled:=MaxLabel.Checked;
end;

procedure TTokenEditor.TokenEnabledCheckChange(Sender: TObject);
begin
  InvalidateControls;
end;

procedure TTokenEditor.LoadControls(Token: TToken);
begin
  with Token do WriteBool(
  [TokenEnabledCheck, MinIntervalLabel, GenNumberLabel, GenTimeLabel, TotalLabel, MaxLabel],
  [InitialSet, MinIntervalSet, GenNumberSet, GenTimeSet, TotalSet, MaxSet]);
  with Token do WriteInteger(
  [InitialEdit, MinIntervalEdit, GenNumberEdit, GenTimeEdit, TotalEdit, MaxEdit],
  [Initial, MinInterval, GenNumber, GenTime, Total, Max]);
  InvalidateControls;
end;

procedure TTokenEditor.SaveControls(Token: TToken);
begin
  with Token do ReadBool(
    [InitialLabel, MinIntervalLabel, GenNumberLabel, GenTimeLabel, TotalLabel, MaxLabel],
    [@InitialSet, @MinIntervalSet, @GenNumberSet, @GenTimeSet, @TotalSet, @MaxSet]);
  with Token do ReadInteger(
    [InitialEdit, MinIntervalEdit, GenNumberEdit, GenTimeEdit, TotalEdit, MaxEdit],
    [@Initial, @MinInterval, @GenNumber, @GenTime, @Total, @Max]);
  // Apply constrains
  if TokenInfiniteCheck.Checked then
  with Token do
  begin
    GenNumberSet:=True; GenNumber:=1;
    GenTimeSet:=True; GenTime:=0;
  end;
  if not TokenEnabledCheck.Checked then
  with Token do AssignBool(false,
    [@InitialSet, @MinIntervalSet, @GenNumberSet, @GenTimeSet, @TotalSet, @MaxSet]);
end;

function TTokenEditor.Introduction: String;
begin
  if TokenEnabledCheck.Checked then
    Result := rsTokenAngBT
  else
    Result := rsTokenAngTT;
end;

function TTokenEditor.Execute(Token: TToken): TModalResult;
begin
  LoadControls(Token);
  InvalidateControls;
  Result := ShowModal;
  if Result=mrOK then SaveControls(Token);
  Hide;
end;

end.


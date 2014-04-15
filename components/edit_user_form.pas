unit edit_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ComCtrls, utils, database;

type

  { TEditUserForm }

  TEditUserForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    HiddenCheck: TCheckBox;
    FirstNameEdit: TEdit;
    LastNameEdit: TEdit;
    IPEdit: TEdit;
    IPLabel: TLabel;
    HiddenLabel: TLabel;
    LastNameLabel: TLabel;
    FirstNameLabel: TLabel;
    NameEdit: TEdit;
    PageControl1: TPageControl;
    PasswordEdit: TEdit;
    NameLabel: TLabel;
    PasswordLabel: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
  private
    procedure Load(AUser: TUser);
    procedure Save(AUser: TUser);
  public
    function Execute(AUser: TUser): TModalResult;
  end;

implementation

{$R *.lfm}

{ TEditUserForm }

procedure TEditUserForm.Load(AUser: TUser);
begin
  NameEdit.Text:=AUser.Name;
  PasswordEdit.Text:=AUser.Password;
  IPEdit.Text:=AUser.IP;
  FirstNameEdit.Text:=AUser.FirstName;
  LastNameEdit.Text:=AUser.LastName;
  HiddenCheck.Checked:=AUser.Hidden;
end;

procedure TEditUserForm.Save(AUser: TUser);
begin
  AUser.Name      := NameEdit.Text;
  AUser.Password  := PasswordEdit.Text;
  AUser.IP        := IPEdit.Text;
  AUser.FirstName := FirstNameEdit.Text;
  AUser.LastName  := LastNameEdit.Text;
  AUser.Hidden    := HiddenCheck.Checked;
end;

function TEditUserForm.Execute(AUser: TUser): TModalResult;
begin
  Load(AUser);
  Result := ShowModal;
  if Result=mrOK then Save(AUser);
end;

end.


unit regex_option_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel,
  utils;

type

  { TRegexOptionForm }

  TRegexOptionForm = class(TForm)
    AExprEdit: TEdit;
    BExprEdit: TEdit;
    ButtonPanel1: TButtonPanel;
    AExprLabel: TLabel;
    BExprLabel: TLabel;
  private
    function GetAExpr: String;
    function GetBExpr: String;
    procedure SetAExpr(AValue: String);
    procedure SetBExpr(AValue: String);
    { private declarations }
  public
    { public declarations }
    function Execute(AExprDef, BExprDef: String) : TModalResult;
    property AExpr: String read GetAExpr write SetAExpr;
    property BExpr: String read GetBExpr write SetBExpr;
  end;

implementation

{$R *.lfm}

{ TRegexOptionForm }

function TRegexOptionForm.GetAExpr: String;
begin
  Result := AExprEdit.Text;
end;

function TRegexOptionForm.GetBExpr: String;
begin
  Result := BExprEdit.Text;
end;

procedure TRegexOptionForm.SetAExpr(AValue: String);
begin
  AExprEdit.Text := AValue;
end;

procedure TRegexOptionForm.SetBExpr(AValue: String);
begin
  BExprEdit.Text := AValue;
end;

function TRegexOptionForm.Execute(AExprDef, BExprDef: String): TModalResult;
begin
  AExpr:=AExprDef;
  BExpr:=BExprDef;
  Result:=ShowModal;
  Hide;
end;

end.


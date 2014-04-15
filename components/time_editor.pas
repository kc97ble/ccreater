unit time_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Calendar,
  EditBtn, StdCtrls, ExtCtrls, Spin, ButtonPanel, dateutils, database;

type

  { TTimeEditor }

  TTimeEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonPanel1: TButtonPanel;
    Calendar1: TCalendar;
    StartTimeSetCheck: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    HourLabel: TLabel;
    MinuteLabel: TLabel;
    DurationLabel: TLabel;
    HourEdit: TSpinEdit;
    MinuteEdit: TSpinEdit;
    DurationEdit: TSpinEdit;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StartTimeSetCheckChange(Sender: TObject);
  private
    procedure InvalidateControls;
  public
    function Execute(Contest: TContest): TModalResult;
    procedure LoadControls(Contest: TContest);
    procedure SaveControls(Contest: TContest);
  end;

implementation

{$R *.lfm}

{ TTimeEditor }

procedure TTimeEditor.StartTimeSetCheckChange(Sender: TObject);
begin
  InvalidateControls;
end;

procedure TTimeEditor.Button1Click(Sender: TObject);
begin
  Calendar1.DateTime:=Today;
end;

procedure TTimeEditor.Button2Click(Sender: TObject);
begin
  HourEdit.Value:=HourOf(Time);
  MinuteEdit.Value:=MinuteOf(Time);
end;

procedure TTimeEditor.InvalidateControls;
begin
  GroupBox1.Enabled := StartTimeSetCheck.Checked;
  GroupBox2.Enabled := StartTimeSetCheck.Checked;
end;

function TTimeEditor.Execute(Contest: TContest): TModalResult;
begin
  LoadControls(Contest);
  InvalidateControls;
  Result := ShowModal;
  if Result=mrOK then SaveControls(Contest);
  Hide;
end;

procedure TTimeEditor.LoadControls(Contest: TContest);
begin
  StartTimeSetCheck.Checked:=Contest.StartTimeSet;
  Calendar1.DateTime:=UnixToDateTime(Contest.StartTime);
  HourEdit.Value:=Contest.StartTime div 3600 mod 24;
  MinuteEdit.Value:=Contest.StartTime div 60 mod 60;
  DurationEdit.Value:=(Contest.StopTime-Contest.StartTime) div 60;
end;

procedure TTimeEditor.SaveControls(Contest: TContest);
begin
  Contest.StartTimeSet := StartTimeSetCheck.Checked;
  Contest.StartTime:=DateTimeToUnix(DateOf(Calendar1.DateTime))
    + HourEdit.Value * 3600 + MinuteEdit.Value * 60;
  Contest.StopTime:=Contest.StartTime+DurationEdit.Value*60;
end;

end.


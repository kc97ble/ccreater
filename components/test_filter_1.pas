unit test_filter_1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ShellCtrls, ExtCtrls, Buttons, Spin, ActnList, Menus,
  ButtonPanel;

type

  { TTestFilter1 }

  TTestFilter1 = class(TForm)
    ButtonPanel1: TButtonPanel;

    Label1: TLabel;
    Panel2: TGroupBox;
    GroupBox1: TGroupBox;
    SpinEdit1: TSpinEdit;
    TreeView1: TTreeView;
    ActionList1: TActionList;
    ListBox1, ListBox2: TListBox;
    Panel1, Panel3, Panel4: TPanel;
    ShellTreeView1: TShellTreeView;
    Splitter1, Splitter2: TSplitter;
    PopupMenu1, PopupMenu2: TPopupMenu;
    InputButton, AddOutputButton: TButton;
    MenuItem1, MenuItem2, MenuItem3, MenuItem4,
    MenuItem5, MenuItem6, MenuItem7: TMenuItem;
    RemItemButton, ClearItemButton, UpItemButton,
    DownItemButton, RemoveNodeButton: TSpeedButton;
    movItemsDown, movItemsUp, remAllItems, remSelectedItem,
    addAsOutput, addAsInput, remSelectedNode: TAction;

    procedure addAsInputExecute(Sender: TObject);
    procedure addAsOutputExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure movItemsDownExecute(Sender: TObject);
    procedure movItemsUpExecute(Sender: TObject);
    procedure remAllItemsExecute(Sender: TObject);
    procedure remSelectedItemExecute(Sender: TObject);
    procedure remSelectedNodeExecute(Sender: TObject);
    procedure ShellTreeView1SelectionChanged(Sender: TObject);
  private
    FInputList, FOutputList: TStrings;
    procedure AddFile(FileName: String);
    procedure AddDirectory(Directory: String; Depth: Integer=0);
    procedure AddSelectedNodeToList(List: TStrings);
    function Compare1(Node1, Node2: TTreeNode): Integer;
    function ExtractSection(S: String): String;
    function GetFocusedListBox(out ListBox: TListBox; out DataList: TStrings
      ): Boolean;
    function GetRoot: String;
    function MaxDepth: Integer;
    procedure LoadDirectory(Dir: String; Depth: Integer);
    procedure SetRoot(AValue: String);
    procedure SortAllSectionData;
    procedure UpdateControl(Sender: TObject);
  public
    procedure LoadControls(AInputList, AOutputList: TStrings);
    procedure SaveControls(AInputList, AOutputList: TStrings);
    function Execute(AInputList, AOutputList: TStrings): TModalResult;
    class function DefaultExecute(AInputList, AOutputList: TStrings): TModalResult;
    property Root: String read GetRoot write SetRoot;
  end;

implementation

{$R *.lfm}

{ TTestFilter1 }

procedure ResizeList(List: TStrings; ACount: Integer);
begin
  while List.Count > ACount do
  List.Delete(List.Count-1);
  while List.Count < ACount do
  List.Add('');
end;

operator ** (A, B: String)C: String;
begin
  C := ConcatPaths([A,B]);
end;

procedure ExtractSimplyPathList(List1, List2: TStrings);

  function Simplify(S: String; Loop: Integer): String;
  begin
    Result := '';
    for Loop := 1 to Loop do
    begin
      Result := ExtractFileName(S) ** Result;
      S := ExtractFileDir(S);
    end;
  end;

var i: Integer;
begin
  ResizeList(List2, List1.Count);
  for i := 0 to List2.Count-1 do
  List2[i] := Simplify(List1[i], 3);
end;

{ Required methods }

procedure TTestFilter1.FormCreate(Sender: TObject);
begin
  FInputList := TStringList.Create;
  FOutputList := TStringList.Create;
end;

procedure TTestFilter1.FormDestroy(Sender: TObject);
begin
  FInputList.Free;
  FOutputList.Free;
end;

procedure TTestFilter1.UpdateControl(Sender: TObject);
begin
  if Sender=ListBox1 then
    ExtractSimplyPathList(FInputList, ListBox1.Items)
  else if Sender=ListBox2 then
    ExtractSimplyPathList(FOutputList, ListBox2.Items)
  else
    raise Exception.Create('Sorry');
end;

procedure TTestFilter1.LoadControls(AInputList, AOutputList: TStrings);
begin
  FInputList.Assign(AInputList);
  FOutputList.Assign(AOutputList);
  UpdateControl(ListBox1);
  UpdateControl(ListBox2);
end;

procedure TTestFilter1.SaveControls(AInputList, AOutputList: TStrings);
begin
  AInputList.Assign(FInputList);
  AOutputList.Assign(FOutputList);
end;

function TTestFilter1.Execute(AInputList, AOutputList: TStrings): TModalResult;
begin
  LoadControls(AInputList, AOutputList);
  Result := ShowModal;
  if Result=mrOK then
  SaveControls(AInputList, AOutputList);
end;

class function TTestFilter1.DefaultExecute(AInputList, AOutputList: TStrings
  ): TModalResult;
var
  Form: TTestFilter1;
begin
  Form := TTestFilter1.Create(nil);
  try
    Result := Form.Execute(AInputList, AOutputList);
    finally Form.Free;
  end;
end;

{ Optional methods }

procedure TTestFilter1.LoadDirectory(Dir: String; Depth: Integer);
begin
  TreeView1.Items.Clear;
  AddDirectory(Dir, Depth);
  SortAllSectionData;
  TreeView1.Invalidate;
  Application.ProcessMessages;
end;

procedure TTestFilter1.SetRoot(AValue: String);
begin
  ShellTreeView1.Root:=AValue;
end;

procedure TTestFilter1.AddSelectedNodeToList(List: TStrings);
var
  i, j: Integer;
begin
  for i := 0 to TreeView1.Items.Count-1 do
  with TreeView1.Items[i] do
  for j := 0 to Count-1 do
  if Selected or Items[j].Selected then
  List.Add(Items[j].Text);
end;

procedure TTestFilter1.addAsInputExecute(Sender: TObject);
begin
  AddSelectedNodeToList(FInputList);
  UpdateControl(ListBox1);
end;

procedure TTestFilter1.addAsOutputExecute(Sender: TObject);
begin
  AddSelectedNodeToList(FOutputList);
  UpdateControl(ListBox2);
end;

function TTestFilter1.Compare1(Node1, Node2: TTreeNode): Integer;
begin
  Result := Length(Node1.Text) - Length(Node2.Text);
  if Result=0 then Result := CompareStr(Node1.Text, Node2.Text);
end;

procedure TTestFilter1.SortAllSectionData;
var
  TreeNode: TTreeNode;
begin
  for TreeNode in TreeView1.Items do
  if TreeNode.Parent=nil then
  TreeNode.CustomSort(@Compare1);
end;

procedure TTestFilter1.remSelectedItemExecute(Sender: TObject);
var
  ListBox: TListBox;
  DataList: TStrings;
  i: Integer;
begin
  if not GetFocusedListBox(ListBox, DataList) then exit;
  for i := ListBox.Items.Count-1 downto 0 do
  if ListBox.Selected[i] then
  begin
    ListBox.Items.Delete(i);
    DataList.Delete(i);
  end;
end;

procedure TTestFilter1.remAllItemsExecute(Sender: TObject);
var
  ListBox: TListBox;
  DataList: TStrings;
begin
  if not GetFocusedListBox(ListBox, DataList) then exit;
  ListBox.Items.Clear;
  DataList.Clear;
end;

function TTestFilter1.GetFocusedListBox(out ListBox: TListBox; out DataList: TStrings): Boolean;
begin
  if ListBox1.Focused then
    begin ListBox := ListBox1; DataList := FInputList; end
  else if ListBox2.Focused then
    begin ListBox := ListBox2; DataList := FOutputList; end
  else exit(False);
  Result := True;
end;

function TTestFilter1.GetRoot: String;
begin
  Result := ShellTreeView1.Root;
end;

function TTestFilter1.MaxDepth: Integer;
begin
  Result := SpinEdit1.Value;
end;

procedure TTestFilter1.movItemsUpExecute(Sender: TObject);
var
  i: Integer;
  CanSwap: Boolean=false;
  ListBox: TListBox;
  DataList: TStrings;
begin
  if not GetFocusedListBox(ListBox, DataList) then exit;
  for i := 0 to DataList.Count-2 do
  begin
    if not ListBox.Selected[i] then CanSwap := true;
    if CanSwap and ListBox.Selected[i+1] then
    begin
      ListBox.Items.Exchange(i, i+1);
      DataList.Exchange(i, i+1);
      ListBox.Selected[i] := true;
      ListBox.Selected[i+1] := false;
    end;
  end;
end;

procedure TTestFilter1.movItemsDownExecute(Sender: TObject);
var
  i: Integer;
  CanSwap: Boolean=false;
  ListBox: TListBox;
  DataList: TStrings;
begin
  if not GetFocusedListBox(ListBox, DataList) then exit;
  for i := DataList.Count-1 downto 1 do
  begin
    if not ListBox.Selected[i] then CanSwap := true;
    if CanSwap and ListBox.Selected[i-1] then
    begin
      ListBox.Items.Exchange(i, i-1);
      DataList.Exchange(i, i-1);
      ListBox.Selected[i] := true;
      ListBox.Selected[i-1] := false;
    end;
  end;
end;


procedure TTestFilter1.remSelectedNodeExecute(Sender: TObject);
begin
  TreeView1.Selected.Free;
end;

procedure TTestFilter1.ShellTreeView1SelectionChanged(Sender: TObject);
begin
  LoadDirectory(ShellTreeView1.Path, MaxDepth);
end;

function TTestFilter1.ExtractSection(S: String): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(S) do
    if not (S[i] in ['0'..'9']) then
      Result += S[i]
    else if (i=1) or not (s[i-1] in ['0'..'9']) then
      Result+='*';
  end;

procedure TTestFilter1.AddFile(FileName: String);
var
  Section: String;
  TreeNode: TTreeNode;
  SectionList: TStrings;
begin
  SectionList:=TStringList.Create;
  try
    for TreeNode in TreeView1.Items do
    if TreeNode.Parent=nil then
    SectionList.AddObject(TreeNode.Text, TreeNode);

    Section := ExtractSection(FileName);
    if SectionList.IndexOf(Section) = -1 then
    SectionList.AddObject(Section, TreeView1.Items.Add(nil, Section));
    TreeNode := SectionList.Objects[SectionList.IndexOf(Section)] as TTreeNode;
    TreeView1.Items.AddChild(TreeNode, FileName);

    finally SectionList.Free;
  end;
end;

procedure Swap(var A, B: TStrings);
var C: TStrings;
begin
  C := A;
  A := B;
  B := C;
end;

procedure TTestFilter1.AddDirectory(Directory: String; Depth: Integer=0);
var
  List, NextList, FoundList: TStrings;
  i: Integer;
  S, S2: String;
begin
  if Depth=0 then Depth := MaxDepth;
  List := TStringList.Create;
  NextList := TStringList.Create;
  try
    NextList.Add(Directory);
    for i := 1 to Depth do
    begin
      Swap(List, NextList); NextList.Clear;
      for S in List do
      begin
        FoundList := FindAllDirectories(S, False);
        NextList.AddStrings(FoundList);
        FoundList.Free;

        FoundList := FindAllFiles(S, '', False);
        for S2 in FoundList do AddFile(S2);
        FoundList.Free;
      end;
    end;
  finally
    List.Free;
    NextList.Free;
  end;
end;

end.


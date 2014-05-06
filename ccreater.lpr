program ccreater;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, lazcontrols, problem_editor, utils, edit_user_form,
  token_editor, limit_editor, multi_update, time_editor, user_database,
  token_database, limit_database, problem_database, basic_database,
  contest_database, database, add_problem_form, DefaultTranslator,
  properties_editor, subtask_database, subtask_editor, workspace_database,
  test_filter_1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TContestEditorForm, ContestEditorForm);
  Application.Run;
end.


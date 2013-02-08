{ ================================================================
  = Project   : Cenw                                             =
  ================================================================
  = Module    : TfrmNotices                                      =
  =                                                              =
  =             Routine to give user output for events that      =
  =             happen in the background,                        =
  ================================================================
  = File      : untNotices.PAS                                   =
  =                                                              =
  = Version   : 4.0                                              =
  ================================================================ }

unit untNotices;

{ Notices form
  This is an MDI child that fills up with notices during a simulation.

  To clear the notices, call the ClearNotices procedure.
  To add a specific notice, call the AddNotice procedure with the text of
  the notice.  The notice window will be created if necessary.

  Note: When adding this unit to the project, be careful to prevent it
  from being autocreated.  Remove this form from the left-hand panel of the
  Forms tab in Project Options, otherwise it will always be created.}

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TfrmNotices = class(TForm)
    memoNotices: TMemo;
    Procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmNotices: TfrmNotices;

Procedure AddNotice(sNotice: string);
Procedure ClearNotices;

implementation

{$R *.DFM}

uses untGraph;

Procedure AddNotice(sNotice: string);
begin
// add the notice
frmNotices.memoNotices.Lines.Add(sNotice);
// add the notice to the graph if we can
if Assigned(frmGraph) then
    frmGraph.AddNotice(sNotice);
end;

Procedure ClearNotices;
begin
// do we have to create a new noticeboard?
if (frmNotices = Nil) then
    frmNotices := TfrmNotices.Create(Application.MainForm);
// clear out all notices if the notice form is active
if Assigned(frmNotices) then
    frmNotices.memoNotices.Clear;
end;

Procedure TfrmNotices.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

Procedure TfrmNotices.FormDestroy(Sender: TObject);
begin
// ensure the notices form must be created anew next time
frmNotices := Nil;
end;

initialization
// reset the form pointer
frmNotices := Nil;
end.

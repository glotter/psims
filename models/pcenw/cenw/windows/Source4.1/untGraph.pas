{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmGraph                                        =
  =                                                              =
  =             Routine to set up the graphics that are          =
  =             displayed during program execution               =
  ================================================================
  = File      : untGraph.PAS                                     =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untGraph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, StdCtrls, Series;

type
  TfrmGraph = class(TForm)
    chtGraph: TChart;
    cmbSeries: TComboBox;
    lblSeries: TLabel;
    btnPrint: TButton;
    btnCopy: TButton;
    Procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Procedure cmbSeriesChange(Sender: TObject);
    Procedure btnPrintClick(Sender: TObject);
    Procedure btnCopyClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Procedure ShowNotice(Sender: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; x, y: Integer);
  public
    { Public declarations }
    Procedure AddNotice(sNotice: string);
  end;

var
  frmGraph: TfrmGraph;

implementation

{$R *.DFM}

uses
  untDeclarations, untNotices;

Procedure TfrmGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  frmGraph := Nil;
end;

Procedure TfrmGraph.cmbSeriesChange(Sender: TObject);
var
  ScreenVar: ScreenOptions;
  fMin, fMax: double;
  iCount: integer;
begin
  // disconnect all the series from the right hand axis first
  for iCount := 1 to chtGraph.SeriesList.Count do
      begin
      chtGraph.SeriesList.Items[iCount - 1].VertAxis := aLeftAxis;
      end;
  // choose the series to be the right hand axis
  if (cmbSeries.ItemIndex = 0) then
      begin
      // nothing to show, so turn off the right hand axis
      chtGraph.RightAxis.Visible := false;
      end
  else
      begin
      // turn on the right hand axis
      chtGraph.RightAxis.Visible := true;
      ScreenVar := ScreenOptions(cmbSeries.Items.Objects[cmbSeries.ItemIndex]);
      fMin := ScreenRec.LowRange[ScreenVar];
      fMax := ScreenRec.UpRange[ScreenVar];
      chtGraph.RightAxis.SetMinMax(fMin, fMax);
      // set the title
      chtGraph.RightAxis.Title.Font.Color := chtGraph.SeriesList.Items[cmbSeries.ItemIndex].SeriesColor;
      chtGraph.RightAxis.Title.Caption := cmbSeries.Items[cmbSeries.ItemIndex];
      chtGraph.SeriesList.Items[cmbSeries.ItemIndex].VertAxis := aBothVertAxis;
      end;
  end;

Procedure TfrmGraph.btnPrintClick(Sender: TObject);
begin
  chtGraph.Print;
end;

Procedure TfrmGraph.btnCopyClick(Sender: TObject);
begin
  chtGraph.CopyToClipboardBitmap;
end;

Procedure TfrmGraph.AddNotice(sNotice: string);
var
  series: TPointSeries;
  DisplayX: Real48;
begin
If Control.NoticeFlags then
   Begin
   // add the notice at the current position
   series := TPointSeries(chtGraph.SeriesList.Items[0]);
   If Control.MaxDays < DayDisplay then
      DisplayX := Control.TotalDays
   Else if Control.MaxDays < MonthDisplay then
      DisplayX := (Control.TotalDays / 30.4375)
   Else
      DisplayX := (Control.TotalDays / 365.25);
   series.AddXY(DisplayX, 1);
   End;
end;

Procedure TfrmGraph.ShowNotice(Sender: TChartSeries; ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  pt: TPoint;
Begin
// show the notice for this ValueIndex from the notice form
ShowMessage(frmNotices.memoNotices.Lines[ValueIndex]);
// need to pretend the mouse was released, otherwise we get a selection box
pt := chtGraph.ClientToScreen(Point(x,y));
PostMessage(chtGraph.Handle, WM_LBUTTONDOWN, pt.x, pt.y);
PostMessage(chtGraph.Handle, WM_LBUTTONUP, pt.x, pt.y);
End;

Procedure TfrmGraph.FormCreate(Sender: TObject);
var
  series: TPointSeries;
begin
  // create the notice series as the first series on the graph
  series := TPointSeries.Create(chtGraph);
  series.ColorEachPoint := false;
  series.Title := '';
  series.Pointer.Style := psDownTriangle;
  series.Pointer.HorizSize := 8;
  series.Pointer.VertSize := 10;
  series.SeriesColor := clRed;
  series.ShowInLegend := false;
  series.OnClick := ShowNotice;
  chtGraph.BottomAxis.Title.Caption := Control.Display;
  // add the series to the graph
  chtGraph.AddSeries(series);
end;

end.

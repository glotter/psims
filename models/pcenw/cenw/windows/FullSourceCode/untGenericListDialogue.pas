{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmHarvestThinningManagement                    =
  =                                                              =
  =             Interface routine to set up parameters for       =
  =             harvesting and thinning                          =
  ================================================================
  = File      : untHarvestThinningManagement.PAS                 =
  =                                                              =
  = Version   : 3.1                                              =
  ================================================================ }

unit untGenericListDialogue;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Grids;

type
  TfrmGenericListDialogue = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpTable: TGroupBox;
    dlgLoadSequence: TOpenDialog;
    dlgSaveSequence: TSaveDialog;
    btnSave: TBitBtn;
    btnLoad: TBitBtn;
    btnDeleteAll: TBitBtn;
    btnDelete: TBitBtn;
    btnDuplicate: TBitBtn;
    btnInsert: TBitBtn;
    rgOne: TRadioGroup;
    rgTwo: TRadioGroup;
    lblTextBox: TLabel;
    edtTextBox: TEdit;
    sgTable: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    btnRedraw: TBitBtn;
    btnLoadExtra: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure sgTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDuplicateClick(Sender: TObject);
    procedure btnDeleteAllClick(Sender: TObject);
    procedure sgTableDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgTableKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgTableAfterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgTableClick(Sender: TObject);
    procedure sgTableValidateEdit(Sender: TObject; col, row: Integer;
      var result: Boolean);
    procedure btnRedrawClick(Sender: TObject);
    procedure btnLoadExtraClick(Sender: TObject);
    procedure rgOneClick(Sender: TObject);
    procedure rgTwoClick(Sender: TObject);
  private
    { Private declarations }
    iLastRow: integer;
  public
    { Public declarations }
  end;

var frmGenericListDialogue: TfrmGenericListDialogue;
    Table_Header, Blank_Line: string;

implementation

{$R *.DFM}

uses untDeclarations, untMiscellaneous, untFieldValidation, untMain;

procedure TfrmGenericListDialogue.FormShow(Sender: TObject);
var
  iCount, iDays, iMonths, iYears, iRow, iCol, W, D, TotalWidth, nRadios,
  Vertical, RadioHeight, Horizontal, LabelHeight, MaxLength: integer;
  sText, St, RowStr: string;
  RadioItem: TStrings;
Begin
btnHelp.HelpContext := List.HelpContext;
sgTable.RowCount := List.nRows + 2;
TotalWidth := 17;
sgTable.ColCount := List.nEntries + 1;
sgTable.ColWidths[0] := 0;
Horizontal := 21;
MaxLength := 0;
For iCol := 0 to List.nEntries -1 do
    Begin
    If Length(List.Text[List.Header, iCol]) > MaxLength then
       MaxLength := Length(List.Text[List.Header, iCol]);
    End;
LabelHeight := 13 + 13 * Trunc(MaxLength div 14);
sgTable.Top := 57 + LabelHeight;
sgTable.Height := 304 - LabelHeight;
If List.RedrawOption then
   btnRedraw.Visible := true
Else
   btnRedraw.Visible := false;
List.Redraw := false;
For iCol := 0 to List.nEntries -1 do
    Begin
    sgTable.ColWidths[iCol + 1] := List.Width[iCol];
    TotalWidth := TotalWidth + List.Width[iCol] + 1;
    Case iCol of
         0: Begin
            Label1.Caption := List.Text[List.Header, iCol];
            Label1.Width := List.Width[iCol] -3;
            Label1.Height := LabelHeight;
            Label1.Left := Horizontal;
            Label1.Visible := true;
            End;
         1: Begin
            Label2.Caption := List.Text[List.Header, iCol];
            Label2.Width := List.Width[iCol] -3;
            Label2.Height := LabelHeight;
            Label2.Left := Horizontal;
            Label2.Visible := true;
            End;
         2: Begin
            Label3.Caption := List.Text[List.Header, iCol];
            Label3.Width := List.Width[iCol] -3;
            Label3.Height := LabelHeight;
            Label3.Left := Horizontal;
            Label3.Visible := true;
            End;
         3: Begin
            Label4.Caption := List.Text[List.Header, iCol];
            Label4.Width := List.Width[iCol] -3;
            Label4.Height := LabelHeight;
            Label4.Left := Horizontal;
            Label4.Visible := true;
            End;
         4: Begin
            Label5.Caption := List.Text[List.Header, iCol];
            Label5.Width := List.Width[iCol] -3;
            Label5.Height := LabelHeight;
            Label5.Left := Horizontal;
            Label5.Visible := true;
            End;
         5: Begin
            Label6.Caption := List.Text[List.Header, iCol];
            Label6.Width := List.Width[iCol] -3;
            Label6.Height := LabelHeight;
            Label6.Left := Horizontal;
            Label6.Visible := true;
            End;
         6: Begin
            Label7.Caption := List.Text[List.Header, iCol];
            Label7.Width := List.Width[iCol] -3;
            Label7.Height := LabelHeight;
            Label7.Left := Horizontal;
            Label7.Visible := true;
            End;
         7: Begin
            Label8.Caption := List.Text[List.Header, iCol];
            Label8.Width := List.Width[iCol] -3;
            Label8.Height := LabelHeight;
            Label8.Left := Horizontal;
            Label8.Visible := true;
            End;
         8: Begin
            Label9.Caption := List.Text[List.Header, iCol];
            Label9.Width := List.Width[iCol] -3;
            Label9.Height := LabelHeight;
            Label9.Left := Horizontal;
            Label9.Visible := true;
            End;
         9: Begin
            Label10.Caption := List.Text[List.Header, iCol];
            Label10.Width := List.Width[iCol] -3;
            Label10.Height := LabelHeight;
            Label10.Left := Horizontal;
            Label10.Visible := true;
            End;
         End;
    Horizontal := Horizontal + List.Width[iCol];
    End;
For iCol := List.nEntries to 9 do
    Begin
    Case iCol of
         0: Label1.Visible := false;
         1: Label2.Visible := false;
         2: Label3.Visible := false;
         3: Label4.Visible := false;
         4: Label5.Visible := false;
         5: Label6.Visible := false;
         6: Label7.Visible := false;
         7: Label8.Visible := false;
         8: Label9.Visible := false;
         9: Label10.Visible := false;
         End;
    End;
frmGenericListDialogue.Width := TotalWidth + 270;
If List.nRadios > 0 then
   Begin
   Vertical := 20;
   rgOne.Caption := List.RadioHeading[1];
   rgTwo.Top := Vertical;
   rgOne.Left := frmGenericListDialogue.Width - 200;
   rgOne.Items.Clear;
   For iRow := 1 to List.RadioOptions[1] do
       rgOne.Items.Add(List.RadioText[1, iRow]);
   nRadios := rgOne.Items.Count;
   RadioHeight := 20 + 20 * nRadios;
   Vertical := RadioHeight;
   rgOne.Height := RadioHeight;
   rgOne.ItemIndex := List.RbtnSelected[1];
   rgOne.Visible := true;
   End
Else
   Begin
   rgOne.Visible := false;
   Vertical := 0;
   End;
If List.nRadios > 1 then
   Begin
   rgTwo.Caption := List.RadioHeading[2];
   rgTwo.Top := Vertical + 20;
   rgTwo.Left := rgOne.Left;
   rgTwo.Items.Clear;
   For iRow := 1 to List.RadioOptions[2] do
       rgTwo.Items.Add(List.RadioText[2, iRow]);
   nRadios := rgTwo.Items.Count;
   RadioHeight := 20 + 20 * nRadios;
   Vertical := Vertical + 20 + RadioHeight;
   rgTwo.Height := RadioHeight;
   rgTwo.ItemIndex := List.RbtnSelected[2];
   rgTwo.Visible := true;
   End
Else
   Begin
   rgTwo.Visible := false;
   End;
If List.TextBox then
   Begin
   lblTextBox.Caption := List.TextBoxCaption;
   lblTextBox.Left := frmGenericListDialogue.Width - 200;
   edtTextBox.Left := frmGenericListDialogue.Width - 140;
   frmMain.FillEdit(Sender, edtTextBox, List.TextBoxEntry , 1);
   lblTextBox.Top := Vertical + 25;
   edtTextBox.Top := Vertical + 45;
   lblTextBox.Visible := true;
   edtTextBox.Visible := true;
   Vertical := Vertical + 80;
   End
Else
   Begin
   Vertical := Vertical + 40;
   lblTextBox.Visible := false;
   edtTextBox.Visible := false;
   End;
btnInsert.Left := frmGenericListDialogue.Width - 200;
btnDuplicate.Left := btnInsert.Left;
btnDelete.Left := btnInsert.Left;
btnDeleteAll.Left := btnInsert.Left;
btnSave.Left := btnInsert.Left;
btnLoad.Left := btnInsert.Left;
btnLoadExtra.Left := btnInsert.Left;
btnInsert.Top := Vertical;
btnDuplicate.Top := btnInsert.Top + 32;
btnDelete.Top := btnDuplicate.Top + 32;
btnDeleteAll.Top := btnDelete.Top + 32;
btnSave.Top := btnDeleteAll.Top + 32;
btnLoad.Top := btnSave.Top + 32;
btnLoadExtra.Top := btnLoad.Top + 32;
frmGenericListDialogue.Caption := List.Caption;
sgTable.Width := TotalWidth + 4;
grpTable.Width := sgTable.Width + 16;
sgTable.Rows[0].CommaText := Table_Header;
sgTable.RowHeights[0] := 0;
sgTable.Rows[sgTable.RowCount - 1].CommaText := Blank_Line;
sgTable.Width := TotalWidth + 4;
dlgLoadSequence.DefaultExt := List.FileExt;
dlgLoadSequence.Title := 'Load a ' + List.FileComment;
dlgLoadSequence.Filter := List.FileComment + ' (*.' + List.FileExt + ')|*' + List.FileExt;
dlgSaveSequence.DefaultExt := List.FileExt;
dlgSaveSequence.Title := 'Save a ' + List.FileComment;
dlgSaveSequence.Filter := List.FileComment + ' (*.' + List.FileExt + ')|*' + List.FileExt;
For iRow := 1 to List.nRows do
    Begin
    RowStr := '';
    For iCol := 0 to List.nEntries -1 do
        Begin
        If List.Data[iRow, iCol] <> -1 then
           Begin
           If List.DataType[iCol] = I then
              RowStr := RowStr + IntToStr(Round(List.Data[iRow, iCol]))
           Else if List.DataType[iCol] = S then
              RowStr := RowStr + (List.StringConst[Round(List.Data[iRow, iCol])])
           Else
              Begin
              GetField(List.Data[iRow, iCol], MaxedtFieldWidth, W, D);
              Str(List.Data[iRow, iCol]:W:D, St);
              RowStr := RowStr + St;
              End;
           End;
        If iCol <> (List.nEntries -1) then
           RowStr := RowStr + ',';
        End;
  sgTable.Rows[iRow].CommaText := ',' + RowStr;
  end;
end;

procedure TfrmGenericListDialogue.rgOneClick(Sender: TObject);
begin
if rgOne.ItemIndex <> List.RbtnSelected[1] then
   Begin
   List.Redraw := true;
   frmGenericListDialogue.btnOKClick(Sender);
   End;
end;

procedure TfrmGenericListDialogue.rgTwoClick(Sender: TObject);
begin
if rgTwo.ItemIndex <> List.RbtnSelected[2] then
   Begin
   List.Redraw := true;
   frmGenericListDialogue.btnOKClick(Sender);
   End;
end;

procedure TfrmGenericListDialogue.btnOKClick(Sender: TObject);
var
  bValid, RowContainsData: Boolean;
  iRow, jRow, LastRow, iCol, iStr, StrFound: integer;
  sText, ChkStr: string;
Begin
List.nRows := 0;
LastRow := sgTable.RowCount - 1;
For iRow := 1 to LastRow do // See how many rows actually contain data
    Begin
    RowContainsData := false;
    For iCol := 0 to List.nEntries -1 do
        Begin
        stext := UpperCase(sgTable.Cells[iCol + 1, iRow]);
        If not ((sText = ' ') or (sText = '')) then
           RowContainsData := true;
        End;
    If RowContainsData then
       List.nRows := List.nRows + 1
    Else if iRow < sgTable.RowCount - 1 then // tidying up blank rows
       Begin
       For jRow := iRow to LastRow do
           For iCol := 0 to List.nEntries -2 do
               sgTable.Cells[iCol + 1, jRow] := sgTable.Cells[iCol + 1, jRow + 1];
       LastRow := LastRow - 1;
       End;
    End;
For iRow := 1 to List.nRows do
    Begin
    For iCol := 0 to List.nEntries -1 do
        Begin
        stext := UpperCase(sgTable.Cells[iCol + 1, iRow]);
        If (sText = ' ') or (sText = '') then
           List.Data[iRow, iCol] := -1
        Else if List.DataType[iCol] = I then
           List.Data[iRow, iCol] := StrToInt(sText)
        Else if List.DataType[iCol] = S then
           Begin
           StrFound := 0;
           For iStr := 1 to List.StrOptions do
               Begin
               ChkStr := List.StringConst[iStr];
               if (sText = ChkStr) or (sText[1] = chkStr[1]) then
                  StrFound := iStr;
               End;
           List.Data[iRow, iCol] := StrFound;
           End
        Else { if List.DataType[iCol] = R then}
           List.Data[iRow, iCol] := StrToFloat(sText)
        End;
    End;
If List.TextBox then
   frmMain.GetEdit(Sender, edtTextBox, List.TextBoxEntry, 1);
List.RbtnSelected[1] := rgOne.ItemIndex;
List.RbtnSelected[2] := rgTwo.ItemIndex;
List.HasChanged := true;
ModalResult := mrOK;
end;

procedure TfrmGenericListDialogue.sgTableSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: String);
begin
  // increase the size of the table if the user has typed in the last row
  if (Value <> '') and (ARow = sgTable.RowCount - 1) then
  begin
    if (sgTable.RowCount < List.MaxRows + 2) then
    begin
      sgTable.RowCount := sgTable.RowCount + 1;
      sgTable.Rows[sgTable.RowCount - 1].CommaText := BLANK_LINE;
    end;
  end;
end;

procedure TfrmGenericListDialogue.btnLoadClick(Sender: TObject);
var
  infile: TextFile;
  sLine: string;
begin
  // load a sequence from file
  if (dlgLoadSequence.Execute) then
  begin
    // try to load the file
    AssignFile(infile, dlgLoadSequence.FileName);
    try
      Reset(infile);
      try
        // erase the grid first
        btnDeleteAllClick(Nil);
        // process all the lines in the file
        while not(eof(infile)) do
        begin
          Readln(infile, sLine);
          if (sLine <> '') then
          begin
            if (sgTable.RowCount < List.MaxRows + 2) then
            begin
              sgTable.RowCount := sgTable.RowCount + 1;
              sgTable.Rows[sgTable.RowCount - 1].CommaText := BLANK_LINE;
              sgTable.Rows[sgTable.RowCount - 2].CommaText := sLine;
            end;
          end;
        end;
      finally
        CloseFile(infile);
      end;
    except
      ShowMessage('Could not load the data sequence sequence!');
    end;
  end;
end;

procedure TfrmGenericListDialogue.btnLoadExtraClick(Sender: TObject);
var
  infile: TextFile;
  sLine: string;
begin
  // load a sequence from file
  if (dlgLoadSequence.Execute) then
  begin
    // try to load the file
    AssignFile(infile, dlgLoadSequence.FileName);
    try
      Reset(infile);
      while not(eof(infile)) do
          begin
          Readln(infile, sLine);
          if (sLine <> '') then
          begin
            if (sgTable.RowCount < List.MaxRows + 2) then
            begin
              sgTable.RowCount := sgTable.RowCount + 1;
              sgTable.Rows[sgTable.RowCount - 1].CommaText := BLANK_LINE;
              sgTable.Rows[sgTable.RowCount - 2].CommaText := sLine;
            end;
          end;
      end;
      CloseFile(infile);
    except
      ShowMessage('Could not load the data sequence sequence!');
    end;
  end;
end;

procedure TfrmGenericListDialogue.btnSaveClick(Sender: TObject);
var
  outfile: TextFile;
  sLine: string;
  iCount: integer;
begin
  // save the sequence to file
  if (dlgSaveSequence.Execute) then
  begin
    // create the file
    AssignFile(outfile, dlgSaveSequence.FileName);
    try
      Rewrite(outfile);
      try
        for iCount:=1 to sgTable.RowCount - 2 do
        begin
          sLine := sgTable.Rows[iCount].CommaText;
          Writeln(outfile, sLine);
        end;
      finally
        CloseFile(outfile);
      end;
    except
      ShowMessage('Could not save the data sequence!');
    end;
  end;
end;

procedure TfrmGenericListDialogue.btnInsertClick(Sender: TObject);
var
  iCount: integer;
  sRow: string;
begin
  // insert a new row before the current row
  if (sgTable.RowCount < List.MaxRows + 2) then
  begin
    sgTable.RowCount := sgTable.RowCount + 1;
    sgTable.Rows[sgTable.RowCount - 1].CommaText := BLANK_LINE;
    for iCount:=sgTable.RowCount - 1 downto sgTable.Row do
    begin
      sRow := sgTable.Rows[iCount - 1].CommaText;
      sgTable.Rows[iCount].CommaText := sRow;
    end;
    sgTable.Rows[sgTable.Row].CommaText := BLANK_LINE;
  end else
  begin
    ShowMessage('Cannot add more rows!');
  end;
end;

procedure TfrmGenericListDialogue.btnDeleteClick(Sender: TObject);
var
  iCount: integer;
  sRow: string;
begin
  // delete the current row
  for iCount:=sgTable.Row to sgTable.RowCount - 2 do
  begin
    sRow := sgTable.Rows[iCount + 1].CommaText;
    sgTable.Rows[iCount].CommaText := sRow;
  end;
  sgTable.RowCount := sgTable.RowCount - 1;
  sgTable.Rows[sgTable.RowCount - 1].CommaText := BLANK_LINE;
end;

procedure TfrmGenericListDialogue.btnDuplicateClick(Sender: TObject);
begin
  // duplicate the current row
  if (sgTable.RowCount < List.MaxRows + 2) then
  begin
    // insert a new row first
    btnInsertClick(Nil);
    // now duplicate it
    sgTable.Rows[sgTable.Row].CommaText := sgTable.Rows[sgTable.Row + 1].CommaText;
  end else
  begin
    ShowMessage('Cannot add more rows!');
  end;
end;

procedure TfrmGenericListDialogue.btnDeleteAllClick(Sender: TObject);
begin
  // delete all rows
  sgTable.RowCount := 2;
  // set the title row
  sgTable.Rows[0].CommaText := Table_Header;
  sgTable.Rows[1].CommaText := Blank_Line;
end;

procedure TfrmGenericListDialogue.sgTableDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  sText: string;
  iWidth, iHeight: integer;
  flags: integer;
begin
  // fill the rectangle
  if (gdSelected in State) xor (ARow = sgTable.Row) then
  begin
    sgTable.Canvas.Brush.Color := clHighLight;
    sgTable.Canvas.Font.Color := clWhite;
  end;
  sgTable.Canvas.FillRect(Rect);
  // draw the text
  if (ARow = 0) then
  begin
    // draw the title row centered
    flags := dt_center or dt_wordbreak or dt_noprefix;
    sText := sgTable.Cells[ACol, ARow];
    DrawText(sgTable.Canvas.Handle, PChar(sText), Length(sText), Rect, flags);
  end else
  begin
    // draw a normal cell right justified
    sText := sgTable.Cells[ACol, ARow];
    iWidth := sgTable.Canvas.TextWidth(sText);
    iHeight := sgTable.Canvas.TextHeight(sText);
    sgTable.Canvas.TextOut(Rect.Right - iWidth,
                           Rect.Top + (Rect.Bottom - Rect.Top - iHeight) div 2, sText);
  end;
end;

procedure TfrmGenericListDialogue.sgTableKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // save the current grid position
  iLastRow := sgTable.Row;
end;

procedure TfrmGenericListDialogue.sgTableAfterKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // check if the grid position has changed
  if (sgTable.Row <> iLastRow) then
    sgTable.Repaint;
end;

procedure TfrmGenericListDialogue.sgTableClick(Sender: TObject);
begin
  // refresh the table to update the row selection
  sgTable.Repaint;
end;

procedure TfrmGenericListDialogue.sgTableValidateEdit(
  Sender: TObject; col, row: Integer; var result: Boolean);
begin
  if (sgTable.Cells[col, row] <> '') and
     (Row = sgTable.RowCount - 1) and
     (sgTable.RowCount >= List.MaxRows + 2) then
  begin
    ShowMessage('Cannot add more rows!');
    sgTable.Cells[col, row] := '';
    result := false;
  end else
  begin
    result := true;
  end;
end;

procedure TfrmGenericListDialogue.btnRedrawClick(Sender: TObject);
begin
List.Redraw := true;
frmGenericListDialogue.btnOKClick(Sender);
end;

end.

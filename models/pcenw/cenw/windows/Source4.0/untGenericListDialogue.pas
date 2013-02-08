{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Modules   : TfrmGenericListDialogue                          =
  =                                                              =
  =             Generic interface routine to set up              =
  =             parameters lists for various parameters          =
  =             and management and other events                  =
  ================================================================
  = File      : untGenericListDialogue.PAS                       =
  =                                                              =
  = Version   : 4.0                                              =
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
    Label11: TLabel;
    btnRedraw: TBitBtn;
    btnLoadExtra: TBitBtn;
    ChkSetConstant: TCheckBox;
    chkIncrement: TCheckBox;
    Procedure TidyData (Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure sgTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    Procedure btnLoadClick(Sender: TObject);
    Procedure btnSaveClick(Sender: TObject);
    Procedure btnInsertClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnDuplicateClick(Sender: TObject);
    Procedure btnDeleteAllClick(Sender: TObject);
    Procedure sgTableDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure sgTableKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    Procedure sgTableAfterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    Procedure sgTableClick(Sender: TObject);
    Procedure sgTableValidateEdit(Sender: TObject; col, row: Integer;
      var result: Boolean);
    Procedure btnRedrawClick(Sender: TObject);
    Procedure btnLoadExtraClick(Sender: TObject);
    Procedure rgOneClick(Sender: TObject);
    Procedure rgTwoClick(Sender: TObject);
    procedure ChkSetConstantClick(Sender: TObject);
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

uses untDeclarations, untMiscellaneous, untFieldValidation, untMain,
  untGenericListColumns;

Procedure TfrmGenericListDialogue.TidyData (Sender: TObject);
var iRow, jRow, iCol, LastRow: Integer;
    RowContainsData: Boolean;
    sText: String;
Begin
List.nRows := 0;
LastRow := sgTable.RowCount - 1;
For iRow := LastRow downto 1 do // See how many rows actually contain data
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
    Else // tidying up blank rows
       Begin
       For jRow := iRow to LastRow do
           For iCol := 1 to List.nEntries do
               sgTable.Cells[iCol, jRow] := sgTable.Cells[iCol, jRow + 1];
       LastRow := LastRow - 1;
       sgTable.RowCount := LastRow + 1;
       End;
    End;
End; {of Procedure 'TidyData'}

Procedure TfrmGenericListDialogue.FormShow(Sender: TObject);
var iRow, iCol, W, D, TotalWidth, nRadioEntries,
    Vertical, RadioHeight, Horizontal, LabelHeight, MaxLength: integer;
    St, RowStr: string;
    FocusSet: Boolean;
Begin
FocusSet := false;
btnHelp.HelpContext := List.HelpContext;
sgTable.RowCount := List.nRows + 2;
TotalWidth := 17;
sgTable.ColCount := List.nEntries + 1;
sgTable.ColWidths[0] := 0;
Horizontal := 11;
MaxLength := 0;
For iCol := 0 to List.nEntries -1 do
    Begin
    If Length(List.Text[List.Header, iCol]) > MaxLength then
       MaxLength := Length(List.Text[List.Header, iCol]);
    End;
LabelHeight := 13 + 13 * Trunc(MaxLength div 12);
sgTable.Top := 57 + LabelHeight;
sgTable.Height := grpTable.Height - LabelHeight - 29;
chkSetConstant.Checked := false;
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
            Label1.Width := List.Width[iCol] -5;
            Label1.Height := LabelHeight;
            Label1.Left := Horizontal;
            Label1.Visible := true;
            End;
         1: Begin
            Label2.Caption := List.Text[List.Header, iCol];
            Label2.Width := List.Width[iCol] -5;
            Label2.Height := LabelHeight;
            Label2.Left := Horizontal;
            Label2.Visible := true;
            End;
         2: Begin
            Label3.Caption := List.Text[List.Header, iCol];
            Label3.Width := List.Width[iCol] -5;
            Label3.Height := LabelHeight;
            Label3.Left := Horizontal;
            Label3.Visible := true;
            End;
         3: Begin
            Label4.Caption := List.Text[List.Header, iCol];
            Label4.Width := List.Width[iCol] -5;
            Label4.Height := LabelHeight;
            Label4.Left := Horizontal;
            Label4.Visible := true;
            End;
         4: Begin
            Label5.Caption := List.Text[List.Header, iCol];
            Label5.Width := List.Width[iCol] -5;
            Label5.Height := LabelHeight;
            Label5.Left := Horizontal;
            Label5.Visible := true;
            End;
         5: Begin
            Label6.Caption := List.Text[List.Header, iCol];
            Label6.Width := List.Width[iCol] -5;
            Label6.Height := LabelHeight;
            Label6.Left := Horizontal;
            Label6.Visible := true;
            End;
         6: Begin
            Label7.Caption := List.Text[List.Header, iCol];
            Label7.Width := List.Width[iCol] -5;
            Label7.Height := LabelHeight;
            Label7.Left := Horizontal;
            Label7.Visible := true;
            End;
         7: Begin
            Label8.Caption := List.Text[List.Header, iCol];
            Label8.Width := List.Width[iCol] -5;
            Label8.Height := LabelHeight;
            Label8.Left := Horizontal;
            Label8.Visible := true;
            End;
         8: Begin
            Label9.Caption := List.Text[List.Header, iCol];
            Label9.Width := List.Width[iCol] -5;
            Label9.Height := LabelHeight;
            Label9.Left := Horizontal;
            Label9.Visible := true;
            End;
         9: Begin
            Label10.Caption := List.Text[List.Header, iCol];
            Label10.Width := List.Width[iCol] -5;
            Label10.Height := LabelHeight;
            Label10.Left := Horizontal;
            Label10.Visible := true;
            End;
        10: Begin
            Label11.Caption := List.Text[List.Header, iCol];
            Label11.Width := List.Width[iCol] -5;
            Label11.Height := LabelHeight;
            Label11.Left := Horizontal;
            Label11.Visible := true;
            End;
         End;
    Horizontal := Horizontal + List.Width[iCol];
    End;
For iCol := List.nEntries to 10 do
    Case iCol of
         0: Begin
            Label1.Caption := '';
            Label1.Visible := false;
            End;
         1: Begin
            Label2.Caption := '';
            Label2.Visible := false;
            End;
         2: Begin
            Label3.Caption := '';
            Label3.Visible := false;
            End;
         3: Begin
            Label4.Caption := '';
            Label4.Visible := false;
            End;
         4: Begin
            Label5.Caption := '';
            Label5.Visible := false;
            End;
         5: Begin
            Label6.Caption := '';
            Label6.Visible := false;
            End;
         6: Begin
            Label7.Caption := '';
            Label7.Visible := false;
            End;
         7: Begin
            Label8.Caption := '';
            Label8.Visible := false;
            End;
         8: Begin
            Label9.Caption := '';
            Label9.Visible := false;
            End;
         9: Begin
            Label10.Caption := '';
            Label10.Visible := false;
            End;
        10: Begin
            Label11.Caption := '';
            Label11.Visible := false;
            End;
         End;
frmGenericListDialogue.Width := TotalWidth + 270;
If List.nRadios > 0 then
   Begin
   Vertical := 33;
   rgOne.Caption := List.RadioHeading[1];
   rgOne.Top := Vertical;
   rgOne.Left := frmGenericListDialogue.Width - 200;
   rgOne.Items.Clear;
   For iRow := 1 to List.RadioOptions[1] do
       rgOne.Items.Add(List.RadioText[1, iRow]);
   nRadioEntries := rgOne.Items.Count;
   RadioHeight := 20 + 20 * nRadioEntries;
   Vertical := Vertical + RadioHeight;
   rgOne.Height := RadioHeight;
   rgOne.ItemIndex := List.RbtnSelected[1];
   rgOne.Visible := true;
   rgOne.SetFocus;
   rgTwo.Top := Vertical;
   FocusSet := true;
   End
Else
   Begin
   rgOne.Visible := false;
   Vertical := 60;
   End;
If List.nRadios > 1 then
   Begin
   Vertical := Vertical + 20;
   rgTwo.Caption := List.RadioHeading[2];
   rgTwo.Top := Vertical;
   rgTwo.Left := rgOne.Left;
   rgTwo.Items.Clear;
   For iRow := 1 to List.RadioOptions[2] do
       rgTwo.Items.Add(List.RadioText[2, iRow]);
   nRadioEntries := rgTwo.Items.Count;
   RadioHeight := 20 + 20 * nRadioEntries;
   Vertical := Vertical + RadioHeight;
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
   lblTextBox.Visible := true;
   edtTextBox.Visible := true;
   if not FocusSet then
      Begin
      edtTextBox.SetFocus;
      FocusSet := true;
      End;
   lblTextBox.Caption := List.TextBoxCaption;
   lblTextBox.Left := frmGenericListDialogue.Width - 200;
   edtTextBox.Left := frmGenericListDialogue.Width - 140;
   frmMain.FillEdit(Sender, edtTextBox, List.TextBoxEntry , 1);
   lblTextBox.Top := Vertical + 25;
   edtTextBox.Top := Vertical + 45;
   Vertical := Vertical + 80;
   End
Else
   Begin
   Vertical := Vertical + 40;
   lblTextBox.Visible := false;
   edtTextBox.Visible := false;
   End;
if not FocusSet then
   Begin
   btnInsert.SetFocus;
   FocusSet := true;
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
if (btnLoadExtra.Top + btnLoadExtra.Height) > (grpTable.Top + grpTable.Height) then
    Begin
    grpTable.Height := btnLoadExtra.Top + btnLoadExtra.Height - grpTable.Top;
    frmGenericListDialogue.Height := grpTable.Top + grpTable.Height + 50;
    End;
frmGenericListDialogue.Height := grpTable.Height + 110;
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

procedure TfrmGenericListDialogue.ChkSetConstantClick(Sender: TObject);
var iRow, iCol, W, D: Integer;
    sText, RowStr: String;
    NextNo: Real48;
    TextIn: array[0..MaxListEntries] of String;
begin
If chkSetConstant.Checked or chkIncrement.Checked then
   Begin
   TidyData (Sender);
   if chkIncrement.Checked  then
      List.TransUseIncrement := true
   Else // if chkSetConstant.Checked then
      List.TransUseIncrement := false;
   List.Trans1stRow := 1;
   List.TransLastRow := sgTable.RowCount - 1;
   if List.TransLastRow < List.Trans1stRow then
      List.TransLastRow := List.Trans1stRow;
   frmGenericListColumns.ShowModal;
   chkSetConstant.Checked := false;
   chkIncrement.Checked := false;
   if List.SetColumn then
      Begin
      if List.TransLastRow > sgTable.RowCount - 1 then
         Begin
         for iRow := sgTable.RowCount -1 to List.TransLastRow do
             For iCol := 0 to List.nEntries -1 do
                 sgTable.Cells[iCol + 1, iRow + 1] := sgTable.Cells[iCol + 1, iRow];
         sgTable.RowCount := List.TransLastRow + 1;
         End;
      for iRow := List.Trans1stRow to List.TransLastRow do
          Begin
          For iCol := 0 to List.nEntries -1 do
              Begin
              stext := UpperCase(sgTable.Cells[iCol + 1, iRow]);
              TextIn[iCol] := sText;
              End;
          if List.DataType[List.TransSetColumn] = I then
             Begin
             TextIn[List.TransSetColumn] := IntToStr(Round(List.Trans1stNo));
             if List.TransUseIncrement then
                TextIn[List.TransSetColumn] := IntToStr(Round(List.Trans1stNo + (iRow - List.Trans1stRow) * List.TransIncrement))
             Else
                TextIn[List.TransSetColumn] := IntToStr(Round(List.Trans1stNo));
             End
          Else if List.DataType[List.TransSetColumn] = R then
             Begin
             if List.TransUseIncrement then
                NextNo := List.Trans1stNo + (iRow - List.Trans1stRow) * List.TransIncrement
             Else
                NextNo := List.Trans1stNo;
             GetField(NextNo, MaxedtFieldWidth, W, D);
             Str(NextNo:W:D, sText);
             TextIn[List.TransSetColumn] := sText;
             End
          Else if List.DataType[List.TransSetColumn] = S then
             TextIn[List.TransSetColumn] := List.TransString;
          RowStr := '';
          For iCol := 0 to List.nEntries -1 do
              RowStr := RowStr + TextIn[iCol] + ',';
          sgTable.Rows[iRow].CommaText := ',' + RowStr;
          End;
      TidyData (Sender);
      sgTable.RowCount := sgTable.RowCount + 1;
      End;
   End;
end;

Procedure TfrmGenericListDialogue.rgOneClick(Sender: TObject);
begin
if rgOne.ItemIndex <> List.RbtnSelected[1] then
   Begin
   List.Redraw := true;
   frmGenericListDialogue.btnOKClick(Sender);
   End;
end;

Procedure TfrmGenericListDialogue.rgTwoClick(Sender: TObject);
begin
if rgTwo.ItemIndex <> List.RbtnSelected[2] then
   Begin
   List.Redraw := true;
   frmGenericListDialogue.btnOKClick(Sender);
   End;
end;

Procedure TfrmGenericListDialogue.btnOKClick(Sender: TObject);
var
  RowContainsData: Boolean;
  iRow, jRow, LastRow, iCol, iStr, StrFound: integer;
  ChkStr, sText: string;
Begin
TidyData (Sender);
{List.nRows := 0;
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
    End;}
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

Procedure TfrmGenericListDialogue.sgTableSetEditText(Sender: TObject;
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

Procedure TfrmGenericListDialogue.btnLoadClick(Sender: TObject);
var infile: TextFile;
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

Procedure TfrmGenericListDialogue.btnLoadExtraClick(Sender: TObject);
var infile: TextFile;
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

Procedure TfrmGenericListDialogue.btnSaveClick(Sender: TObject);
var outfile: TextFile;
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
        for iCount := 1 to sgTable.RowCount - 2 do
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

Procedure TfrmGenericListDialogue.btnInsertClick(Sender: TObject);
var iCount: integer;
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
   end
else
   ShowMessage('Cannot add more rows!');
end;

Procedure TfrmGenericListDialogue.btnDeleteClick(Sender: TObject);
var iCount: integer;
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

Procedure TfrmGenericListDialogue.btnDuplicateClick(Sender: TObject);
begin
// duplicate the current row
if (sgTable.RowCount < List.MaxRows + 2) then
    begin
    // insert a new row first
    btnInsertClick(Nil);
    // now duplicate it
    sgTable.Rows[sgTable.Row].CommaText := sgTable.Rows[sgTable.Row + 1].CommaText;
    end
else
    ShowMessage('Cannot add more rows!');
end;

Procedure TfrmGenericListDialogue.btnDeleteAllClick(Sender: TObject);
begin
// delete all rows
sgTable.RowCount := 2;
// set the title row
sgTable.Rows[0].CommaText := Table_Header;
sgTable.Rows[1].CommaText := Blank_Line;
end;

Procedure TfrmGenericListDialogue.sgTableDrawCell(Sender: TObject;
          ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var sText: string;
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
    end
else
    begin
    // draw a normal cell right justified
    sText := sgTable.Cells[ACol, ARow];
    iWidth := sgTable.Canvas.TextWidth(sText);
    iHeight := sgTable.Canvas.TextHeight(sText);
    sgTable.Canvas.TextOut(Rect.Right - iWidth,
                           Rect.Top + (Rect.Bottom - Rect.Top - iHeight) div 2, sText);
    end;
end;

Procedure TfrmGenericListDialogue.sgTableKeyDown(Sender: TObject;
          var Key: Word; Shift: TShiftState);
begin
// save the current grid position
iLastRow := sgTable.Row;
end;

Procedure TfrmGenericListDialogue.sgTableAfterKeyDown(
          Sender: TObject; var Key: Word; Shift: TShiftState);
begin
// check if the grid position has changed
if (sgTable.Row <> iLastRow) then
    sgTable.Repaint;
end;

Procedure TfrmGenericListDialogue.sgTableClick(Sender: TObject);
begin
// refresh the table to update the row selection
sgTable.Repaint;
end;

Procedure TfrmGenericListDialogue.sgTableValidateEdit(
          Sender: TObject; col, row: Integer; var result: Boolean);
begin
if (sgTable.Cells[col, row] <> '') and
   (Row = sgTable.RowCount - 1) and
   (sgTable.RowCount >= List.MaxRows + 2) then
    begin
    ShowMessage('Cannot add more rows!');
    sgTable.Cells[col, row] := '';
    result := false;
    end
else
    result := true;
end;

Procedure TfrmGenericListDialogue.btnRedrawClick(Sender: TObject);
begin
List.Redraw := true;
frmGenericListDialogue.btnOKClick(Sender);
end;

end.

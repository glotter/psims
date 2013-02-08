{ ================================================================
  = Project   : CenW                                             =
  ================================================================
  = Unit      : handle run of the model in spatial mode.         =
  =                                                              =
  ================================================================
  = File      : Geographical.pas                                 =
  =                                                              =
  = Version   : 4.0.0                                            =
  ================================================================ }

Unit untGeographical;

Interface
uses WinCrt, WinDos, WinProcs, WinTypes,      { Units to have acces to the }
     Win31, Strings,                          { Windows API.               }
     Objects, OWindows, ODialogs, OStdDlgs,   { Units that export Borlands }
     Validate,                                { class library OWL v1.0     }
     V_GetFld,                                { General units              }
     CenwObj, ID_Const;                       { Unit with useful objects   }
                                              { and declaration for Cenw.   }

type
   PGeographyDialog = ^TGeographyDialog;
   TGeographyDialog = object(TDialog)
      constructor Init(AParent : PWindowsObject; AName : PChar);

      procedure Ok(var msg : TMessage); virtual
         id_First + id_Ok;
      procedure EvIdbHelp(var msg : TMessage); virtual
         id_First + idb_Help;

   private
      GeographyTransRec : TGeographyTransRec;
   end;

Implementation

constructor TGeographyDialog.Init(AParent : PWindowsObject; AName : PChar);
var E : PEdit;
    C: PCheckbox;
    R1, R2, R3, R4: PRadioButton;

   Procedure Do_String (Datum: String; Var TransRec: FileTransferRecord; idc: word);
       var DotPos: Integer;
       Begin
       If Datum <> '' then
          Begin
          DotPos := Pos('.', Datum) - 1;
          If DotPos = -1 then
             DotPos := Length(Datum)
          Else if DotPos = 0 then
             DotPos := 1;
          Datum := Copy(Datum, 1, DotPos);
          End;
       new(E, InitResource(@self, idc, String_t_len));
       E^.SetValidator(new(PFilterValidator, Init(['0'..'9','\',':','_','a'..'z', 'A'..'Z'])));
       strPCopy(TransRec, Datum);
       End; {of Procedure 'Do_String'}

   Procedure Do_Details (Datum: Real; Var TransRec: TransferRecord; idc: word);
       var s : string[num_t_len-1];
       Width, Digits: integer;
       Begin
       new(E, InitResource(@self, idc, num_t_len));
       E^.SetValidator(new(PFilterValidator, Init(['0'..'9','+','-','.','e','E'])));
       GetField(Datum, 6, Width, Digits);
       Str(Datum:0:Digits, s);
       strpcopy(TransRec, s);
       End; {of Procedure 'Do_Details'}

   Procedure Do_Integer (Datum: Integer; Var TransRec: TransferRecord; idc: word);
       var s : string[num_t_len-1];
       Width, Digits: integer;
       Begin
       new(E, InitResource(@self, idc, num_t_len));
       E^.SetValidator(new(PFilterValidator, Init(['0'..'9','+','-','.','e','E'])));
       Str(Datum:0, s);
       strpcopy(TransRec, s);
       End; {of Procedure 'Do_Integer'}

begin
   inherited Init(AParent, AName);
   Do_String (Control.GeographyFile, GeographyTransRec.GeographyFile, idc_GeographyFile);
   Do_Integer (Control.Geography.Initial, GeographyTransRec.Initial, idc_GeographyInitial);
   Do_Integer (Control.Geography.Calcs, GeographyTransRec.Calcs, idc_GeographyCalcs);
   Do_Details (Control.Geography.RainMin, GeographyTransRec.RainMin, idc_GeographyRainMin);
   Do_Details (Control.Geography.RainMax, GeographyTransRec.RainMax, idc_GeographyRainMax);
   Do_Details (Control.Geography.TempMin, GeographyTransRec.TempMin, idc_GeographyTempMin);
   Do_Details (Control.Geography.TempMax, GeographyTransRec.TempMax, idc_GeographyTempMax);
   Do_Details (Control.Geography.LongMin, GeographyTransRec.LongMin, idc_GeographyLongMin);
   Do_Details (Control.Geography.LongMax, GeographyTransRec.LongMax, idc_GeographyLongMax);
   Do_Details (Control.Geography.LongInterval, GeographyTransRec.LongInterval, idc_GeographyLongInterval);
   Do_Details (Control.Geography.LatMin, GeographyTransRec.LatMin, idc_GeographyLatMin);
   Do_Details (Control.Geography.LatMax, GeographyTransRec.LatMax, idc_GeographyLatMax);
   Do_Details (Control.Geography.LatInterval, GeographyTransRec.LatInterval, idc_GeographyLatInterval);
   new(C, InitResource(@self, idc_GeographyDisplay));
   If Control.Geography.ShowNumeric then
      GeographyTransRec.Display := 1
   else
      GeographyTransRec.Display := 0;
   new(R1, InitResource(@self, idc_GeographyPlantType1));
   new(R2, InitResource(@self, idc_GeographyPlantType2));
   If Control.GeographyPlant = Optimal then
      Begin
      GeographyTransRec.PlantType1 := 0;
      GeographyTransRec.PlantType2 := 1;
      End
   else {If Control.GeographyPlant = Optimal then}
      Begin
      GeographyTransRec.PlantType1 := 1;
      GeographyTransRec.PlantType2 := 0;
      End;
   new(R3, InitResource(@self, idc_GeographySOMType1));
   new(R4, InitResource(@self, idc_GeographySOMType2));
   If Control.GeographySOM = UseInitial then
      Begin
      GeographyTransRec.SOMType1 := 0;
      GeographyTransRec.SOMType2 := 1;
      End
   else {If Control.GeographySOM = Equil then}
      Begin
      GeographyTransRec.SOMType1 := 1;
      GeographyTransRec.SOMType2 := 0;
      End;
   TransferBuffer := @GeographyTransRec;
   TransferData(tf_SetData);
end;

procedure TGeographyDialog.Ok(var msg : TMessage);
var ps, s: array[0..255] of char;
    code: integer;

   Procedure Get_String (Var Datum: FileNameType; Extension: String; TransRec: FileTransferRecord);
       var s : string[String_t_len-1];
       Begin
       s := strpas(TransRec);
       Datum := s + Extension;
       End; {of Procedure 'Get_String'}

   Procedure Get_Integer (Var Datum: Integer; TransRec: TransferRecord);
       var s : string[num_t_len-1];
       Begin
       s := strpas(TransRec);
       Val(s, Datum, code);
       End; {of Procedure 'Get_Integers'}

   Procedure Get_Real (Var Datum: Real; TransRec: TransferRecord);
       var s : string[num_t_len-1];
       Begin
       s := strpas(TransRec);
       Val(s, Datum, code);
       End; {of Procedure 'Get_Real'}

begin
   TransferData(tf_GetData);
   Get_String(Control.GeographyFile,'.BT!', GeographyTransRec.GeographyFile);
   Get_Integer (Control.Geography.Initial, GeographyTransRec.Initial);
   Get_Integer (Control.Geography.Calcs, GeographyTransRec.Calcs);
   Get_Real (Control.Geography.RainMin, GeographyTransRec.RainMin);
   Get_Real (Control.Geography.RainMax, GeographyTransRec.RainMax);
   Get_Real (Control.Geography.TempMin, GeographyTransRec.TempMin);
   Get_Real (Control.Geography.TempMax, GeographyTransRec.TempMax);
   Get_Real (Control.Geography.LongMin, GeographyTransRec.LongMin);
   Get_Real (Control.Geography.LongMax, GeographyTransRec.LongMax);
   Get_Real (Control.Geography.LongInterval, GeographyTransRec.LongInterval);
   Get_Real (Control.Geography.LatMin, GeographyTransRec.LatMin);
   Get_Real (Control.Geography.LatMax, GeographyTransRec.LatMax);
   Get_Real (Control.Geography.LatInterval, GeographyTransRec.LatInterval);
   If GeographyTransRec.Display = 1 then
      Control.Geography.ShowNumeric := true
   else
      Control.Geography.ShowNumeric := false;
   If GeographyTransRec.PlantType1 = 1 then
      Control.GeographyPlant := Specific
   Else {If GeographyTransRec.PlantType2 = 1 then}
      Control.GeographyPlant := Optimal;
   If GeographyTransRec.SOMType1 = 1 then
      Control.GeographySOM := Equil
   Else {If GeographyTransRec.SOMType2 = 1 then}
      Control.GeographySOM := UseInitial;
   Control.ProjectHasChanged := true;
   Control.GeographyMode := true;
   EndDlg(id_Ok);
end;

procedure TGeographyDialog.EvIdbHelp(var msg : TMessage);
begin
   WinHelp(HWindow, 'Cenw.HLP', HELP_CONTEXT, idm_Simulate);
end;

End.

{ --- end of file GEOGRAPH.PAS ------------------------------------------- }
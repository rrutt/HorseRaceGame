program HorseRace;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  HorseRaceMainForm;

begin
  Randomize;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THorseRaceMainForm, MainForm);
  Application.Run;
end.


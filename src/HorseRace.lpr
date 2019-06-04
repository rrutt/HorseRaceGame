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
  HorseRaceMainForm, HorsePlayerForm;

begin
  Randomize;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THorseRaceMainForm, MainForm);
  Application.CreateForm(THorsePlayerForm, PlayerForm);
  Application.Run;
end.


unit HorseRaceMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  HorseRaceTrack;

type

  { THorseRaceMainForm }

  THorseRaceMainForm = class(TForm)
    StartRace: TButton;
    LoadHorses: TButton;
    TimeMillisecondsLabel: TLabel;
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure LoadHorsesClick(Sender: TObject);
    procedure StartRaceClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  end;

  var
    MainForm: THorseRaceMainForm;

implementation

{$R *.lfm}

  var
    TheTrack: THorseRaceTrack;
    TimeInMilliseconds: Int64 = 0;
    //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

  procedure LoadHorsesForNewRace;
  begin

  end;

  procedure THorseRaceMainForm.FormCreate(Sender: TObject);
  begin
    TheTrack := THorseRaceTrack.Create(Self);
    TheTrack.Width := Self.Width;
    TheTrack.Top := 0;
    TheTrack.Left := 0;
    TheTrack.Parent := Self;
    TheTrack.Initialize;
    TheTrack.LoadHorses;
    TheTrack.DoubleBuffered := True;

    Self.Width := TheTrack.Width;

    Timer1.Interval := 10; // milliseconds
    Timer1.Enabled := False;

    LoadHorses.Enabled := False;
    StartRace.Enabled := True;
    TimeMillisecondsLabel.Caption := '';
  end;

  procedure THorseRaceMainForm.LoadHorsesClick(Sender: TObject);
  begin
    LoadHorses.Enabled := False;
    StartRace.Enabled := True;

    TimeInMilliseconds := 0;
    TimeMillisecondsLabel.Caption := '';

    TheTrack.LoadHorses;
    TheTrack.Paint;
  end;

  procedure THorseRaceMainForm.StartRaceClick(Sender: TObject);
  begin
    StartRace.Enabled := False;
    Timer1.Enabled := True;
  end;

  procedure THorseRaceMainForm.Timer1Timer(Sender: TObject);
  begin
    TimeInMilliseconds += Timer1.Interval;
    TimeMillisecondsLabel.Caption := Format('%d ms', [TimeInMilliseconds]);

    TheTrack.MoveHorses;
    TheTrack.Paint;

    if (TheTrack.RaceOver) then begin
      LoadHorses.Enabled := True;
      Timer1.Enabled := false;
    end;
  end;
end.


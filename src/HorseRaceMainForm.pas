unit HorseRaceMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  HorseRaceTrack;

type

  { THorseRaceMainForm }

  THorseRaceMainForm = class(TForm)
    ShowHorseInfo: TButton;
    MemoHorseInfo: TMemo;
    StartRace: TButton;
    LoadHorses: TButton;
    TimeMillisecondsLabel: TLabel;
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure LoadHorsesClick(Sender: TObject);
    procedure ShowHorseInfoClick(Sender: TObject);
    procedure StartRaceClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure LoadHorseInfo;
  end;

  var
    MainForm: THorseRaceMainForm;

implementation

{$R *.lfm}

  var
    TheTrack: THorseRaceTrack;
    TimeInMilliseconds: Int64 = 0;
    //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

  procedure THorseRaceMainForm.LoadHorseInfo;
  var
    horseInfo: TStringList;
  begin
    horseInfo := TheTrack.GetHorseInfo;
    MemoHorseInfo.Lines.Assign(horseInfo);
    horseInfo.Free;
    MemoHorseInfo.BringToFront;
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

    Self.LoadHorseInfo;

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

    Self.LoadHorseInfo;
  end;

  procedure THorseRaceMainForm.ShowHorseInfoClick(Sender: TObject);
  begin
    MemoHorseInfo.BringToFront;
  end;

  procedure THorseRaceMainForm.StartRaceClick(Sender: TObject);
  begin
    StartRace.Enabled := False;
    Timer1.Enabled := True;
    MemoHorseInfo.SendToBack;
  end;

  procedure THorseRaceMainForm.Timer1Timer(Sender: TObject);
  var
    TimeInSeconds: single;
  begin
    TimeInMilliseconds += Timer1.Interval;
    TimeInSeconds := TimeInMilliseconds * 0.001;
    TimeMillisecondsLabel.Caption := Format('%.2f sec', [TimeInSeconds]);

    TheTrack.MoveHorses;
    TheTrack.Paint;

    if (TheTrack.RaceOver) then begin
      LoadHorses.Enabled := True;
      Timer1.Enabled := false;
    end;
  end;
end.


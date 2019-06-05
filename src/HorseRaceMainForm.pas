unit HorseRaceMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  HorseRaceTrack,
  HorsePlayerForm;

type

  { THorseRaceMainForm }

  THorseRaceMainForm = class(TForm)
    MemoPayoffs: TMemo;
    ShowPayoffs: TButton;
    MemoHorseOdds: TMemo;
    RaceDistanceLabel: TLabel;
    ShowHorseOdds: TButton;
    MemoHorseInfo: TMemo;
    AddPlayer: TButton;
    StartRace: TButton;
    LoadHorses: TButton;
    TimeMillisecondsLabel: TLabel;
    Timer1: TTimer;

    procedure AddPlayerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadHorsesClick(Sender: TObject);
    procedure ShowHorseOddsClick(Sender: TObject);
    procedure ShowPayoffsClick(Sender: TObject);
    procedure StartRaceClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure LoadHorseAndOddsInfo;
  end;

  var
    MainForm: THorseRaceMainForm;
    PlayerFormWrappers: TCollection;

implementation

{$R *.lfm}

  var
    TheTrack: THorseRaceTrack;
    TimeInMilliseconds: Int64 = 0;
    //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

  procedure THorseRaceMainForm.LoadHorseAndOddsInfo;
  var
    horseInfo: TStringList;
    oddsInfo: TStringList;
  begin
    ShowPayoffs.Enabled := False;
    ShowHorseOdds.Enabled := False;

    MemoPayoffs.SendToBack;

    RaceDistanceLabel.Caption := Format('%d yards', [TheTrack.RaceDistance]);

    horseInfo := TheTrack.GetHorseInfo;
    MemoHorseInfo.Lines.Assign(horseInfo);
    horseInfo.Free;
    MemoHorseInfo.BringToFront;

    oddsInfo := TheTrack.GetOddsInfo;
    MemoHorseOdds.Lines.Assign(oddsInfo);
    oddsInfo.Free;
    MemoHorseOdds.BringToFront;
  end;

  procedure THorseRaceMainForm.FormCreate(Sender: TObject);
  var
    PlayerFormWrapper: THorsePlayerFormWrapper;
 begin
    TheTrack := THorseRaceTrack.Create(Self);
    TheTrack.Width := Self.Width;
    TheTrack.Top := 0;
    TheTrack.Left := 0;
    TheTrack.Parent := Self;
    TheTrack.Initialize;
    TheTrack.LoadHorses;
    TheTrack.DoubleBuffered := True;

    Self.LoadHorseAndOddsInfo;

    Self.Width := TheTrack.Width;

    Timer1.Interval := 10; // milliseconds
    Timer1.Enabled := False;

    LoadHorses.Enabled := False;
    StartRace.Enabled := True;
    TimeMillisecondsLabel.Caption := '';

    PlayerFormWrappers := TCollection.Create(THorsePlayerFormWrapper);
    PlayerFormWrapper := THorsePlayerFormWrapper(PlayerFormWrappers.Add);
    Application.CreateForm(THorsePlayerForm, PlayerFormWrapper.PlayerForm);
  end;

  procedure THorseRaceMainForm.AddPlayerClick(Sender: TObject);
  begin
  end;

  procedure THorseRaceMainForm.LoadHorsesClick(Sender: TObject);
  begin
    LoadHorses.Enabled := False;
    StartRace.Enabled := True;

    TimeInMilliseconds := 0;
    TimeMillisecondsLabel.Caption := '';

    TheTrack.LoadHorses;
    TheTrack.Paint;

    Self.LoadHorseAndOddsInfo;
  end;

  procedure THorseRaceMainForm.ShowHorseOddsClick(Sender: TObject);
  begin
    ShowHorseOdds.Enabled := False;
    MemoHorseOdds.BringToFront;
  end;

  procedure THorseRaceMainForm.ShowPayoffsClick(Sender: TObject);
  var
    payoffInfo: TStringList;
  begin
    ShowPayoffs.Enabled := False;

    payoffInfo := TheTrack.GetPayoffInfo;
    MemoPayoffs.Lines.Assign(payoffInfo);
    payoffInfo.Free;
    MemoPayoffs.BringToFront;
  end;

  procedure THorseRaceMainForm.StartRaceClick(Sender: TObject);
  begin
    StartRace.Enabled := False;
    Timer1.Enabled := True;
    MemoHorseInfo.SendToBack;
    MemoHorseOdds.SendToBack;
    MemoPayoffs.SendToBack;
  end;

  procedure THorseRaceMainForm.Timer1Timer(Sender: TObject);
  var
    TimeInSeconds: single;
    Wrapper: TCollectionItem;
    PlayerFormWrapper: THorsePlayerFormWrapper;
  begin
    TimeInMilliseconds += Timer1.Interval;
    TimeInSeconds := TimeInMilliseconds * 0.001;
    TimeMillisecondsLabel.Caption := Format('%.2f sec', [TimeInSeconds]);

    TheTrack.MoveHorses;
    TheTrack.Paint;

    if (TheTrack.RaceOver) then begin
      TheTrack.ComputePayoffInfo;

      for Wrapper in PlayerFormWrappers do begin
        PlayerFormWrapper := THorsePlayerFormWrapper(Wrapper);
        PlayerFormWrapper.PlayerForm.PayoffBets(TheTrack.ResultsAndPayoffs);
      end;

      LoadHorses.Enabled := True;
      ShowPayoffs.Enabled := True;
      ShowHorseOdds.Enabled := True;
      Timer1.Enabled := false;
    end;
  end;
end.


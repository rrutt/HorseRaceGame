unit HorseRaceMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, HorseRaceTrack;

type

  { THorseRaceMainForm }

  THorseRaceMainForm = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);
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

  procedure THorseRaceMainForm.FormCreate(Sender: TObject);
  begin
    TheTrack := THorseRaceTrack.Create(Self);
    TheTrack.Width := Self.Width;
    TheTrack.Top := 0;
    TheTrack.Left := 0;
    TheTrack.Parent := Self;
    TheTrack.Initialize();
    TheTrack.DoubleBuffered := True;

    Timer1.Interval := 10; // milliseconds
    Timer1.Enabled := True;
  end;

  procedure THorseRaceMainForm.Timer1Timer(Sender: TObject);
  begin
    TimeInMilliseconds += Timer1.Interval;
    Label1.Caption := Format('%d ms', [TimeInMilliseconds]);

    TheTrack.MoveHorses();
    TheTrack.Paint();
  end;
end.


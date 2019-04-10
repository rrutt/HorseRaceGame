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
  TheTrack: THorseRaceTrack;
  Counter: Integer = 0;
  ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

implementation

{$R *.lfm}

  procedure THorseRaceMainForm.FormCreate(Sender: TObject);
  begin
    TheTrack := THorseRaceTrack.Create(Self);
    TheTrack.Width := Self.Width;
    TheTrack.Top := 0;
    TheTrack.Left := 0;
    TheTrack.Parent := Self;
    TheTrack.Initialize();
    TheTrack.DoubleBuffered := True;

    Timer1.Interval := 1000; // milliseconds
    Timer1.Enabled := True;
  end;

  procedure THorseRaceMainForm.Timer1Timer(Sender: TObject);
  begin
    Inc(Counter);
    Label1.Caption := Format('%ds',[Counter]);

    TheTrack.MoveHorses();
    TheTrack.Paint();
  end;
end.


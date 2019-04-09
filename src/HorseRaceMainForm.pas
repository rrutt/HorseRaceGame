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

  private

  public

end;

var
  MainForm: THorseRaceMainForm;
  TheTrack: THorseRaceTrack;
  Counter: Integer;

implementation

{$R *.lfm}

  procedure THorseRaceMainForm.FormCreate(Sender: TObject);
  begin
    Counter := 0;

    TheTrack := THorseRaceTrack.Create(Self);
    TheTrack.Height := 400;
    TheTrack.Width := 500;
    TheTrack.Top := 0;
    TheTrack.Left := 0;
    TheTrack.Parent := Self;
    TheTrack.DoubleBuffered := True;

    Timer1.Interval := 1000; // milliseconds
    Timer1.Enabled := True;
  end;

  procedure THorseRaceMainForm.Timer1Timer(Sender: TObject);
  begin
    Inc(Counter);
    Label1.Caption := Format('%ds',[Counter]);

    TheTrack.DrawRandomBlock();
    TheTrack.Paint();
  end;
end.


unit HorseRaceTrack;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

const
  HORSE_COUNT = 10;
  HORSE_SPEED = 3;

type
  THorseRaceTrack = class(TCustomControl)
    private
      TrackSurfaceImage: TPortableNetworkGraphic;
      ToteImage: array[1..HORSE_COUNT] of TPortableNetworkGraphic;
      HorseImage: array[1..HORSE_COUNT] of TPortableNetworkGraphic;
      HorsePosition: array[1..HORSE_COUNT] of integer;
      HorseSpeed: array[1..HORSE_COUNT] of integer;
      HorseFinishOrder: array[1..HORSE_COUNT] of integer;
      HorseHeight: integer;
      HorseWidth: integer;
      FinishLine: integer;
      FinishedHorseCount: integer;
      RaceIsOver: boolean;
    public
      procedure Initialize();
      procedure LoadHorses();
      procedure MoveHorses();
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
      property RaceOver: boolean read RaceIsOver;
  end;

implementation

  procedure THorseRaceTrack.Initialize();
  var
    i: integer;
    toteName: string;
    tote: TPortableNetworkGraphic;
    horseName: string;
    horse: TPortableNetworkGraphic;
  begin
    TrackSurfaceImage := TPortableNetworkGraphic.Create;
    TrackSurfaceImage.LoadFromResourceName(HInstance, 'TRACK_SURFACE');
    for i := 1 to HORSE_COUNT do begin
      toteName := Format('TOTE_%d', [i]);
      tote := TPortableNetworkGraphic.Create;
      tote.LoadFromResourceName(HInstance, toteName);
      ToteImage[i] := tote;

      horseName := Format('HORSE_%d', [i]);
      horse := TPortableNetworkGraphic.Create;
      horse.LoadFromResourceName(HInstance, horseName);
      HorseHeight := horse.Height;
      HorseWidth := horse.Width;
      HorseImage[i] := horse;
      HorseSpeed[i] := HORSE_SPEED;
    end;

    Self.Height := HorseHeight * (1 + HORSE_COUNT);
    Self.Width := TrackSurfaceImage.Width;

    FinishLine := Self.Width - HorseWidth;
    FinishedHorseCount := 0;
    RaceIsOver := false;
  end;

  procedure THorseRaceTrack.LoadHorses();
  var
    i: integer;
  begin
    for i := 1 to HORSE_COUNT do begin
      HorsePosition[i] := 0;
      HorseFinishOrder[i] := 0;
    end;

    FinishedHorseCount := 0;
    RaceIsOver := false;
  end;

  procedure THorseRaceTrack.MoveHorses();
  var
    i: integer;
  begin
    for i := 1 to HORSE_COUNT do begin
      HorsePosition[i] += Round(HorseSpeed[i] * Random());
      if (HorsePosition[i] > FinishLine) then begin
        HorsePosition[i] := FinishLine;
        if (HorseFinishOrder[i]  = 0) then begin
          Inc(FinishedHorseCount);
          HorseFinishOrder[i] := FinishedHorseCount;
          RaceIsOver := (FinishedHorseCount >= HORSE_COUNT);
        end;
      end;
    end;
  end;

  procedure THorseRaceTrack.EraseBackground(DC: HDC);
  begin
    // Uncomment this to enable default background erasing
    //inherited EraseBackground(DC);
  end;

  procedure THorseRaceTrack.Paint;
  var
    i: Integer;
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create;
    try
      // Initializes the Bitmap Size
      Bitmap.Height := Height;
      Bitmap.Width := Width;

      // Draws the background
      Bitmap.Canvas.Draw(0, 0, TrackSurfaceImage);

      Bitmap.Canvas.Pen.Color := clWhite;
      Bitmap.Canvas.Line(FinishLine, 0, FinishLine, Height);

      for i := 1 to HORSE_COUNT do begin
        Bitmap.Canvas.Draw(HorsePosition[i], (i - 1) * HorseHeight, HorseImage[i]);
        if (HorseFinishOrder[i] > 0) then begin
          Bitmap.Canvas.Draw((HorseFinishOrder[i] - 1) * HorseWidth, HORSE_COUNT * HorseHeight, ToteImage[i]);
        end;
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.


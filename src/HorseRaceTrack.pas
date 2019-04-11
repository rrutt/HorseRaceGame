unit HorseRaceTrack;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType,
  RaceHorse
  ;

const
  HORSE_COUNT = 10;
  HORSE_START_POSITION = 10.0;

type
  THorseRaceTrack = class(TCustomControl)
    private
      TrackSurfaceImage: TPortableNetworkGraphic;
      GateClosedImage: TPortableNetworkGraphic;
      GateOpenImage: TPortableNetworkGraphic;
      GateWidth: integer;
      ToteImage: array[1..HORSE_COUNT] of TPortableNetworkGraphic;
      HorseImage: array[1..HORSE_COUNT] of TPortableNetworkGraphic;
      HorseImageHeight: integer;
      HorseImageWidth: integer;
      FinishLine: integer;
      FinishPosition: integer;
      FinishedHorseCount: integer;
      RaceHasStarted: boolean;
      RaceIsOver: boolean;
      RaceHorse: array[1..HORSE_COUNT] of TRaceHorse;
    public
      procedure Initialize;
      procedure LoadHorses;
      procedure MoveHorses;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
      property RaceOver: boolean read RaceIsOver;
  end;

implementation

  procedure THorseRaceTrack.Initialize;
  var
    i: integer;
    toteImageName: string;
    toteBitmap: TPortableNetworkGraphic;
    horseImageName: string;
    horseBitmap: TPortableNetworkGraphic;
  begin
    TrackSurfaceImage := TPortableNetworkGraphic.Create;
    TrackSurfaceImage.LoadFromResourceName(HInstance, 'TRACK_SURFACE_WIDE');

    GateClosedImage := TPortableNetworkGraphic.Create;
    GateClosedImage.LoadFromResourceName(HInstance, 'STARTING_GATE_CLOSED');

    GateOpenImage := TPortableNetworkGraphic.Create;
    GateOpenImage.LoadFromResourceName(HInstance, 'STARTING_GATE_OPEN');
    GateWidth := GateOpenImage.Width;

    for i := 1 to HORSE_COUNT do begin
      toteImageName := Format('TOTE_%d', [i]);
      toteBitmap := TPortableNetworkGraphic.Create;
      toteBitmap.LoadFromResourceName(HInstance, toteImageName);
      ToteImage[i] := toteBitmap;

      horseImageName := Format('HORSE_%d', [i]);
      horseBitmap := TPortableNetworkGraphic.Create;
      horseBitmap.LoadFromResourceName(HInstance, horseImageName);
      HorseImageHeight := horseBitmap.Height;
      HorseImageWidth := horseBitmap.Width;
      HorseImage[i] := horseBitmap;

      RaceHorse[i] := TRaceHorse.Create;
    end;

    Self.Height := HORSE_COUNT * HorseImageHeight;
    Self.Width := TrackSurfaceImage.Width;

    FinishLine := Self.Width - HorseImageWidth;
    FinishPosition := FinishLine - HorseImageWidth;
    FinishedHorseCount := 0;
    RaceHasStarted := false;
    RaceIsOver := false;
  end;

  procedure THorseRaceTrack.LoadHorses;
  var
    i: integer;
  begin
    for i := 1 to HORSE_COUNT do begin
      RaceHorse[i].LoadHorse(HORSE_START_POSITION);
    end;

    FinishedHorseCount := 0;
    RaceHasStarted := false;
    RaceIsOver := false;
  end;

  procedure THorseRaceTrack.MoveHorses;
  var
    i: integer;
  begin
    RaceHasStarted := true;

    for i := 1 to HORSE_COUNT do begin
      RaceHorse[i].MoveHorse(FinishLine);
      if (RaceHorse[i].Position > FinishPosition) then begin
        if (RaceHorse[i].FinishOrder  = 0) then begin
          Inc(FinishedHorseCount);
          RaceHorse[i].FinishOrder := FinishedHorseCount;
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
      Bitmap.Height := Height;
      Bitmap.Width := Width;

      Bitmap.Canvas.Draw(0, 0, TrackSurfaceImage);

      for i := 1 to HORSE_COUNT do begin
        Bitmap.Canvas.Draw(Round(RaceHorse[i].Position), (i - 1) * HorseImageHeight, HorseImage[i]);

        if (RaceHasStarted) then begin
          Bitmap.Canvas.Draw(0, (i - 1) * HorseImageHeight, GateOpenImage);
        end else begin
          Bitmap.Canvas.Draw(0, (i - 1) * HorseImageHeight, GateClosedImage);
        end;

        if (RaceHorse[i].FinishOrder > 0) then begin
          Bitmap.Canvas.Draw(GateWidth + ((RaceHorse[i].FinishOrder - 1) * HorseImageWidth), (HORSE_COUNT - 1) * HorseImageHeight, ToteImage[i]);
        end;
      end;

      Bitmap.Canvas.Pen.Color := clWhite;
      Bitmap.Canvas.Line(FinishLine, 0, FinishLine, Height);

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.


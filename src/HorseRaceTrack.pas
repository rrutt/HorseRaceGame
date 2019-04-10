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
  public
    procedure Initialize();
    procedure MoveHorses();
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Paint; override;
  end;

implementation

  var
    HorseImage: array[1..HORSE_COUNT] of TPortableNetworkGraphic;
    HorsePosition: array[1..HORSE_COUNT] of integer;
    HorseSpeed: array[1..HORSE_COUNT] of integer;
    FinishLine: integer;

  procedure THorseRaceTrack.Initialize();
  var
    i: integer;
    horseName: string;
    image: TPortableNetworkGraphic;
    totalHeight: integer = 0;
  begin
    for i := 1 to HORSE_COUNT do begin
      horseName := Format('HORSE_%d', [i]);
      image := TPortableNetworkGraphic.Create;
      image.LoadFromResourceName(HInstance, horseName);
      totalHeight += image.Height;
      FinishLine := Self.Width - image.Width;
      HorseImage[i] := image;
      HorsePosition[i] := 0;
      HorseSpeed[i] := HORSE_SPEED;
    end;

    Self.Height := totalHeight;
  end;

  procedure THorseRaceTrack.MoveHorses();
  var
    i: integer;
  begin
    for i := 1 to HORSE_COUNT do begin
      HorsePosition[i] += Round(HorseSpeed[i] * Random());
      if (HorsePosition[i] > FinishLine) then begin
        HorsePosition[i] := FinishLine;
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
      Bitmap.Canvas.Pen.Color := clWhite;
      Bitmap.Canvas.Rectangle(0, 0, Width, Height);

      // Draws squares
      Bitmap.Canvas.Pen.Color := clBlack;
      for i := 1 to HORSE_COUNT do begin
        Bitmap.Canvas.Rectangle(
          0, Round((i - 1) * Height / HORSE_COUNT),
          Width, Round(i * Height / HORSE_COUNT));
      end;

      Bitmap.Canvas.Pen.Color := clGreen;
      Bitmap.Canvas.Line(FinishLine, 0, FinishLine, Height);

      for i := 1 to HORSE_COUNT do begin
        Bitmap.Canvas.Draw(HorsePosition[i], Round((i - 1) * Height / HORSE_COUNT), HorseImage[i]);
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.


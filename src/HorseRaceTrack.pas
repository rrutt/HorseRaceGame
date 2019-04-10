unit HorseRaceTrack;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

const
  HORSE_COUNT = 10;

type
  THorseRaceTrack = class(TCustomControl)
  public
    procedure Initialize();
    procedure MoveHorses();
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  end;

implementation

  var
    RandomX: Integer;
    RandomY: Integer;
    HorseImage: array[1..HORSE_COUNT] of TPortableNetworkGraphic;
    HorsePosition: array[1..HORSE_COUNT] of integer;

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
      HorseImage[i] := image;
      HorsePosition[i] := 0;
    end;

    Self.Height := totalHeight;
  end;

  procedure THorseRaceTrack.MoveHorses();
  var
    i: integer;
  begin
    for i := 1 to HORSE_COUNT do begin
      HorsePosition[i] := Random(Width);
    end;
  end;

  procedure THorseRaceTrack.EraseBackground(DC: HDC);
  begin
    // Uncomment this to enable default background erasing
    //inherited EraseBackground(DC);
  end;

  procedure THorseRaceTrack.Paint;
  var
    x, y: Integer;
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
      for y := 1 to HORSE_COUNT do begin
        Bitmap.Canvas.Rectangle(
          0, Round((y - 1) * Height / HORSE_COUNT),
          Width, Round(y * Height / HORSE_COUNT));
      end;

      for y := 1 to HORSE_COUNT do begin
        Bitmap.Canvas.Draw(HorsePosition[y], Round((y - 1) * Height / HORSE_COUNT), HorseImage[y]);
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.


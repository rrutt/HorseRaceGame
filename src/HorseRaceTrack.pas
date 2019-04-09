unit HorseRaceTrack;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

type
  THorseRaceTrack = class(TCustomControl)

  public
    procedure DrawRandomBlock();
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  end;

var
  RandomX: Integer;
  RandomY: Integer;

implementation

  procedure THorseRaceTrack.DrawRandomBlock();
  begin
    RandomX := Random(8);
    RandomY := Random(8);
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
      for x := 1 to 8 do
        for y := 1 to 8 do begin
          if ((x = RandomX) and (y = RandomY)) then
          begin
            Bitmap.Canvas.Pen.Color := clRed;
          end;
          Bitmap.Canvas.Rectangle(Round((x - 1) * Width / 8), Round((y - 1) * Height / 8),
            Round(x * Width / 8), Round(y * Height / 8));
        end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.


unit RaceHorse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  BASE_HORSE_SPEED = 2.0;

type
  TRaceHorse = class
    private
      HorsePosition: single;
      HorseSpeed: single;
      HorseFinishOrder: integer;
    public
      constructor Create;
      procedure LoadHorse(StartPosition: single);
      procedure MoveHorse(FinishLine: integer);
      property Position: single read HorsePosition;
      property FinishOrder: integer read HorseFinishOrder write HorseFinishOrder;
  end;

implementation
  constructor TRaceHorse.Create;
  begin
    HorseSpeed := BASE_HORSE_SPEED;
  end;

  procedure TRaceHorse.LoadHorse(StartPosition: single);
  begin
    HorsePosition := StartPosition;
    HorseFinishOrder := 0;
  end;

  procedure TRaceHorse.MoveHorse(FinishLine: integer);
  begin
    HorsePosition += HorseSpeed * Random;

    if (HorsePosition > FinishLine) then begin
      HorsePosition := FinishLine;
    end;
  end;
end.


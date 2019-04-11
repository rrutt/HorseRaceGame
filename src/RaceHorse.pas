unit RaceHorse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MINIMUM_HORSE_SPEED = 1.0;
  AVERAGE_HORSE_SPEED = 2.0;
  MAXIMUM_HORSE_SPEED = 3.0;

type
  HorseSpeedParameters = record
    BreakSpeed: single;
    BreakDistance: single;
    EarlySpeed: single;
    EarlyDistance: single;
    LateSpeed: single;
    LateDistance: single;
    ClosingSpeed: single;
  end;

  TRaceHorse = class
    private
      HorsePosition: single;
      HorseFinishOrder: integer;
      SpeedInfo: HorseSpeedParameters;
    public
      constructor Create(
        StartPosition: single;
        TrackLength: integer);
      procedure LoadHorse(StartPosition: single);
      procedure MoveHorse(FinishLine: integer);
      property Position: single read HorsePosition;
      property FinishOrder: integer read HorseFinishOrder write HorseFinishOrder;
  end;

implementation
  function Greatest(Value1: single; Value2: single): single;
  begin
    Result := Value1;
    if (Value1 < Value2) then begin
      Result := Value2;
    end;
  end;

  function Least(Value1: single; Value2: single): single;
  begin
    Result := Value1;
    if (Value1 > Value2) then begin
      Result := Value2;
    end;
  end;

  function RandomizeSpeedInfo(
    StartPosition: single;
    TrackLength: single): HorseSpeedParameters;
  var
    SpeedInfo: HorseSpeedParameters;
  begin
    with SpeedInfo do begin
      BreakSpeed := MINIMUM_HORSE_SPEED + (Random * AVERAGE_HORSE_SPEED);
      BreakDistance := StartPosition + (0.25 * Random * TrackLength);
      EarlySpeed := Least(BreakSpeed + (Random * AVERAGE_HORSE_SPEED), MAXIMUM_HORSE_SPEED);
      EarlyDistance := BreakDistance + (0.25 * Random * TrackLength);
      LateSpeed :=
        Greatest(
          Least(
            EarlySpeed + ((Random - 0.5) * MAXIMUM_HORSE_SPEED),
            MAXIMUM_HORSE_SPEED),
          MINIMUM_HORSE_SPEED);
      LateDistance := EarlyDistance + (0.5 * Random * TrackLength);
      ClosingSpeed :=
        Greatest(
          Least(
            LateSpeed + ((Random - 0.5) * MAXIMUM_HORSE_SPEED),
            MAXIMUM_HORSE_SPEED),
          MINIMUM_HORSE_SPEED);
    end;
    Result := SpeedInfo
  end;

  function ComputeCurrentSpeed(
    SpeedInfo: HorseSpeedParameters;
    Position: single): single;
  begin
    with SpeedInfo do begin
      if (Position <= BreakDistance) then Result := BreakSpeed
      else if (Position <= EarlyDistance) then Result := EarlySpeed
      else if (Position <= LateDistance) then Result := LateSpeed
      else Result := ClosingSpeed;
    end;
  end;

  constructor TRaceHorse.Create(
    StartPosition: single;
    TrackLength: integer);
  begin
    SpeedInfo := RandomizeSpeedInfo(StartPosition, TrackLength);
  end;

  procedure TRaceHorse.LoadHorse(StartPosition: single);
  begin
    HorsePosition := StartPosition;
    HorseFinishOrder := 0;
  end;

  procedure TRaceHorse.MoveHorse(FinishLine: integer);
  var
    HorseSpeed: single;
  begin
    HorseSpeed := ComputeCurrentSpeed(SpeedInfo, HorsePosition);
    HorsePosition += HorseSpeed * Random;

    if (HorsePosition > FinishLine) then begin
      HorsePosition := FinishLine;
    end;
  end;
end.


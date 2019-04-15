unit RaceHorse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjsonrtti;

const
  MINIMUM_HORSE_SPEED = 1.0;
  AVERAGE_HORSE_SPEED = 2.0;
  MAXIMUM_HORSE_SPEED = 3.0;

type
  THorseSpeedParameters = class(TPersistent)
    private
      fBreakSpeed: single;
      fBreakDistance: single;
      fEarlySpeed: single;
      fEarlyDistance: single;
      fLateSpeed: single;
      fLateDistance: single;
      fClosingSpeed: single;
    published
      property BreakSpeed: single read fBreakSpeed write fBreakSpeed;
      property BreakDistance: single read fBreakDistance write fBreakDistance;
      property EarlySpeed: single read fEarlySpeed write fEarlySpeed;
      property EarlyDistance: single read fEarlyDistance write fEarlyDistance;
      property LateSpeed: single read fLateSpeed write fLateSpeed;
      property LateDistance: single read fLateDistance write fLateDistance;
      property ClosingSpeed: single read fClosingSpeed write fClosingSpeed;
  end;

  TRaceHorse = class(TCollectionItem)
    private
      HorsePosition: single;
      HorseFinishOrder: integer;
      FSpeedInfo: THorseSpeedParameters;
    public
      procedure RandomizeSpeedInfo(
        StartPosition: single;
        TrackLength: single);
      procedure LoadHorse(StartPosition: single);
      procedure MoveHorse(FinishLine: integer);
      property Position: single read HorsePosition;
      property FinishOrder: integer read HorseFinishOrder write HorseFinishOrder;
      procedure FromJson(JsonString: string);
      function ToJson: string;
    published
      property SpeedInfo: THorseSpeedParameters read FSpeedInfo write FSpeedInfo;
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

  procedure TRaceHorse.RandomizeSpeedInfo(
    StartPosition: single;
    TrackLength: single);
  begin
    SpeedInfo := THorseSpeedParameters.Create;
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
  end;

  function ComputeCurrentSpeed(
    SpeedInfo: THorseSpeedParameters;
    Position: single): single;
  begin
    with SpeedInfo do begin
      if (Position <= BreakDistance) then Result := BreakSpeed
      else if (Position <= EarlyDistance) then Result := EarlySpeed
      else if (Position <= LateDistance) then Result := LateSpeed
      else Result := ClosingSpeed;
    end;
  end;

  procedure TRaceHorse.LoadHorse(StartPosition: single);
  begin
    HorsePosition := StartPosition;
    HorseFinishOrder := 0;
  end;

  procedure TRaceHorse.MoveHorse(FinishLine: integer);
  var
    HorseSpeed: single;
    FinishOrderOffset: single;
  begin
    HorseSpeed := ComputeCurrentSpeed(SpeedInfo, HorsePosition);
    HorsePosition += HorseSpeed * Random;

    FinishOrderOffset := 6 * (HorseFinishOrder - 1);
    if (HorsePosition > (FinishLine - FinishOrderOffset)) then begin
      HorsePosition := FinishLine - FinishOrderOffset;
    end;
  end;

  // http://wiki.freepascal.org/Streaming_JSON
  procedure TRaceHorse.FromJson(JsonString: string);
  var
    DeStreamer: TJSONDeStreamer;
  begin
    try
      DeStreamer := TJSONDeStreamer.Create(nil);
      SpeedInfo := THorseSpeedParameters.Create;
      DeStreamer.JSONToObject(JsonString, Self);
    finally
      DeStreamer.Destroy;
    end;
  end;

  function TRaceHorse.ToJson: string;
  var
    Streamer: TJSONStreamer;
    JsonString: string;
  begin
    try
      Streamer := TJSONStreamer.Create(nil);
      JsonString := Streamer.ObjectToJSONString(Self);
    finally
      Streamer.Destroy;
    end;
    result := JsonString;
  end;
end.


unit HorseRaceTrack;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType,
  RaceHorse,
  RaceHorsePopulation
  ;

const
  GATE_COUNT = 10;
  HORSE_START_POSITION = 10.0;

type
  THorseRaceTrack = class(TCustomControl)
    private
      TrackSurfaceImage: TPortableNetworkGraphic;
      TrackSurfaceImageWidth: integer;
      GateClosedImage: TPortableNetworkGraphic;
      GateOpenImage: TPortableNetworkGraphic;
      GateWidth: integer;
      ToteImage: array[1..GATE_COUNT] of TPortableNetworkGraphic;
      ToteImageWidth: integer;
      HorseImage: array[1..GATE_COUNT] of TPortableNetworkGraphic;
      HorseImageHeight: integer;
      HorseImageWidth: integer;
      FinishLine: integer;
      FinishPosition: integer;
      FinishedHorseCount: integer;
      RaceDistanceInYards: integer;
      RaceHasStarted: boolean;
      RaceIsOver: boolean;
      RaceHorse: array[1..GATE_COUNT] of TRaceHorse;
      HorseOdds: array[1..GATE_COUNT] of integer;
      HorsePopulation: TRaceHorsePopulation;
    public
      procedure Initialize;
      procedure LoadHorses;
      function GetHorseInfo: TStringList;
      function GetOddsInfo: TStringList;
      function GetPayoffInfo: TStringList;
      procedure MoveHorses;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
      property RaceOver: boolean read RaceIsOver;
      property RaceDistance: integer read RaceDistanceInYards;
  end;

implementation
  const
    RACE_DISTANCE_COUNT = 5;

  var
    RaceDistancesInYards: array [1..RACE_DISTANCE_COUNT] of integer = (
      220,
      300,
      330,
      400,
      440
      );

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
    TrackSurfaceImageWidth := TrackSurfaceImage.Width;

    GateClosedImage := TPortableNetworkGraphic.Create;
    GateClosedImage.LoadFromResourceName(HInstance, 'STARTING_GATE_CLOSED');

    GateOpenImage := TPortableNetworkGraphic.Create;
    GateOpenImage.LoadFromResourceName(HInstance, 'STARTING_GATE_OPEN');
    GateWidth := GateOpenImage.Width;

    for i := 1 to GATE_COUNT do begin
      toteImageName := Format('TOTE_%d', [i]);
      toteBitmap := TPortableNetworkGraphic.Create;
      toteBitmap.LoadFromResourceName(HInstance, toteImageName);
      ToteImageWidth := toteBitmap.Width;
      ToteImage[i] := toteBitmap;

      horseImageName := Format('HORSE_%d', [i]);
      horseBitmap := TPortableNetworkGraphic.Create;
      horseBitmap.LoadFromResourceName(HInstance, horseImageName);
      HorseImageHeight := horseBitmap.Height;
      HorseImageWidth := horseBitmap.Width;
      HorseImage[i] := horseBitmap;
    end;

    Self.Height := GATE_COUNT * HorseImageHeight;
    Self.Width := TrackSurfaceImageWidth;

    FinishedHorseCount := 0;
    RaceHasStarted := false;
    RaceIsOver := false;

    //HorsePopulation := TRaceHorsePopulation.CreateFromResource;
    //HorsePopulation := TRaceHorsePopulation.CreateFromFile;
    HorsePopulation := TRaceHorsePopulation.CreateRandom(HORSE_POPULATION_SIZE, HORSE_START_POSITION, TrackSurfaceImageWidth);
    //HorsePopulation.WriteToFile;
  end;

  procedure THorseRaceTrack.LoadHorses;
  var
    i: integer;
    RaceDistanceIndex: integer;
    RaceDistanceMaxYards: single;
    RaceDistanceFloat: single;
    RaceDistanceFraction: single;
    FinishLineMaxYards: integer;
  begin
    HorsePopulation.SortRandomly;
    for i := 1 to GATE_COUNT do begin
      RaceHorse[i] := HorsePopulation.LoadHorse(i, HORSE_START_POSITION);
    end;

    FinishedHorseCount := 0;
    RaceHasStarted := false;
    RaceIsOver := false;

    RaceDistanceIndex := 1 + Random(RACE_DISTANCE_COUNT);
    RaceDistanceInYards := RaceDistancesInYards[RaceDistanceIndex];
    RaceDistanceMaxYards := RaceDistancesInYards[RACE_DISTANCE_COUNT];

    RaceDistanceFloat := RaceDistanceInYards;
    RaceDistanceFraction := RaceDistanceFloat / RaceDistanceMaxYards;

    FinishLineMaxYards := TrackSurfaceImageWidth - HorseImageWidth;
    FinishLine := Round(FinishLineMaxYards * RaceDistanceFraction);

    FinishPosition := FinishLine - HorseImageWidth;
  end;

  function THorseRaceTrack.GetHorseInfo: TStringList;
  var
    horse: TRaceHorse;
    horseInfo: TStringList;
    i: integer;
  begin;
    horseInfo := TStringList.Create;
    for i := 1 to GATE_COUNT do begin
      horse := RaceHorse[i];
      horseInfo.Add(horse.Name);
      horseInfo.Add(
        Format(
          '  Speed Index: %d  Early Pace: %d  Late Pace: %d',
          [horse.SpeedIndex, horse.EarlyPace, horse.LatePace]));
    end;
    result := horseInfo;
  end;

  function THorseRaceTrack.GetOddsInfo: TStringList;
  var
    horse: TRaceHorse;
    oddsInfo: TStringList;
    i: integer;
    minSpeedIndex: single;
    maxSpeedIndex: single;
    totalSpeedIndex: single;
    deltaSpeedIndex: single;
    speedIndexFloat: single;
  begin;
    minSpeedIndex := 999;
    maxSpeedIndex := 0;
    totalSpeedIndex := 0;
    for i := 1 to GATE_COUNT do begin
      horse := RaceHorse[i];
      speedIndexFloat := horse.SpeedIndex;
      if (speedIndexFloat < minSpeedIndex) then begin
        minSpeedIndex := speedIndexFloat;
      end;
      if (speedIndexFloat > maxSpeedIndex) then begin
        maxSpeedIndex := speedIndexFloat;
      end;
      totalSpeedIndex += speedIndexFloat;
    end;
    deltaSpeedIndex := 1.0 + maxSpeedIndex - minSpeedIndex;

    oddsInfo := TStringList.Create;
    for i := 1 to GATE_COUNT do begin
      horse := RaceHorse[i];
      speedIndexFloat := horse.SpeedIndex;
      HorseOdds[i] := 1 + Round(10.0 * ((maxSpeedIndex - speedIndexFloat) / deltaSpeedIndex));
      oddsInfo.Add(horse.Name);
      oddsInfo.Add(
        Format(
          '  #%d = %d-1',
          [i, HorseOdds[i]]));
    end;
    result := oddsInfo;
  end;

  function EnsureMinPayoff(odds: single): currency;
  var
    payoff: currency;
  begin
    payoff := 2.0 * (1 + odds);
    if (payoff < 2.10) then begin
      payoff := 2.10;
    end;
    result := payoff
  end;

  function THorseRaceTrack.GetPayoffInfo: TStringList;
  var
    horse: TRaceHorse;
    payoffInfo: TStringList;
    i: integer;
    finishIndex: integer;
    finishedHorseIndex: array [1..GATE_COUNT] of integer;
    finishedOdds: array [1..GATE_COUNT] of integer;
    winPayoff: currency;
    placePayoff: currency;
    showPayoff: currency;
  begin;
    for i := 1 to GATE_COUNT do begin
      horse := RaceHorse[i];
      finishIndex := horse.FinishOrder;
      finishedHorseIndex[finishIndex] := i;
      finishedOdds[finishIndex] := HorseOdds[i];
    end;

    payoffInfo := TStringList.Create;

    horse := RaceHorse[finishedHorseIndex[1]];
    payoffInfo.Add(
      Format(
        '#%d  %s',
        [finishedHorseIndex[1], horse.Name]));
    winPayoff := EnsureMinPayoff(finishedOdds[1]);
    placePayoff := EnsureMinPayoff(finishedOdds[1] * 0.25);
    showPayoff := EnsureMinPayoff(finishedOdds[1] * 0.125);
    payoffInfo.Add(
      Format(
        '      Win %m  Place %m  Show %m',
        [winPayoff, placePayoff, showPayoff]));

    horse := RaceHorse[finishedHorseIndex[2]];
    payoffInfo.Add(
      Format(
        '#%d  %s',
        [finishedHorseIndex[2], horse.Name]));
    placePayoff := EnsureMinPayoff(finishedOdds[2] * 0.25);
    showPayoff := EnsureMinPayoff(finishedOdds[2] * 0.125);
    payoffInfo.Add(
      Format(
        '                 Place %m  Show %m',
        [placePayoff, showPayoff]));

    horse := RaceHorse[finishedHorseIndex[3]];
    payoffInfo.Add(
      Format(
        '#%d  %s',
        [finishedHorseIndex[3], horse.Name]));
    showPayoff := EnsureMinPayoff(finishedOdds[3] * 0.125);
    payoffInfo.Add(
      Format(
        '                              Show %m',
        [showPayoff]));

    result := payoffInfo;
  end;

  procedure THorseRaceTrack.MoveHorses;
  var
    i: integer;
  begin
    RaceHasStarted := true;

    for i := 1 to GATE_COUNT do begin
      RaceHorse[i].MoveHorse(FinishLine);
      if (RaceHorse[i].Position > FinishPosition) then begin
        if (RaceHorse[i].FinishOrder  = 0) then begin
          Inc(FinishedHorseCount);
          RaceHorse[i].FinishOrder := FinishedHorseCount;
          RaceIsOver := (FinishedHorseCount >= GATE_COUNT);
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

      for i := 1 to GATE_COUNT do begin
        Bitmap.Canvas.Draw(Round(RaceHorse[i].Position), (i - 1) * HorseImageHeight, HorseImage[i]);

        if (RaceHasStarted) then begin
          Bitmap.Canvas.Draw(0, (i - 1) * HorseImageHeight, GateOpenImage);
        end else begin
          Bitmap.Canvas.Draw(0, (i - 1) * HorseImageHeight, GateClosedImage);
        end;

        if (RaceHorse[i].FinishOrder > 0) then begin
          Bitmap.Canvas.Draw(GateWidth + ((RaceHorse[i].FinishOrder - 1) * ToteImageWidth), (GATE_COUNT - 1) * HorseImageHeight, ToteImage[i]);
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


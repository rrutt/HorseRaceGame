unit RaceHorsePopulation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, resource,
  RaceHorse;

type
  TRaceHorsePopulation = class(TPersistent)
    private
      fRaceHorseCollection: TCollection;
    public
      constructor CreateRandom(
        PopulationSize: integer;
        StartPosition: single;
        TrackLength: integer);
      function LoadHorse(
        GateIndex: integer;
        StartPosition: single): TRaceHorse;
      procedure WriteToFile;
      constructor CreateFromFile;
      constructor CreateFromResource;
    published
      property RaceHorseCollection: TCollection read fRaceHorseCollection write fRaceHorseCollection;
  end;

implementation
  constructor TRaceHorsePopulation.CreateRandom(
    PopulationSize: integer;
    StartPosition: single;
    TrackLength: integer);
  var
    i: integer;
    horse: TRaceHorse;
  begin
    RaceHorseCollection := TCollection.Create(TRaceHorse);
    for i := 1 to PopulationSize do begin
      horse := TRaceHorse(RaceHorseCollection.Add);
      horse.RandomizeSpeedInfo(StartPosition, TrackLength);
    end;
  end;

  function TRaceHorsePopulation.LoadHorse(
    GateIndex: integer;
    StartPosition: single): TRaceHorse;
  var
    horse: TRaceHorse;
  begin
    horse := TRaceHorse(RaceHorseCollection.Items[GateIndex - 1]);
    horse.LoadHorse(StartPosition);
    result := horse;
  end;

  procedure TRaceHorsePopulation.WriteToFile;
  var
    horseCount: integer;
    i: integer;
    horse: TRaceHorse;
    json: string;
    fsOut: TFileStream;
  begin
    fsOut := TFileStream.Create('HorsePopulation.data', fmCreate);
    horseCount := RaceHorseCollection.Count;
    fsOut.WriteDWord(horseCount);
    for i := 1 to horseCount do begin
      horse := TRaceHorse(RaceHorseCollection.Items[i - 1]);
      json := horse.ToJson;
      fsOut.WriteAnsiString(json);
    end;
    fsOut.Free;
  end;

  constructor TRaceHorsePopulation.CreateFromFile;
  var
    horseCount: integer;
    i: integer;
    horse: TRaceHorse;
    json: string;
    fsIn: TFileStream;
  begin
    RaceHorseCollection := TCollection.Create(TRaceHorse);
    fsIn := TFileStream.Create('HorsePopulation.data', fmOpenRead);
    horseCount := fsIn.ReadDWord;
    for i := 1 to horseCount do begin
      json := fsIn.ReadAnsiString;
      horse := TRaceHorse(RaceHorseCollection.Add);
      horse.FromJson(json);
    end;
    fsIn.Free;
  end;

  constructor TRaceHorsePopulation.CreateFromResource;
  var
    horseCount: integer;
    i: integer;
    horse: TRaceHorse;
    json: string;
    rsIn: TResourceStream;
  begin
    RaceHorseCollection := TCollection.Create(TRaceHorse);
    rsIn := TResourceStream.Create(HInstance, 'HORSEPOPULATION', MAKEINTRESOURCE(RT_RCDATA));
    horseCount := rsIn.ReadDWord;
    for i := 1 to horseCount do begin
      json := rsIn.ReadAnsiString;
      horse := TRaceHorse(RaceHorseCollection.Add);
      horse.FromJson(json);
    end;
    rsIn.Free;
  end;
end.


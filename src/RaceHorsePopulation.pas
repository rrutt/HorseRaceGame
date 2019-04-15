unit RaceHorsePopulation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjsonrtti,
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
      constructor CreateFromJson(JsonString: string);
      function ToJson: string;
      procedure WriteToJsonFile;
      constructor CreateFromJsonFile;
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

  // http://wiki.freepascal.org/Streaming_JSON
  constructor TRaceHorsePopulation.CreateFromJson(JsonString: string);
  var
    DeStreamer: TJSONDeStreamer;
  begin
    try
      DeStreamer := TJSONDeStreamer.Create(nil);
      RaceHorseCollection := TCollection.Create(TRaceHorse);
      DeStreamer.JSONToObject(JsonString, RaceHorseCollection);
    finally
      DeStreamer.Destroy;
    end;
  end;

  function TRaceHorsePopulation.ToJson: string;
  var
    Streamer: TJSONStreamer;
    JsonString: string;
  begin
    try
      Streamer := TJSONStreamer.Create(nil);
      JsonString := Streamer.ObjectToJSONString(RaceHorseCollection);
    finally
      Streamer.Destroy;
    end;
    result := JsonString;
  end;

  procedure TRaceHorsePopulation.WriteToJsonFile;
  var
    JsonString: string;
    fsOut: TFileStream;
  begin
    JsonString := Self.ToJson;
    fsOut := TFileStream.Create('HorsePopulation.json', fmCreate);
    fsOut.WriteAnsiString(JsonString);
    fsOut.Free;
  end;

  constructor TRaceHorsePopulation.CreateFromJsonFile;
  var
    JsonString: string;
    fsIn: TFileStream;
    DeStreamer: TJSONDeStreamer;
  begin
    fsIn := TFileStream.Create('HorsePopulation.json', fmOpenRead);
    JsonString := fsIn.ReadAnsiString;
    fsIn.Free;
    try
      DeStreamer := TJSONDeStreamer.Create(nil);
      RaceHorseCollection := TCollection.Create(TRaceHorse);
      DeStreamer.JSONToObject(JsonString, RaceHorseCollection);
    finally
      DeStreamer.Destroy;
    end;
  end;
end.


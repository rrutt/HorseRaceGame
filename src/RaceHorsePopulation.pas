unit RaceHorsePopulation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, resource,
  RaceHorse;

const
  HORSE_POPULATION_SIZE = 60;

type
  TRaceHorsePopulation = class(TPersistent)
    private
      fRaceHorseCollection: TCollection;
    public
      constructor CreateRandom(
        PopulationSize: integer;
        StartPosition: single;
        TrackWidth: integer);
      function LoadHorse(
        GateIndex: integer;
        StartPosition: single): TRaceHorse;
      procedure WriteToFile;
      constructor CreateFromFile;
      constructor CreateFromResource;
      procedure SortRandomly;
    published
      property RaceHorseCollection: TCollection read fRaceHorseCollection write fRaceHorseCollection;
  end;

implementation
  var
    HorseNames: array [1..HORSE_POPULATION_SIZE] of string = (
      'Fast Fourier',
      'Famous Amos',
      'Bourbon Believer',
      'Whisky River',
      'Victory Vodka',
      'Tequila Teaser',
      'Rum Forest Rum',
      'Gone For Gin',
      'Hay Now',
      'Oats For Otis',
      'Streaking And Peaking',
      'Kentucky Colonel',
      'Sam I Am',
      'Happy Hippy',
      'Bourbon Barrel Beer',
      'Bluegrass Boss',
      'Derby Dreamer',
      'Say No More',
      'Happy Horton',
      'Slippery Slope',
      'Pony Express',
      'Stewball',
      'Old Spice',
      'Freddie Freeloader',
      'Rocket Ready',
      'Meadow Man',
      'Paddock Pappy',
      'Italian Stallion',
      'Distillery Dan',
      'Mountain Man',
      'One Eyed Jack',
      'Suicide King',
      'Le Roi de Coeur',
      'Talk Like A Pirate',
      'Rickhouse Racer',
      'Lucky Louie',
      'Diamond Mine',
      'Gold Dust Guy',
      'Black Widow',
      'Deadwood Dealer',
      'Woodstock Wanderer',
      'Aisle Of White',
      'Restless Ridgling',
      'Finnicky Filly',
      'Pedal Steel Pony',
      'Swimming Pool Shark',
      'Lanky Lifeguard',
      'Femme Fatale',
      'Midnight Marauder',
      'Come Back Shane',
      'OK Corral',
      'Duncan Dough Nut',
      'Phil M Noir',
      'Endless Summer',
      'Channel Won',
      'Honeymooner',
      'Southern Dancer',
      'Goes To Eleven',
      'Dynasty Diva',
      'Rogue Agent'
      );

  constructor TRaceHorsePopulation.CreateRandom(
    PopulationSize: integer;
    StartPosition: single;
    TrackWidth: integer);
  var
    i: integer;
    horse: TRaceHorse;
  begin
    RaceHorseCollection := TCollection.Create(TRaceHorse);
    for i := 1 to PopulationSize do begin
      horse := TRaceHorse(RaceHorseCollection.Add);
      horse.Name := HorseNames[i];
      horse.RandomizeSpeedInfo(StartPosition, TrackWidth);
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

  function RandomCompare(
    {%H-}Item1: TCollectionItem;
    {%H-}Item2: TCollectionItem): integer;
  begin
    result := Random(10) - 5;
  end;

  procedure TRaceHorsePopulation.SortRandomly;
  begin;
    RaceHorseCollection.Sort(@RandomCompare);
  end;
end.


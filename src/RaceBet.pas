unit RaceBet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBetType = (WinBet, PlaceBet, ShowBet, QuinellaBet, ExactaBet, TrifectaBet);

  TRaceBet = class(TPersistent)
    private
      fBetType: TBetType;
      fHorseNumber1: integer;
      fHorseNumber2: integer;
      fHorseNumber3: integer;
    published
      property BetType: TBetType read fBetType;
      property HorseNumber1: integer read fHorseNumber1;
      property HorseNumber2: integer read fHorseNumber2;
      property HorseNumber3: integer read fHorseNumber3;
    public
      constructor Win(horse: integer);
      constructor Place(horse: integer);
      constructor Show(horse: integer);
      constructor Quinella(horse1: integer; horse2: integer);
      constructor Exacta(horse1: integer; horse2: integer);
      constructor Trifecta(horse1: integer; horse2: integer; horse3: integer);
      function FormatDisplayString(): string;
  end;

implementation
  constructor TRaceBet.Win(horse: integer);
  begin
    fBetType := WinBet;
    fHorseNumber1 := horse;
  end;

  constructor TRaceBet.Place(horse: integer);
  begin
    fBetType := PlaceBet;
    fHorseNumber1 := horse;
  end;

  constructor TRaceBet.Show(horse: integer);
  begin
    fBetType := ShowBet;
    fHorseNumber1 := horse;
  end;

  constructor TRaceBet.Quinella(horse1: integer; horse2: integer);
  begin
    fBetType := QuinellaBet;
    fHorseNumber1 := horse1;
    fHorseNumber2 := horse2;
  end;

  constructor TRaceBet.Exacta(horse1: integer; horse2: integer);
  begin
    fBetType := ExactaBet;
    fHorseNumber1 := horse1;
    fHorseNumber2 := horse2;
  end;

  constructor TRaceBet.Trifecta(horse1: integer; horse2: integer; horse3: integer);
  begin
    fBetType := TrifectaBet;
    fHorseNumber1 := horse1;
    fHorseNumber2 := horse2;
    fHorseNumber3 := horse3;
  end;

  function TRaceBet.FormatDisplayString(): string;
  begin
    if (fBetType = WinBet) then begin
      Result :=
        Format(
          '$2 Win %d',
          [fHorseNumber1]);
    end else if (fBetType = PlaceBet) then begin
      Result :=
        Format(
          '$2 Place %d',
          [fHorseNumber1]);
    end else if (fBetType = ShowBet) then begin
      Result :=
        Format(
          '$2 Show %d',
          [fHorseNumber1]);
    end else if (fBetType = QuinellaBet) then begin
      Result :=
        Format(
          '$2 Quinella %d / %d',
          [fHorseNumber1, fHorseNumber2]);
    end else if (fBetType = ExactaBet) then begin
      Result :=
        Format(
          '$2 Exacta %d / %d',
          [fHorseNumber1, fHorseNumber2]);
    end else if (fBetType = TrifectaBet) then begin
      Result :=
        Format(
          '$2 Trifecta %d / %d / %d',
          [fHorseNumber1, fHorseNumber2, fHorseNumber3]);
    end;
  end;
end.


unit RaceBet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBetType = (WinBet, PlaceBet, ShowBet, QuinellaBet, ExactaBet, TrifectaBet);

  TRaceBet = class(TCollectionItem)
    private
      fBetType: TBetType;
      fHorseNumber1: integer;
      fHorseNumber2: integer;
      fHorseNumber3: integer;
    public
      class function Win(horse: integer; bets: TCollection): TRaceBet; static;
      class function Place(horse: integer; bets: TCollection): TRaceBet; static;
      class function Show(horse: integer; bets: TCollection): TRaceBet; static;
      class function Quinella(horse1: integer; horse2: integer; bets: TCollection): TRaceBet; static;
      class function Exacta(horse1: integer; horse2: integer; bets: TCollection): TRaceBet; static;
      class function Trifecta(horse1: integer; horse2: integer; horse3: integer; bets: TCollection): TRaceBet;
      function FormatDisplayString(): string;
  end;

implementation
  class function TRaceBet.Win(horse: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fBetType := WinBet;
    bet.fHorseNumber1 := horse;
    Result := bet;
  end;

  class function TRaceBet.Place(horse: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fBetType := PlaceBet;
    bet.fHorseNumber1 := horse;
    Result := bet;
  end;

  class function TRaceBet.Show(horse: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fBetType := ShowBet;
    bet.fHorseNumber1 := horse;
    Result := bet;
  end;

  class function TRaceBet.Quinella(horse1: integer; horse2: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fBetType := QuinellaBet;
    bet.fHorseNumber1 := horse1;
    bet.fHorseNumber2 := horse2;
    Result := bet;
  end;

  class function TRaceBet.Exacta(horse1: integer; horse2: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fBetType := ExactaBet;
    bet.fHorseNumber1 := horse1;
    bet.fHorseNumber2 := horse2;
    Result := bet;
  end;

  class function TRaceBet.Trifecta(horse1: integer; horse2: integer; horse3: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fBetType := TrifectaBet;
    bet.fHorseNumber1 := horse1;
    bet.fHorseNumber2 := horse2;
    bet.fHorseNumber3 := horse3;
    Result := bet;
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


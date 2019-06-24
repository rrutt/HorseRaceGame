unit RaceBet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  RaceResults;

type
  TBetType = (WinBet, PlaceBet, ShowBet, QuinellaBet, ExactaBet, TrifectaBet);

  TRaceBet = class(TCollectionItem)
    private
      fBetType: TBetType;
      fHorseNumber1: integer;
      fHorseNumber2: integer;
      fHorseNumber3: integer;
      fPending: boolean;
      fPayoff: currency;
    public
      class function Win(horse: integer; bets: TCollection): TRaceBet; static;
      class function Place(horse: integer; bets: TCollection): TRaceBet; static;
      class function Show(horse: integer; bets: TCollection): TRaceBet; static;
      class function Quinella(horse1: integer; horse2: integer; bets: TCollection): TRaceBet; static;
      class function Exacta(horse1: integer; horse2: integer; bets: TCollection): TRaceBet; static;
      class function Trifecta(horse1: integer; horse2: integer; horse3: integer; bets: TCollection): TRaceBet;

      property Pending: boolean read fPending;
      function FormatDisplayString(): string;
      function ApplyRaceResults(TheResults: TRaceResults): currency;
  end;

implementation
  class function TRaceBet.Win(horse: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fPending := true;
    bet.fBetType := WinBet;
    bet.fHorseNumber1 := horse;
    Result := bet;
  end;

  class function TRaceBet.Place(horse: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fPending := true;
    bet.fBetType := PlaceBet;
    bet.fHorseNumber1 := horse;
    Result := bet;
  end;

  class function TRaceBet.Show(horse: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fPending := true;
    bet.fBetType := ShowBet;
    bet.fHorseNumber1 := horse;
    Result := bet;
  end;

  class function TRaceBet.Quinella(horse1: integer; horse2: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fPending := true;
    bet.fBetType := QuinellaBet;
    if (horse1 < horse2) then begin
      bet.fHorseNumber1 := horse1;
      bet.fHorseNumber2 := horse2;
    end else begin
      bet.fHorseNumber1 := horse2;
      bet.fHorseNumber2 := horse1;
    end;
    Result := bet;
  end;

  class function TRaceBet.Exacta(horse1: integer; horse2: integer; bets: TCollection): TRaceBet;
  var
    bet: TRaceBet;
  begin
    bet := TRaceBet(bets.Add);
    bet.fPending := true;
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
    bet.fPending := true;
    bet.fBetType := TrifectaBet;
    bet.fHorseNumber1 := horse1;
    bet.fHorseNumber2 := horse2;
    bet.fHorseNumber3 := horse3;
    Result := bet;
  end;

  function TRaceBet.FormatDisplayString(): string;
  begin
    if (fPending) then begin
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
    end else begin
      if (fBetType = WinBet) then begin
        Result :=
          Format(
            '$2 Win %d = %m',
            [fHorseNumber1, fPayoff]);
      end else if (fBetType = PlaceBet) then begin
        Result :=
          Format(
            '$2 Place %d = %m',
            [fHorseNumber1, fPayoff]);
      end else if (fBetType = ShowBet) then begin
        Result :=
          Format(
            '$2 Show %d = %m',
            [fHorseNumber1, fPayoff]);
      end else if (fBetType = QuinellaBet) then begin
        Result :=
          Format(
            '$2 Quinella %d / %d = %m',
            [fHorseNumber1, fHorseNumber2, fPayoff]);
      end else if (fBetType = ExactaBet) then begin
        Result :=
          Format(
            '$2 Exacta %d / %d = %m',
            [fHorseNumber1, fHorseNumber2, fPayoff]);
      end else if (fBetType = TrifectaBet) then begin
        Result :=
          Format(
            '$2 Trifecta %d / %d / %d = %m',
            [fHorseNumber1, fHorseNumber2, fHorseNumber3, fPayoff]);
      end;
    end;
  end;

  function TRaceBet.ApplyRaceResults(TheResults: TRaceResults): currency;
  begin
    if (fPending) then begin
      fPayoff := 0;
      fPending := false;

      if (fBetType = WinBet) then begin
        if (TheResults.WinHorseIndex = fHorseNumber1) then begin
          fPayoff := TheResults.WinHorsePayoffWin;
        end;
      end else if (fBetType = PlaceBet) then begin
        if (TheResults.WinHorseIndex = fHorseNumber1) then begin
          fPayoff := TheResults.WinHorsePayoffPlace;
        end else if (TheResults.PlaceHorseIndex = fHorseNumber1) then begin
          fPayoff := TheResults.PlaceHorsePayoffPlace;
        end;
      end else if (fBetType = ShowBet) then begin
        if (TheResults.WinHorseIndex = fHorseNumber1) then begin
          fPayoff := TheResults.WinHorsePayoffShow;
        end else if (TheResults.PlaceHorseIndex = fHorseNumber1) then begin
          fPayoff := TheResults.PlaceHorsePayoffShow;
        end else if (TheResults.ShowHorseIndex = fHorseNumber1) then begin
          fPayoff := TheResults.ShowHorsePayoff;
        end;
      end else if (fBetType = QuinellaBet) then begin
        if ((TheResults.WinHorseIndex = fHorseNumber1) and
            (TheResults.PlaceHorseIndex = fHorseNumber2)) then begin
          fPayoff := TheResults.QuinellaPayoff;
        end else if ((TheResults.WinHorseIndex = fHorseNumber2) and
            (TheResults.PlaceHorseIndex = fHorseNumber1)) then begin
          fPayoff := TheResults.QuinellaPayoff;
        end;
      end else if (fBetType = ExactaBet) then begin
        if ((TheResults.WinHorseIndex = fHorseNumber1) and
            (TheResults.PlaceHorseIndex = fHorseNumber2)) then begin
          fPayoff := TheResults.ExactaPayoff;
        end;
      end else if (fBetType = TrifectaBet) then begin
        if ((TheResults.WinHorseIndex = fHorseNumber1) and
            (TheResults.PlaceHorseIndex = fHorseNumber2) and
            (TheResults.ShowHorseIndex = fHorseNumber3)) then begin
          fPayoff := TheResults.TrifectaPayoff;
        end;
      end;
      Result := fPayoff;
    end else begin
      Result := 0;
    end;
 end;
end.


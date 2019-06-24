unit RaceResults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRaceResults = class(TPersistent)
    private
      fWinHorseIndex: integer;
      fPlaceHorseIndex: integer;
      fShowHorseIndex: integer;

      fWinHorseOdds: integer;
      fPlaceHorseOdds: integer;
      fShowHorseOdds: integer;

      fWinHorsePayoffWin: currency;
      fWinHorsePayoffPlace: currency;
      fWinHorsePayoffShow: currency;

      fPlaceHorsePayoffPlace: currency;
      fPlaceHorsePayoffShow: currency;

      fShowHorsePayoff: currency;

      fQuinellaPayoff: currency;
      fExactaPayoff: currency;
      fTrifectaPayoff: currency;
    public
      property WinHorseIndex: integer read fWinHorseIndex write fWinHorseIndex;
      property PlaceHorseIndex: integer read fPlaceHorseIndex write fPlaceHorseIndex;
      property ShowHorseIndex: integer read fShowHorseIndex write fShowHorseIndex;

      property WinHorseOdds: integer read fWinHorseOdds write fWinHorseOdds;
      property PlaceHorseOdds: integer read fPlaceHorseOdds write fPlaceHorseOdds;
      property ShowHorseOdds: integer read fShowHorseOdds write fShowHorseOdds;

      property WinHorsePayoffWin: currency read fWinHorsePayoffWin write fWinHorsePayoffWin;
      property WinHorsePayoffPlace: currency read fWinHorsePayoffPlace write fWinHorsePayoffPlace;
      property WinHorsePayoffShow: currency read fWinHorsePayoffShow write fWinHorsePayoffShow;

      property PlaceHorsePayoffPlace: currency read fPlaceHorsePayoffPlace write fPlaceHorsePayoffPlace;
      property PlaceHorsePayoffShow: currency read fPlaceHorsePayoffShow write fPlaceHorsePayoffShow;

      property ShowHorsePayoff: currency read fShowHorsePayoff write fShowHorsePayoff;

      property QuinellaPayoff: currency read fQuinellaPayoff write fQuinellaPayoff;
      property ExactaPayoff: currency read fExactaPayoff write fExactaPayoff;
      property TrifectaPayoff: currency read fTrifectaPayoff write fTrifectaPayoff;
  end;

implementation

end.


unit HorsePlayerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RaceBet;

type

  { THorsePlayerForm }

  THorsePlayerForm = class(TForm)
    BankrollLabel: TLabel;
    BankrollEdit: TEdit;
    BetsLabel: TLabel;
    ExactaCombo1: TComboBox;
    ExactaCombo2: TComboBox;
    ClearBetsButton: TButton;
    ShowLabel: TLabel;
    QuinellaLabel: TLabel;
    ExactaLabel: TLabel;
    TrifectaLabel: TLabel;
    WinLabel: TLabel;
    ShowCombo: TComboBox;
    TrifectaCombo1: TComboBox;
    TrifectaCombo2: TComboBox;
    TrifectaCombo3: TComboBox;
    PlaceBetsButton: TButton;
    WinCombo: TComboBox;
    MemoBets: TMemo;
    NameEdit: TEdit;
    NameLabel: TLabel;
    PlaceCombo: TComboBox;
    QuinellaCombo1: TComboBox;
    QuinellaCombo2: TComboBox;
    PlaceLabel: TLabel;
    BetPriceLabel: TLabel;
    procedure ClearBetsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PlaceBetsButtonClick(Sender: TObject);

  private
    Bankroll: currency;
    PlayerBets: TFPlist;

  public

  end;

var
  PlayerForm: THorsePlayerForm;

implementation

{$R *.lfm}

{ THorsePlayerForm }

  procedure THorsePlayerForm.FormCreate(Sender: TObject);
  begin
    NameEdit.Text := ' ';
    Bankroll := 1000;
    BankrollEdit.Text := Format('%m', [Bankroll]);
    PlayerBets := TFPList.Create;
    MemoBets.Lines.Clear;
  end;

  procedure THorsePlayerForm.ClearBetsButtonClick(Sender: TObject);
  begin
    WinCombo.ItemIndex := -1;
    PlaceCombo.ItemIndex := -1;
    ShowCombo.ItemIndex := -1;

    QuinellaCombo1.ItemIndex := -1;
    QuinellaCombo2.ItemIndex := -1;

    ExactaCombo1.ItemIndex := -1;
    ExactaCombo2.ItemIndex := -1;

    TrifectaCombo1.ItemIndex := -1;
    TrifectaCombo2.ItemIndex := -1;
    TrifectaCombo3.ItemIndex := -1;
  end;

  procedure THorsePlayerForm.PlaceBetsButtonClick(Sender: TObject);
  var
    betsInfo: TStringList;
    bet: TRaceBet;
  begin
    betsInfo := TStringList.Create;

    if (WinCombo.ItemIndex > 0) then begin
      bet := TRaceBet.Win(WinCombo.ItemIndex);
      PlayerBets.Add(bet);
      betsInfo.Add(bet.FormatDisplayString());
      Bankroll := Bankroll - 2;
      WinCombo.ItemIndex := -1;
    end;

    if (PlaceCombo.ItemIndex > 0) then begin
      bet := TRaceBet.Place(PlaceCombo.ItemIndex);
      PlayerBets.Add(bet);
      betsInfo.Add(bet.FormatDisplayString());
      Bankroll := Bankroll - 2;
      PlaceCombo.ItemIndex := -1;
    end;

    if (ShowCombo.ItemIndex > 0) then begin
      bet := TRaceBet.Show(ShowCombo.ItemIndex);
      PlayerBets.Add(bet);
      betsInfo.Add(bet.FormatDisplayString());
      Bankroll := Bankroll - 2;
      ShowCombo.ItemIndex := -1;
    end;

    if ((QuinellaCombo1.ItemIndex > 0) and (QuinellaCombo2.ItemIndex > 0) and
        (QuinellaCombo1.ItemIndex <> QuinellaCombo2.ItemIndex)) then begin
      bet := TRaceBet.Quinella(QuinellaCombo1.ItemIndex, QuinellaCombo2.ItemIndex);
      PlayerBets.Add(bet);
      betsInfo.Add(bet.FormatDisplayString());
      Bankroll := Bankroll - 2;
    end;

    if ((ExactaCombo1.ItemIndex > 0) and (ExactaCombo2.ItemIndex > 0) and
        (ExactaCombo1.ItemIndex <> ExactaCombo2.ItemIndex)) then begin
      bet := TRaceBet.Exacta(ExactaCombo1.ItemIndex, ExactaCombo2.ItemIndex);
      PlayerBets.Add(bet);
      betsInfo.Add(bet.FormatDisplayString());
      Bankroll := Bankroll - 2;
    end;

    if ((TrifectaCombo1.ItemIndex > 0) and (TrifectaCombo2.ItemIndex > 0) and (TrifectaCombo3.ItemIndex > 0) and
        (TrifectaCombo1.ItemIndex <> TrifectaCombo2.ItemIndex) and
        (TrifectaCombo1.ItemIndex <> TrifectaCombo3.ItemIndex) and
        (TrifectaCombo2.ItemIndex <> TrifectaCombo3.ItemIndex)) then begin
      bet := TRaceBet.Trifecta(TrifectaCombo1.ItemIndex, TrifectaCombo2.ItemIndex, TrifectaCombo3.ItemIndex);
      PlayerBets.Add(bet);
      betsInfo.Add(bet.FormatDisplayString());
      Bankroll := Bankroll - 2;
    end;

    MemoBets.Lines.AddStrings(betsInfo);
    BankrollEdit.Text := Format('%m', [Bankroll]);
  end;

end.


unit HorsePlayerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RaceBet,
  RaceResults;

type

  { THorsePlayerForm }

  THorsePlayerForm = class(TForm)
    BankrollLabel: TLabel;
    BankrollEdit: TEdit;
    BetsLabel: TLabel;
    CancelBetsButton: TButton;
    TrifectaBox: TCheckBox;
    ExactaCombo1: TComboBox;
    ExactaCombo2: TComboBox;
    ResetBetsButton: TButton;
    ShowLabel: TLabel;
    QuinellaLabel: TLabel;
    ExactaLabel: TLabel;
    TrifectaLabel: TLabel;
    WinLabel: TLabel;
    ShowCombo: TComboBox;
    TrifectaCombo1: TComboBox;
    TrifectaCombo2: TComboBox;
    TrifectaCombo3: TComboBox;
    ApplyBetsButton: TButton;
    WinCombo: TComboBox;
    MemoBets: TMemo;
    NameEdit: TEdit;
    NameLabel: TLabel;
    PlaceCombo: TComboBox;
    QuinellaCombo1: TComboBox;
    QuinellaCombo2: TComboBox;
    PlaceLabel: TLabel;
    BetPriceLabel: TLabel;
    procedure CancelBetsButtonClick(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure ResetBetsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplyBetsButtonClick(Sender: TObject);
    procedure DisplayBets;
    procedure PayoffBets(TheRaceResults: TRaceResults);
    procedure SetPlayerName(PlayerName: string);

  private
    Bankroll: currency;
    PlayerBetCollection: TCollection;

  public

  end;

  THorsePlayerFormWrapper = class(TCollectionItem)
    public
      PlayerForm: THorsePlayerForm;
  end;

implementation

{$R *.lfm}

{ THorsePlayerForm }

  procedure THorsePlayerForm.FormCreate(Sender: TObject);
  begin
    NameEdit.Text := ' ';
    Bankroll := 1000;
    BankrollEdit.Text := Format('%m', [Bankroll]);
    PlayerBetCollection := TCollection.Create(TRaceBet);
    MemoBets.Lines.Clear;
  end;

  procedure THorsePlayerForm.ResetBetsButtonClick(Sender: TObject);
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
    TrifectaBox.Checked := false;
  end;

  procedure THorsePlayerForm.CancelBetsButtonClick(Sender: TObject);
  var
    betItem: TCollectionItem;
    bet: TRaceBet;
  begin
    for betItem in PlayerBetCollection do begin
      bet := TRaceBet(betItem);
      if (bet.Pending) then begin
        Bankroll := Bankroll + 2.0;
      end;
    end;
    BankrollEdit.Text := Format('%m', [Bankroll]);
    MemoBets.Lines.Clear;
    PlayerBetCollection.Clear;
  end;

procedure THorsePlayerForm.NameEditChange(Sender: TObject);
begin
  Self.Caption := Format('Horse Player: %s', [NameEdit.Text]);
end;

  procedure THorsePlayerForm.ApplyBetsButtonClick(Sender: TObject);
  var
    {%H-}bet: TRaceBet;
  begin
    if (WinCombo.ItemIndex > 0) then begin
      bet := TRaceBet.Win(WinCombo.ItemIndex, PlayerBetCollection);
      Bankroll := Bankroll - 2;
      WinCombo.ItemIndex := -1;
    end;

    if (PlaceCombo.ItemIndex > 0) then begin
      bet := TRaceBet.Place(PlaceCombo.ItemIndex, PlayerBetCollection);
      Bankroll := Bankroll - 2;
      PlaceCombo.ItemIndex := -1;
    end;

    if (ShowCombo.ItemIndex > 0) then begin
      bet := TRaceBet.Show(ShowCombo.ItemIndex, PlayerBetCollection);
      Bankroll := Bankroll - 2;
      ShowCombo.ItemIndex := -1;
    end;

    if ((QuinellaCombo1.ItemIndex > 0) and (QuinellaCombo2.ItemIndex > 0) and
        (QuinellaCombo1.ItemIndex <> QuinellaCombo2.ItemIndex)) then begin
      bet := TRaceBet.Quinella(QuinellaCombo1.ItemIndex, QuinellaCombo2.ItemIndex, PlayerBetCollection);
      Bankroll := Bankroll - 2;
    end;

    if ((ExactaCombo1.ItemIndex > 0) and (ExactaCombo2.ItemIndex > 0) and
        (ExactaCombo1.ItemIndex <> ExactaCombo2.ItemIndex)) then begin
      bet := TRaceBet.Exacta(ExactaCombo1.ItemIndex, ExactaCombo2.ItemIndex, PlayerBetCollection);
      Bankroll := Bankroll - 2;
    end;

    if ((TrifectaCombo1.ItemIndex > 0) and (TrifectaCombo2.ItemIndex > 0) and (TrifectaCombo3.ItemIndex > 0) and
        (TrifectaCombo1.ItemIndex <> TrifectaCombo2.ItemIndex) and
        (TrifectaCombo1.ItemIndex <> TrifectaCombo3.ItemIndex) and
        (TrifectaCombo2.ItemIndex <> TrifectaCombo3.ItemIndex)) then begin
      if (TrifectaBox.Checked) then begin
        bet := TRaceBet.Trifecta(TrifectaCombo1.ItemIndex, TrifectaCombo2.ItemIndex, TrifectaCombo3.ItemIndex, PlayerBetCollection);
        bet := TRaceBet.Trifecta(TrifectaCombo1.ItemIndex, TrifectaCombo3.ItemIndex, TrifectaCombo2.ItemIndex, PlayerBetCollection);
        bet := TRaceBet.Trifecta(TrifectaCombo2.ItemIndex, TrifectaCombo1.ItemIndex, TrifectaCombo3.ItemIndex, PlayerBetCollection);
        bet := TRaceBet.Trifecta(TrifectaCombo2.ItemIndex, TrifectaCombo3.ItemIndex, TrifectaCombo1.ItemIndex, PlayerBetCollection);
        bet := TRaceBet.Trifecta(TrifectaCombo3.ItemIndex, TrifectaCombo1.ItemIndex, TrifectaCombo2.ItemIndex, PlayerBetCollection);
        bet := TRaceBet.Trifecta(TrifectaCombo3.ItemIndex, TrifectaCombo2.ItemIndex, TrifectaCombo1.ItemIndex, PlayerBetCollection);
        Bankroll := Bankroll - 12;
      end else begin
        bet := TRaceBet.Trifecta(TrifectaCombo1.ItemIndex, TrifectaCombo2.ItemIndex, TrifectaCombo3.ItemIndex, PlayerBetCollection);
        Bankroll := Bankroll - 2;
      end;
    end;

    DisplayBets;
    BankrollEdit.Text := Format('%m', [Bankroll]);
  end;

  procedure THorsePlayerForm.DisplayBets;
  var
    betItem: TCollectionItem;
    bet: TRaceBet;
  begin
    MemoBets.Lines.Clear;
    for betItem in PlayerBetCollection do begin
      bet := TRaceBet(betItem);
      MemoBets.Lines.Add(bet.FormatDisplayString);
    end;
  end;

  procedure THorsePlayerForm.PayoffBets(TheRaceResults: TRaceResults);
  var
    betItem: TCollectionItem;
    bet: TRaceBet;
    betPayoff: currency;
  begin
    for betItem in PlayerBetCollection do begin
      bet := TRaceBet(betItem);
      betPayoff := bet.ApplyRaceResults(TheRaceResults);
      Bankroll := Bankroll + betPayoff;
    end;
    BankrollEdit.Text := Format('%m', [Bankroll]);
    DisplayBets;
  end;

  procedure THorsePlayerForm.SetPlayerName(PlayerName: string);
  begin
    NameEdit.Text := PlayerName;
  end;

end.


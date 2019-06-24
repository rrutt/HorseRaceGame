# Horse Race Game

Welcome to **Digital Downs**, a simulated Quarter Horse race track.

## What is a Quarter Horse?

As described in [Wikipedia](https://en.wikipedia.org/wiki/American_Quarter_Horse):

- The American Quarter Horse, or Quarter Horse, is an American breed of horse that excels at sprinting short distances. Its name came from its ability to outdistance other horse breeds in races of a quarter mile or less; some have been clocked at speeds up to 55 mph (88.5 km/h).
- Quarter Horse race horses are bred to sprint short distances ranging from 220 to 870 yards. Thus, they have long legs and are leaner than their stock type counterparts, but are still characterized by muscular hindquarters and powerful legs. Quarter Horses race primarily against other Quarter Horses, and their sprinting ability has earned them the nickname, "the world's fastest athlete".   

## The Software

"Horse Race Game" is an open source software game that simulates a horse race track with the following features:

- Simulated races along the lines of **[Sigma Derby](http://www.frontdesktip.com/1171/get-know-sigma-derby-gone/)** or **[Fortune Cup](http://www.frontdesktip.com/1418/fortune-cup-modernizes-horse-racing-games-casinos/)**.
- Betting menu with simulated parimutuel odds.
- Quarter Horse straight track racing.
- Parameterized horse physiology:
  - Break speed and distance.
  - Early speed and distance.
  - Late speed and distance.
  - Closing speed.
- _Speed Index_ figures derived from the horse physiology, including _Early Pace_ and _Late Pace_ values.

The software is a self-contained executable program, written in **[Free Pascal](https://www.freepascal.org/)**, that runs on Microsoft Windows.
(No separate run-time environment is required to run the program.)
The **[Lazarus Integrated Development Environment](https://www.lazarus-ide.org/)** was used to develop the Horse Race Game.
(Both Free Pascal and the Lazarus IDE are free open-source software products.) 

## Running the Program

You can run the Horse Race Game program on Microsoft Windows as follows:

- Download the **HorseRace.exe** binary executable file from the **bin** sub-folder from this GitHub.com page.
- Double-click the downloaded copy of **HorseRace.exe**

## Using the Program

When the program starts it displays the **Horse Race** main form and a **Horse Player** betting form.

The Main Form contains these elements:

- The horses for the next race are loaded into the Starting Gate on the left side of the form.
- The name, speed index, early pace, and late pace figures appear for each horse in the middle of the form.
- The name, betting number, and current Win odds appear for each horse on the right side of the form.
- A set of action buttons appear on the bottom of the form, along with the distance in yards for the next race.  (Some of the buttons are _disabled_ until after the race is run.)

![Horse Race Main Form](img/HorseRaceMainForm.png?raw=true "Horse Race Main Form")

The Horse Player Form contains these elements:

- A **Name** text entry field that allows you to change the name of the player appears in the upper left of the form.  The name also appears in the title bar for the form.
- a **Bankroll** display field that shows the current imaginary money available to the player.  This starts with $1,000 when the program is first started, or a new Horse Player Form is created with the **Add Player** button on the Main Form.
- A set of drop-down selectors for each bet type appear in the middle of the form. 
- A set of action buttons appear in the upper right of the form.
- A **Bets** log message box appears at the bottom of the form.

![Horse Player Form](img/HorsePlayerForm.png?raw=true "Horse Player Form")

Click the **Start Race** button on the Main Form to watch a simulated race without making any bets.

After the race completes the Main Form appears similar to this:

![Race Complete](img/RaceComplete.png?raw=true "Race Complete")

- The horses appear past the Finish Line, with the winning horse positioned farthest to the right.  The other horses appear positioned relative to the their finish order.
- The full finish order of the race appears at the bottom of the track area, with the winning horse bet number to the left.

Click the **Show Payoffs* button to seen the payoff prices for each kind of bet:

![Show Payoffs](img/ShowPayoffs.png?raw=true "Show Payoffs")

Click the **Show Horse Odds" button to repeat the display of the odds for each horse:

![Show Horse Odds](img/ShowHorseOdds.png?raw=true "Show Horse Odds")

Click the **Load Horses** button to prepare for another race.

## Betting on a Race

Use the Horse Player form to make your bet selections for the next race.

Use one or more drop-down selectors to choose the desire horse betting numbers for the desired bet type.

Here are the bet types allowed:

- **Win** - The selected horse must finish in first place.
- **Place** - The selected horse must finish in first or second place.
- **Show** - The selected horse must finish in first, second, or third place.
- **Quinella** - Select two horses.  The selected horses must finish in first and second place, _in either order_.
- **Exacta** - Select two horses.  The selected horses must finish in first and second place, _in the exact order_ selected.
- **Trifecta** - Select three horses.  The selected horses must finish in first, second, and third place, _in the exact order_ selected.
- **Trifecta Box** - Select three horses.  The selected horses must finish in first, second, and third place, _in any order_.  (This is recorded as 6 separate bets, representing all possible finish orders for the 3 horses.)
 
Here is an example of selections for a **Win** bet on horse #8 and a **Place** bet on horse #3:

![Bet Setup](img/BetWinPlaceSetup.png?raw=true "Bet Setup")

Click the **Apply Bet Selections** button to complete the bets:

![Bet Applied](img/BetWinPlaceApplied.png?raw=true "Bet Applied")

(To cancel any pending bets prior to the running of the next race, click the **Clear Bets** button.)

Here is an example of **Quinella**, **Exacta**, and **Trifecta** bets, which are often referred to as _exotic_ bets:

![Exotic Bet Setup](img/ExoticBetsSetup.png?raw=true "Exotic Bet Setup")

![Exotic Bet Applied](img/ExoticBetsApplied.png?raw=true "Exotic Bet Applied")

For the _exotic_ bets, the horse selections are _not_ cleared after the **Applied Bet Selections** button is clicked.
This allows you to adjust the horse selections to add additional _bet combinations_ for the associated bet type(s).

To clear all pending horse selection drop-downs, click the **Reset Bet Selections** button.
To clear a single horse selection drop-down, select the blank entry from the drop-down.

Here is the result of adding another **Exacta** combination and another **Trifecta** combination to the preceding exotic bets:

![Exotic Bet Applied 2](img/ExoticBetsApplied2.png?raw=true "Exotic Bet Applied 2")

Here is an example placing a **Trifecta Box** bet:

![Trifecta Box Bet](img/TrifectaBoxBet.png?raw=true "Trifecta Box Bet")

After a race has been run, the **Bets** message box in the Horse Player Form is updated to show the payoffs, if any, for the active bets:

![Player Bet Payoffs](img/PlayerBetPayoffs.png?raw=true "Player Bet Payoffs")

After reviewing your winnings (or losses), click the **Load Horses** button on the Main Form to prepare for the next race.
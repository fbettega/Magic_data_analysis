# Modern_data_analysis

Personal verion of magic metagame analysis

Result are avaible [here](https://mtg-modern.serveur-du-placard.xyz/)

## Dependencies

### Github repo and script

The initial json is obtained from the [badaro
parser](https://github.com/Badaro/MTGOArchetypeParser).  
To get scryfall data in csv i use my own script you can find it
[here](https://github.com/fbettega/fetch_scryfall_data_to_csv) file
generated should be put in \*Modern_data_analysis\_data\*.

## format includes

Table below list format includes and starting date

| Format  | Start date |
|:--------|:-----------|
| Modern  | 26/08/2024 |
| Legacy  | 26/08/2024 |
| Pauper  | 13/05/2024 |
| Pioneer | 26/08/2024 |
| Vintage | 26/08/2024 |

## Base data format

| Column name | Type | Definition |
|:--:|:--:|:--:|
| id | Integer | unique file id (for unnest of lists) |
| Tournament | Character | the type of tournament (note that leauges can cause problems because only 5-0s are in the file, so survivor bias ++++) |
| Meta | Character | large time unit (large change extension output or ban) |
| Week | Integer | number of the week |
| Date | Date | Tournament date |
| Result | Character | tournament results, column varies by tournament |
| Points | Character | sum of results (win = 3 Losse = 0 Draw = 1) |
| Win | Integer | number of wins |
| Loss | Integer | number of loss |
| Draw | Integer | number of draw |
| Player | Character | Player name |
| AnchorUri | Character | Url of the decklist |
| Archetype | Character | the archetype from the parser modified by FB and aggregated by the initial scrypt into a macro archetype. |
| Color | Character | deck colour (U = Blue, G : Green, R : Red, B : black, W: White) |
| Companion | Character | specific type of card defining decks |
| ReferenceArchetype_Archetype | Character | archetype of deck obtained with badaro’s initial parser |
| ReferenceArchetype_Color | Character | deck colour obtained with badaro’s initial parser (U = Blue, G : Green, R : Red, B : black, W: White) |
| ReferenceArchetype_Companion | Character | specific type of card defining decks obtained with badaro’s initial parser |
| Mainboard | List | Mainboard decklist |
| Sideboard | List | Sideboard decklist |
| Matchups | List | List of matchupes |
| Matches | Integer | number of matches played |
| Base_Archetype | Character | the archetype from the parser modified by FB and aggregated by the initial scrypt into a macro archetype. |
| Archetype_count | Integer | Number of times the Archetype is present in the dataset |

## Outpout

A quarto book for each format define above

## Warning

**Grid Search can be really long aroud 3 days for me**

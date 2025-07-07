# Modern_data_analysis

Personal verion of magic metagame analysis

Result are avaible [here](https://mtg-modern.serveur-du-placard.xyz/)

## Dependencies

**Warning**: I’ve tried to make this script as reproducible as possible,
but as it wasn’t my initial intention, some of the details may be
missing. I may have forgotten some packages or folders that are
necessary for the script to work properly.

### Github repo and script

The initial json is obtained from the [badaro
parser](https://github.com/Badaro/MTGOArchetypeParser).

Normally the pull of the necessary github repo is automatic (to clone
the repos concerned, simply uncomment the git clone lines in the
[S0_one_file_to_rule_them_all.R file](./S0_one_file_to_rule_them_all.R)
).

**Currently the automatic github pull does not work because to obtain
results similar to mine it is necessary to merge in Archetype parser the
decklist cach repo of
[Badaro](https://github.com/Badaro/MTGODecklistCache) and
[Jiliac](https://github.com/Jiliac/MTGODecklistCache/).** Format data
pull is automatic but can also be found
[here](https://github.com/Badaro/MTGOFormatData)

To get scryfall data in csv i use my own script you can find it
[here](https://github.com/fbettega/fetch_scryfall_data_to_csv) file
generated should be put in ’Modern_data_analysis\_data’.

### Packages

Install packages script can be found
[here](../sources/S3_install_packages.R)

List of package below :

| package    |
|:-----------|
| quarto     |
| tidyverse  |
| plotly     |
| devtools   |
| hdi        |
| purrr      |
| scryr      |
| rlang      |
| tictoc     |
| proxyC     |
| knitr      |
| scales     |
| htmltools  |
| DT         |
| conflicted |
| bslib      |
| flextable  |
| gtsummary  |
| car        |
| gtools     |
| caret      |
| lubridate  |
| kableExtra |
| rwantshue  |
| future     |
| tidymodels |
| baguette   |
| xgboost    |

## format includes

Table below list format includes and starting date

| Format  | Start date |
|:--------|:-----------|
| Modern  | 31/03/2025 |
| Legacy  | 31/03/2025 |
| Pauper  | 31/03/2025 |
| Pioneer | 16/12/2024 |
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

**Grid Search can be really long around 3 days for me**  
A downloadable version of the grid search results for each model is
available
[here](https://mtg-modern.serveur-du-placard.xyz/grid_dl/grid.zip)
**Warning aroud 4 Go**.

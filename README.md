# Modern_data_analysis
Version personnel de l'analyse du metagame modern

## Initialisation
The initial json is obtained from the [badaro parser](https://github.com/Badaro/MTGOArchetypeParser). Using a slightly modified version of the modern format.

## Base data format

|          Column name         	|    Type    	|                                                       Definition                                                       	|
|:----------------------------:	|:----------:	|:----------------------------------------------------------------------------------------------------------------------:	|
|              id              	|  Integer   	|                                          unique file id (for unnest of lists)                                          	|
|          Tournament          	| Character  	| the type of tournament (note that leauges can cause problems because only 5-0s are in the file, so survivor bias ++++) 	|
|             Meta             	| Character  	|                                 large time unit (large change extension output or ban)                                 	|
|             Week             	|  Integer   	|                                                   number of the week                                                   	|
|             Date             	|    Date    	|                                                     Tournament date                                                    	|
|            Result            	| Character  	|                                     tournament results, column varies by tournament                                    	|
|              Points          	| Character  	|                                       sum of results (win = 3 Losse = 0 Draw = 1)                                      	|
|              Win             	|  Integer   	|                                                     number of wins                                                     	|
|             Loss             	|  Integer   	|                                                     number of loss                                                     	|
|             Draw             	|  Integer   	|                                                     number of draw                                                     	|
|           Player             	| Character  	|                                                       Player name                                                      	|
|           AnchorUri          	| Character  	|                                                   Url of the decklist                                                  	|
|          Archetype           	| Character  	|        the archetype from the parser modified by FB and aggregated by the initial scrypt into a macro archetype.       	|
|             Color            	| Character  	|                             deck colour (U = Blue, G : Green, R : Red, B : black, W: White)                            	|
|           Companion          	| Character  	|                                          specific type of card defining decks                                          	|
| ReferenceArchetype_Archetype 	| Character  	|                                 archetype of deck obtained with badaro's initial parser                                	|
|   ReferenceArchetype_Color   	| Character  	|          deck colour obtained with badaro's initial parser (U = Blue, G : Green, R : Red, B : black, W: White)         	|
| ReferenceArchetype_Companion 	| Character  	|                       specific type of card defining decks obtained with badaro's initial parser                       	|
|           Mainboard          	|    List    	|                                                   Mainboard decklist                                                   	|
|           Sideboard          	|    List    	|                                                   Sideboard decklist                                                   	|
|           Matchups           	|    List    	|                                                    List of matchupes                                                   	|
|            Matches           	|  Integer   	|                                                number of matches played                                                	|
|        Base_Archetype        	| Character  	|                            the archetype from the parser modified by FB without aggregation                            	|
|        Archetype_count       	|  Integer   	|                                 Number of times the Archetype is present in the dataset                                	|



## Outpout
### new_card
Contains an analysis of the cards limited to certain recent sets to assess the impact of the new cards on the archetypes.

### presence_archetype
Gives a graphical representation of the distribution in terms of presence of the different modern archetypes over time.


### Card_win_rate_table
Presents the win rate of each card in each archetype in the form of multiple tables.

### matrix_WR
Matrix of win rates for different modern archetypes.


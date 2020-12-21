project requires SBT to compile, test, run, package;

1) compile project with `sbt compile`
2) execute tests with `sbt test`
3) to run from sbt: `sbt run` choose 1 for server and 2 for test client

------

CONNECT TO THE SERVER:
`websocat "ws://127.0.0.1:9002/roulette"`

EXAMPLE OF CLIENT REQUEST: 
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Re", "placedNumbers": []}`

this will:
- place a bet of 10 scores to the all red values
------

RULES: 
- All players start with 100 scores
- Players can bet only in first phase - BETS_OPENED
- Players can bet any number of bets, considering their scores
- Players can delete their bets before second phase started - BETS_CLOSED
- Players will get their results in start of third phase - RESULT_ANNOUNCED

------
LIST OF POSSIBLE BETS:
- Single - single bet on 1 number
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Si", "placedNumbers": [5]}`
- Split - bet on 2 numbers which stands together
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Sp", "placedNumbers": [1,2]}`
- Street - bet on street (3 numbers vertically), need only first number in trinity
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "St", "placedNumbers": [1]}`
- Square - bet on 4 numbers in square
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Sq", "placedNumbers": [1,2,4,5]}`
- Double street - bet on 2 streets, need only first number
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Sq", "placedNumbers": [1]}`
- Basket - bet on 3 numbers, one of them must be zero, other two 1,2 ot 2,3. Need only 1 or 3.
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Ba", "placedNumbers": [1]}`
- First four - bet on 4 numbers - 0,1,2,3
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Ba", "placedNumbers": []}`
- Red - bet on all red numbers
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Re", "placedNumbers": []}`
- Black - bet on all black numbers
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Bl", "placedNumbers": []}`
- Even - bet on all even numbers
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Ev", "placedNumbers": []}`
- Odd - bet on all odd numbers
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Od", "placedNumbers": []}`
- Small - bet on numbers from 1 to 18
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Sm", "placedNumbers": []}`
- Big - bet on numbers from 19 to 36
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Bi", "placedNumbers": []}`
- Dozen - bet on dozen (1-12, 13-24, 25 - 36), need only first number in dozen
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Do", "placedNumbers": [25]}`
- Row - bet on row (for example 2,5,8,11,14,17,20,23,26,29,32,35), need only last number in a row
`{"requestType": { "PLACE_BET": {}}, "placedScores": "10", "betType": "Do", "placedNumbers": [35]}`

HOW TO REMOVE THE BET:
`{"requestType": { "REMOVE_BET": {}}, "placedScores": "10", "betType": "Re", "placedNumbers": []}`

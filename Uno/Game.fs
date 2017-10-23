module Game

[<Struct>]
type PlayerCount = PlayerCount of int

[<Struct>]
type PlayerId = PlayerId of int

type Command = 
    | StartGame of StartGame
    | PlayCard of PlayCard

and StartGame = {
    Players: PlayerCount
    FirstCard: Card }

and PlayCard = {
    Player: PlayerId
    Card: Card
}

type Event = 
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | WrongCardPlayed of CardPlayed
    | WrongPlayer of WrongPlayer
    | PlayerChanged of PlayerChanged

and GameStarted = {
    Players: PlayerCount
    FirstCard: Card
}
and CardPlayed = {
    Player: PlayerId
    Card: Card
}
and WrongPlayer = {
    Player: PlayerId
}
and PlayerChanged = {
    Player: PlayerId
}

type Direction =
    | Left
    | Right

type State = 
    | InitialState
    | Playing of Playing

and Playing = {
    LastCard: Card // The card that was last played and is at the top of the stack
    NextPlayer: PlayerId // The PlayerId of the player that should play next
    Direction: Direction // The current direction - not used yet
    NumberOfPlayers: PlayerCount // Total number of players needed to determine next player
}    

type GameError = 
    | GameAlreadyStarted
    | TooFewPlayers
    | GameNotStarted
    | CardInvalid

type Decide = Command -> State -> Result<Event list, GameError>
type Evolve = State -> Event -> State

// Step 1:
// Make the simplest implementation for the following signature
// Command -> State -> Event list Result

// Determine the next player. Currently very simple, only increment and wrap around.
// Skip-Card not yet implemented
let next (PlayerId id) (PlayerCount n) = PlayerId(id+1 % n)

let decide : Decide = fun  command state -> 
    match state, command with
    | InitialState, StartGame data when data.Players < PlayerCount 2 -> 
        Error TooFewPlayers
    | InitialState, PlayCard data ->
        Error GameNotStarted
    | InitialState, StartGame data -> 
        Ok [GameStarted {
            Players = data.Players
            FirstCard = data.FirstCard
        }]
    | Playing pData, StartGame data ->
        Error GameAlreadyStarted
    | Playing pData, PlayCard cData when pData.NextPlayer <> cData.Player ->
        Ok [WrongPlayer {Player = cData.Player}]
    | Playing pData, PlayCard cData ->
        match pData.LastCard, cData.Card with // Match several possible correct moves, that all fire two Events: CardPlayed and PlayerChanged
        | Skip c1, Skip c2 -> Ok [ CardPlayed { Player = cData.Player; Card = cData.Card }; PlayerChanged { Player = next cData.Player pData.NumberOfPlayers} ]
        | Skip (sColor), Digit (digit, dColor) when dColor = sColor -> Ok [ CardPlayed { Player = cData.Player; Card = cData.Card }; PlayerChanged { Player = next cData.Player pData.NumberOfPlayers} ]
        | Digit (digit, dColor), Skip (sColor) when dColor = sColor -> Ok [ CardPlayed { Player = cData.Player; Card = cData.Card }; PlayerChanged { Player = next cData.Player pData.NumberOfPlayers} ]
        | Digit (firstDigit, firstColor), Digit (secondDigit, secondColor) when firstColor = secondColor || firstDigit = secondDigit -> Ok [ CardPlayed { Player = cData.Player; Card = cData.Card }; PlayerChanged { Player = next cData.Player pData.NumberOfPlayers} ]
        | _ -> // Otherwise a wrong card was played
            Ok [WrongCardPlayed {
                Player = cData.Player
                Card = cData.Card
            }; PlayerChanged { Player = next cData.Player pData.NumberOfPlayers} ]

// Step 2:
// Make the simplest implementation for the following signature
// State -> Event list -> State
// s -> Event [] -> sbyte

//  (s + ([a;b;c] @ [e;f;g])) = (s + [a;b;c]) + [e; f; g]) 

// Evolve should be kept very simple. There should be no business rules, that might change over time.
// Otherwise it becomes problematic to replay the events after a change in the business rules.
// To prevent this, important rules should be in the decide-function and stored in the Events.
// Evolve only transfers the data from the Event to the State: NextPlayer in our case.
let evolve : Evolve =
    fun state event -> 
        match state, event with
        | InitialState, GameStarted e -> Playing { 
            LastCard = e.FirstCard
            Direction = Right
            NextPlayer = PlayerId 0
            NumberOfPlayers = e.Players }
        | Playing s, CardPlayed e -> Playing { 
            LastCard = e.Card 
            Direction = Right
            NextPlayer = s.NextPlayer
            NumberOfPlayers = s.NumberOfPlayers }
        | Playing s, PlayerChanged p -> Playing {
            LastCard = s.LastCard 
            Direction = Right
            NextPlayer = p.Player
            NumberOfPlayers = s.NumberOfPlayers }
        | _ -> state

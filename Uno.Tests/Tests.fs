module Tests

open Expecto
open Game

// Step 3:
// Implement => to make the test run
let (=>) events command = 
//    let state = List.fold evolve InitialState events
//    decide command state
    events
    |> List.fold evolve InitialState
    |> decide command

let (==) result expected =
    Expect.equal result (Ok expected) "Should equal expected"

let (=!) result expected =
    Expect.equal result (Error expected) "Should be expected error"
    

[<Tests>]
let tests =
  testList "samples" [
    // Step 4:
    // Change the decide function to make this test pass
    testCase "Game should start" <| fun _ ->
        []
        => StartGame { Players = PlayerCount 3; FirstCard = Digit(Three, Red)}
        == [ GameStarted { Players = PlayerCount 3; FirstCard = Digit(Three, Red) } ]

    // Step 5:
    // Change the decide function to make this test pass
    testCase "Playing alone is not fun" <| fun _ ->
      []
      => StartGame { Players = PlayerCount 1; FirstCard = Digit( Seven, Yellow)}
      =! TooFewPlayers
      

    // Step 6:
    // What should you change to make this test pass ?
    testCase "Game should not be started twice" <| fun _ ->
        [ GameStarted { Players= PlayerCount 2; FirstCard = Digit(Nine, Yellow) } ]
        => StartGame { Players = PlayerCount 3; FirstCard = Digit(Three, Red)}
        =! GameAlreadyStarted


    // Step 7:
    // Make this two tests pass... doing the simplest thing that work
    testCase "Card with same value can be played" <| fun _ ->
        [ GameStarted { Players=PlayerCount 2; FirstCard = Digit(Nine, Yellow) } ]
        => PlayCard { Player = PlayerId 0; Card = Digit(Nine, Green)}
        == [CardPlayed { Player = PlayerId 0; Card = Digit(Nine, Green)}; PlayerChanged { Player = PlayerId 1} ]

    testCase "Card with same color can be played" <| fun _ ->
        [ GameStarted { Players=PlayerCount 2; FirstCard = Digit(Nine, Yellow) } ]
        => PlayCard { Player = PlayerId 0; Card = Digit(Six, Yellow)}
        == [CardPlayed { Player = PlayerId 0; Card = Digit(Six, Yellow) }; PlayerChanged { Player = PlayerId 1} ]

    testCase "Card with same color can be played after second card" <| fun _ ->
        [ GameStarted { Players=PlayerCount 3; FirstCard = Digit(Nine, Yellow) };  CardPlayed { Player = PlayerId 0; Card = Digit(Six, Yellow)}; PlayerChanged { Player = PlayerId 1} ]
        => PlayCard { Player = PlayerId 1; Card = Digit(Nine, Red)}
        == [WrongCardPlayed { Player = PlayerId 1; Card = Digit(Nine, Red)}; PlayerChanged { Player = PlayerId 2} ]

    testCase "Skip with same color can be played" <| fun _ ->
        [ GameStarted { Players=PlayerCount 2; FirstCard = Digit(Nine, Yellow) } ]
        => PlayCard { Player = PlayerId 0; Card = Skip(Yellow)}
        == [CardPlayed { Player = PlayerId 0; Card = Skip(Yellow)}; PlayerChanged { Player = PlayerId 1} ]

    testCase "Digit with same color after Skip can be played" <| fun _ ->
        [ GameStarted { Players=PlayerCount 2; FirstCard = Skip(Red) } ]
        => PlayCard { Player = PlayerId 0; Card = Digit(Nine, Red)}
        == [CardPlayed { Player = PlayerId 0; Card = Digit(Nine, Red)}; PlayerChanged { Player = PlayerId 1} ]

    // Step 8:
    // Make this test pass
    testCase "Card can be played only once game is started" <| fun _ ->
        []
        => PlayCard { Player = PlayerId 1; Card = Digit(Six, Yellow)}
        =! GameNotStarted

    // Step 9:
    // What happens here ?!
    testCase "Card should be same color or same value" <| fun _ ->
        [ GameStarted { Players=PlayerCount 2; FirstCard = Digit(Nine, Yellow) } ]
        => PlayCard { Player = PlayerId 0; Card = Digit(Six, Green)}
        == [WrongCardPlayed { Player = PlayerId 0; Card = Digit(Six, Green)}; PlayerChanged { Player = PlayerId 1} ]
      // ...

    // Step 10:
    // What happens here ?!
    testCase "Player should play during his turn" <| fun _ ->
        [ GameStarted { Players=PlayerCount 2; FirstCard = Digit(Nine, Yellow) } ]
        => PlayCard { Player = PlayerId 2; Card = Digit(Six, Green)}
        == [WrongPlayer { Player = PlayerId 2 }]

    // Step 11:
    // Testing a full round
    testCase "The after a table round, the dealer plays" <| fun _ ->
        failwith "Not implemented"

    testCase "The after a table round, the dealer turn start" <| fun _ ->
        failwith "Not implemented"
    // Step 12:
    // Look at the evolve function...
    // It starts to contains logic.
    // Try to remove the logic from the evolve function 
    // to put it back in the decide function 

    // Step 13:
    // Make this test pass
    testCase "Player can interrupt" <| fun _ ->
        failwith "Not implemented"

    // Step 14:
    // Missing an interrupt is not concidered as playing at the wrong turn.
    // So what happens here ?
    testCase "Player get no penalty when missing an interrupt" <| fun _ ->
        failwith "Not implemented"


    // Step 15:
    // Uncomment the Kickback card and implement it.
    // The kickback changes the direction of the game.
        
  ]
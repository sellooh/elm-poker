module Poker exposing (main)

import Browser
import Html exposing (Html, button, div, span, text, input)
import Html.Attributes exposing (style, disabled, placeholder, value, max, type_, hidden)
import Html.Events exposing (onClick, onInput)
import Cards exposing (..)
import Deck exposing (..)
import Random
import List.Extra


type alias Player =
    { hand : Maybe (Card, Card)
    , stack : Int
    }

type alias Model =
    { deck : ShuffledDeck
    , pot : Int
    , currentBet : Int
    , previousBet : Int
    , formBet : Int
    , phase : Phase
    , tableCards : List Card
    , players : (Player, Player)
    }

type Bettor = Player1 | Player2

type Phase =
    Zero
    | WaitPlayer1
    | ShowPlayer1
    | WaitPlayer2
    | ShowPlayer2
    | Bet Bettor
    | Winner (Maybe Bettor)
    | TurnResult
    


initialModel : Model
initialModel =
    { deck = fullDeck
    , pot = 10
    , currentBet = 0
    , previousBet = 0
    , formBet = 1
    --, phase = TurnResult
    , phase = Zero
    , tableCards =
        [
        --Card Spades Three
        --, Card Clubs Three
        --, Card Hearts Three
        --, Card Spades King
        --, Card Spades Queen
        ]
    , players =
        ( { hand = Nothing, stack = 95 }
        , { hand = Nothing, stack = 95 }
        )
        --( { hand =
        --    Just ( Card Diamonds Four
        --    , Card Hearts Four
        --    ), stack = 95 }
        --, { hand =
        --    Just ( Card Spades Ace
        --    , Card Clubs Three
        --    ), stack = 95 })
    }


type Bet
    = Fold
    | Call
    | Check
    | Raise Int


type Msg
    = ShuffleDeck ShuffledDeck
    | SwitchPhase Phase
    | MakeBet Bettor Bet
    | BindRaise String
    | EvalResults
    | DistributeChips Bettor
    | SplitPot
    | Start


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck deck ->
            ( { model | deck = deck } , Cmd.none )
        
        SwitchPhase phase ->
            ( { model | phase = phase } , Cmd.none )
        
        MakeBet bettor bet ->
            case bet of
                Fold ->
                    let
                        nextPlayer =
                            findNextPlayer model.phase

                        player =
                            extractBettor bettor <| model.players
                        
                        newPlayer =
                            { player | hand = Nothing }
                    in
                    ( { model
                        | phase = Winner <| Just nextPlayer
                        , players = (mapBettor bettor <| newPlayer) model.players
                    }, Cmd.none )

                Check ->
                    let
                        nextPlayer =
                            findNextPlayer model.phase
                        
                        ( drawnCards, deck ) =
                            if nextPlayer == Player1 && List.length model.tableCards < 5 then
                                turnCard nextPlayer model.tableCards model.deck
                            else
                                ( [], model.deck )
                        
                        phase =
                            if nextPlayer == Player1 && List.length model.tableCards == 5 then
                                TurnResult
                            else
                                Bet nextPlayer
                    in
                    ( { model
                        | phase = phase
                        , tableCards = model.tableCards ++ drawnCards
                        , deck = deck
                     } , Cmd.none )

                Raise amount ->
                    let
                        player =
                            extractBettor bettor <| model.players
                        
                        newPlayer =
                            { player | stack = player.stack - amount }
                            
                        nextPlayer =
                            findNextPlayer model.phase
                    in
                    ( { model
                        | pot = model.pot + amount
                        , currentBet = amount
                        , previousBet = model.currentBet
                        , formBet = amount + 1
                        , phase = Bet nextPlayer
                        , players = (mapBettor bettor <| newPlayer) model.players }
                    , Cmd.none )

                Call ->
                    let
                        player =
                            extractBettor bettor <| model.players
                        
                        newPlayer =
                            { player | stack = player.stack - ( model.currentBet - model.previousBet ) }
                            
                        nextPlayer =
                            findNextPlayer model.phase
                            
                        ( drawnCards, deck ) =
                            if List.length model.tableCards < 5 then
                                turnCard nextPlayer model.tableCards model.deck
                            else
                                ( [], model.deck )
                        
                        phase =
                            if List.length model.tableCards == 5 then
                                TurnResult
                            else
                                Bet nextPlayer
                    in
                    ( { model
                        | pot = model.pot + model.currentBet - model.previousBet 
                        , currentBet = 0
                        , previousBet = 0
                        , phase = phase
                        , tableCards = model.tableCards ++ drawnCards
                        , deck = deck
                        , players = (mapBettor bettor <| newPlayer) model.players }
                    , Cmd.none )
        
        BindRaise raise ->
            case model.phase of
                Bet Player1 ->
                    let
                        stack = (.stack <| Tuple.first model.players)
                    in
                    if Maybe.withDefault 0 (String.toInt raise) > stack then
                        ( model, Cmd.none )
                    else
                        ( { model | formBet = Basics.max (model.currentBet + 1)
                            <| Maybe.withDefault 0 ( String.toInt raise ) }
                        , Cmd.none )
                
                Bet Player2 ->
                    let
                        stack = (.stack <| Tuple.second model.players)
                    in
                    if Maybe.withDefault 0 (String.toInt raise) > stack then
                        ( model, Cmd.none )
                    else
                        ( { model | formBet = Basics.max (model.currentBet + 1)
                            <| Maybe.withDefault 0 ( String.toInt raise ) }
                        , Cmd.none )
                
            
                _ -> ( model, Cmd.none )
                
        SplitPot ->
            let
                ( player1, player2 ) =
                    ( Tuple.first model.players
                    , Tuple.second model.players
                    )
                
                nextPlayers =
                    ( { player1 | hand = Nothing, stack = player1.stack + ( model.pot // 2 ) - 5 }
                    , { player2 | hand = Nothing, stack = player2.stack + ( model.pot // 2 ) - 5 }
                    )
            in
            ( { model
                | pot = 5 + 5
                , phase = Zero
                , tableCards = []
                , players = nextPlayers }
                , Random.generate ShuffleDeck randomDeck )
        
        DistributeChips winner ->
            let
                player =
                    extractBettor winner <| model.players
                        
                newPlayer =
                    { player | stack = player.stack + model.pot }
                
                players =
                    (mapBettor winner <| newPlayer) model.players
                
            
                ( player1, player2 ) =
                    ( Tuple.first players
                    , Tuple.second players
                    )
                
                nextPlayers =
                    ( { player1 | hand = Nothing, stack = player1.stack - 5 }
                    , { player2 | hand = Nothing, stack = player2.stack - 5 }
                    )
            in
            ( { model
                | pot = 5 + 5
                , phase = Zero
                , tableCards = []
                , players = nextPlayers }
                , Random.generate ShuffleDeck randomDeck )
        
        Start ->
            let
                ( card1, deck1 ) =
                    draw model.deck
                    
                ( card2, deck2 ) =
                    draw deck1
                    
                ( card3, deck3 ) =
                    draw deck2
                    
                ( card4, deck4 ) =
                    draw deck3

                ( player1, player2 ) =
                    ( Tuple.first model.players
                    , Tuple.second model.players
                    )
                
                players =
                    ( { player1 | hand = Just (card1, card2) }
                    , { player2 | hand = Just (card3, card4) }
                    )

            in
            ( { model | deck = deck4 , players = players, phase = WaitPlayer1 } , Cmd.none )

        EvalResults ->
            let
                ( player1, player2 ) =
                    ( Tuple.first model.players
                    , Tuple.second model.players
                    )
                
                ( maybeHand1, maybeHand2 ) =
                    ( player1.hand, player2.hand )
            in
            case ( maybeHand1, maybeHand2 ) of
                ( Just ( card11, card12 ), Just ( card21, card22 ) ) ->
                    
                    let
                        ( tier1, score1 ) =
                            evaluateCards <| [ card11, card12 ] ++ model.tableCards
                            
                        ( tier2, score2 ) =
                            evaluateCards <| [ card21, card22 ] ++ model.tableCards
                        
                        _ =
                            Debug.log "result one:" <| ( tier1, score1 )
                            
                        _ =
                            Debug.log "result two:" <| ( tier2, score2 )

                        ( ( f1, f2 ), ( f3, f4 ) ) =
                            case ( ( card11, card12 ), ( card21, card22 ) ) of
                                ( ( Card _ f11, Card _ f12 ), ( Card _ f21, Card _ f22 ) ) ->
                                    ( ( f11, f12 ), ( f21, f22 ) )
                                
                                _ -> Debug.todo "" ""

                        winner =
                            case compare tier1 tier2 of
                            GT -> Just Player1
                            LT -> Just Player2
                            EQ ->
                                if tier1 /= 1 then
                                    case compare score1 score2 of
                                        GT -> Just Player1
                                        LT -> Just Player2
                                        EQ -> Nothing
                                else
                                    case compare (Tuple.second (mapCardAceHigh card11) + Tuple.second (mapCardAceHigh card12)) (Tuple.second (mapCardAceHigh card21) + Tuple.second (mapCardAceHigh card22)) of
                                        GT -> Just Player1
                                        LT -> Just Player2
                                        EQ -> Nothing

                        _ =
                            Debug.log "winner" winner
                    in
                    ( { model | phase = Winner winner }, Cmd.none )    
                        
                _ ->
                    ( model, Cmd.none )
                    

evaluateCards cards =
    let
        mappedCardsAceHigh =
            List.sortWith comparison
                <| List.map mapCardAceHigh cards
                
        mappedCardsAceLow =
            List.sortWith comparison
                <| List.map mapCardAceLow cards
                
        ( tier1, score1 ) =
            evaluateCardsInternal mappedCardsAceHigh
                
        ( tier2, score2 ) =
            evaluateCardsInternal mappedCardsAceLow
    in
        case compare tier1 tier2 of
        GT -> ( tier1, score1 )
        LT -> ( tier2, score2 )
        EQ -> ( tier1, Basics.max score1 score2 )

evaluateCardsInternal mappedCards =
    let
        --mappedCards =
          --  List.sortWith comparison
            --    <| List.map mapCard cards

        _ =
            Debug.log "log" mappedCards
        
        tupleCards =
            case mappedCards of
                c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: [] ->
                    ( ( c1, c2 ), ( c3, c4, c5 ), ( c6, c7 ) )

                _ ->
                    Debug.todo "not supported state" "."
                
        ( isStraightFlush, _ ) =
            evaluateStraightFlush tupleCards
    in
    if isStraightFlush then
        ( 9, 0 )

    else
        let
            ( isFourOfAKind, fourOfAKindHit ) =
                evaluateFourOfAKind tupleCards
        in
        if isFourOfAKind then
            ( 8, fourOfAKindHit )

        else
            let
                ( isFullHouse, _ ) =
                    evaluateFullHouse tupleCards 
            in
            if isFullHouse then
                ( 7, 0 )

            else
                let
                    ( isFlush, _ ) =
                        evaluateFlush tupleCards
                in
                if isFlush then
                    ( 6, 0 )

                else
                    let
                        ( isAStraight, _ ) =
                            evaluateStraight tupleCards
                    in
                    if isAStraight then
                        ( 5, 0 )

                    else
                        let
                            ( isThreeOfAKind, threeOfAKindHit ) =
                                evaluateThreeOfAKind tupleCards
                        in
                        if isThreeOfAKind then
                            ( 4, threeOfAKindHit )

                        else
                            let
                                ( isTwoPairs, twoPairsHit ) =
                                    evaluateTwoPairs tupleCards
                            in
                            if isTwoPairs then
                                ( 3, twoPairsHit )

                            else
                                let
                                    ( isPair, pairHit ) =
                                        evaluatePair tupleCards
                                in
                                if isPair then
                                    ( 2, pairHit )

                                else
                                    ( 1, 0 )


mapCardAceHigh card =
    case card of
        Card suit Ace -> ( suit, 13 )
        Card suit face -> ( suit, defaultFace face )
        _ -> Debug.todo "not supported state" "."


mapCardAceLow card =
    case card of
        Card suit Ace -> ( suit, 1 )
        Card suit face -> ( suit, defaultFace face )
        _ -> Debug.todo "not supported state" "."


evaluateStraightFlush cards =
    let
        ( ( ( s1, f1 ), ( s2, f2 ) )
            , ( ( s3, f3 ), ( s4, f4 ), ( s5, f5 ) )
            , ( ( s6, f6 ), ( s7, f7 ) ))
            = cards
        
        possibilities =
            [ isStraight [ f2 - f1, f3 - f2, f4 - f3, f5 - f4 ]
                && evaluateFlushInternal [ s1, s2, s3, s4, s5 ]
            , isStraight [ f3 - f2, f4 - f3, f5 - f4, f6 - f5 ]
                && evaluateFlushInternal [ s2, s3, s4, s5, s6 ]
            , isStraight [ f4 - f3, f5 - f4, f6 - f5, f7 - f6 ]
                && evaluateFlushInternal [ s3, s4, s5, s6, s7 ]
            ]
    in
        ( List.any (\b -> b) possibilities, 0 )


evaluateFullHouse cards =
    let
        ( ( ( _, f1 ), ( _, f2 ) )
            , ( ( _, f3 ), ( _, f4 ), ( _, f5 ) )
            , ( ( _, f6 ), ( _, f7 ) ))
            = cards
        
        possibilities =
            [ isSameFace [ f1, f2, f3 ]
                && ( isSameFace [ f4, f5 ] || isSameFace [ f5, f6 ] || isSameFace [ f6, f7 ] )
            , isSameFace [ f2, f3, f4 ]
                && ( isSameFace [ f5, f6 ] || isSameFace [ f6, f7 ] )
            , isSameFace [ f3, f4, f5 ]
                && ( isSameFace [ f1, f2 ] || isSameFace [ f6, f7 ] )
            , isSameFace [ f4, f5, f6 ]
                && ( isSameFace [ f1, f2 ] || isSameFace [ f2, f3 ] )
            , isSameFace [ f5, f6, f7 ]
                && ( isSameFace [ f1, f2 ] || isSameFace [ f2, f3 ] || isSameFace [ f3, f4 ] )
            ]
    in
        ( List.any (\b -> b) possibilities, 0 )


evaluateFourOfAKind cards =
    let
        ( ( ( _, f1 ), ( _, f2 ) )
            , ( ( _, f3 ), ( _, f4 ), ( _, f5 ) )
            , ( ( _, f6 ), ( _, f7 ) ))
            = cards
        
        possibilities =
            [ isSameFace [ f1, f2, f3, f4 ]
            , isSameFace [ f2, f3, f4, f5 ]
            , isSameFace [ f3, f4, f5, f6 ]
            , isSameFace [ f4, f5, f6, f7 ]
            ]
    in
        --List.any (\b -> b) possibilities
        case possibilities of
            _ :: _ :: _ :: True :: [] ->
                ( True, f4 )

            _ :: _ :: True :: _ :: [] ->
                ( True, f3 )

            _ :: True :: _ :: _ :: [] ->
                ( True, f2 )

            True :: _ :: _ :: _ :: [] ->
                ( True, f1 )

            _ ->
                ( False, 0 )


evaluateThreeOfAKind cards =
    let
        ( ( ( _, f1 ), ( _, f2 ) )
            , ( ( _, f3 ), ( _, f4 ), ( _, f5 ) )
            , ( ( _, f6 ), ( _, f7 ) ))
            = cards
        
        possibilities =
            [ isSameFace [ f1, f2, f3 ]
            , isSameFace [ f2, f3, f4 ]
            , isSameFace [ f3, f4, f5 ]
            , isSameFace [ f4, f5, f6 ]
            , isSameFace [ f5, f6, f7 ]
            ]
    in
        ( List.any (\b -> b) possibilities, 0 )


evaluateTwoPairs cards =
    let
        ( ( ( _, f1 ), ( _, f2 ) )
            , ( ( _, f3 ), ( _, f4 ), ( _, f5 ) )
            , ( ( _, f6 ), ( _, f7 ) ))
            = cards
        
        possibilities =
            [ isSameFace [ f1, f2 ]
                && ( isSameFace [ f3, f4 ] || isSameFace [ f4, f5 ] || isSameFace [ f5, f6 ] || isSameFace [ f6, f7 ] )
            , isSameFace [ f2, f3 ]
                && ( isSameFace [ f4, f5 ] || isSameFace [ f5, f6 ] || isSameFace [ f6, f7 ] )
            , isSameFace [ f3, f4 ]
                && ( isSameFace [ f5, f6 ] || isSameFace [ f6, f7 ] )
            , isSameFace [ f4, f5 ]
                && ( isSameFace [ f6, f7 ] )
            ]
    in
        ( List.any (\b -> b) possibilities, 0 )


evaluatePair cards =
    let
        ( ( ( _, f1 ), ( _, f2 ) )
            , ( ( _, f3 ), ( _, f4 ), ( _, f5 ) )
            , ( ( _, f6 ), ( _, f7 ) ))
            = cards
        
        possibilities =
            [ isSameFace [ f1, f2 ]
            , isSameFace [ f2, f3 ]
            , isSameFace [ f3, f4 ]
            , isSameFace [ f4, f5 ]
            , isSameFace [ f5, f6 ]
            , isSameFace [ f6, f7 ]
            ]
    in
        case possibilities of
            _ :: _ :: _ :: _ :: _ :: True :: [] ->
                ( True, f6 )

            _ :: _ :: _ :: _ :: True :: _ :: [] ->
                ( True, f5 )

            _ :: _ :: _ :: True :: _ :: _ :: [] ->
                ( True, f4 )

            _ :: _ :: True :: _ :: _ :: _ :: [] ->
                ( True, f3 )

            _ :: True :: _ :: _ :: _ :: _ :: [] ->
                ( True, f2 )

            True :: _ :: _ :: _ :: _ :: _ :: [] ->
                ( True, f1 )

            _ ->
                ( False, 0 )


evaluateFlush cards =
    let
        ( ( ( s1, _ ), ( s2, _ ) )
            , ( ( s3, _ ), ( s4, _ ), ( s5, _ ) )
            , ( ( s6, _ ), ( s7, _ ) ))
            = cards
    in
    ( evaluateFlushInternal [ s1, s2, s3, s4, s5, s6, s7 ], 0 )


evaluateFlushInternal suits =
    let
        spadesTest =
            List.filter (\s -> s == Spades) suits

        diamondsTest =
            List.filter (\s -> s == Diamonds) suits

        clubsTest =
            List.filter (\s -> s == Clubs) suits

        heartsTest =
            List.filter (\s -> s == Hearts) suits
        
        hits =
            List.map List.length [ spadesTest, diamondsTest, clubsTest, heartsTest ]
    in
        ( Maybe.withDefault 0
            <| List.maximum hits ) >= 5


evaluateStraight cards =
    let
        ( ( ( _, f1 ), ( _, f2 ) )
            , ( ( _, f3 ), ( _, f4 ), ( _, f5 ) )
            , ( ( _, f6 ), ( _, f7 ) ))
            = cards
        
        possibilities =
            [ isStraight [ f2 - f1, f3 - f2, f4 - f3, f5 - f4 ]
            , isStraight [ f3 - f2, f4 - f3, f5 - f4, f6 - f5 ]
            , isStraight [ f4 - f3, f5 - f4, f6 - f5, f7 - f6 ]
            ]
    in
        ( List.any (\b -> b) possibilities, 0 )


isStraight numbers =
    case numbers of
        1 :: 1 :: 1 :: 1 :: [] ->
            True
        
        _ ->
            False


isSameFace numbers =
    ( List.length <| List.Extra.unique numbers) == 1


comparison a b =
    let
        ( _ , faceA) = a
        ( _ , faceB) = b
    in
    compare faceA faceB


turnCard nextPlayer tableCards deck =
    if List.length tableCards == 0 then
        let
            ( card1, deck1 ) =
                draw deck

            ( card2, deck2 ) =
                draw deck1

            ( card3, deck3 ) =
                draw deck2
        in
        ( [ card1, card2, card3 ], deck3 )
    else
        let
            ( card1, deck1 ) =
                draw deck
        in
        ( [ card1 ], deck1 )


findNextPlayer phase =
    case phase of
        Bet Player1 -> Player2
        Bet Player2 -> Player1
        _ -> Player1


mapBettor bettor player tuple =
    let
        ( player1, player2 ) = tuple
    in
    case bettor of
        Player1 -> ( player, player2 )
        Player2 -> ( player1, player )


extractBettor bettor =
    case bettor of
        Player1 -> Tuple.first
        Player2 -> Tuple.second


viewHand : Player -> Html Msg
viewHand player =
    case player.hand of
        Just hand ->
            let
                ( color1, card1 ) =
                    viewCard <| Tuple.first hand

                ( color2, card2 ) =
                    viewCard <| Tuple.second hand
            in
            div []
                [ span [ style "font-size" "7em", style "color" color1 ]
                    [ text card1 ]
                , span [ style "font-size" "7em", style "color" color2 ]
                    [ text card2 ]
            ]
        Nothing -> div [] []


viewACard : Card -> Html Msg
viewACard currentCard =
    let
        ( color, card ) =
            viewCard currentCard
    in
        span [ style "font-size" "5em", style "color" color ]
            [ text card ]


view : Model -> Html Msg
view model =
    div [ style "font-size" "3em", style "margin" "1em" ]
    <| case model.phase of
        Zero ->
            [ button [ onClick Start ] [ text "Start Elm Poker!" ]
            ]

        WaitPlayer1 ->
            [ button [ onClick <| SwitchPhase ShowPlayer1 ] [ text "Show Player 1 Hand" ]
            ]

        ShowPlayer1 ->
            [ button [ onClick <| SwitchPhase WaitPlayer2 ] [ text "Done" ]
            , viewHand <| Tuple.first model.players
            ]

        WaitPlayer2 ->
            [ button [ onClick <| SwitchPhase ShowPlayer2 ] [ text "Show Player 2 Hand" ]
            ]

        ShowPlayer2 ->
            [ button [ onClick <| SwitchPhase <| Bet Player1 ] [ text "Done" ]
            , viewHand <| Tuple.second model.players
            ]

        Winner winner ->
            [ div [] [ text <| "Pot:" ++ String.fromInt model.pot ]
            , text <|
                case winner of
                    Just Player1 -> "First Player Won"
                    Just Player2 -> "Second Player Won"
                    Nothing -> "Tie"
            , button [
                case winner of
                    Just winner_ -> onClick <| DistributeChips winner_
                    Nothing -> onClick <| SplitPot
                ] [ text "Get Chips from table" ]
            , viewHand <| Tuple.first model.players
            , viewHand <| Tuple.second model.players
            , div []
                <| List.map viewACard model.tableCards
            ]
        
        TurnResult ->
            [ div [] [ text <| "Pot:" ++ String.fromInt model.pot ]
            , text "It's done!"
            , button [
                    onClick <| EvalResults
                ] [ text "Turn results" ]
            , div []
                <| List.map viewACard model.tableCards
            ]

        Bet bettor ->
            [ div [] [ text <| "Pot:" ++ String.fromInt model.pot ]
            , text <|
                case bettor of
                    Player1 -> "First Player Turn"
                    Player2 -> "Second Player Turn"
            , div [ style "font-size" ".5em" ] [
                text <| String.fromInt model.currentBet
                ]
            , div []
                <|
                [ button [
                    onClick <| MakeBet bettor Fold
                ] [ text "Fold" ]
                , button [
                    onClick <| MakeBet bettor Call
                    , disabled <| model.currentBet == 0
                ] [ text "Call" ]
                , button [
                    onClick <| MakeBet bettor Check
                    , disabled <| model.currentBet > 0
                ] [ text "Check" ]
                , div [] [
                    input [
                        placeholder "Value to Raise"
                        , value <| String.fromInt model.formBet
                        , onInput BindRaise
                        , type_ "number"
                     ] []
                    , button [
                        onClick <| MakeBet bettor <| Raise model.formBet
                    ] [ text "Raise" ]
                    ]
                , div [ style "font-size" ".5em"
                    , style "font-weight" <| if bettor == Player1 then "bold" else "normal"
                        ] [
                            text <| "Player 1: " ++ String.fromInt (.stack <| Tuple.first model.players)
                    ]
                , div [ style "font-size" ".5em"
                    , style "font-weight" <| if bettor == Player2 then "bold" else "normal"
                    --, case (.hand <| Tuple.second model.players) of
                      --  Just _ -> hidden False
                        --_ -> hidden True
                        ] [
                            text <| "Player 2: " ++ String.fromInt (.stack <| Tuple.second model.players)
                    ]
                ] ++ List.map viewACard model.tableCards
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, Random.generate ShuffleDeck randomDeck)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

















module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, max, value)
import Html.Events
import Page exposing (Page)
import Process
import Task
import View exposing (View)



-- PAGE


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { enemyHealth : Int
    , playerHealth : Int
    , status : Status
    }


type Status
    = WaitingForPlayer
    | ChoosingMove
    | FailedToEscape
    | NoItemsAvailable
    | OnLastJokemon
    | UsingMove KittyMove
    | ShowMoveEffect String


fromStatusToMessage : Status -> String
fromStatusToMessage status =
    case status of
        WaitingForPlayer ->
            "What will you do?"

        ChoosingMove ->
            "What move should Kitty use?"

        FailedToEscape ->
            "There's no running from a doge!"

        NoItemsAvailable ->
            "You ain't got none!"

        OnLastJokemon ->
            "Kitty is your only hope!"

        UsingMove kittyMove ->
            "Kitty used " ++ kittyMoveToString kittyMove

        ShowMoveEffect message ->
            message


init : ( Model, Cmd Msg )
init =
    ( { enemyHealth = maxHealth
      , playerHealth = maxHealth
      , status = WaitingForPlayer
      }
    , Cmd.none
    )


maxHealth : Int
maxHealth =
    100



-- UPDATE


type Msg
    = UserClickedFight
    | UserClickedPokemon
    | UserClickedItems
    | UserClickedRun
    | UserClickedPlayAgain
    | UserClickedMove KittyMove
    | MoveTimerStarted KittyMove
    | MoveTimerCompleted


type KittyMove
    = Claw
    | Hiss
    | Meow
    | Sits


kittyMoveToString : KittyMove -> String
kittyMoveToString kittyMove =
    case kittyMove of
        Claw ->
            "Claw"

        Hiss ->
            "Hiss"

        Meow ->
            "Meow"

        Sits ->
            "Sits"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedFight ->
            ( { model | status = ChoosingMove }
            , Cmd.none
            )

        UserClickedPokemon ->
            ( { model | status = OnLastJokemon }
            , Cmd.none
            )

        UserClickedItems ->
            ( { model | status = NoItemsAvailable }
            , Cmd.none
            )

        UserClickedRun ->
            ( { model | status = FailedToEscape }
            , Cmd.none
            )

        UserClickedPlayAgain ->
            ( { model
                | enemyHealth = maxHealth
                , playerHealth = maxHealth
                , status = WaitingForPlayer
              }
            , Cmd.none
            )

        UserClickedMove kittyMove ->
            ( { model
                | status = UsingMove kittyMove
              }
            , delay 1000 (MoveTimerStarted kittyMove)
            )

        MoveTimerStarted Claw ->
            ( { model | status = ShowMoveEffect "Doge was clawed!" }
                |> dealDamageToEnemy 10
            , delay 1000 MoveTimerCompleted
            )

        MoveTimerStarted Hiss ->
            ( { model | status = ShowMoveEffect "Kitty hissed so loud!" }
                |> dealDamageToEnemy 20
            , delay 1000 MoveTimerCompleted
            )

        MoveTimerStarted Meow ->
            ( { model | status = ShowMoveEffect "Kitty's meow was ok!" }
                |> dealDamageToEnemy 5
            , delay 1000 MoveTimerCompleted
            )

        MoveTimerStarted Sits ->
            ( { model | status = ShowMoveEffect "Kitty sat in a box!" }
                |> dealDamageToEnemy 50
            , delay 1000 MoveTimerCompleted
            )

        MoveTimerCompleted ->
            ( { model | status = WaitingForPlayer }
            , Cmd.none
            )


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


dealDamageToEnemy : Int -> Model -> Model
dealDamageToEnemy amount model =
    { model
        | enemyHealth =
            Basics.max
                0
                (model.enemyHealth - amount)
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Jokemon"
    , body =
        [ div [ class "game border-regular" ]
            (if model.enemyHealth <= 0 then
                [ span [ class "message" ]
                    [ text "You won!" ]
                , viewPlayAgainButton
                ]

             else if model.playerHealth <= 0 then
                [ span [ class "message" ]
                    [ text "You lost!" ]
                , viewPlayAgainButton
                ]

             else
                [ viewFightScene model
                , viewControls model
                ]
            )
        ]
    }


viewFightScene : Model -> Html msg
viewFightScene model =
    div [ class "col pad-md gap-md" ]
        [ viewEnemyRow model
        , viewPlayerRow model.playerHealth
        ]


viewEnemyRow : Model -> Html msg
viewEnemyRow { status, enemyHealth } =
    let
        withFlashingIfUnderAttack : Html msg -> Html msg
        withFlashingIfUnderAttack innerView =
            case status of
                ShowMoveEffect _ ->
                    div [ class "flash" ] [ innerView ]

                _ ->
                    innerView
    in
    div [ class "row center-h gap-md pad-x-md" ]
        [ viewPokemonStatus
            { name = "Doge"
            , level = 5
            , health = ( enemyHealth, maxHealth )
            , border = BottomLeft
            }
        , withFlashingIfUnderAttack
            (viewSprite "🐕")
        ]


type BorderSide
    = BottomLeft
    | BottomRight


viewPokemonStatus :
    { name : String
    , level : Int
    , health : ( Int, Int )
    , border : BorderSide
    }
    -> Html msg
viewPokemonStatus options =
    div
        [ class "col pad-sm"
        , classList
            [ ( "border-bottom-regular", True )
            , ( "border-left-regular"
              , options.border == BottomLeft
              )
            , ( "border-right-regular"
              , options.border == BottomRight
              )
            ]
        ]
        [ span [] [ text options.name ]
        , span []
            [ text
                ("Level: "
                    ++ String.fromInt options.level
                )
            ]
        , viewHealthbar options.health
        ]


viewSprite : String -> Html msg
viewSprite emoji =
    div [ class "sprite" ] [ text emoji ]


viewHealthbar : ( Int, Int ) -> Html msg
viewHealthbar ( value, max ) =
    div [ class "healthbar" ]
        [ span [] [ text "HP:" ]
        , progress
            [ Attr.value (String.fromInt value)
            , Attr.max (String.fromInt max)
            ]
            []
        ]


viewPlayerRow : Int -> Html msg
viewPlayerRow playerHealth =
    div [ class "row center-h gap-md pad-x-md" ]
        [ viewSprite "🐈"
        , viewPokemonStatus
            { name = "Kitty"
            , level = 5
            , health = ( playerHealth, maxHealth )
            , border = BottomRight
            }
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ class "row border-top-regular" ]
        [ span [ class "fill pad-sm", Attr.style "width" "10rem" ]
            [ text (fromStatusToMessage model.status)
            ]
        , span [ class "fill pad-sm" ]
            [ div [ class "grid border-regular pad-sm gap-sm" ]
                (if model.status == ChoosingMove then
                    List.map viewMoveButton [ Claw, Hiss, Meow, Sits ]

                 else
                    [ button
                        [ Html.Events.onClick UserClickedFight ]
                        [ text "Fight" ]
                    , button
                        [ Html.Events.onClick UserClickedPokemon ]
                        [ text "PKMN" ]
                    , button
                        [ Html.Events.onClick UserClickedItems ]
                        [ text "Item" ]
                    , button
                        [ Html.Events.onClick UserClickedRun ]
                        [ text "Run" ]
                    ]
                )
            ]
        ]


viewMoveButton : KittyMove -> Html Msg
viewMoveButton kittyMove =
    button
        [ Html.Events.onClick (UserClickedMove kittyMove) ]
        [ text (kittyMoveToString kittyMove) ]


viewPlayAgainButton : Html Msg
viewPlayAgainButton =
    button
        [ Html.Events.onClick UserClickedPlayAgain
        ]
        [ text "Play again" ]

module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, max, value)
import Html.Events
import Page exposing (Page)
import View exposing (View)



-- PAGE


page : Page Model Msg
page =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { enemyHealth : Int
    , playerHealth : Int
    , message : Maybe String
    , isChoosingMove : Bool
    }


init : Model
init =
    { enemyHealth = maxHealth
    , playerHealth = maxHealth
    , message = Nothing
    , isChoosingMove = False
    }


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedFight ->
            { model
                | message = Just "What should Kitty do?"
                , isChoosingMove = True
            }

        UserClickedPokemon ->
            { model
                | message = Just "Kitty is your last Jokemon!"
            }

        UserClickedItems ->
            { model
                | message = Just "You ain't got none!"
            }

        UserClickedRun ->
            { model
                | message = Just "There's no running from a Jokemon battle!"
            }

        UserClickedPlayAgain ->
            { model
                | enemyHealth = maxHealth
                , playerHealth = maxHealth
            }

        UserClickedMove kittyMove ->
            { model
                | message = Just ("Kitty used " ++ kittyMoveToString kittyMove)
                , isChoosingMove = False
            }


dealDamageToEnemy : Model -> Model
dealDamageToEnemy model =
    { model
        | enemyHealth =
            Basics.max
                0
                (model.enemyHealth - 10)
    }



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
        [ viewEnemyRow model.enemyHealth
        , viewPlayerRow model.playerHealth
        ]


viewEnemyRow : Int -> Html msg
viewEnemyRow enemyHealth =
    div [ class "row center-h gap-md pad-x-md" ]
        [ viewPokemonStatus
            { name = "Doge"
            , level = 5
            , health = ( enemyHealth, maxHealth )
            , border = BottomLeft
            }
        , viewSprite "🐕"
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
            [ case model.message of
                Just message ->
                    text message

                Nothing ->
                    text ""
            ]
        , span [ class "fill pad-sm" ]
            [ div [ class "grid border-regular pad-sm gap-sm" ]
                (if model.isChoosingMove then
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

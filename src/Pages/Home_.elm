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
    }


init : Model
init =
    { enemyHealth = maxHealth
    , playerHealth = maxHealth
    }


maxHealth : Int
maxHealth =
    100



-- UPDATE


type Msg
    = UserClickedFight
    | UserClickedRun
    | UserClickedPlayAgain


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedFight ->
            { model
                | enemyHealth =
                    Basics.max
                        0
                        (model.enemyHealth - 10)
            }

        UserClickedRun ->
            { model
                | playerHealth =
                    Basics.max
                        0
                        (model.playerHealth - 10)
            }

        UserClickedPlayAgain ->
            { model
                | enemyHealth = maxHealth
                , playerHealth = maxHealth
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
                , viewControls
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


viewControls : Html Msg
viewControls =
    div [ class "row border-top-regular" ]
        [ span [ class "fill" ] []
        , span [ class "fill pad-sm" ]
            [ div [ class "grid border-regular pad-md" ]
                [ button
                    [ Html.Events.onClick UserClickedFight
                    ]
                    [ text "Fight" ]
                , button [] [ text "PKMN" ]
                , button [] [ text "Item" ]
                , button
                    [ Html.Events.onClick UserClickedRun
                    ]
                    [ text "Run" ]
                ]
            ]
        ]


viewPlayAgainButton : Html Msg
viewPlayAgainButton =
    button
        [ Html.Events.onClick UserClickedPlayAgain
        ]
        [ text "Play again" ]

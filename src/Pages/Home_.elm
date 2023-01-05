module Pages.Home_ exposing (page)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, max, value)
import View exposing (View)


page : View msg
page =
    { title = "Jokemon"
    , body =
        [ div [ class "game border-regular" ]
            [ viewFightScene
            , viewControls
            ]
        ]
    }


viewFightScene : Html msg
viewFightScene =
    div [ class "col pad-md gap-md" ]
        [ viewEnemyRow
        , viewPlayerRow
        ]


viewEnemyRow : Html msg
viewEnemyRow =
    div [ class "row center-h gap-md pad-x-md" ]
        [ viewPokemonStatus
            { name = "Doge"
            , level = 5
            , health = ( 50, 100 )
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


viewPlayerRow : Html msg
viewPlayerRow =
    div [ class "row center-h gap-md pad-x-md" ]
        [ viewSprite "🐈"
        , viewPokemonStatus
            { name = "Kitty"
            , level = 5
            , health = ( 10, 100 )
            , border = BottomRight
            }
        ]


viewControls : Html msg
viewControls =
    div [ class "row border-top-regular" ]
        [ span [ class "fill" ] []
        , span [ class "fill pad-sm" ]
            [ div [ class "grid border-regular pad-md" ]
                [ span [] [ text "Fight" ]
                , span [] [ text "PKMN" ]
                , span [] [ text "Item" ]
                , span [] [ text "Run" ]
                ]
            ]
        ]

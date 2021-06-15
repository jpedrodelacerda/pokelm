module Main exposing (main)

import Browser
import Element exposing (Attribute, Color, Element, alignRight, alignTop, centerX, column, el, fill, fillPortion, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (placeholder, search)
import Html exposing (Html)
import Html.Events exposing (on)
import Http
import Json.Decode as Decode exposing (Decoder, field, map2, string)
import Random


type alias Model =
    { pokemon : Pokemon
    , searchBar : String
    , status : Status
    }


type Status
    = Waiting
    | Loading
    | Failed String
    | Success


type alias Pokemon =
    { name : String
    , sprite : String
    }


type Msg
    = GotPokemon (Result Http.Error Pokemon)
    | UpdateSearch String
    | FetchPokemon
    | FetchRandomPokemon Int
    | PickRandomPokemon


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokemon (Ok pokemon) ->
            ( { model
                | pokemon = pokemon
                , status = Success
              }
            , Cmd.none
            )

        GotPokemon (Err _) ->
            ( { model
                | status = Failed "Oops, something went wrong"
              }
            , Cmd.none
            )

        UpdateSearch searchStr ->
            ( { model
                | searchBar = searchStr
              }
            , Cmd.none
            )

        FetchPokemon ->
            ( { model
                | status = Loading
              }
            , fetchPokemon (String.toLower model.searchBar)
            )

        FetchRandomPokemon id ->
            ( { model
                | searchBar = String.fromInt id
                , status = Loading
              }
            , fetchPokemon (String.fromInt id)
            )

        PickRandomPokemon ->
            ( model
            , fetchRandomPokemon
            )



--- VIEW


view : Model -> Html Msg
view model =
    let
        col =
            Element.column [ Background.color colors.commentGrey, width fill, height fill ]

        title =
            viewTitlebar [ height <| fillPortion 2 ]

        search =
            viewSearchBar [ height <| fillPortion 1, alignTop ] model

        pokeInfo =
            viewPokemon [ height <| fillPortion 4, alignTop ] model.pokemon
    in
    case model.status of
        Loading ->
            Element.layout []
                (col
                    [ title
                    , viewSearchBar [] model
                    , pokeInfo
                    ]
                )

        Waiting ->
            Element.layout []
                (col
                    [ title
                    , search
                    , pokeInfo
                    ]
                )

        Success ->
            Element.layout []
                (col
                    [ title
                    , search
                    , pokeInfo
                    ]
                )

        Failed _ ->
            Element.layout []
                (col
                    [ title
                    , search
                    , pokeInfo
                    ]
                )


viewSearchBar : List (Attribute msg) -> Model -> Element Msg
viewSearchBar _ model =
    row
        [ centerX
        , Background.color colors.white
        , Border.width 1
        , Border.color colors.gutterGrey
        , Border.rounded 4
        , padding 5
        , spacing 20
        ]
        [ Input.search
            [ onEnter FetchPokemon ]
            { onChange = UpdateSearch
            , text = model.searchBar
            , placeholder =
                Just
                    (Input.placeholder []
                        (text "pokémon name or id")
                    )
            , label = Input.labelHidden "Search bar"
            }
        , Input.button
            [ Border.color colors.black, Border.width 1, Border.solid, Border.rounded 4, padding 10, Background.color colors.black, Font.color colors.darkRed ]
            { onPress = Just FetchPokemon
            , label = text "Catch it!"
            }
        , Input.button
            [ Border.color colors.black, Border.width 1, Border.solid, Border.rounded 4, padding 10, Background.color colors.black, Font.color colors.lightYellow ]
            { onPress = Just PickRandomPokemon
            , label = text "Random"
            }
        ]


viewTitlebar : List (Attribute Msg) -> Element Msg
viewTitlebar al =
    column ([ centerX, alignTop, width fill ] ++ al)
        [ el [ padding 20, centerX, Font.size 20, Font.bold ] (text "PokÉlm")
        , el [ centerX ] (text "A simple pokédex built with Elm")
        ]


viewPokemon : List (Attribute msg) -> Pokemon -> Element msg
viewPokemon al pokemon =
    row
        ([ alignTop, centerX ] ++ al)
        [ el [] (Element.image [] { src = pokemon.sprite, description = pokemon.name })
        , el [ alignRight ] (viewPokeInfo pokemon)
        ]


viewPokeInfo : Pokemon -> Element msg
viewPokeInfo pokemon =
    column []
        [ el [] (text "Pokemon: ")
        , el [ Font.color colors.black ] (text (capitalize pokemon.name))
        ]


baseUrl =
    "https://pokeapi.co/api/v2/pokemon/"


fetchPokemon : String -> Cmd Msg
fetchPokemon id =
    Http.get
        { url = baseUrl ++ id
        , expect = Http.expectJson GotPokemon pokeDecoder
        }


fetchRandomPokemon : Cmd Msg
fetchRandomPokemon =
    Random.generate FetchRandomPokemon (Random.int 1 802)


pokeDecoder : Decoder Pokemon
pokeDecoder =
    map2 Pokemon nameDecoder spriteDecoder


nameDecoder : Decoder String
nameDecoder =
    field "name" string


spriteDecoder : Decoder String
spriteDecoder =
    field "sprites" (field "front_default" string)


initModel =
    { pokemon =
        { name = ""
        , sprite = ""
        }
    , searchBar = ""
    , status = Waiting
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }



--- HELPERS


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


capitalize : String -> String
capitalize s =
    case String.uncons s of
        Nothing ->
            s

        Just ( firstLetter, rest ) ->
            String.cons (Char.toUpper firstLetter) rest


type alias ColorSheet =
    { black : Color
    , white : Color
    , lightRed : Color
    , darkRed : Color
    , green : Color
    , lightYellow : Color
    , darkYellow : Color
    , blue : Color
    , magenta : Color
    , cyan : Color
    , gutterGrey : Color
    , commentGrey : Color
    }


colors : ColorSheet
colors =
    { black =
        Element.rgb255 40 44 52
    , white =
        Element.rgb255 171 178 191
    , lightRed =
        Element.rgb255 224 108 117
    , darkRed =
        Element.rgb255 190 80 70
    , green =
        Element.rgb255 152 195 121
    , lightYellow =
        Element.rgb255 229 192 123
    , darkYellow =
        Element.rgb255 209 154 102
    , blue =
        Element.rgb255 97 175 239
    , magenta =
        Element.rgb255 198 120 221
    , cyan =
        Element.rgb255 86 182 194
    , gutterGrey =
        Element.rgb255 76 82 99
    , commentGrey =
        Element.rgb255 92 99 112
    }

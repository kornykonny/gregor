module Main exposing (..)

import Browser
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html
import Http
import Json.Decode as JD


type alias Model =
    { searchText : String , searchText1 : String
    , results : List Locatio
    }

type alias Locatio =
    { lat : String
    , lon : String }

type alias Location1 = 
    { lat : String 
    , lon : String}

type Msg
    = MsgSearch
    | MsgGotResults (Result Http.Error (List Locatio
    ))
    | MsgInputTextField String
    | MsgInputTextField1 String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { searchText = "" , searchText1 = ""
    , results = []
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        MsgInputTextField1 newTextInput1 ->
            ( { model | searchText1 = newTextInput1 }, Cmd.none )

        MsgInputTextField newTextInput ->
            ( { model | searchText = newTextInput }, Cmd.none )

        MsgSearch ->
            ( model, cmdSearch model)

        MsgGotResults result ->
            case result of
                Ok data ->
                    ( { model | results = data }, Cmd.none )
                Err error ->
                    ( model, Cmd.none )


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none


viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (E.column [] [ viewSearchBar model, viewResults model, viewSearchBarStandort model, viewResults1 model] )


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.row []
        [ EI.search []
            { onChange = MsgInputTextField
            , text = model.searchText
            , placeholder = Nothing
            , label = EI.labelLeft [] (E.text "Stadt")
            }
        , viewSearchButton
        ]
viewSearchBarStandort : Model -> E.Element Msg
viewSearchBarStandort model =
    E.row []
        [ EI.search []
            { onChange = MsgInputTextField1
            , text = model.searchText1
            , placeholder = Nothing
            , label = EI.labelLeft [] (E.text "Aktueller Standort")
            }
        , viewSearchButtonStandort
        ]

viewResults : Model -> E.Element msg
viewResults model =
    E.column [] [
        case List.head model.results of
            Just firstItem ->
                E.text firstItem.lat
            Nothing ->
                E.text "Empty"
    ]
viewResults1 : Model -> E.Element msg
viewResults1 model =
    E.column [] [
        case List.head model.results of
            Just firstItem ->
                E.text firstItem.lat
            Nothing ->
                E.text "Empty"
    ]



viewSearchButton : E.Element Msg
viewSearchButton =
    EI.button
        [ EBG.color (E.rgb255 0x00 0x33 0x66)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just MsgSearch
        , label = E.text "Search"
        }

viewSearchButtonStandort : E.Element Msg
viewSearchButtonStandort =
    EI.button
        [ EBG.color (E.rgb255 0x00 0x33 0x66)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 0x33 0x66 0x99)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just MsgSearch
        , label = E.text "Eingabe"
        }


cmdSearch model =
    Http.get {
        url = "https://nominatim.openstreetmap.org/search?q=%7B"++model.searchText++"%7D&format=json&limit=1" 
        , expect = Http.expectJson MsgGotResults locationDecoder
    }

cmdSearch1 model =
    Http.get {
        url = "https://nominatim.openstreetmap.org/search?q=%7B"++model.searchText1++"%7D&format=json&limit=1" 
        , expect = Http.expectJson MsgGotResults locationDecoder1
    }

locationDecoder : JD.Decoder ( List Locatio )
locationDecoder =
    JD.list 
    (JD.map2 Locatio
        (JD.field "lat" JD.string)
        (JD.field "lon" JD.string))

locationDecoder1 : JD.Decoder ( List Location1 )
locationDecoder1 =
    JD.list 
    (JD.map2 Location1
        (JD.field "lat" JD.string)
        (JD.field "lon" JD.string))
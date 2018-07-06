-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html


module Main exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Random
import Set exposing (Set)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


alphaNumericCharArray =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"
        |> String.toList
        |> Array.fromList



-- MODEL


type alias Model =
    { allStrings : Set String
    , currentLink : String
    , workingLinks : List String
    , brokenLinks : List String
    , savedLinks : List String
    , fit : Fit
    , isOn : Bool
    }


type Fit
    = Contain
    | Cover


init : ( Model, Cmd Msg )
init =
    ( { allStrings = Set.empty
      , currentLink = ""
      , workingLinks = []
      , brokenLinks = []
      , savedLinks = []
      , fit = Contain
      , isOn = True
      }
    , getRandomInts
    )


getRandomInts =
    Random.int 0 61
        |> Random.list idLen
        |> Random.generate NewString


idLen =
    5



-- UPDATE


type Msg
    = ToggleFit
    | ToggleOn
    | NewString (List Int)
    | TestURL (Result Http.Error String)
    | SaveLink String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveLink newLink ->
            ( { model | savedLinks = newLink :: model.savedLinks }, Cmd.none )

        ToggleFit ->
            let
                flipFit =
                    case model.fit of
                        Cover ->
                            Contain

                        Contain ->
                            Cover
            in
                ( { model | fit = flipFit }, Cmd.none )

        ToggleOn ->
            let
                stopOrNextPic =
                    case not model.isOn of
                        True ->
                            getRandomInts

                        False ->
                            Cmd.none
            in
                ( { model | isOn = not model.isOn }, stopOrNextPic )

        NewString ints ->
            let
                newString =
                    List.map (\x -> Maybe.withDefault '!' <| Array.get x alphaNumericCharArray) ints
                        |> List.map String.fromChar
                        |> String.concat

                isNewValue =
                    not (Set.member newString model.allStrings)
            in
                case isNewValue of
                    True ->
                        ( { model
                            | allStrings = Set.insert newString model.allStrings
                            , currentLink = newString
                          }
                        , doTest newString
                        )

                    False ->
                        ( model
                        , getRandomInts
                        )

        TestURL result ->
            let
                newModel =
                    case result of
                        Ok "200" ->
                            ({ model | workingLinks = model.currentLink :: model.workingLinks })

                        _ ->
                            ({ model | brokenLinks = model.currentLink :: model.brokenLinks })

                newCmds =
                    case model.isOn of
                        True ->
                            getRandomInts

                        False ->
                            Cmd.none
            in
                ( newModel, newCmds )


doTest : String -> Cmd Msg
doTest x =
    let
        url =
            imgurUrl x
    in
        Http.send TestURL (getHeader url)


getHeader : String -> Http.Request String
getHeader url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse extractHeader
        , timeout = Nothing
        , withCredentials = False
        }


extractHeader : Http.Response String -> Result String String
extractHeader resp =
    case Maybe.withDefault "200" <| Dict.get "last-modified" resp.headers of
        "Wed, 14 May 2014 05:44:36 GMT" ->
            Err "Image Removed"

        _ ->
            Ok "200"



-- SUBSCRIPTIONS


imgurUrl : String -> String
imgurUrl x =
    "https://i.imgur.com/" ++ x ++ ".jpg"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        toggleOn =
            case model.isOn of
                True ->
                    "Turn Off"

                False ->
                    "Turn On"

        containOrCover =
            case model.fit of
                Contain ->
                    " contain"

                Cover ->
                    " cover"

        imgur x =
            a
                [ href <| (imgurUrl x)
                , onDoubleClick (SaveLink x)
                , target "_blank"
                , class <| "animated zoomIn flex-grow-1 br2 ba bw1 white-20 bg-white-05 bg-center br1 overflow-hidden ma1 link" ++ containOrCover
                , style
                    [ ( "background-image", "url('" ++ (imgurUrl x) ++ "')" ) ]
                ]
                [ img
                    [ src <| (imgurUrl x)
                    , class "o-0"
                    , style [ ( "max-height", "15em" ), ( "max-width", "15em" ) ]
                    ]
                    []
                ]
    in
        div [ class "flex flex-column-reverse items-stretch vh-100 w-100 sans-serif white bg-black-90" ]
            [ nav
                [ class "flex-none bg-black flex items-center" ]
                [ button
                    [ onClick ToggleOn, class "flex-none pa3 ma0 f4 bg-white black bn" ]
                    [ text toggleOn ]
                , div [ class "flex-auto flex items-center overflow-auto" ] []
                , button [ onClick ToggleFit, class "flex-none pa3 ma0 f4 bg-white black bn" ] [ text containOrCover ]
                ]
            , div
                [ class "flex-auto overflow-auto" ]
                [ div [ class "w-100 flex flex-wrap-reverse-ns flex-column-reverse flex-row-ns items-stretch after-grow pa2" ] <|
                    List.map imgur <|
                        List.reverse model.workingLinks
                ]
            , meter
                [ class "h1 flex-none w-100 self-stretch"
                , Html.Attributes.min "0"
                , value <| toString <| List.length model.workingLinks
                , Html.Attributes.max <| toString <| List.length model.workingLinks + List.length model.brokenLinks
                ]
                []
            ]

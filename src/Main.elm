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
    { randomInts : List Int
    , allStrings : Set String
    , queuedStrings : Set String
    , workingLinks : List String
    , brokenLinks : List String
    , savedLinks : List String
    , fit : Fit
    }


type Fit
    = Contain
    | Cover


init : ( Model, Cmd Msg )
init =
    ( Model [] Set.empty Set.empty [] [] [] Cover, Cmd.none )


genRoll =
    Random.generate NewFace (Random.int 0 61)


rollSize =
    100


idLen =
    5



-- UPDATE


type Msg
    = ToggleFit
    | Roll
    | NewFace Int
    | Test (Result Http.Error String)
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

        Roll ->
            ( model, genRoll )

        NewFace newFace ->
            case List.length (Set.toList model.queuedStrings) >= rollSize of
                True ->
                    ( model, testHead model.queuedStrings )

                False ->
                    let
                        newString =
                            List.map (\x -> Maybe.withDefault '!' <| Array.get x alphaNumericCharArray) model.randomInts
                                |> List.map String.fromChar
                                |> String.concat

                        isNewValue =
                            not (Set.member newString model.allStrings)

                        longEnough =
                            List.length model.randomInts >= idLen
                    in
                        case longEnough && isNewValue of
                            True ->
                                ( { model
                                    | queuedStrings = Set.insert newString model.queuedStrings
                                    , allStrings = Set.insert newString model.allStrings
                                    , randomInts = List.drop idLen model.randomInts
                                  }
                                , genRoll
                                )

                            False ->
                                ( { model | randomInts = newFace :: model.randomInts }
                                , genRoll
                                )

        Test result ->
            let
                broken =
                    ( { model
                        | brokenLinks = addHead model.brokenLinks
                        , queuedStrings = tail
                      }
                    , testHead tail
                    )

                head =
                    Set.toList model.queuedStrings
                        |> List.head
                        |> Maybe.withDefault ""

                addHead list =
                    Set.toList model.queuedStrings
                        |> List.head
                        |> Maybe.map (\x -> x :: list)
                        |> Maybe.withDefault list

                tail =
                    Set.remove head model.queuedStrings
            in
                case result of
                    Ok "200" ->
                        ( { model
                            | workingLinks = addHead model.workingLinks
                            , queuedStrings = tail
                          }
                        , testHead tail
                        )

                    _ ->
                        broken


testHead : Set String -> Cmd Msg
testHead setStr =
    case List.head <| Set.toList setStr of
        Just str ->
            doTest str

        Nothing ->
            Cmd.none


doTest : String -> Cmd Msg
doTest randomStr =
    let
        url =
            "https://i.imgur.com/" ++ randomStr ++ ".jpg"
    in
        Http.send Test (getHeader url)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        containOrCover =
            case model.fit of
                Contain ->
                    " contain"

                Cover ->
                    " cover"

        imgur x =
            a
                [ href <| "https://i.imgur.com/" ++ x ++ ".jpg"
                , onDoubleClick (SaveLink x)
                , target "_blank"
                , class <| "flex-grow-1 br2 ba bw1 white-20 bg-white-05 bg-center br1 overflow-hidden ma1 link" ++ containOrCover
                , style
                    [ ( "background-image", "url('" ++ "https://i.imgur.com/" ++ x ++ ".jpg" ++ "')" ) ]
                ]
                [ img [ src <| "https://i.imgur.com/" ++ x ++ ".jpg", class "o-0 h4-ns h3" ] []
                ]
    in
        div [ class "flex flex-column-reverse items-stretch vh-100 w-100 sans-serif white bg-black-90" ]
            [ nav
                [ class "flex-none bg-black flex items-center" ]
                [ button
                    [ onClick Roll, class "flex-none pa3 ma0 f4 bg-white black bn" ]
                    [ text "Random Images" ]
                , div [ class "flex-auto flex items-center overflow-auto" ] <|
                    List.map (\x -> span [ class "flex-none pa2" ] [ text x ]) <|
                        Set.toList model.queuedStrings
                , button [ onClick ToggleFit, class "flex-none pa3 ma0 f4 bg-white black bn" ] [ text containOrCover ]
                ]
            , div
                [ class "flex-auto overflow-auto" ]
                [ div [ class "w-100 flex flex-wrap items-stretch after-grow pa2" ] <|
                    List.map imgur <|
                        List.reverse model.workingLinks
                ]
            , progress
                [ class "h1 flex-none w-100 self-stretch"
                , value <| toString <| List.length <| Set.toList model.queuedStrings
                , Html.Attributes.max <| toString rollSize
                ]
                []
            , meter
                [ class "h1 flex-none w-100 self-stretch"
                , Html.Attributes.min "0"
                , value <| toString <| List.length model.workingLinks
                , Html.Attributes.max <| toString <| List.length model.workingLinks + List.length model.brokenLinks
                ]
                []
            ]

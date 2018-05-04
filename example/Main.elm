module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)
import Set
import UuidStream exposing (..)


type alias Model =
    { uuidStream : UuidStringStream
    , uuidList : List String
    }


type Msg
    = Generate String


type alias InitData =
    { firstInt : Int, extensions : List Int }


init : InitData -> ( Model, Cmd msg )
init seed =
    (,)
        { uuidStream = uuidStringStream seed.firstInt seed.extensions
        , uuidList = []
        }
        Cmd.none


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Generate numberStr ->
            case String.toInt numberStr of
                Err _ ->
                    ( model, Cmd.none )

                Ok n ->
                    let
                        ( uuids, newStream ) =
                            consumeMany n model.uuidStream
                    in
                        (,)
                            { model
                                | uuidStream = newStream
                                , uuidList = uuids
                            }
                            Cmd.none


view : Model -> Html Msg
view model =
    let
        duplicates =
            List.foldl
                (\uuid ( acc, set ) ->
                    if Set.member uuid set then
                        ( acc + 1, set )

                    else
                        ( acc, Set.insert uuid set )
                )
                ( 0, Set.empty )
                model.uuidList
                |> Tuple.first
    in
        Html.div [] <|
            [ Html.input [ type_ "number", onInput Generate ] []
            , Html.text <| " Duplicates: " ++ toString duplicates
            ]
                ++ List.map (\uuid -> Html.p [] [ Html.text uuid ]) model.uuidList


main : Program InitData Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

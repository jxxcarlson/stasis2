module Frontend exposing (Model, app)

-- import CellGrid exposing (CellGrid)
-- import CellGrid.Render exposing (CellRenderer)

import Array
import CellGrid exposing (CellGrid)
import CellGrid.Render exposing (CellRenderer)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, field, string)
import Lamdera.Frontend
import Lamdera.Types exposing (Milliseconds, WsError)
import Msg exposing (FrontendMsg(..), ToBackend(..), ToFrontend(..))
import Random
import Utility
import World exposing (Resource(..), World, WorldChange)
import WorldGrid exposing (State(..))


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.Frontend.application is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.Frontend.application
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Lamdera counter app"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> FNoop
        , onUrlRequest = \_ -> FNoop
        }


type alias Model =
    { counter : Int
    , stagedWorldChange : WorldChange
    , world : World
    , cellGrid : CellGrid State
    , selectedState : State
    , randomFloat : Float
    , message : String
    , message2 : String
    }


init : ( Model, Cmd FrontendMsg )
init =
    ( { counter = 0
      , stagedWorldChange =
            { cities = 0
            , crops = 0
            , nature = 0
            }
      , world =
            World.init
      , cellGrid =
            WorldGrid.emptyGrid gridWidth gridWidth
                |> WorldGrid.setRandomCell 0.2 (Occupied City)
                |> WorldGrid.setRandomCell 0.8 (Occupied Crop)
      , selectedState = Occupied Crop
      , randomFloat = 0.0
      , message = "Game starting.  Try to make CO2 offsets larger (now at -2)."
      , message2 = ""
      }
    , sendToBackend ClientJoin
    )


view : Model -> Html FrontendMsg
view model =
    viewGame model



-- Html.div [ Html.Attributes.style "padding" "30px" ]
--     [ Html.button [ onClick Increment ] [ text "+" ]
--     , Html.text (String.fromInt model.counter)
--     , Html.button [ onClick Decrement ] [ text "-" ]
--     , viewGame model
--     ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, sendToBackend CounterDecremented )

        FNoop ->
            ( model, Cmd.none )

        ChangeTheWorld ->
            let
                newModel =
                    stageWorldChange model

                co2Offset =
                    (World.score model.world).co2Offset
            in
            ( { newModel | message = co2Message co2Offset, message2 = "" }
            , Random.generate NewRandomFloat2 (Random.float 0 1)
            )

        StageResource newResource ->
            let
                stagedResource =
                    model.stagedWorldChange
            in
            case newResource of
                World.Nature ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | nature = stagedResource.nature + 1 }
                        , cellGrid = WorldGrid.setRandomCell model.randomFloat (Occupied Nature) model.cellGrid
                      }
                    , Random.generate NewRandomFloat (Random.float 0 1)
                    )

                World.Crop ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | crops = stagedResource.crops + 1 }
                        , cellGrid = WorldGrid.setRandomCell model.randomFloat (Occupied Crop) model.cellGrid
                      }
                    , Random.generate NewRandomFloat (Random.float 0 1)
                    )

                World.City ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | cities = stagedResource.cities + 1 }
                        , cellGrid = WorldGrid.setRandomCell model.randomFloat (Occupied City) model.cellGrid
                      }
                    , Random.generate NewRandomFloat (Random.float 0 1)
                    )

        CellGrid msg_ ->
            case msg_ of
                CellGrid.Render.MouseClick ( i, j ) ( x, y ) ->
                    let
                        message =
                            "(i,j) = (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")"

                        maybeSelectedCell =
                            CellGrid.cellAtMatrixIndex ( i, j ) model.cellGrid

                        ( newStagedWorldChange, newCellGrid ) =
                            case model.selectedState of
                                Unoccupied ->
                                    ( model.stagedWorldChange, model.cellGrid )

                                Occupied resource ->
                                    case World.resourceAvailable resource model.stagedWorldChange model.world of
                                        True ->
                                            ( updateWordChange maybeSelectedCell model.selectedState model.stagedWorldChange
                                            , WorldGrid.toggleState model.selectedState ( i, j ) model.cellGrid
                                            )

                                        False ->
                                            ( model.stagedWorldChange, model.cellGrid )
                    in
                    ( { model
                        | stagedWorldChange = newStagedWorldChange
                        , cellGrid = newCellGrid
                      }
                    , Cmd.none
                    )

        ChooseCity ->
            ( { model | selectedState = Occupied City }, Cmd.none )

        ChooseCrop ->
            ( { model | selectedState = Occupied Crop }, Cmd.none )

        ChooseNature ->
            ( { model | selectedState = Occupied Nature }, Cmd.none )

        ChooseUnoccupied ->
            ( { model | selectedState = Unoccupied }, Cmd.none )

        NewRandomFloat p ->
            ( { model | randomFloat = p }, Cmd.none )

        NewRandomFloat2 p ->
            let
                s =
                    World.score model.world

                pd =
                    adjustProbability s.co2Offset gameParameters.probabilityOfDisaster

                ( deltaCrop, newCellGrid ) =
                    if p < pd then
                        WorldGrid.changeFractionOfGivenState p 0.33 (Occupied Crop) Unoccupied model.cellGrid

                    else
                        ( 0, model.cellGrid )

                numberOfCropAreas =
                    WorldGrid.indicesOfCellsOfGivenState (Occupied Crop) newCellGrid
                        |> List.length

                numberOfCities =
                    WorldGrid.indicesOfCellsOfGivenState (Occupied City) newCellGrid
                        |> List.length

                excessCities =
                    numberOfCities - numberOfCropAreas

                fractionalExcessCites =
                    toFloat excessCities / toFloat numberOfCities

                ( deltaCities, newCellGrid2 ) =
                    WorldGrid.changeFractionOfGivenState p fractionalExcessCites (Occupied City) Unoccupied newCellGrid

                stagedResource =
                    model.stagedWorldChange

                newStagedWorldChange =
                    { stagedResource | crops = stagedResource.crops - deltaCrop, cities = stagedResource.cities - deltaCities }

                disasterMessage =
                    if deltaCrop > 0 then
                        "Environment under stress! You have lost " ++ String.fromInt deltaCrop ++ " units of cropland"

                    else
                        ""

                disasterMessage2 =
                    if deltaCities > 0 then
                        "Mayday, Mayday! we have lost " ++ String.fromInt deltaCities ++ " cities"

                    else
                        ""
            in
            ( { model
                | cellGrid = newCellGrid2
                , stagedWorldChange = newStagedWorldChange
                , randomFloat = p
                , message = disasterMessage
                , message2 = disasterMessage2
              }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue ->
            ( { model | counter = newValue }, Cmd.none )


sendToBackend : Msg.ToBackend -> Cmd Msg.FrontendMsg
sendToBackend msg =
    Lamdera.Frontend.sendToBackend 1000 (\_ -> FNoop) msg



---
--- PASTED IN
---


type alias GameParameters =
    { probabilityOfDisaster : Float
    }


gameParameters =
    { probabilityOfDisaster = 0.2
    }


gridDisplayWidth =
    550.0


gridWidth =
    16


fundsAvailable : Model -> Int
fundsAvailable model =
    (World.score model.world).productivity
        - model.stagedWorldChange.nature
        - 2
        * model.stagedWorldChange.crops
        - 3
        * model.stagedWorldChange.cities


fundsAvailableMessage : Model -> String
fundsAvailableMessage model =
    "Funds available: " ++ String.fromInt (fundsAvailable model)


adjustProbability : Int -> Float -> Float
adjustProbability co2Offset p =
    probabilityFactor co2Offset * p |> clamp 0 1


probabilityFactor : Int -> Float
probabilityFactor co2offset =
    let
        c =
            co2offset |> toFloat
    in
    if c > 0 then
        1 / (1 + c / 10)

    else
        1 - c / 10


stageWorldChange : Model -> Model
stageWorldChange model =
    { model
        | stagedWorldChange =
            { cities = 0
            , crops = 0
            , nature = 0
            }
        , world =
            model.world ++ [ model.stagedWorldChange ]
    }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.none


viewGame : Model -> Html FrontendMsg
viewGame model =
    div
        [ Html.Attributes.style "font-size" "40px"
        ]
        [ div [ Html.Attributes.style "float" "left" ]
            [ renderGrid model
            , div [ Html.Attributes.style "float" "left" ] [ palette model ]
            ]
        , div [ Html.Attributes.style "float" "left", Html.Attributes.style "margin-left" "20px" ]
            [ turnView model.world
            , model.world
                |> World.view model.stagedWorldChange
                |> Html.map StageResource
            , Html.button
                [ Html.Events.onClick ChangeTheWorld
                , Html.Attributes.style "font-size" "40px"
                , Html.Attributes.style "margin-top" "10px"
                ]
                [ text "Change the World! 🌏 🙌" ]
            , div
                [ style "font-size" "24px"
                , Html.Attributes.style "margin-top" "20px"
                , style "width" "200px"
                , style "word-wrap" "break-word"
                ]
                [ text <| fundsAvailableMessage model ]
            , div
                [ Html.Attributes.style "font-size" "24px"
                , Html.Attributes.style "margin-top" "20px"
                ]
                [ text <| model.message ]
            , div
                [ Html.Attributes.style "font-size" "24px"
                , Html.Attributes.style "margin-top" "20px"
                , Html.Attributes.style "color" "red"
                ]
                [ text <| model.message2 ]
            , disasterProbabilityView model
            ]
        ]


disasterProbabilityView : Model -> Html FrontendMsg
disasterProbabilityView model =
    let
        s =
            World.score model.world

        p =
            adjustProbability s.co2Offset gameParameters.probabilityOfDisaster
                |> Utility.roundTo 3

        r =
            model.randomFloat |> Utility.roundTo 3
    in
    div
        [ Html.Attributes.style "font-size" "24px"
        , Html.Attributes.style "margin-top" "20px"
        ]
        [ text <| "p = " ++ String.fromFloat p ++ ", r = " ++ String.fromFloat r ]


turnView : World -> Html msg
turnView world =
    text ("Turn number: " ++ String.fromInt (List.length world))



--
-- Added by JC
--


renderGrid : Model -> Html FrontendMsg
renderGrid model =
    CellGrid.Render.asHtml
        gridDisplayWidth
        gridDisplayWidth
        cellrenderer
        model.cellGrid
        |> Html.map CellGrid


palette : Model -> Html FrontendMsg
palette model =
    div []
        [ div st [ paletteButton model City, text <| String.fromInt model.stagedWorldChange.cities ]
        , div st [ paletteButton model Crop, text <| String.fromInt model.stagedWorldChange.crops ]
        , div st [ paletteButton model Nature, text <| String.fromInt model.stagedWorldChange.nature ]
        ]


paletteButton : Model -> Resource -> Html FrontendMsg
paletteButton model resource =
    let
        dimensions =
            if model.selectedState == Occupied resource then
                "70px"

            else
                "60px"
    in
    Html.button
        [ Html.Attributes.style "width" dimensions
        , Html.Attributes.style "height" dimensions
        , Html.Attributes.style "font-color" "white"
        , Html.Attributes.style "background-color" (colorOfResource resource)
        , Html.Attributes.style "margin-right" "10px"
        , Html.Attributes.style "font-size" "30px"
        , Html.Events.onClick (handlerOfResource resource)
        , Html.Attributes.disabled (not (World.resourceAvailable resource model.stagedWorldChange model.world))
        ]
        [ text <| labelForResource resource ]


st =
    [ Html.Attributes.style "margin-right" "10px" ]


handlerOfResource : Resource -> FrontendMsg
handlerOfResource resource =
    case resource of
        City ->
            ChooseCity

        Crop ->
            ChooseCrop

        Nature ->
            ChooseNature


colorOfResource : Resource -> String
colorOfResource resource =
    case resource of
        City ->
            "blue"

        Crop ->
            "#ee5"

        Nature ->
            "#5f5"


labelForResource : Resource -> String
labelForResource resource =
    case resource of
        City ->
            World.emojiFromResource City

        Crop ->
            World.emojiFromResource Crop

        Nature ->
            World.emojiFromResource Nature


updateWordChange : Maybe State -> State -> WorldChange -> WorldChange
updateWordChange maybeSelectedCell chosenState worldChange =
    if maybeSelectedCell /= Just chosenState then
        case chosenState of
            Occupied Crop ->
                { worldChange | crops = worldChange.crops + 1 }

            Occupied City ->
                { worldChange | cities = worldChange.cities + 1 }

            Occupied Nature ->
                { worldChange | nature = worldChange.nature + 1 }

            Unoccupied ->
                worldChange

    else
        case chosenState of
            Occupied Crop ->
                { worldChange | crops = worldChange.crops - 1 }

            Occupied City ->
                { worldChange | cities = worldChange.cities - 1 }

            Occupied Nature ->
                { worldChange | nature = worldChange.nature - 1 }

            Unoccupied ->
                worldChange


cellrenderer =
    { cellSize = gridDisplayWidth / toFloat gridWidth
    , cellColorizer =
        \state ->
            case state of
                Occupied Crop ->
                    Color.rgb 1 1 0

                Occupied City ->
                    Color.rgb 0 0 1

                Occupied Nature ->
                    Color.rgb 0 1 0

                Unoccupied ->
                    Color.rgb 0 0 0
    , defaultColor = Color.rgb 0 0 0
    , gridLineWidth = 0.5
    , gridLineColor = Color.rgb 0 0 1
    }


co2Message : Int -> String
co2Message co2Offset =
    if co2Offset < -10 then
        "CO2 levels very high: extreme danger."

    else if co2Offset < 0 then
        "CO2 leve;s high: take remedial action immediately"

    else if co2Offset < -5 then
        "CO2 levels high"

    else
        "CO2 levels better ... keep working on it!"

module World exposing
    ( Resource(..)
    , World
    , WorldChange
    , aggregate
    , emojiFromResource
    , init
    , resourceAvailable
    , score
    , view
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)
import Html.Attributes
import Html.Events
import Style



-- import Msg exposing (FrontendMsg)


type alias World =
    List WorldChange


type alias WorldChange =
    { nature : Int
    , crops : Int
    , cities : Int
    }


type alias Score =
    { co2Offset : Int
    , cropYield : Int
    , productivity : Int
    , cropUse : Int
    }


type Resource
    = Nature
    | Crop
    | City


init : World
init =
    [ { nature = 0
      , crops = 1
      , cities = 1
      }
    ]


score :
    World
    -> Score
score world =
    List.map scoreForWorldChange world
        |> List.foldl
            (\worldChange scoreSoFar ->
                { co2Offset =
                    scoreSoFar.co2Offset
                        + worldChange.co2Offset
                , cropYield =
                    scoreSoFar.cropYield
                        + worldChange.cropYield
                , productivity =
                    scoreSoFar.productivity
                        + worldChange.productivity
                , cropUse =
                    scoreSoFar.cropUse
                        + worldChange.cropUse
                }
            )
            zeroScore


zeroScore : Score
zeroScore =
    { co2Offset = 0, cropYield = 0, productivity = 0, cropUse = 0 }


scoreForWorldChange : WorldChange -> Score
scoreForWorldChange worldChange =
    { cropYield = worldChange.crops
    , cropUse = worldChange.cities
    , productivity = worldChange.cities * 3
    , co2Offset =
        worldChange.nature
            * 2
            - (worldChange.cities + worldChange.crops)
    }


view : WorldChange -> World -> Element Resource
view stagedWorldChange world =
    column [ spacing 36 ]
        [ scoreView (score world)
        , column
            []
            [ resourceView Nature stagedWorldChange world
            , resourceView Crop stagedWorldChange world
            , resourceView City stagedWorldChange world
            ]
        ]


resourceView : Resource -> WorldChange -> World -> Element Resource
resourceView resourceMsg stagedWorldChange world =
    row [ Font.size 48, spacing 12, Element.htmlAttribute (Html.Attributes.disabled (not (resourceAvailable resourceMsg stagedWorldChange world))) ]
        [ el [ width (px 48) ] <| Element.text (String.fromInt (aggregate world |> getGetter resourceMsg))
        , el [ width (px 48) ] <| Element.text (String.fromInt (stagedWorldChange |> getGetter resourceMsg))
        , case resourceAvailable resourceMsg stagedWorldChange world of
            True ->
                Input.button
                    []
                    { onPress = Just resourceMsg
                    , label =
                        el []
                            (Element.text <| emojiFromResource resourceMsg)
                    }

            False ->
                Input.button
                    []
                    { onPress = Nothing
                    , label =
                        el [ Border.color Style.gray, Border.width 8, Font.size 24 ]
                            (Element.text <| emojiFromResource resourceMsg)
                    }

        -- ++ laborForResource resourceMsg
        -- |> Element.text
        ]


resourceAvailable : Resource -> WorldChange -> World -> Bool
resourceAvailable resource stagedChange world =
    let
        productivityAvailable =
            score (world ++ [])
                |> .productivity
    in
    productivityAvailable
        >= productivityNeeded stagedChange resource
        && enoughCropAvailable resource stagedChange world


enoughCropAvailable : Resource -> WorldChange -> World -> Bool
enoughCropAvailable resource worldChange world =
    let
        totalScore =
            score world

        cropNeeded =
            cropNeededForResource resource
                + worldChange.cities
                + totalScore.cropUse
    in
    totalScore.cropYield >= cropNeeded


cropNeededForResource : Resource -> Int
cropNeededForResource resource =
    case resource of
        Nature ->
            0

        Crop ->
            0

        City ->
            1


productivityNeeded : WorldChange -> Resource -> Int
productivityNeeded stagedChange resource =
    stagedChange.cities
        * 3
        + stagedChange.crops
        * 2
        + stagedChange.nature
        * 1
        + (case resource of
            City ->
                3

            Crop ->
                2

            Nature ->
                1
          )



-- 4


laborForResource : Resource -> String
laborForResource resourceMsg =
    ((case resourceMsg of
        Nature ->
            1

        Crop ->
            2

        City ->
            3
     )
        |> String.fromInt
    )
        ++ " "


getGetter : Resource -> (WorldChange -> Int)
getGetter addResource =
    case addResource of
        Nature ->
            .nature

        Crop ->
            .crops

        City ->
            .cities


emojiFromResource : Resource -> String
emojiFromResource resource =
    case resource of
        Nature ->
            "🌳"

        Crop ->
            "🌾"

        City ->
            "🏢"


aggregate : World -> WorldChange
aggregate world =
    world
        |> List.foldl aggregatorThing
            { cities = 0
            , crops = 0
            , nature = 0
            }


aggregatorThing : WorldChange -> WorldChange -> WorldChange
aggregatorThing worldChange changeSoFar =
    { cities =
        worldChange.cities
            + changeSoFar.cities
    , crops =
        worldChange.crops
            + changeSoFar.crops
    , nature =
        worldChange.nature
            + changeSoFar.nature
    }



-- scoreView : Score -> Element msg


scoreView worldScore =
    let
        data : List ( String, String )
        data =
            [ ( "🍅"
              , (worldScore.cropUse |> String.fromInt)
                    ++ "/"
                    ++ (worldScore.cropYield |> String.fromInt)
              )

            --, ( "👷\u{200D}♀️", worldScore.productivity |> String.fromInt )
            , ( "🌬", worldScore.co2Offset |> String.fromInt )
            ]

        viewDatum : ( String, String ) -> Element msg
        viewDatum =
            \( key, value ) -> row [] [ Element.text (key ++ " " ++ value) ]
    in
    column [ Font.size 48 ] (List.map viewDatum data)

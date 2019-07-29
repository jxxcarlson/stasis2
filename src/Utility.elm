module Utility exposing (chooseRandomSubList, getListElement, maxFloat, minFloat, roundTo, uniquefyList)


getListElement : Int -> List a -> Maybe a
getListElement k list =
    list |> List.drop (k - 1) |> List.head


uniquefyList : List comparable -> List comparable
uniquefyList list =
    list
        |> List.sort
        |> List.foldl
            (\item acc ->
                if Just item /= List.head acc then
                    item :: acc

                else
                    acc
            )
            []


{-| Choose a random fraction p of the input list.
For now, that fraction is taken from the front of the
list.
-}
chooseRandomSubList : Float -> Float -> List a -> List a
chooseRandomSubList seed p list =
    let
        m =
            List.length list |> toFloat

        n =
            round (p * m)
    in
    List.take n list


roundTo : Int -> Float -> Float
roundTo places x =
    let
        f =
            10 ^ places |> toFloat
    in
    toFloat (round (f * x)) / f


minFloat : List Float -> Maybe Float
minFloat xs =
    List.sort xs |> List.head


maxFloat : List Float -> Maybe Float
maxFloat xs =
    List.sort xs |> List.reverse |> List.head

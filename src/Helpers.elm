module Helpers exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
groupWhile : (a -> a -> Bool) -> List a -> List ( a, List a )
groupWhile isSameGroup items =
    List.foldr
        (\x acc ->
            case acc of
                [] ->
                    [ ( x, [] ) ]

                ( y, restOfGroup ) :: groups ->
                    if isSameGroup x y then
                        ( x, y :: restOfGroup ) :: groups

                    else
                        ( x, [] ) :: acc
        )
        []
        items

maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
maybeFilter fun = Maybe.andThen (\x -> if fun x then Just x else Nothing)

find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                dropWhile predicate xs

            else
                list

onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.succeed onEnterAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy extract list =
    let
        testFn : (a -> a -> Bool)
        testFn = (\a b -> extract a == extract b)

        helper : List a -> List ( comparable, List a ) -> List ( comparable, List a )
        helper scattered gathered =
            case scattered of
                [] -> gathered
                toGather :: population ->
                    let
                        ( gathering, remaining ) =
                            List.partition (testFn toGather) population
                    in
                    helper remaining (( extract toGather, toGather :: gathering ) :: gathered)
    in
    helper list [] |> Dict.fromList
module Model.Label exposing (Label, Id, Data, Collection, new, getData, emptyCollection, fromList, getCollection, idMapCollection, idToString, stringToId)

import Dict exposing (Dict)

type Label
    = Label Data

type alias Data =
    {
        name: String,
        color: String
    }

type Id = Id String

type Collection
    = Collection (Dict String Label)

new: String -> String -> Label
new name color =
    Label { name = name, color = color }

getData: Label -> Data
getData (Label data) = data

emptyCollection: Collection
emptyCollection = Collection Dict.empty

fromList: List (String, Label) -> Collection
fromList list = Collection (Dict.fromList list)

getCollection: Id -> Collection -> Maybe Label
getCollection (Id id) (Collection dict) =
    Dict.get id dict

idMapCollection: (Id -> Label -> a) -> Collection -> List a
idMapCollection f (Collection dict) =
    Dict.values <| Dict.map (\id card -> f (Id id) card) dict

idToString: Id -> String
idToString (Id id) = id

stringToId: String -> Id
stringToId string = Id string
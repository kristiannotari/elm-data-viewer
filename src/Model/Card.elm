module Model.Card exposing (Card, Collection, Id, Data, getData, new, emptyCollection, insertCollection, updateCollection, getCollection, idMapCollection, idToString)

import Time
import Task
import Model.Label as Label exposing (Label)
import Dict exposing (Dict)
import Task exposing (Task)

type Card
    = Card Data

type alias Data =
    {title : String
    , spentTime : Int
    , plannedTime : Maybe Int
    , label : Maybe Label.Id
    }

type Id = Id Int

type Collection
    = Collection (Dict Int Card)


new: Data -> Card
new data =
    Card data


getData: Card -> Data
getData (Card data) =
    data


idToString: Id -> String
idToString (Id id) = String.fromInt id


generateId: Card -> Task x Int
generateId _ =
    Task.andThen (\posix -> Task.succeed <| Time.posixToMillis posix) Time.now

emptyCollection: Collection
emptyCollection =
    Collection Dict.empty

insertCollection: Card -> Collection -> Task x Collection
insertCollection card (Collection dict) =
    Task.andThen (\id -> Task.succeed <| Collection <| Dict.insert id card dict) (generateId card)

updateCollection: Id -> Card -> Collection -> Collection
updateCollection (Id id) card (Collection dict) =
    Collection <| Dict.update id (\m -> Just card) dict

getCollection: Id -> Collection -> Maybe Card
getCollection (Id id) (Collection dict) =
    Dict.get id dict

idMapCollection: (Id -> Card -> a) -> Collection -> List a
idMapCollection f (Collection dict) =
    Dict.values <| Dict.map (\id card -> f (Id id) card) dict
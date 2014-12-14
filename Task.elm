module Task where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Json.Decode as Json
import LocalChannel as LC
import Maybe
import Signal
import String


-- MODEL

type alias Model =
    { description : String
    , completed   : Bool
    , edits       : Maybe String
    , id          : Int
    }


init : String -> Int -> Model
init desc id =
    { description = desc
    , completed = False 
    , edits = Nothing
    , id = id
    }


-- UPDATE

type Action
    = Focus
    | Edit String
    | Cancel
    | Commit
    | Completed Bool
    | Delete


update : Action -> Model -> Maybe Model
update update task =
    case update of
      Focus ->
          Just { task | edits <- Just task.description }

      Edit description ->
          Just { task | edits <- Just description }

      Cancel ->
          Just { task | edits <- Nothing }

      Commit ->
          case task.edits of
            Nothing ->
                Nothing

            Just rawDescription ->
                let description = String.trim rawDescription in
                if String.isEmpty description then Nothing else
                    Just
                      { task |
                          edits <- Nothing,
                          description <- String.trim description
                      }

      Completed bool ->
          Just { task | completed <- bool }

      Delete ->
          Nothing


-- VIEW

view : LC.LocalChannel (Int, Action) -> Model -> Html
view channel task =
    let className =
            (if task.completed then "completed " else "") ++
              case task.edits of
                Just _ -> "editing"
                Nothing -> ""

        description =
            Maybe.withDefault task.description task.edits
    in

    li
      [ class className ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked task.completed
              , onClick (LC.send channel (task.id, Completed (not task.completed)))
              ]
              []
          , label
              [ onDoubleClick (LC.send channel (task.id, Focus)) ]
              [ text description ]
          , button
              [ class "destroy"
              , onClick (LC.send channel (task.id, Delete))
              ]
              []
          ]
      , input
          [ class "edit"
          , value description
          , name "title"
          , id ("todo-" ++ toString task.id)
          , on "input" targetValue (\desc -> LC.send channel (task.id, Edit desc))
          , onBlur (LC.send channel (task.id, Cancel))
          , onEnter (LC.send channel (task.id, Commit))
          ]
          []
      ]

onEnter : Signal.Message -> Attribute
onEnter message =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

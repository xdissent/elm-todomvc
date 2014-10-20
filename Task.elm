module Task where

import String


type Task =
    { description : String
    , completed   : Bool
    , edits       : Maybe String
    , id          : Int
    }


init : String -> Int -> Task
init desc id =
    { description = desc
    , completed = False 
    , edits = Nothing
    , id = id
    }


data Update
    = Focus
    | Edit String
    | Cancel
    | Commit


step : Update -> Task -> Maybe Task
step update task =
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

            Just description ->
                Just
                  { task |
                      edits <- Nothing,
                      description <- String.trim description
                  }

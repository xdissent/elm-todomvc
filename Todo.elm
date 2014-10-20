module Todo where
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

This clean division of concerns is a core part of Elm. You can read more about
this in the Pong tutorial: http://elm-lang.org/blog/Pong.elm

This program is not particularly large, so definitely see the following
document for notes on structuring more complex GUIs with Elm:
https://gist.github.com/evancz/2b2ba366cae1887fe621
-}

import Graphics.Input
import Graphics.Input as Input
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Tags (..)
import Html.Optimize.RefEq as Ref
import List
import Maybe
import String
import Window

import Task


---- MODEL ----

-- The full application state of our todo app.
type State =
    { tasks      : [Task.Task]
    , field      : String
    , uid        : Int
    , visibility : String
    }

emptyState : State
emptyState =
    { tasks = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }


---- UPDATE ----

-- A description of the kinds of updates that can change the state of the
-- application. See the following post for more info on this pattern and
-- some alternatives: https://gist.github.com/evancz/2b2ba366cae1887fe621
data Update
    = NoOp
    | UpdateField String
    | Add
    | Edit Int Task.Update
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


-- How we step the state forward for any given update
step : Update -> State -> State
step update state =
    case update of
      NoOp -> state

      UpdateField str ->
          { state | field <- str }

      Add ->
          let description = String.trim state.field in
          if String.isEmpty description then state else
              { state |
                  uid <- state.uid + 1,
                  field <- "",
                  tasks <- state.tasks ++ [Task.init description state.uid]
              }

      Edit id taskUpdate ->
          let update task =
                  if task.id == id
                      then Task.step taskUpdate task
                      else Just task
          in
              { state | tasks <- List.filterMap update state.tasks }

      Delete id ->
          { state | tasks <- filter (\t -> t.id /= id) state.tasks }

      DeleteComplete ->
          { state | tasks <- filter (not << .completed) state.tasks }

      Check id isCompleted ->
          let update t = if t.id == id then { t | completed <- isCompleted } else t
          in  { state | tasks <- map update state.tasks }

      CheckAll isCompleted ->
          let update t = { t | completed <- isCompleted } in
          { state | tasks <- map update state.tasks }

      ChangeVisibility visibility ->
          { state | visibility <- visibility }


---- VIEW ----

view : State -> Html
view state =
    div
      [ class "todomvc-wrapper"
      , style [ prop "visibility" "hidden" ]
      ]
      [ section
          [ id "todoapp" ]
          [ Ref.lazy taskEntry state.field
          , Ref.lazy2 taskList state.visibility state.tasks
          , Ref.lazy2 controls state.visibility state.tasks
          ]
      , infoFooter
      ]

onEnter : Input.Handle a -> a -> Attribute
onEnter handle value =
    on "keydown" (when (\k -> k.keyCode == 13) getKeyboardEvent) handle (always value)

taskEntry : String -> Html
taskEntry task =
    header 
      [ id "header" ]
      [ h1 [] [ text "todos" ]
      , input
          [ id "new-todo"
          , placeholder "What needs to be done?"
          , autofocus True
          , value task
          , name "newTodo"
          , on "input" getValue updates.handle UpdateField
          , onEnter updates.handle Add
          ]
          []
      ]

taskList : String -> [Task.Task] -> Html
taskList visibility tasks =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              "All" -> True

        allCompleted = all .completed tasks

        cssVisibility = if isEmpty tasks then "hidden" else "visible"
    in
    section
      [ id "main"
      , style [ prop "visibility" cssVisibility ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onclick updates.handle (\_ -> CheckAll (not allCompleted))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (map viewTask (filter isVisible tasks))
      ]


viewTask : Task.Task -> Html
viewTask task =
    let className =
            (if task.completed then "completed " else "") ++
            (Maybe.maybe "" (always "editing") task.edits)

        description =
            Maybe.maybe task.description identity task.edits
    in

    li
      [ class className ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked task.completed
              , onclick updates.handle (\_ -> Check task.id (not task.completed))
              ]
              []
          , label
              [ ondblclick updates.handle (\_ -> Edit task.id Task.Focus) ]
              [ text description ]
          , button
              [ class "destroy"
              , onclick updates.handle (always (Delete task.id))
              ]
              []
          ]
      , input
          [ class "edit"
          , value description
          , name "title"
          , id ("todo-" ++ show task.id)
          , on "input" getValue updates.handle (\desc -> Edit task.id (Task.Edit desc))
          , onblur updates.handle (Edit task.id Task.Cancel)
          , onEnter updates.handle (Edit task.id Task.Commit)
          ]
          []
      ]


controls : String -> [Task.Task] -> Html
controls visibility tasks =
    let tasksCompleted = length (filter .completed tasks)
        tasksLeft = length tasks - tasksCompleted
        item_ = if tasksLeft == 1 then " item" else " items"
    in
    footer
      [ id "footer"
      , hidden (isEmpty tasks)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (show tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul
          [ id "filters" ]
          [ visibilitySwap "#/" "All" visibility
          , text " "
          , visibilitySwap "#/active" "Active" visibility
          , text " "
          , visibilitySwap "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , onclick updates.handle (always DeleteComplete)
          ]
          [ text ("Clear completed (" ++ show tasksCompleted ++ ")") ]
      ]

visibilitySwap : String -> String -> String -> Html
visibilitySwap uri visibility actualVisibility =
    let className = if visibility == actualVisibility then "selected" else "" in
    li
      [ onclick updates.handle (always (ChangeVisibility visibility)) ]
      [ a [ class className, href uri ] [ text visibility ] ]

infoFooter : Html
infoFooter =
    footer [ id "info" ]
      [ p [] [ text "Double-click to edit a todo" ]
      , p [] [ text "Written by "
             , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
             ]
      , p [] [ text "Part of "
             , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
             ]
      ]


---- INPUTS ----

-- wire the entire application together
main : Signal Element
main =
    lift2 scene state Window.dimensions

-- manage the state of our application over time
state : Signal State
state =
    foldp step emptyState updates.signal

-- updates from user input
updates : Input.Input Update
updates =
    Input.input NoOp

scene : State -> (Int,Int) -> Element
scene state (w,h) =
    container w h midTop (toElement 550 h (view state))


port focus : Signal String
port focus =
    let needsFocus act =
            case act of
              Edit id (Task.Focus) -> True
              _ -> False

        toSelector (Edit id _) = ("#todo-" ++ show id)
    in
        toSelector <~ keepIf needsFocus (Edit 0 Task.Focus) updates.signal

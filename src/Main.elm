import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Todo =
  { title : String
  , english : String
  , description : String
  }

type alias Model =
  { todos : List Todo
  , newTodo : Todo
  }

init : Model
init = 
  { todos = []
  , newTodo = ( Todo "" "" "" ) }

type Msg 
  = Add 
  | InputTitle String
  | InputEnglish String
  | InputDescription String


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add ->
      { model | todos = model.newTodo :: model.todos }
    InputTitle newTitle ->
      { model | newTodo = updateNewTodo msg model.newTodo }
    InputEnglish newEnglish ->
      { model | newTodo = updateNewTodo msg model.newTodo }
    InputDescription newDescription ->
      { model | newTodo = updateNewTodo msg model.newTodo }

updateNewTodo : Msg -> Todo -> Todo
updateNewTodo msg newTodo =
  case msg of
    Add ->
      newTodo
    InputTitle newTitle ->
      { newTodo | title = newTitle }
    InputEnglish newEnglish ->
      { newTodo | english = newEnglish }
    InputDescription newDescription ->
      { newTodo | description = newDescription }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewTable model
    , button [onClick Add ] [ text "Add" ]
    ]

viewTable : Model -> Html Msg
viewTable model =
  table []
    [ thead []
      [ th [] [ text "Title" ]
      , th [] [ text "English"]
      , th [] [ text "Descriotion" ]
      ]
    , tbody [] ( ( inputTr model.newTodo ) :: ( List.map viewTr model.todos ) )
    ]

viewTr : Todo -> Html Msg
viewTr todo =
  tr []
    [ td [] [ text todo.title ]
    , td [] [ text todo.english ]
    , td [] [ text todo.description ]
    ]

inputTr : Todo -> Html Msg
inputTr newTodo =
  tr []
    [ inputTextTd "input title" InputTitle newTodo.title
    , inputTextTd "input english" InputEnglish newTodo.english
    , inputTextTd "input description" InputDescription newTodo.description 
    ]

inputTextTd : String -> (String -> Msg) -> String -> Html Msg
inputTextTd p toMsg v =
  td [] [ input [ type_ "text"
  , placeholder p 
  , onInput toMsg
  , value v ] [] 
  ]
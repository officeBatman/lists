module Main exposing (..)

import Browser exposing (Document)
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, Attribute, div, span, input, button, text, ul, li, toUnstyled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)

import Json.Decode as Decode
import Json.Encode as Encode
import Storage

-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


-- MODEL


type alias Model =
    { nodes: Nodes
    , nextId: Id
    --, currentlyEditing
    }
  
    
type alias Nodes = Dict Id NodeInfo


type alias NodeInfo =
    { id: Id
    , owner: Id
    , text: String
    , crossedOut: Bool
    , children: List Id
    }
    
      
type alias Id = Int


rootId : Id
rootId = 0


type Node =
    Node
    { id: Int
    , text: String
    , crossedOut: Bool
    , children: List Node
    }
    

encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "nodes", encodeNodes model.nodes )
        , ( "nextId", Encode.int model.nextId )
        ]


encodeNodes : Nodes -> Encode.Value
encodeNodes =
    Encode.dict String.fromInt encodeNodeInfo


modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map4 Model
        (Decode.field "nodes" (Decode.dict nodeInfoDecoder))
        (Decode.field "nextId" Decode.int)


init : Decode.Value -> (Model, Cmd Msg)
init Decode.Value =
    case Decode.decodeValue modelDecoder value of
        Ok model ->
            ( model, Cmd.none )

        Err err ->
            ( initModel, Cmd.none )


-- UPDATE


type Msg
    = NewNode { owner: Id }
    | CrossNode Id
    | RemoveNode Id
    | TextChange Id String
    | Save


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let noMessage : Model -> (Model, Cmd Msg)
        noMessage x = (x, Cmd.none)
    in

    case msg of
        NewNode { owner } ->
            { model
            | nodes =
                model.nodes
                |> Dict.insert model.nextId
                    { id = model.nextId
                    , owner = owner
                    , text = ""
                    , crossedOut = False
                    , children = []
                    }
            , nextId = model.nextId + 1
            }
            |> updateOwner owner model.nextId
            |> noMessage

        CrossNode id ->
            model
            |> updateNodes (Dict.update id (Maybe.map crossNode))
            |> noMessage

        RemoveNode id ->
            model
            |> updateNodes (removeNode id)
            |> noMessage

        TextChange id text ->
            model |> updateNodes (Dict.update id (Maybe.map (\node ->
                { node | text = text }
            )))
            |> noMessage

        Save ->
            ( model
            , Storage.set
                { key = "nodes"
                , value = encodeNodes model.nodes
                }
            )


encodeNodes : Nodes -> Encode.Value
encodeNodes =
    Encode.dict String.fromInt encodeNodeInfo


encodeNodeInfo : NodeInfo -> Encode.Value
encodeNodeInfo node =
    Encode.object
        [ ( "id", Encode.int node.id )
        , ( "owner", Encode.int node.owner )
        , ( "text", Encode.string node.text )
        , ( "crossedOut", Encode.bool node.crossedOut )
        , ( "children", Encode.list Encode.int node.children )
        ]


crossNode : NodeInfo -> NodeInfo
crossNode node =
    { node | crossedOut = not node.crossedOut }
           
         
removeNode : Id -> Nodes -> Nodes
removeNode id nodes =
    case Dict.get id nodes of
        Nothing -> nodes
         
        Just nodeInfo ->
            let removeChildren : Nodes -> Nodes
                removeChildren = 
                    List.map removeNode nodeInfo.children
                    |> List.foldr (>>) (\x -> x)
            in
            nodes
            |> Dict.remove id
            |> Dict.update nodeInfo.owner (Maybe.map (\owner ->
                { owner | children = owner.children |> List.filter (\x -> x /= id) }
            ))
            
      
updateNodes : (Dict Id NodeInfo -> Dict Id NodeInfo) -> Model -> Model
updateNodes f model =
    { model | nodes = f model.nodes }
          
                
updateOwner : Id -> Id -> Model -> Model
updateOwner owner id model =
    { model
    | nodes =
        model.nodes
        |> Dict.update owner (Maybe.map (\node ->
            { node | children = node.children ++ [id] }
        ))
    }



-- VIEW


makeNode : Dict Id NodeInfo -> Id -> Maybe Node
makeNode dict id =
    Dict.get id dict |> Maybe.map (\nodeInfo ->
        Node
            { id = id
            , text = nodeInfo.text
            , crossedOut = nodeInfo.crossedOut
            , children = List.filterMap (makeNode dict) nodeInfo.children
            }
    )


view : Model -> Document Msg
view model =
    makeNode model.nodes rootId
    |> Maybe.map (viewNode True)
    |> Maybe.withDefault (text "Error")
    |> \viewRootNode ->
        div
         [ css
             [ displayFlex
             , flexDirection column
             , alignItems center
             , justifyContent center
             , padding (px 10)
             ]
         ]
         [ viewRootNode
         , viewSaveButton
         ]
    |> toUnstyled
    |> \body -> Document "List" [ body ]


viewNodes : List Node -> Html Msg
viewNodes list =
    ul
        [ css
        [ margin (px 0)
        ]
        ]
        (List.map (viewNode False) list)


viewNode : Bool -> Node -> Html Msg
viewNode isRoot (Node node) =
    div
        [ css
            [ maxWidth (pct 100)
            ]
        ]
        [ span
            [ css
                [ styleFlex
                ]
            ]
            [ span
                [ css 
                    [ styleFlex
                    ]
                ]
                [ viewMarker
                , viewNodeTextInput node.text (TextChange node.id)
                ]
              |> if node.crossedOut then crossedOut else identity
            , span 
                []
                [ viewCrossoutButton node.id
                , viewAddButton node.id
                , if node.id /= rootId then viewRemoveButton node.id else text ""
                ]
            ]
        , div
            [ css
                [ paddingLeft (px 10)
                ]
            ]
            [ viewNodes node.children
            ]
        ]


viewMarker : Html Msg
viewMarker =
    span
        [ css
            [ Css.width (px 5)
            , Css.height (px 5)
            , borderRadius (px 10)
            , backgroundColor (rgb 0 0 0)
            , marginRight (px 10)
            ]
        ]
        []


styleFlex : Style
styleFlex =
    batch
        [ displayFlex
        , alignItems center
        , justifyContent center
        ]


viewNodeTextInput : String -> (String -> msg) -> Html msg
viewNodeTextInput text msg =
    input
        [ value text
        , placeholder "New task..."
        , onInput msg
        , css
            [ border (px 0)
            , marginTop (px 2)
            , marginBottom (px 2)
            , flexGrow (int 2)
            ]
        ]
        []


viewAddButton : Id -> Html Msg
viewAddButton owner =
    viewNodeActionButton (NewNode { owner = owner }) [ text "+" ]


viewRemoveButton : Id -> Html Msg
viewRemoveButton id =
    viewNodeActionButton (RemoveNode id) [ text "x" ]


viewCrossoutButton : Id -> Html Msg
viewCrossoutButton id =
    viewNodeActionButton (CrossNode id) [ text "-" ]


viewNodeActionButton : msg -> List (Html msg) -> Html msg
viewNodeActionButton msg inner =
    button
        [ onClick msg
        , css
            [ border (px 0)
            , backgroundColor transparent
            , cursor pointer
            ]
        ]
        inner


crossedOut : Html msg -> Html msg
crossedOut el =
    let height = 2 in
    div
        [ css
            [ position relative
            ]
        ]
        [ el
        , div
            [ css
                [ position absolute
                , top (calc (pct 50) minus (px (height / 2)))
                , Css.width (pct 100)
                , Css.height (px height)
                , backgroundColor (rgb 0 0 0)
                ]
            ]
            []
        ]


viewSaveButton : Html Msg
viewSaveButton =
    button
        [ onClick Save
        , css
            [ border (px 0)
            , backgroundColor transparent
            , cursor pointer
            ]
        ]
        [ text "Save"
        ]


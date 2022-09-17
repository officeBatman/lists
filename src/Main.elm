module Main exposing (..)

import Browser exposing (Document)
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Html, button, div, input, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Json.Encode as Encode
import Storage



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { nodes : Nodes
    , nextId : Id
    , uiSize : Float

    --, currentlyEditing
    }


type alias Nodes =
    Dict Id NodeInfo


type alias NodeInfo =
    { id : Id
    , owner : Id
    , text : String
    , crossedOut : Bool
    , children : List Id
    }


type alias Id =
    Int


rootId : Id
rootId =
    0


nextId : Id -> Id
nextId id =
    id + 1


type Node
    = Node
        { id : Int
        , text : String
        , crossedOut : Bool
        , children : List Node
        }


emptyModel : Model
emptyModel =
    { nodes =
        Dict.empty
            |> Dict.insert rootId
                { id = rootId
                , owner = rootId
                , text = "My List"
                , crossedOut = False
                , children = []
                }
    , nextId = rootId |> nextId
    , uiSize = 5
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


encodeNodeInfo : NodeInfo -> Encode.Value
encodeNodeInfo node =
    Encode.object
        [ ( "id", Encode.int node.id )
        , ( "owner", Encode.int node.owner )
        , ( "text", Encode.string node.text )
        , ( "crossedOut", Encode.bool node.crossedOut )
        , ( "children", Encode.list Encode.int node.children )
        ]


modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map3 Model
        (Decode.field "nodes" (DecodeExtra.dict2 Decode.int nodeInfoDecoder))
        (Decode.field "nextId" Decode.int)
        (Decode.field "uiSize" (Decode.oneOf [ Decode.float, Decode.succeed 1 ]))


nodeInfoDecoder : Decode.Decoder NodeInfo
nodeInfoDecoder =
    Decode.map5 NodeInfo
        (Decode.field "id" Decode.int)
        (Decode.field "owner" Decode.int)
        (Decode.field "text" Decode.string)
        (Decode.field "crossedOut" Decode.bool)
        (Decode.field "children" (Decode.list Decode.int))


init : Decode.Value -> ( Model, Cmd Msg )
init value =
    case Decode.decodeValue modelDecoder value of
        Ok model ->
            ( model, Cmd.none )

        Err err ->
            ( emptyModel, Cmd.none )



-- UPDATE


type Msg
    = NewNode { owner : Id }
    | CrossNode Id
    | RemoveNode Id
    | TextChange Id String
    | Save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noMessage : Model -> ( Model, Cmd Msg )
        noMessage x =
            ( x, Cmd.none )
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
            model
                |> updateNodes
                    (Dict.update id
                        (Maybe.map
                            (\node ->
                                { node | text = text }
                            )
                        )
                    )
                |> noMessage

        Save ->
            ( model
            , Storage.set (encodeModel model)
            )


crossNode : NodeInfo -> NodeInfo
crossNode node =
    { node | crossedOut = not node.crossedOut }


removeNode : Id -> Nodes -> Nodes
removeNode id nodes =
    case Dict.get id nodes of
        Nothing ->
            nodes

        Just nodeInfo ->
            let
                removeChildren : Nodes -> Nodes
                removeChildren =
                    List.map removeNode nodeInfo.children
                        |> List.foldr (>>) (\x -> x)
            in
            nodes
                |> Dict.remove id
                |> removeChildren
                |> Dict.update nodeInfo.owner
                    (Maybe.map
                        (\owner ->
                            { owner | children = owner.children |> List.filter (\x -> x /= id) }
                        )
                    )


updateNodes : (Dict Id NodeInfo -> Dict Id NodeInfo) -> Model -> Model
updateNodes f model =
    { model | nodes = f model.nodes }


updateOwner : Id -> Id -> Model -> Model
updateOwner owner id model =
    { model
        | nodes =
            model.nodes
                |> Dict.update owner
                    (Maybe.map
                        (\node ->
                            { node | children = node.children ++ [ id ] }
                        )
                    )
    }



-- VIEW


makeNode : Dict Id NodeInfo -> Id -> Maybe Node
makeNode dict id =
    Dict.get id dict
        |> Maybe.map
            (\nodeInfo ->
                Node
                    { id = id
                    , text = nodeInfo.text
                    , crossedOut = nodeInfo.crossedOut
                    , children = List.filterMap (makeNode dict) nodeInfo.children
                    }
            )


size : Model -> Float -> Px
size model x =
    px (model.uiSize * x)


view : Model -> Document Msg
view model =
    let
        name =
            model.nodes
            |> Dict.get rootId
            |> Maybe.map .text
            |> Maybe.withDefault "My List"
    in
    makeNode model.nodes rootId
        |> Maybe.map (viewNode model True)
        |> Maybe.withDefault (text "Error")
        |> (\viewRootNode ->
                div
                    [ css
                        [ displayFlex
                        , flexDirection column
                        , alignItems center
                        , justifyContent center
                        , padding (size model 10)
                        ]
                    ]
                    [ viewRootNode
                    , viewSaveButton
                    ]
                    |> toUnstyled
                    |> (\body -> Document name [ body ])
           )


viewNodes : Model -> List Node -> Html Msg
viewNodes model list =
    ul
        [ css
            [ margin (px 0)
            ]
        ]
        (List.map (viewNode model False) list)


viewNode : Model -> Bool -> Node -> Html Msg
viewNode model isRoot (Node node) =
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
                [ viewMarker model
                , viewNodeTextInput model node.text (TextChange node.id)
                ]
                |> (if node.crossedOut then
                        crossedOut model

                    else
                        identity
                   )
            , span
                []
                [ viewCrossoutButton node.id
                , viewAddButton node.id
                , if node.id /= rootId then
                    viewRemoveButton node.id

                  else
                    text ""
                ]
            ]
        , div
            [ css
                [ paddingLeft (size model 10)
                ]
            ]
            [ viewNodes model node.children
            ]
        ]


viewMarker : Model -> Html Msg
viewMarker model =
    span
        [ css
            [ Css.width (size model 5)
            , Css.height (size model 5)
            , borderRadius (size model 10)
            , backgroundColor (rgb 0 0 0)
            , marginRight (size model 10)
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


viewNodeTextInput : Model -> String -> (String -> msg) -> Html msg
viewNodeTextInput model text msg =
    input
        [ value text
        , placeholder "New task..."
        , onInput msg
        , css
            [ border (px 0)
            , marginTop (size model 2)
            , marginBottom (size model 2)
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


crossedOut : Model -> Html msg -> Html msg
crossedOut model el =
    let
        height =
            2
    in
    div
        [ css
            [ position relative
            ]
        ]
        [ el
        , div
            [ css
                [ position absolute
                , top (calc (pct 50) minus (size model (height / 2)))
                , Css.width (pct 100)
                , Css.height (size model height)
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

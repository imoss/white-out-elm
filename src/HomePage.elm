module HomePage exposing (main)

import Browser
import Draggable
import Draggable.Events exposing (onDragBy, onDragStart)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events exposing (onMouseUp)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Box =
    { id : Id
    , position : Vec2
    , height : Float
    , width : Float
    }


type alias Id =
    { uid : String
    , corner : Corner
    }


type Corner
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | None


makeBox : Id -> Vec2 -> Box
makeBox id position =
    Box id position 50.0 100.0


dragBoxBy : Vec2 -> Corner -> Box -> Box
dragBoxBy delta corner box =
    let
        record =
            Vector2.toRecord delta
    in
    case corner of
        BottomRight ->
            let
                newHeight =
                    if box.height + record.y <= 1 then
                        1

                    else
                        box.height + record.y

                newWidth =
                    if box.width + record.x <= 1 then
                        1

                    else
                        box.width + record.x
            in
            { box
                | height = newHeight
                , width = newWidth
            }

        _ ->
            { box | position = box.position |> Vector2.add delta }


type alias BoxGroup =
    { movingBox : Maybe Box
    , idleBoxes : List Box
    }


emptyGroup : BoxGroup
emptyGroup =
    BoxGroup Nothing []


addBox : Vec2 -> BoxGroup -> BoxGroup
addBox position ({ idleBoxes } as group) =
    let
        count =
            List.length idleBoxes + 1

        id =
            { uid = String.fromInt count
            , corner = None
            }
    in
    { group | idleBoxes = makeBox id position :: idleBoxes }


makeBoxGroup : List Vec2 -> BoxGroup
makeBoxGroup positions =
    positions
        |> List.foldl addBox emptyGroup


allBoxes : BoxGroup -> List Box
allBoxes { movingBox, idleBoxes } =
    movingBox
        |> Maybe.map (\a -> a :: idleBoxes)
        |> Maybe.withDefault idleBoxes


startDragging : Id -> BoxGroup -> BoxGroup
startDragging id ({ idleBoxes, movingBox } as group) =
    let
        ( targetAsList, others ) =
            List.partition (\box -> id.uid == box.id.uid) idleBoxes

        _ =
            Debug.log "targetAsList" targetAsList

        _ =
            Debug.log "others" others
    in
    { group
        | idleBoxes = others
        , movingBox = targetAsList |> List.head
    }


stopDragging : BoxGroup -> BoxGroup
stopDragging group =
    { group
        | idleBoxes = allBoxes group
        , movingBox = Nothing
    }


dragActiveBy : Vec2 -> Corner -> BoxGroup -> BoxGroup
dragActiveBy delta corner group =
    { group | movingBox = group.movingBox |> Maybe.map (dragBoxBy delta corner) }


type alias Model =
    { boxGroup : BoxGroup
    , drag : Draggable.State Id
    , currentCorner : Corner
    }


type Msg
    = DragMsg (Draggable.Msg Id)
    | OnDragBy Vec2
    | StartDragging Id
    | StopDragging
    | AddBox


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { boxGroup = makeBoxGroup []
      , drag = Draggable.init
      , currentCorner = None
      }
    , Cmd.none
    )


dragConfig : Draggable.Config Id Msg
dragConfig =
    Draggable.customConfig
        [ onDragBy (\( dx, dy ) -> Vector2.vec2 dx dy |> OnDragBy)
        , onDragStart StartDragging
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxGroup } as model) =
    case msg of
        OnDragBy delta ->
            let
                corner =
                    model.currentCorner
            in
            ( { model | boxGroup = boxGroup |> dragActiveBy delta corner }, Cmd.none )

        StartDragging id ->
            ( { model | currentCorner = id.corner, boxGroup = boxGroup |> startDragging id }, Cmd.none )

        StopDragging ->
            ( { model | boxGroup = boxGroup |> stopDragging }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        AddBox ->
            ( { model | boxGroup = boxGroup |> addBox (Vector2.vec2 100 100) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag



-- VIEW


boxSize : Vec2
boxSize =
    Vector2.vec2 100 50


view : Model -> Html Msg
view { boxGroup } =
    Html.div
        []
        [ Html.div [] [ Html.button [ onClick AddBox ] [ Html.text "Add Box" ] ]
        , Svg.svg
            [ Attr.style "top: 20px; height: 100vh; width: 100vw; position: absolute;"
            ]
            [ boxesView boxGroup
            ]
        , Html.img [ Html.Attributes.src "img/dan-da-dan.jpg" ] []
        ]


boxesView : BoxGroup -> Svg Msg
boxesView boxGroup =
    boxGroup
        |> allBoxes
        |> List.reverse
        |> List.concatMap boxView
        |> Svg.node "g" []


boxView : Box -> List (Svg Msg)
boxView { id, position, height, width } =
    let
        uid =
            id.uid
    in
    [ Svg.rect
        [ num Attr.width <| width
        , num Attr.height <| height
        , num Attr.x (getX position)
        , num Attr.y (getY position)
        , Attr.fill "white"
        , Attr.stroke "white"
        , Attr.cursor "move"
        , Draggable.mouseTrigger { uid = uid, corner = None } DragMsg
        , onMouseUp StopDragging
        ]
        []
    , Svg.circle
        [ Attr.cx (String.fromFloat (getX position + width))
        , Attr.cy (String.fromFloat (getY position + height))
        , Attr.r "5"
        , Attr.fill "red"
        , Draggable.mouseTrigger { uid = uid, corner = BottomRight } DragMsg
        , onMouseUp StopDragging
        ]
        []
    ]


num : (String -> Svg.Attribute msg) -> Float -> Svg.Attribute msg
num attr value =
    attr (String.fromFloat value)

-- Modified version of https://github.com/zaboco/elm-draggable/blob/master/examples/MultipleTargetsExample.elm


module HomePage exposing (main)

import Browser
import Draggable
import Draggable.Events exposing (onDragBy, onDragStart)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onMouseUp)
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events



-- TYPES


type Corner
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | None


type alias Box =
    { details : DragDetails
    , position : Vec2
    , height : Float
    , width : Float
    }


type alias BoxGroup =
    { movingBox : Maybe Box
    , idleBoxes : List Box
    }


type alias DragDetails =
    { uid : Uid
    , corner : Corner
    }


type alias Model =
    { boxGroup : BoxGroup
    , drag : Draggable.State DragDetails
    , currentCorner : Corner
    , hoverBoxId : Maybe Uid
    , savedOutput : List (Html Msg)
    }


type alias Uid =
    String



-- INIT


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { boxGroup = makeBoxGroup []
      , drag = Draggable.init
      , currentCorner = None
      , hoverBoxId = Nothing
      , savedOutput = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DragMsg (Draggable.Msg DragDetails)
    | OnDragBy Vec2
    | StartDragging DragDetails
    | StopDragging
    | AddBox
    | DisplayCorners Uid
    | HideCorners
    | Save
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boxGroup } as model) =
    case msg of
        OnDragBy delta ->
            ( { model | boxGroup = boxGroup |> dragActiveBy delta model.currentCorner }, Cmd.none )

        StartDragging details ->
            ( { model | currentCorner = details.corner, boxGroup = boxGroup |> startDragging details }, Cmd.none )

        StopDragging ->
            ( { model | boxGroup = boxGroup |> stopDragging }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        AddBox ->
            ( { model | boxGroup = boxGroup |> addBox (Vector2.vec2 defaultStartX defaultStartY) }, Cmd.none )

        DisplayCorners uid ->
            ( { model | hoverBoxId = Just uid }, Cmd.none )

        HideCorners ->
            ( { model | hoverBoxId = Nothing }, Cmd.none )

        Save ->
            ( { model | savedOutput = allBoxes boxGroup |> buildOutput }, Cmd.none )

        Clear ->
            ( { model | boxGroup = makeBoxGroup [] }, Cmd.none )


makeBox : DragDetails -> Vec2 -> Box
makeBox id position =
    Box id position defaultStartHeight defaultStartWidth


dragBoxBy : Vec2 -> Corner -> Box -> Box
dragBoxBy delta corner box =
    let
        record =
            Vector2.toRecord delta

        existingPosRecord =
            Vector2.toRecord box.position
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

        TopRight ->
            let
                newHeight =
                    if box.height - record.y <= 1 then
                        1

                    else
                        box.height - record.y

                newWidth =
                    if box.width + record.x <= 1 then
                        1

                    else
                        box.width + record.x

                newYPos =
                    if box.height == 1 then
                        existingPosRecord.y

                    else
                        existingPosRecord.y + record.y
            in
            { box
                | height = newHeight
                , width = newWidth
                , position = Vector2.vec2 existingPosRecord.x newYPos
            }

        TopLeft ->
            let
                newHeight =
                    if box.height - record.y <= 1 then
                        1

                    else
                        box.height - record.y

                newWidth =
                    if box.width - record.x <= 1 then
                        1

                    else
                        box.width - record.x

                newXPos =
                    if box.width == 1 then
                        existingPosRecord.x

                    else
                        existingPosRecord.x + record.x

                newYPos =
                    if box.height == 1 then
                        existingPosRecord.y

                    else
                        existingPosRecord.y + record.y
            in
            { box
                | height = newHeight
                , width = newWidth
                , position = Vector2.vec2 newXPos newYPos
            }

        BottomLeft ->
            let
                newHeight =
                    if box.height + record.y <= 1 then
                        1

                    else
                        box.height + record.y

                newWidth =
                    if box.width - record.x <= 1 then
                        1

                    else
                        box.width - record.x

                newXPos =
                    if box.width == 1 then
                        existingPosRecord.x

                    else
                        existingPosRecord.x + record.x

                newYPos =
                    existingPosRecord.y
            in
            { box
                | height = newHeight
                , width = newWidth
                , position = Vector2.vec2 newXPos newYPos
            }

        None ->
            { box | position = box.position |> Vector2.add delta }



-- VIEW


view : Model -> Html Msg
view { boxGroup, hoverBoxId, savedOutput } =
    let
        height =
            500

        width =
            500
    in
    Html.div
        []
        [ Html.div [ HA.height 30 ]
            [ Html.button [ onClick AddBox ] [ Html.text "Add Redaction" ]
            , Html.button [ onClick Save ] [ Html.text "Save" ]
            , Html.button [ onClick Clear ] [ Html.text "Clear" ]
            ]
        , Html.div
            [ HA.class "svg-container"
            , onMouseUp StopDragging
            ]
            [ Svg.svg
                [ Attr.style "position: absolute;"
                , Attr.height (String.fromInt height)
                , Attr.width (String.fromInt width)
                ]
                [ boxesView boxGroup hoverBoxId ]
            , Html.img [ HA.src "img/cats.png", HA.width width, HA.height height ] []
            ]
        , Html.div
            [ HA.class "output" ]
            savedOutput
        ]


boxesView : BoxGroup -> Maybe Uid -> Svg Msg
boxesView boxGroup maybeUid =
    boxGroup
        |> allBoxes
        |> List.reverse
        |> List.concatMap (boxView maybeUid)
        |> Svg.node "g" []


allBoxes : BoxGroup -> List Box
allBoxes { movingBox, idleBoxes } =
    movingBox
        |> Maybe.map (\a -> a :: idleBoxes)
        |> Maybe.withDefault idleBoxes


boxView : Maybe Uid -> Box -> List (Svg Msg)
boxView maybeHoverUid { details, position, height, width } =
    let
        cornerVisibility =
            case maybeHoverUid of
                Just hoverUid ->
                    if details.uid == hoverUid then
                        "visible"

                    else
                        "hidden"

                Nothing ->
                    "hidden"

        cornerRadius =
            7

        cornerCommonAttrs =
            [ Attr.r (String.fromInt cornerRadius)
            , Attr.fill "rgb(170, 170, 170)"
            , Attr.stroke "rgb(170, 170, 170)"
            , Attr.strokeWidth "2"
            , Attr.fillOpacity ".5"
            , Attr.visibility cornerVisibility
            , Html.Events.onMouseLeave HideCorners
            , Html.Events.onMouseEnter (DisplayCorners details.uid)
            ]
    in
    [ Svg.rect
        [ num Attr.width <| width
        , num Attr.height <| height
        , num Attr.x (getX position)
        , num Attr.y (getY position)
        , Attr.fill "white"
        , Attr.stroke "white"
        , Attr.cursor "move"
        , Draggable.mouseTrigger { uid = details.uid, corner = None } DragMsg
        , Html.Events.onMouseLeave HideCorners
        , Html.Events.onMouseEnter (DisplayCorners details.uid)
        ]
        []
    , Svg.circle
        ([ Attr.cx (String.fromFloat (getX position + width))
         , Attr.cy (String.fromFloat (getY position + height))
         , Draggable.mouseTrigger { uid = details.uid, corner = BottomRight } DragMsg
         ]
            ++ cornerCommonAttrs
        )
        []
    , Svg.circle
        ([ Attr.cx (String.fromFloat (getX position + width))
         , Attr.cy (String.fromFloat (getY position))
         , Draggable.mouseTrigger { uid = details.uid, corner = TopRight } DragMsg
         ]
            ++ cornerCommonAttrs
        )
        []
    , Svg.circle
        ([ Attr.cx (String.fromFloat (getX position))
         , Attr.cy (String.fromFloat (getY position))
         , Draggable.mouseTrigger { uid = details.uid, corner = TopLeft } DragMsg
         ]
            ++ cornerCommonAttrs
        )
        []
    , Svg.circle
        ([ Attr.cx (String.fromFloat (getX position))
         , Attr.cy (String.fromFloat (getY position + height))
         , Draggable.mouseTrigger { uid = details.uid, corner = BottomLeft } DragMsg
         ]
            ++ cornerCommonAttrs
        )
        []
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag



-- HELPERS


addBox : Vec2 -> BoxGroup -> BoxGroup
addBox position ({ idleBoxes } as group) =
    let
        id =
            { uid = String.fromInt (List.length idleBoxes + 1)
            , corner = None
            }
    in
    { group | idleBoxes = makeBox id position :: idleBoxes }


buildOutput : List Box -> List (Html Msg)
buildOutput boxes =
    boxes
        |> List.map
            (\{ details, position, height, width } ->
                Html.div
                    []
                    [ Html.text
                        (String.concat
                            [ "Redaction at ("
                            , String.fromFloat (getX position)
                            , ", "
                            , String.fromFloat (getY position)
                            , ") with height "
                            , String.fromFloat height
                            , " and width "
                            , String.fromFloat width
                            ]
                        )
                    ]
            )


dragActiveBy : Vec2 -> Corner -> BoxGroup -> BoxGroup
dragActiveBy delta corner group =
    { group | movingBox = group.movingBox |> Maybe.map (dragBoxBy delta corner) }


dragConfig : Draggable.Config DragDetails Msg
dragConfig =
    Draggable.customConfig
        [ onDragBy (\( dx, dy ) -> Vector2.vec2 dx dy |> OnDragBy)
        , onDragStart StartDragging
        ]


defaultStartX : Float
defaultStartX =
    100


defaultStartY : Float
defaultStartY =
    100


defaultStartHeight : Float
defaultStartHeight =
    50


defaultStartWidth : Float
defaultStartWidth =
    100


emptyGroup : BoxGroup
emptyGroup =
    BoxGroup Nothing []


makeBoxGroup : List Vec2 -> BoxGroup
makeBoxGroup positions =
    positions
        |> List.foldl addBox emptyGroup


num : (String -> Svg.Attribute msg) -> Float -> Svg.Attribute msg
num attr value =
    attr (String.fromFloat value)


startDragging : DragDetails -> BoxGroup -> BoxGroup
startDragging details ({ idleBoxes, movingBox } as group) =
    let
        ( targetAsList, others ) =
            List.partition (\box -> details.uid == box.details.uid) idleBoxes
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

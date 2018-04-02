module ElmApp exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Random exposing (..)
import Keyboard exposing (downs)


type alias Ball = {colorBall : String, position : {x : Int, y : Int}, radius : Int, vel : Int}

type alias Balls = List Ball


--Model

type alias Model = {selfPosition : {x : Int, y : Int}, balls : Balls, h: Int, w : Int, level : Int, counter : Int, continue : Bool }

--Msg

type Msg = Restart | Tick Float | AddBall | KeyMsg Int | OnAdd Int 



randomInt : Int -> Int -> Cmd Msg
randomInt x y = Random.generate OnAdd (Random.int x y)

--List of balls which will spawn balls based on the level

addBall : Model -> Int -> Ball
addBall model val       = case (model.level % 3) of
                                0 -> {colorBall = "Purple", position = {x = 0 , y = val}, radius = 20, vel = 10}
                                1 -> {colorBall = "Green", position = {x = 0, y = val}, radius = 30, vel = 7}
                                2 -> {colorBall = "Red", position = {x = 0, y = val}, radius = 35, vel = 5}
                                _ -> {colorBall = "Purple", position = {x = 0, y = val}, radius = 10, vel = 10}

firstBall : Ball
firstBall = {colorBall = "Purple", position = {x = 0 , y = 300}, radius = 20, vel = 10} -- Need one initial ball

init : (Model, Cmd Msg)
init = ({selfPosition = {x = 500, y = 300}, balls = [firstBall], h = 20, w = 20, level = 1, counter = 0, continue = True },Cmd.none)

--Update, need to update positions with tick rates, and need to check for collisions in other functions and have cases for their messages


selfCollision : Int -> Int -> Balls -> Bool --Attempt to check every ball for a collision with the model, if there is one change model.continue to False
selfCollision x y balls       = case balls of
                                        (b::bs) -> (distance (toFloat x) (toFloat y) b) > (toFloat b.radius) && (selfCollision x y bs)
                                        [] -> True
                                    
distance : Float -> Float -> Ball -> Float
distance x y b           = Basics.sqrt((((toFloat b.position.x) - (x + 20))^2) + ((toFloat b.position.y) - (y + 20))^2)


view : Model -> Html Msg
view model      = case model.continue of
                    False -> lossMenu model
                    True -> playView model

buildCircles : Balls -> List (Svg.Svg Msg)
buildCircles balls          = case balls of
                                (b::bs) -> (Svg.circle [ cx (toString (b.position.x)), cy (toString (b.position.y)), r (toString (b.radius)), fill b.colorBall ] []) :: (buildCircles bs)
                                [] -> [] --return empty
 

playView : Model -> Html Msg
playView model          = let
                                posX = toString (model.selfPosition.x)
                                posY = toString (model.selfPosition.y)
                                selfHeight = toString (model.h)
                                selfWidth = toString (model.w)
                                ballss = buildCircles (model.balls) 
                            in 
                                div [Html.Attributes.style[("margin","0"),("padding","0"),("overflow","hidden")]]
                                    [
                                        svg [Html.Attributes.style [("background-color","#2c2d2d"), ("position","fixed"), ("top","0"), ("left","0"), ("height","500"), ("width","1000") ] ]
                                        (List.append [ Svg.rect [x posX, y posY, rx "2", ry "2", Svg.Attributes.width selfWidth, Svg.Attributes.height selfHeight, fill "dodgerBlue" ] [] ]
                                        (ballss)
                                        )
                                    ]


lossMenu : Model -> Html Msg
lossMenu model          = div [Html.Attributes.style [("font-size", "50px"),("text-align","center") ] ]
                            [ Html.button [ onClick Restart ] [Html.text "Restart"] ]


update : Msg -> Model -> (Model,Cmd Msg)
update msg model     =      if model.continue == True
                                then
                                    case msg of
                                        (Tick time) -> ballUpdate time model
                                        (KeyMsg val) -> keyUpdate val model
                                        (AddBall) -> (model, randomInt 0 100) 
                                        (OnAdd x) -> ({ model | balls = model.balls ++ [(addBall model x)] }, Cmd.none)
                                        (Restart) -> init
                                else
                                    case msg of
                                        (Restart) -> init
                                        _         -> (model,Cmd.none)



keyUpdate : Int -> Model -> (Model,Cmd Msg)
keyUpdate keyCode model = if model.continue == False
                                then
                                    let
                                        posX = model.selfPosition.x
                                        posY = model.selfPosition.y
                                        modelN = { model | selfPosition = {x = posX, y = posY } }
                                    in (modelN, Cmd.none)
                            else
                                    case keyCode of
                                        65 -> ({ model | selfPosition = {x = (model.selfPosition.x-15) % 1000, y = (model.selfPosition.y) % 500} },Cmd.none)
                                        68 -> ({ model | selfPosition = {x = (model.selfPosition.x+15) % 1000, y = (model.selfPosition.y) % 500} },Cmd.none)
                                        87 -> ({ model | selfPosition = {x = (model.selfPosition.x) % 1000, y = (model.selfPosition.y-15) % 500} },Cmd.none)
                                        83 -> ({ model | selfPosition = {x = (model.selfPosition.x) % 1000, y = (model.selfPosition.y+15) % 500} },Cmd.none)
                                        _  -> ({ model | selfPosition = {x = (model.selfPosition.x) % 1000, y = (model.selfPosition.y) % 500} },Cmd.none)
                                    

ballUpdate : Float -> Model -> (Model,Cmd Msg)
ballUpdate time model       =   let
                                    collision = selfCollision (model.selfPosition.x) (model.selfPosition.y) (model.balls)
                                    newBalls = List.map (ballMovement time) (model.balls)
                                    newLevel = round <| (toFloat (model.counter) / 300 )
                                    newCount = model.counter + 1
                                    modelN = { model | balls = newBalls, continue = collision, counter = newCount, level = newLevel }
                                in if newLevel > model.level
                                        then 
                                            (modelN,Random.generate OnAdd (Random.int 0 500))
                                    else
                                        (modelN,Cmd.none)


ballMovement : Float -> Ball -> Ball
ballMovement time ball       =  let
                                    posX = (ball.position.x + ball.vel) % 1000
                                    posY = round <| toFloat (ball.position.y) + ( sin (time/1000) )
                                    ballN = {ball | position = {x = posX, y = posY } }
                                in (ballN)

subscriptions : Model -> Sub Msg
subscriptions model         = Sub.batch [
                                            Keyboard.downs KeyMsg,
                                            Anim.times Tick
                                        ]
main : Program Never Model Msg
main = Html.program {
                        init = init,
                        view = view,
                        update = update,
                        subscriptions = subscriptions
                    }
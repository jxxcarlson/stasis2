module Msg exposing (BackendMsg(..), FrontendMsg(..), ToBackend(..), ToFrontend(..))

import CellGrid.Render
import World


type FrontendMsg
    = Increment
    | Decrement
    | FNoop
    | Reset
    | ChangeTheWorld
    | StageResource World.Resource
    | CellGrid CellGrid.Render.Msg
    | ChooseCity
    | ChooseCrop
    | ChooseNature
    | ChooseUnoccupied
    | NewRandomFloat Float
    | NewRandomFloat2 Float


type ToBackend
    = ClientJoin
    | CounterIncremented
    | CounterDecremented


type BackendMsg
    = Noop


type ToFrontend
    = CounterNewValue Int

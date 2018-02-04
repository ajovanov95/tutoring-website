module Model exposing(..)

import Window

type Page = PageProgramming | 
            PageAlgorithms  |
            PageMathematics

type alias Model = {
    page: Page,
    
    windowSize: Window.Size,
    screenIsSmartphone: Bool,
    screenIsPortrait: Bool 
}

initialModel : Model
initialModel = {
        page = PageProgramming,
        windowSize = {width = 1366, height = 768},
        screenIsSmartphone = False,
        screenIsPortrait = False
    }

type Msg = ProgrammingClicked  |
            AlgorithmsClicked  | 
            MathematicsClicked |
            WindowResized Window.Size

module Model exposing(..)

type Page = PageProgramming | 
            PageAlgorithms  |
            PageMathematics

type alias Model = {
    page: Page
}

type Msg = ProgrammingClicked |
            AlgorithmsClicked | 
            MathematicsClicked

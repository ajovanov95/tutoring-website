module Pages.Programming exposing (programmingPage)

import Html exposing (..)
import Html.Attributes exposing (..)

-- import Flex
import Grid

import Model exposing (..)
import Styles exposing (..)


-- Reusable components to build pages with
newline : Html msg
newline = br [] []

rule : Html msg
rule = hr [] []

card : String -> Html Msg
card content = Grid.item [cardStyle] [text content]

cardLink : String -> String -> List String -> Html Msg
cardLink content url tags = 
    Grid.item [cardStyle] 
        [text <| "[" ++ String.join ", " tags ++ "] ", a [href url] [text content]]

-- Sections
section1 : Model -> List (Html Msg)
section1 model =  
    [h2 [style [("margin", "0.2em")], class "is-size-3"] [text "Области кои ги обработуваме"],
     rule,
    Grid.container (Grid.autoSpecification model.isMobile) [] 
    <| List.map card 
        ["Структурно и објектно-ориентирано програмирање со C и Java",
        "Програмирање за анализа на податоци со R или Python",
        "Функционално програмирање со Haskell, Elm или LISP",
        "Развој на веб страни со Elm, CSS и Bootstrap",
        "Развој на видео игри за Андроид со LibGDX",
        "Linux и bash shell скрипти"]
    ]

section2 : Model -> List (Html Msg)
section2 model = [
    h2 [style [("margin", "0.5em")], class "is-size-3"] [text "Интегрирани развојни околини и едитори"],
    rule,
    Grid.container (Grid.autoSpecification model.isMobile) [] 
    <| List.map (\(c, u, t) -> cardLink c u t) 
       [
            ("Visual Studio Code", "https://code.visualstudio.com/", ["Едитор"]),
            ("CodeBlocks", "http://www.codeblocks.org/", ["C", "C++"]),
            ("InteliJ IDEA", "https://www.jetbrains.com/idea/", ["Java"]),
            ("RStudio", "https://www.rstudio.com/", ["R"]),
            ("PyCharm", "https://www.jetbrains.com/pycharm/", ["Python"])
       ]
    ]

section3 : Model -> List (Html Msg)
section3 model = 
    [
        h2 [style [("margin", "0.5em")], class "is-size-3"] [text "Околини за извршување и пакети за развој"],
        rule,
        Grid.container (Grid.autoSpecification model.isMobile) [] 
        <| List.map (\(c, u, t) -> cardLink c u t) 
           [("Java Development Kit", "http://www.oracle.com/technetwork/java/javase/downloads/index.html", ["Java"]),
           ("GNU Compiler Collection - Linux", "https://www.crybit.com/how-to-install-gcc-gnu-c-c-compiler-unixlinux/", ["C", "C++"]),
           ("GNU Compiler Collection - Windows", "http://mingw.org/", ["C", "C++"]),
           ("Anaconda", "https://www.anaconda.com/", ["Python"]),
           ("R runtime", "https://www.r-project.org/", ["R"]),
           ("Stack", "https://docs.haskellstack.org/en/stable/README/", ["Haskell"])]
    ]

section4 : Model -> List (Html Msg)
section4 model =
     [  
         h2 [style [("margin", "0.5em")], class "is-size-3"] [text "Линукс дистрибуции"],
         rule,
         p [] [text "За почетници препорачана линукс дистрибуција е ", 
               a [href "https://www.ubuntu.com/"] [text "Убунту"]
              ]
    ]

programmingPage : Model -> Html Msg
programmingPage model = 
    let
        html =  
            [section1, section2, section3, section4] |>
             List.map (\f -> f model)                |>
             List.intersperse [newline]              |>
             List.concat
    in
        div [] html
        -- Flex.container Flex.Column model.isMobile [] html (<hr /> is hidden?)
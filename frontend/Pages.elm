module Pages exposing (pageProgramming, 
                       pageAlgorithms,
                       pageMathematics)

import Model exposing (..)
import Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Grid

newline : Html msg
newline = br [] []

programmingText : String
programmingText = """
    Сите часови се по договор со можност за организирање на курс ако има голема побарувачка.
    Еден час трае по сат време. Доаѓаме кај вас дома или се наоѓаме на факултет.
    Часови за средношколци се 150 денари, додека часови за на факултет се 250 денари.
    Часови за луѓе кои се надвор од образовниот процес се 400 денари.
    Блок од два часа има 20% попуст, додека блок од 3 часа има 30% попуст.
    Ние можеме да ви инсталираме се што е неопходно но препорачуваме 
    да не се губи време и сами да ги инсталирате неопходните програми од линковите подолу.
    Детали за контакт има на крај на страната. Работно време, секој ден од 9ч - 18ч.
"""

card : String -> Html Msg
card content = Grid.item [cardStyle] [text content]

cardLink : String -> String -> List String -> Html Msg
cardLink content url tags = 
    Grid.item [cardLinkStyle] 
        [text <| "[" ++ String.join ", " tags ++ "] ", a [href url] [text content]]

pageProgramming : Model -> Html Msg
pageProgramming model = div [] 
    [Grid.container 
        (Grid.autoSpecification model.isMobile) 
        [] 
        (List.map card 
          ["Структурно и објектно-ориентирано програмирање со C и Java",
          "Програмирање за анализа на податоци со R или Python",
          "Функционално програмирање со Haskell, Elm или LISP",
          "Развој на веб страни со Elm, CSS и Bootstrap",
          "Развој на видео игри за Андроид со LibGDX",
          "Linux и bash shell скрипти"]),
     -- INFORMATIONS
     newline,
     p [] [text programmingText],
     hr [] [],
     -- USEFUL LINKS
     h2 [style [("margin", "0.5em")]] [text "Интегрирани развојни околини и едитори"],
     Grid.container 
        (Grid.autoSpecification model.isMobile) 
        [] 
        (List.map (\(c, u, t) -> cardLink c u t) 
          [("Visual Studio Code", "https://code.visualstudio.com/", ["Едитор"]),
           ("CodeBlocks", "http://www.codeblocks.org/", ["C", "C++"]),
           ("InteliJ IDEA", "https://www.jetbrains.com/idea/", ["Java"]),
           ("RStudio", "https://www.rstudio.com/", ["R"]),
           ("PyCharm", "https://www.jetbrains.com/pycharm/", ["Python"])
          ]),
      h2 [style [("margin", "0.5em")]] [text "Околини за извршување и пакети за развој"],
      Grid.container 
        (Grid.autoSpecification model.isMobile) 
        [] 
        (List.map (\(c, u, t) -> cardLink c u t) 
          [("Java Development Kit", "http://www.oracle.com/technetwork/java/javase/downloads/index.html", ["Java"]),
           ("GNU Compiler Collection - Linux", "https://www.crybit.com/how-to-install-gcc-gnu-c-c-compiler-unixlinux/", ["C", "C++"]),
           ("GNU Compiler Collection - Windows", "http://mingw.org/", ["C", "C++"]),
           ("Anaconda", "https://www.anaconda.com/", ["Python"]),
           ("R runtime", "https://www.r-project.org/", ["R"]),
           ("Stack", "https://docs.haskellstack.org/en/stable/README/", ["Haskell"])
          ]),
      h2 [style [("margin", "0.5em")]] [text "Линукс дистрибуции"],
      p [] [text "За почетници препорачана линукс дистрибуција е ", a [href "https://www.ubuntu.com/"] [text "Убунту"]]
    ]

pageAlgorithms : Model -> Html Msg
pageAlgorithms model = div [] []

pageMathematics : Model -> Html Msg
pageMathematics model = div [] []
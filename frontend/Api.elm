module Api exposing (..)

import Json.Encode as E
import Json.Decode as D

import Http
import Date

import Date.Format exposing (format)

import Model exposing (..)

backendUrl : String
backendUrl = "http://localhost:8000/"

decodeNews : D.Decoder (List (NewsGroup))
decodeNews = 
    let
        dateDecoder = 
            D.map (Result.withDefault (Date.fromTime 0) << Date.fromString) D.string

        newsDecoder =
            D.map3 News 
                (D.field "newsTitle" D.string)
                (D.field "newsContent" D.string)
                (D.field "newsDateCreated" dateDecoder)

        groupDecoder = 
            D.map3 NewsGroup 
                (D.field "year" D.int)
                (D.field "month" D.int)
                (D.field "news" <| D.list newsDecoder)
    in
        D.list groupDecoder

getNewsCmd : Cmd Msg
getNewsCmd =
    let
        request = Http.get (backendUrl ++ "news?limit=10") decodeNews
    in
        Http.send NewsArrived request
        
sendEmailCmd : Model -> Cmd Msg
sendEmailCmd model =
    let 
        emailReqBody = 
            E.object [("receiverAddress", E.string model.emailAddressTo), 
                      ("messageContent", E.string model.emailContent)] |> 
            Http.jsonBody   

        url = backendUrl ++ "send-mail"
        
        cmd = 
            Http.send EmailConfirmationArrived 
                (Http.post url emailReqBody D.string)
    in
        if model.isAddrValid then cmd else Cmd.none

insertNewsCmd : News -> String -> Cmd AdminMsg
insertNewsCmd news token =
    let 
        url = backendUrl ++ "insert-news/" ++ token ++ "/"

        dateStr = format "%Y-%m-%dT%H:%M:%SZ" news.dateCreated

        reqBody =
            E.object [("newsTitle", E.string news.title),
                      ("newsContent", E.string news.content),
                      ("newsDateCreated", E.string dateStr)] |> Http.jsonBody
        
        req = Http.post url reqBody D.string

        resultParse r =
            case r of
                Ok s -> s
                Err e -> "HTTP ERROR: " ++ (toString e)

        cmd = Http.send (PostResponse << resultParse) req
    in
        cmd

requestTokenCmd : Cmd AdminMsg
requestTokenCmd =
    let
        url = backendUrl ++ "request-token"

        req = Http.post url Http.emptyBody (D.succeed "")

        resultParse r =
            case r of
                Ok s -> s
                Err e -> "HTTP ERROR: " ++ (toString e)

        cmd = Http.send (PostResponse << resultParse) req
    in
        cmd
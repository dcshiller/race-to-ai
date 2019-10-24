module Main exposing (main)

import Array
import Browser
import Debug
import Html exposing (Html, a, br, button, div, h1, h2, img, input, li, p, span, text, ul)
import Html.Attributes exposing (class, href, id, placeholder, src)
import Html.Events
import Random
import String
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Start
    | Restart
    | Pause
    | Unpause
    | BriefPause
    | BriefUnpause
    | ShowAbout
    | SetSeed Int
    | SetName String
    | SetSafety Org String
    | SetPracticality Org String
    | AdvanceAllOrgs


type alias Org =
    { name : String
    , description : String
    , progressAI : Int
    , progressSafety : Int
    , divisionSafety : Int
    , divisionPracticality : Int
    , startingDivisionSafety : Int
    , startingDivisionPracticality : Int
    , seed : Random.Seed
    , playerType : PlayerType
    , successText : String
    , logo : String
    , portrait : String
    , portraitAlt : String
    , comment : String
    , silent : Bool
    }


type alias DifficultySchedule =
    { tiers : List ( Int, Float )
    , seed : Random.Seed
    }


type GameState
    = Before
    | Ongoing
    | Over


type PauseState
    = Unpaused
    | Paused
    | PausedForAbout
    | BrieflyPaused


type PlayerType
    = Player
    | Computer


type alias Model =
    { orgs : List Org
    , difficultyAI : DifficultySchedule
    , difficultySafety : DifficultySchedule
    , step : Int
    , status : GameState
    , pauseState : PauseState
    }


template =
    { name = ""
    , progressAI = 0
    , progressSafety = 0
    , divisionSafety = 80
    , divisionPracticality = 60
    , startingDivisionSafety = 80
    , startingDivisionPracticality = 60
    , seed = Random.initialSeed 0
    , playerType = Computer
    , description = ""
    , successText = ""
    , logo = ""
    , portrait = ""
    , portraitAlt = ""
    , comment = "We're optimistic."
    , silent = False
    }


startingOrgs =
    [ { template
        | name = "Little A.I. Corp"
        , seed = Random.initialSeed 1
        , playerType = Player
        , successText = "allowing you to shape the future as you see fit."
        , portrait = "./images/advisor.png"
      }
    , { template
        | name = "Google"
        , seed = Random.initialSeed 3
        , description = "Corporate Behemoth"
        , startingDivisionSafety = 70
        , startingDivisionPracticality = 75
        , successText = "securing a perpetual technocratic paradise."
        , logo = "./images/google.png"
        , portrait = "./images/Fei-FeiLi.jpeg"
        , portraitAlt = "Fei-Fei Li"
      }
    , { template
        | name = "Tesla"
        , seed = Random.initialSeed 2
        , description = "Silicon Valley Darling"
        , divisionSafety = 80
        , divisionPracticality = 60
        , startingDivisionSafety = 80
        , startingDivisionPracticality = 60
        , successText = "allowing them to upload all our minds for eternal life in the cloud."
        , logo = "./images/tesla.png"
        , portrait = "./images/AndrejKarpathy.jpeg"
        , portraitAlt = "Andrej Karpathy"
      }
    , { template
        | name = "The CCP"
        , seed = Random.initialSeed 4
        , description = "Paternalistic Autocracy"
        , divisionSafety = 90
        , divisionPracticality = 100
        , startingDivisionSafety = 90
        , startingDivisionPracticality = 100
        , successText = "allowing them to welcome the rest of the world into their benevolent autocracy."
        , logo = "./images/china.png"
        , portrait = "./images/WangZhigang.png"
        , portraitAlt = "Wang Zhigang"
      }
    , { template
        | name = "The DOD"
        , seed = Random.initialSeed 5
        , description = "Military Bureaucracy"
        , divisionSafety = 70
        , divisionPracticality = 100
        , startingDivisionSafety = 70
        , startingDivisionPracticality = 100
        , successText = "permitting them to enforce American commercial interests in every corner of the world."
        , logo = "./images/defense.png"
        , portrait = "./images/AratiPrabhakar.png"
        , portraitAlt = "Arait Prabhakar"
      }
    , { template
        | name = "Amazon"
        , seed = Random.initialSeed 6
        , description = "Corporate Behemoth"
        , divisionSafety = 80
        , divisionPracticality = 85
        , startingDivisionSafety = 80
        , startingDivisionPracticality = 85
        , successText = "leading to a capitalist paradise."
        , logo = "./images/amazon.png"
        , portrait = "./images/AndrewWeigend.jpg"
        , portraitAlt = "Andrew Weigend"
      }
    , { template
        | name = "???"
        , seed = Random.initialSeed 7
        , description = "Stealth Startup"
        , divisionSafety = 100
        , divisionPracticality = 50
        , startingDivisionSafety = 100
        , startingDivisionPracticality = 50
        , successText = "enabling them to shape world history to the grand vision of its founder."
        , logo = "./images/stealth.png"
        , portrait = "./images/blank.png"
        , silent = True
      }
    , { template
        | name = "The NSA"
        , seed = Random.initialSeed 8
        , description = "Big Brother"
        , divisionSafety = 80
        , divisionPracticality = 100
        , startingDivisionSafety = 80
        , startingDivisionPracticality = 100
        , successText = "enabling them to stamp out all dissent and preserving American ideals forever."
        , logo = "./images/nsa.png"
        , portrait = "./images/DeborahFrincke.png"
        , portraitAlt = "./images/Deborah Frincke"
      }
    , { template
        | name = "Stanford"
        , seed = Random.initialSeed 9
        , description = "Academic Working Group"
        , divisionSafety = 60
        , divisionPracticality = 20
        , startingDivisionSafety = 60
        , startingDivisionPracticality = 20
        , successText = "producing a benevolent cybrocracy."
        , logo = "./images/stanford.png"
        , portrait = "./images/AndrewNg.jpg"
        , portraitAlt = "Andrew Ng"
      }
    ]


startingSchedule =
    { tiers = [ ( -1, 15 ) ]
    , seed = Random.initialSeed 1
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { orgs = startingOrgs
      , difficultyAI = startingSchedule
      , difficultySafety = startingSchedule
      , step = 0
      , status = Before
      , pauseState = Unpaused
      }
    , Random.generate SetSeed (Random.int 0 10000)
    )



-- Org functions


setOrgName : String -> Org -> Org
setOrgName name org =
    { org | name = name }


setPracticality : Int -> Org -> Org
setPracticality int org =
    { org | divisionPracticality = int }


setSafety : Int -> Org -> Org
setSafety int org =
    { org | divisionSafety = int }


difficultyFrom : DifficultySchedule -> Org -> Float
difficultyFrom difficultySchedule org =
    difficultySchedule.tiers
        |> List.filter (\scheduling -> Tuple.first scheduling < org.progressAI)
        |> List.reverse
        |> List.head
        |> supplyDefaultScheduling
        |> Tuple.second


incrementAIProgress : Float -> Org -> Org
incrementAIProgress difficulty org =
    let
        ( ( random1, random2 ), seed ) =
            Random.step (Random.pair oneInHundred oneInThousand) org.seed

        targetPractical =
            round (toFloat org.divisionSafety * (toFloat org.divisionPracticality / 100) * difficulty)

        targetTheoretical =
            round (toFloat org.divisionSafety * (toFloat (100 - org.divisionPracticality) / 150))
    in
    if random2 < targetTheoretical // 15 then
        { org | progressAI = maxHundred (org.progressAI + 50), seed = seed }

    else if random2 < targetTheoretical // 10 then
        { org | progressAI = maxHundred (org.progressAI + 40), seed = seed }

    else if random2 < targetTheoretical // 5 then
        { org | progressAI = maxHundred (org.progressAI + 30), seed = seed }

    else if random2 < targetTheoretical // 3 then
        { org | progressAI = maxHundred (org.progressAI + 20), seed = seed }

    else if random2 < targetTheoretical then
        { org | progressAI = maxHundred (org.progressAI + 10), seed = seed }

    else if random1 < (targetPractical // 3) then
        { org | progressAI = maxHundred (org.progressAI + 4), seed = seed }

    else if random1 < ((targetPractical * 2) // 3) then
        { org | progressAI = maxHundred (org.progressAI + 2), seed = seed }

    else if random1 < targetPractical then
        { org | progressAI = org.progressAI + 1, seed = seed }

    else
        { org | seed = seed }


incrementSafetyProgress : Float -> Org -> Org
incrementSafetyProgress difficulty org =
    let
        ( random, seed ) =
            Random.step oneInHundred org.seed

        target =
            round (toFloat (100 - org.divisionSafety) * difficulty * 2)
    in
    if random < (target // 4) then
        { org | progressSafety = maxHundred (org.progressSafety + 4), seed = seed }

    else if random < (target // 2) then
        { org | progressSafety = maxHundred (org.progressSafety + 2), seed = seed }

    else if random < target then
        { org | progressSafety = org.progressSafety + 1, seed = seed }

    else
        { org | seed = seed }


adjustDivisions : Model -> Org -> Org
adjustDivisions model org =
    let
        ( randomNum, newSeed ) =
            Random.step (Random.int 0 100) org.seed

        maxProgress =
            model.orgs
                |> List.filter (\o -> not (o == org))
                |> List.map .progressAI
                |> List.maximum
                |> supplyDefault 0

        currentDifficulty =
            difficultyFrom model.difficultyAI org

        ( newDivisionSafety, newComment ) =
            if org.playerType == Player then
                ( org.divisionSafety, "" )

            else if maxProgress > (org.progressAI + 10) then
                ( maxHundred (org.divisionSafety + 1), "We're focusing on catching up quickly. We can worry about ironing out the kinks later." )

            else if org.progressAI > 80 && maxProgress < (org.progressAI - 10) then
                ( minZero (org.divisionSafety - 5), "We're going to slow down and make sure we get this right." )

            else if maxProgress < (org.progressAI - 3) then
                ( minZero (org.divisionSafety - 1), "We're examining the social impact of our technology." )

            else if org.divisionSafety < (org.startingDivisionSafety - 5) then
                ( org.divisionSafety + 1, org.comment )

            else if org.divisionSafety > (org.startingDivisionSafety + 5) then
                ( org.divisionSafety - 1, org.comment )

            else
                ( org.divisionSafety, org.comment )

        ( newDivisionPracticality, newCommentAlt ) =
            if org.playerType == Player then
                ( org.divisionPracticality, "" )

            else if currentDifficulty < 0.5 then
                ( minZero (org.divisionPracticality - 1), "We don't foresee progress without some significant changes to the current paradigm." )

            else if currentDifficulty > 1.5 then
                ( maxHundred (org.divisionPracticality + 1), "We expect to make rapid progress in the coming years." )

            else if org.divisionPracticality < (org.startingDivisionPracticality - 5) then
                ( org.divisionPracticality + 1, org.comment )

            else if org.divisionPracticality > (org.startingDivisionPracticality + 5) then
                ( org.divisionPracticality - 1, org.comment )

            else
                ( org.divisionPracticality, org.comment )
    in
    { org
        | divisionSafety = newDivisionSafety
        , divisionPracticality = newDivisionPracticality
        , comment =
            if randomNum < 33 then
                newComment

            else if randomNum < 66 then
                newCommentAlt

            else
                org.comment
    }


focusSummary org =
    if (org.divisionSafety > 80 && org.progressSafety < 50) || (org.divisionSafety > 60 && org.progressAI > (org.progressSafety + 10)) then
        if org.divisionPracticality > 80 then
            "You are focused on making incremental progress with little concern for safety."

        else if org.divisionPracticality < 50 then
            "You are focused on making a big theoretical breakthrough with little concern for safety."

        else
            "You are investing in both practical and theoretical advances with little concern for safety."

    else if org.divisionSafety < 60 || (org.divisionSafety < 80 && org.progressAI < (org.progressSafety - 10)) then
        if org.divisionPracticality > 80 then
            "You are focused on making incremental progress in a safe manner."

        else if org.divisionPracticality < 50 then
            "You are focused on making a big breakhtrough while investing in figuring out how to do it safely."

        else
            "You are investing in both practical and theoretical advances with attention to safety."

    else
        "You are adopting a middle of the road approach."


advisorInfo model org =
    let
        difficultyLevel =
            difficultyFrom model.difficultyAI org

        leadingOrg =
            model.orgs
                |> List.filter (\o -> not (o == org))
                |> List.sortBy .progressAI
                |> List.reverse
                |> List.head
                |> supplyDefaultOrg

        progressAI =
            org.progressAI

        progressSafety =
            org.progressSafety

        divisionPracticality =
            org.divisionPracticality

        divisionSafety =
            org.divisionPracticality

        randomNum =
            Tuple.first <| Random.step oneInHundred org.seed
    in
    if difficultyLevel < 0.3 && divisionPracticality > 50 && randomNum < 50 then
        "Our current challenges are proving to be extremely difficult. It would be wise to invest in finding a theoretical breakthrough."

    else if difficultyLevel < 0.3 && divisionPracticality < 51 && randomNum < 50 then
        "Our current challenges are proving to be extremely difficult. We're hoping to hit a major breakthrough to move forward."

    else if leadingOrg.progressAI < org.progressAI - 10 && progressSafety < 50 && divisionSafety > 70 then
        "We're well ahead of our nearest competitor, maybe we should spend a little time thinking about the practical ramifications?"

    else if leadingOrg.progressAI > org.progressAI + 20 && randomNum > 75 then
        leadingOrg.name ++ " is doing something right. Maybe we should figure out what that is."

    else if leadingOrg.progressAI > org.progressAI + 10 && randomNum < 80 then
        leadingOrg.name ++ " is making excellent progress."

    else if difficultyLevel < 0.8 && divisionPracticality < 51 then
        "We're making good progress right now. Maybe we should consider focusing on only incremental advances."

    else if difficultyLevel < 2 && divisionPracticality < 70 then
        "We're making excellent progress right now. Maybe we should consider focusing on only incremental advances."

    else if randomNum < 50 then
        "Our researchers are greatful for your leadership."

    else
        "You have our full confidence."


publicInfo org =
    let
        random =
            Tuple.first <| Random.step (Random.int 0 100) org.seed
    in
    if org.progressAI == 100 && random < 50 then
        "You'll love our vision of the world."

    else if org.progressAI == 100 then
        "Don't worry, you'll still have a place in our new world order."

    else if org.silent then
        ""

    else if random < 50 then
        org.comment

    else if org.progressAI > 95 && org.progressSafety < 60 then
        "We promise we'll address everyone's concerns in version 2.0."

    else if org.progressAI > 90 && org.progressSafety < 40 then
        "Maybe we should be a bit more careful before releasing this."

    else if org.progressAI > 80 then
        "We're confident we'll develop A.I. soon."

    else if org.progressAI < 10 && org.divisionPracticality < 70 then
        "We're planning on surveying the possibilities before investing in any one strategy."

    else if org.progressAI < 10 && org.divisionPracticality > 80 then
        "We think there are only a few small steps between us and strong A.I."

    else if org.progressAI < 10 then
        "We're just getting started, but the road is clear."

    else if org.divisionSafety > 80 || org.progressAI > (org.progressSafety + 10) then
        if org.divisionSafety > 90 then
            "We're hoping the first generation of A.I. will tell us how to make the second generation safer."

        else if org.divisionPracticality > 80 then
            "We're investing in making small steps in the near future."

        else if org.divisionPracticality < 50 then
            "We're trying out some pretty radical ideas."

        else
            "We're not concerned about safety."

    else if org.divisionSafety < 50 then
        if org.divisionPracticality > 80 then
            "We've got top minds working on A.I. safety."

        else if org.divisionPracticality < 50 then
            "We're looking at alternative strategies to safely move the ball forward."

        else
            "We're keeping our ears open for any promising opportunities."

    else
        "We're excited by the progress we're making."


victoryTextFor : Org -> String
victoryTextFor org =
    let
        random =
            Tuple.first <| Random.step oneInHundred org.seed

        failureIndex =
            Tuple.first <| Random.step (Random.int 0 (Array.length failureText)) org.seed

        failureText =
            Array.fromList
                [ "It plasters the universe with hedonium."
                , "It traps us all in an idyllic virtual reality for our own good."
                , "It enshrines 21st century moral quirks in perpetuity."
                , "A lone misanthrope uses it to build nanobots that convert the world to grey goo."
                , "Captured by servants of Roko's Basilisk, it imprisons and tortures everyone else."
                , "It decides that humanity has a negative expected utility and kindly euthanizes our species."
                ]
    in
    if random > org.progressSafety then
        org.name ++ " develops strong artificial intelligence. " ++ supplyDefaultFailure (Array.get failureIndex failureText)

    else
        org.name ++ " succeeded in safely developing strong artificial intelligence, " ++ org.successText



-- Difficulty Schedule Functions


updateSchedules : (DifficultySchedule -> DifficultySchedule) -> Model -> Model
updateSchedules func model =
    { model
        | difficultyAI = func model.difficultyAI
        , difficultySafety = func model.difficultySafety
    }


setSeed : Int -> DifficultySchedule -> DifficultySchedule
setSeed seed schedule =
    { schedule | seed = Random.initialSeed (Tuple.first (Random.step oneInThousand schedule.seed) + seed) }


setTiers : DifficultySchedule -> DifficultySchedule
setTiers schedule =
    { schedule | tiers = generateTiers schedule.seed }



-- Calculation Helpers


maxHundred : Int -> Int
maxHundred num =
    min 100 num


minZero : Int -> Int
minZero num =
    max 0 num


oneInHundred : Random.Generator Int
oneInHundred =
    Random.int 0 100


oneInThousand : Random.Generator Int
oneInThousand =
    Random.int 0 1000


supplyDefault def maybe =
    case maybe of
        Nothing ->
            def

        Just a ->
            a


supplyDefaultInt maybeInt =
    supplyDefault 50 maybeInt


supplyDefaultFailure maybeString =
    supplyDefault "which plasters the universe with hedonium." maybeString


supplyDefaultOrg maybeOrg =
    supplyDefault { template | name = "missing", divisionSafety = 0 } maybeOrg


supplyDefaultScheduling maybeScheduling =
    supplyDefault ( 100, 1.0 ) maybeScheduling



-- Model functions


checkEnd : Model -> Model
checkEnd model =
    if List.any (\org -> org.progressAI == 100) model.orgs then
        { model | status = Over }

    else
        model


playerOrg model =
    model.orgs |> List.filter (\o -> o.playerType == Player) |> List.head |> supplyDefaultOrg


advanceStep model =
    { model | step = model.step + 1 }


updateList : (Org -> Org) -> Model -> Org -> Model
updateList func model orgToUpdate =
    { model
        | orgs =
            List.map
                (\org ->
                    if org == orgToUpdate then
                        func org

                    else
                        org
                )
                model.orgs
    }


updateOrgs : Model -> Model
updateOrgs model =
    let
        incrementAIBySchedule : Org -> Org
        incrementAIBySchedule org =
            incrementAIProgress (difficultyFrom model.difficultyAI org) org

        incrementSafetyBySchedule org =
            incrementSafetyProgress (difficultyFrom model.difficultySafety org) org
    in
    { model
        | orgs =
            model.orgs
                |> List.map incrementAIBySchedule
                |> List.map incrementSafetyBySchedule
                |> List.map (adjustDivisions model)
    }


generateTiers : Random.Seed -> List ( Int, Float )
generateTiers seed =
    let
        listLength =
            Tuple.first (Random.step (Random.int 2 15) seed)

        weightedFloat =
            Random.weighted
                ( 10, 0.05 )
                [ ( 20, 0.1 )
                , ( 10, 0.3 )
                , ( 10, 0.7 )
                , ( 20, 1 )
                , ( 10, 1.5 )
                , ( 10, 2 )
                , ( 10, 3 )
                , ( 10, 5 )
                ]
    in
    Tuple.first (Random.step (Random.list listLength (Random.pair (Random.int 0 100) weightedFloat)) seed)
        |> List.sortBy Tuple.first


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | status = Ongoing }, Cmd.none )

        Restart ->
            ( { model | status = Before, orgs = startingOrgs, step = 0 }, Random.generate SetSeed (Random.int 0 10000) )

        Pause ->
            ( { model | pauseState = Paused }, Cmd.none )

        Unpause ->
            ( { model | pauseState = Unpaused }, Cmd.none )

        BriefUnpause ->
            if model.pauseState == BrieflyPaused then
                ( { model | pauseState = Unpaused }, Cmd.none )

            else
                ( model, Cmd.none )

        ShowAbout ->
            ( { model | pauseState = PausedForAbout }, Cmd.none )

        BriefPause ->
            if model.pauseState == Unpaused && model.status == Ongoing then
                ( { model | pauseState = BrieflyPaused }, Cmd.none )

            else
                ( model, Cmd.none )

        SetSeed int ->
            ( model
                |> updateSchedules (setSeed int)
                |> updateSchedules setTiers
            , Cmd.none
            )

        SetName val ->
            ( updateList (setOrgName val) model (playerOrg model), Cmd.none )

        SetSafety org stringVal ->
            ( updateList (setSafety (supplyDefaultInt <| String.toInt stringVal)) model org, Cmd.none )

        SetPracticality org stringVal ->
            ( updateList (setPracticality (supplyDefaultInt <| String.toInt stringVal)) model org, Cmd.none )

        AdvanceAllOrgs ->
            ( model
                |> updateOrgs
                |> advanceStep
                |> checkEnd
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Ongoing ->
            if model.pauseState == Unpaused then
                Time.every 1000 (\pos -> AdvanceAllOrgs)

            else if model.pauseState == BrieflyPaused then
                Time.every 5000 (\pos -> AdvanceAllOrgs)

            else
                Sub.none

        Before ->
            Sub.none

        Over ->
            Sub.none


renderPortrait org speech =
    div [ class "portrait-container" ]
        [ img
            [ Html.Events.onMouseOver BriefPause
            , Html.Events.onMouseLeave BriefUnpause
            , class "portrait"
            , src org.portrait
            , Html.Attributes.alt org.portraitAlt
            ]
            []
        , div [ class "speech-bubble" ] [ text speech ]
        ]


renderProgress org =
    div []
        [ div [ class "row" ]
            [ span [ class "label" ] [ text "Progress on A.I. " ]
            , div [ class "progress-bar-container" ]
                [ div [ class ("progress-ai width-" ++ String.fromInt org.progressAI) ] []
                ]
            ]
        , div [ class "row" ]
            [ span [ class "label" ] [ text "Progress on A.I. Safety " ]
            , div [ class "progress-bar-container" ]
                [ div [ class ("progress-safety width-" ++ String.fromInt org.progressSafety) ] []
                ]
            ]
        ]


renderOrgCard model org =
    case org.playerType of
        Player ->
            li [ class "org player" ]
                [ h1 [] [ text org.name ]
                , div [ class "subtitle" ] [ text "You" ]
                , renderPortrait org (advisorInfo model org)
                , h2 [] [ text "Current Progress" ]
                , renderProgress org
                , h2 [] [ text "Research Focuses" ]
                , div [ class "row range" ]
                    [ span [ class "range label" ] [ text "Safety" ]
                    , input [ Html.Attributes.type_ "range", Html.Attributes.value (String.fromInt org.divisionSafety), Html.Events.onInput (SetSafety org) ] []
                    , span [ class "range label" ] [ text "Progress" ]
                    , text <| String.fromInt org.divisionSafety ++ "%"
                    ]
                , div [ class "row range" ]
                    [ span [ class "range label" ] [ text "Breakthrough" ]
                    , input [ Html.Attributes.type_ "range", Html.Attributes.value (String.fromInt org.divisionPracticality), Html.Events.onInput (SetPracticality org) ] []
                    , span [ class "range label" ] [ text "Incremental" ]
                    , text <| String.fromInt org.divisionPracticality ++ "%"
                    ]
                , div [ class "explanatory-text" ] [ text (focusSummary org) ]
                ]

        Computer ->
            li [ class "org computer" ]
                [ h1 [] [ text org.name ]
                , img [ class "logo", src org.logo ] []
                , renderPortrait org (publicInfo org)
                , div [ class "subtitle" ] [ text org.description ]
                , renderProgress org
                ]


renderDate step =
    let
        months =
            Array.fromList [ "January", "May", "September" ]
    in
    supplyDefault "January" (Array.get (modBy 3 step) months) ++ ", " ++ String.fromInt ((step // 3) + 2020)


renderStateButton model =
    case model.status of
        Ongoing ->
            case model.pauseState of
                Unpaused ->
                    button [ id "pause", Html.Events.onClick Pause ] [ text "Pause" ]

                BrieflyPaused ->
                    button [ id "pause", Html.Events.onClick Pause ] [ text "Pause" ]

                PausedForAbout ->
                    button [ id "unpause", Html.Events.onClick Unpause ] [ text "Unpause" ]

                Paused ->
                    button [ id "unpause", Html.Events.onClick Unpause ] [ text "Unpause" ]

        Before ->
            text ""

        Over ->
            button [ id "restart", Html.Events.onClick Restart ] [ text "Restart" ]


renderHeader model =
    div [ class "header" ]
        [ h1 [] [ text "Race to A.I." ]
        , span [ class "date" ] [ text (renderDate model.step) ]
        , div [ class "row" ]
            [ renderStateButton model
            , span
                [ id "about"
                , Html.Events.onClick
                    (if model.pauseState == PausedForAbout then
                        Unpause

                     else
                        ShowAbout
                    )
                ]
                [ text "?" ]
            ]
        ]


renderVictoryText model =
    let
        winningOrg =
            model.orgs
                |> List.filter (\o -> o.progressAI == 100)
                |> List.head
                |> supplyDefaultOrg
    in
    if not (winningOrg.progressAI == 100) then
        div [ class "victory" ] []

    else
        div [ class "victory" ] [ p [] [ text <| victoryTextFor winningOrg ] ]


renderModal child =
    div []
        [ div [ class "overlay" ] []
        , div [ class "modal" ] [ child ]
        ]


renderStartText =
    div []
        [ p [] [ text "Welcome! In Race to A.I., you are the owner of a startup in the A.I. space. You are competing with other companies and organizations to be the first to generate strong artificial intelligence. If you are the first, your technological dominance will allow you to secure your vision of the good society for centuries! If you are not, your children will live in the world created by someone else." ]
        , p [] [ text "Beware, A.I. is tricky. If you do not develop the proper safety protocols, you might not like the world you create." ]
        , br [] []
        , p [] [ text "To get started, enter your company's name." ]
        , br [] []
        , input [ placeholder "Company Name", Html.Events.onInput SetName ] []
        , br [] []
        , br [] []
        , button [ id "start", Html.Events.onClick Start ] [ text "Start" ]
        ]


renderAbout model =
    if model.pauseState == PausedForAbout then
        renderModal
            (div []
                [ h1 [] [ text "Premise" ]
                , p [] [ text "The game is premised on the idea that A.I. is extremely unpredictable and that the tremendous capabilities of A.I. mean we only get one shot at doing it right. Whoever creates the first A.I. will have an immense advantage over their nearest competitors. There are many different parties interested in being the first, and their competition with each other will drive them to take big risks." ]
                , h1 [] [ text "Object" ]
                , p [] [ text "The objective of Race to A.I. is to lead your organization to be the first to develop strong A.I. and to do so in a safe manner. If your organization is the first to fill up its progress bar, a random check will be made against your safety progress. If the result falls short, the A.I. developed will possess some significant flaw." ]
                , p [] [ text "To do well in the game, you should aim to be the first to develop A.I. and have made significant progress in A.I. safety. This is generally not possible. You have many competitors who are as competent as you are. Some of them will take shortcuts. Such is life." ]
                , h1 [] [ text "Gameplay" ]
                , p [] [ text "As the leader of your organization, you have control over two things: you can adjust how much of your available resources you devote to progress in A.I. vs. A.I. safety and you can shift between trying for a major theoretical breakthrough and making incremental progress. At the start of the game, the challenges of developing A.I. will be randomly determined. The result is a difficulty schedule, which reflects how difficult advancement is at each point in the progress bar. Attempts at a major breakthrough are evaluated relative to a fixed difficulty rating independent of the progress bar. Since a major breakthrough will advance you signficantly, it is advantageous to invest in a breakthrough when you get stuck." ]
                , p [] [ text "Each organization is handled exactly the same way and shares the same difficulty schedule. Your competitors will have different default divisions of their resource, but they will shift them in response to how the game has developed. If an organization gets stuck, they will look for a breakthrough. If they fall behind, they will invest less in safety. You can hover over their portraits to get some sense of what each organization is doing." ]
                , h1 [] [ text "Further Reading" ]
                , p [] [ text "This game was inspired by the following books. If you have an interest in A.I. or the continued existence of humanity, I strongly recommend them." ]
                , p [] [ text "James Barrat, Our Final Invention" ]
                , p [] [ text "Nick Bostrom, Superintelligence" ]
                , h1 [] [ text "About" ]
                , p [] [ text "This game was developed in elm between Oct. 25th and October 31st 2019 for the Philosophy Game Jam #2" ]
                , p []
                    [ text "Code available on "
                    , a [ href "https://github.com/dcshiller/race-to-ai" ] [ text "Github" ]
                    ]
                ]
            )

    else
        div [] []


view : Model -> Html Msg
view model =
    case model.status of
        Before ->
            div [ class "container" ]
                [ renderHeader model
                , renderModal renderStartText
                , ul [ class "org-list" ] (List.map (renderOrgCard model) model.orgs)
                ]

        Ongoing ->
            div [ class "container" ]
                [ renderHeader model
                , ul [] (List.map (renderOrgCard model) model.orgs)
                , renderAbout model
                ]

        Over ->
            div [ class "container" ]
                [ renderHeader model
                , renderModal (renderVictoryText model)
                , ul [ class "org-list" ] (List.map (renderOrgCard model) model.orgs)
                ]

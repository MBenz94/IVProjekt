module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as UP exposing ((</>))
import Path
import Axis
import Shape exposing (defaultPieConfig)
import Array exposing (Array)
import Color exposing (Color)
import Chart.Bar as Bar
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, path, rect, style, svg, text_, line, polygon)
import TypedSvg.Attributes exposing (class, d, fontFamily, fontSize, textAnchor, transform, viewBox, fill, dy, stroke)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y, x1, y1, x2, y2)
import TypedSvg.Attributes.InPx as SAPX
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..), Paint(..), em)
css : String
css =
  """
  body {
  font-family : Sans-Serif;
  }
  .chart-wrapper{
    margin : 20px
    border: 1px solid #c4c4c4;
  }
  .column rect {
  fill: #c2c2c2;
  }
  .axis path,
  .axis line {
  stroke: #b7b7b7;
  }
  text {
   fill: #333
   }
  
"""


-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
type Status
    = Endangered 
    | Threatened_with_Extinction--
    | Common --
    | Critically_Endangered --
    | Hardly_Endangered
    | Secured --
    | No_Information
    | Rare
 
type DistributionArea
    = Africa
    | Europe
    | Asia
    | Australia
    | South_America
    | North_America
    | Arctic

type alias Animal =
    { animalName : String
    , status : Status
    , distributionArea : DistributionArea
    , height : Maybe Int -- in cm
    , way_of_life : Maybe Int
    , weight :  Maybe Float -- in kg
    , food   : Maybe Float
    , age    : Maybe Int --in years
    }



type Route
    = Internal InternalRoute
    | External String


type InternalRoute
    = Home
    | CommonSite
    | SecuredSite
    | EndangeredSite
    | Threatened_with_ExtinctionSite
    | RareSite
    | No_InformationSite
    | Hardly_EndangeredSite
    | Critically_EndangeredSite
    | NotFound


basepath : String
basepath =
    "/github/elm-examples/spa-basic"


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.s "github"
        </> UP.s "elm-examples"
        </> UP.oneOf
                [ UP.map (External "/github/elm-examples") UP.top
                , UP.s "spa-basic"
                    </> UP.oneOf
                            [ UP.map (Internal Home) UP.top
                            , UP.map (Internal CommonSite) (UP.s "common")
                            , UP.map (Internal SecuredSite) (UP.s "secured")
                            , UP.map (Internal EndangeredSite) (UP.s "endangered")
                            , UP.map (Internal Threatened_with_ExtinctionSite) (UP.s "threatened")
                            , UP.map (Internal RareSite) (UP.s "rare")
                            , UP.map (Internal No_InformationSite) (UP.s "no_Information")
                            , UP.map (Internal Hardly_EndangeredSite) (UP.s "hardly")
                            , UP.map (Internal Critically_EndangeredSite) (UP.s "critically")
                                
                            ]
                ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault (Internal NotFound) (UP.parse routeParser url)



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , internalRoute : InternalRoute
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( internalRoute, cmd ) =
            case urlToRoute url of
                Internal iroute ->
                    ( iroute, Cmd.none )

                External href ->
                    {- This case should be impossible, but handle it still properly -}
                    ( NotFound, Nav.load href )
    in
    ( { key = key
      , url = url
      , internalRoute = internalRoute
      }
    , cmd
    )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case urlToRoute url of
                Internal internalRoute ->
                    ( { model
                        | url = url
                        , internalRoute = internalRoute
                      }
                    , Cmd.none
                    )

                External href ->
                    ( model, Nav.load href )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.internalRoute of
                Home ->
                    pageHome

                CommonSite ->
                    pageCommon

                SecuredSite ->
                    pageSecured
                EndangeredSite ->
                    pageEndangered
                Threatened_with_ExtinctionSite ->
                    pageThreatened
                RareSite ->
                    pageRare
                No_InformationSite->
                    pageNo_Information
                Critically_EndangeredSite ->
                    pageCritically
                Hardly_EndangeredSite ->
                    pageHardly
                NotFound ->
                    pageNotFound model.url
    in
    { title = page.title ++ "Projekt IV"
    , body =
        viewHeader model.internalRoute
            ++ [ Html.h1 [] [ Html.text page.title ] ]
            ++ page.content
            ++ [ p [] [] ]
            ++ viewFooter 
    }



viewFooter : List (Html msg)
viewFooter =
    [ a [ href "#" ] [ Html.text "HomeButton" ]
    , hr [] []
    ]


viewExternalLink : String -> String -> Html msg
viewExternalLink linkText linkHref =
    a [ href linkHref, target "_blank" ] [ Html.text linkText ]


viewLinkOrText : InternalRoute -> InternalRoute -> String -> String -> Html msg
viewLinkOrText currentRoute linkRoute linkText linkHref =
    if currentRoute == linkRoute then
        b [] [ Html.text linkText ]

    else
        a [ href linkHref ] [ Html.text linkText ]


viewHeader : InternalRoute -> List (Html msg)
viewHeader currentRoute =
    [ hr [] []
    , viewLinkOrText currentRoute Home "Home" basepath
    , Html.text " ☆ "
    , viewLinkOrText currentRoute CommonSite "Common" (basepath ++ "/common")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute SecuredSite "Secured" (basepath ++ "/secured")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute EndangeredSite "Endangered" (basepath ++ "/endangered")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute Hardly_EndangeredSite "Hardly_Endangered" (basepath ++ "/hardly")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute RareSite "Rare" (basepath ++ "/rare")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute No_InformationSite "No_Information" (basepath ++ "/no_Information")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute Critically_EndangeredSite "Critically_Endangered" (basepath ++ "/critically")
    , Html.text " ☆ "
    , viewLinkOrText currentRoute Threatened_with_ExtinctionSite "Threatened_with_Extinction" (basepath ++ "/threatened")
   
   
    ]



-- PAGE


type alias Page msg =
    { title : String
    , content : List (Html msg)
    }


pageHome : Page msg
pageHome =
    { title = "Projekt IV - Übersicht"
    , content =
        [ Html.h1 [] [Html.text "Säugetiere und ihr Bedrohungsstatus"]
        , br [] []
        , Html.h3 [] [Html.text "-Analyse der Bedrohungsstatus im Hinblieb auf die Größe, das Alter und den Lebenraum der Tiere-"]
        ,Html.text ("Der verwendete Datensatz besteht aus " ++ (String.fromInt (List.length animals ))++ "  Tieren")
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africa )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_America)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_America)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asia)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europe)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australia)))
         ] 
        , br [][]
        , Html.text " Scatterplot Average Height aller Tiere in der Liste"
        , scatterplotStatus animalFiltered listAllAnimalsBelowAverage   listAllAnimalsAboveAverage  meanAllHeight

        , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanAllHeight)))
        , br [][]
        , Html.text ("Above Average " ++ (String.fromInt (List.length aboveHeightAll)) )
        , pieview aboveAlldata
        , Html.text ("Below Average " ++ (String.fromInt (List.length belowHeightAll)))
        , pieview belowAlldata 
        ]
    }


pageCommon : Page msg
pageCommon =
    { title = "Common"
    , content =
        [ Html.text "Bedrohungsstatus Common"
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africac )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americac)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americac)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiac)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europec)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiac)))
         ] 
         , br [][]
        , scatterplotStatus animalFiltered listCommonAnimalsBelowAverage listCommonAnimalsAboveAverage meanCommonHeight
        ,commonchart
        , br [][]
        , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanCommonHeight)))
        , br [][]
        , Html.text ("Above meanheight "++ (String.fromInt (List.length aboveCommonHeight)))
        , pieview abovecommondata
        , Html.text ("Below meanheight " ++ (String.fromInt (List.length belowCommonHeight)))
        ,pieview belowcommondata
        ]
    }
pageEndangered : Page msg
pageEndangered =
    { title = "Endangered"
        , content =
        [ Html.text "Bedrohungsstatus Endangered"
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africae )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americae)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americae)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiae)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europee)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiae)))
         ] 
         , br [][]
        , scatterplotStatus animalFiltered listEndangeredAnimalsBelowAverage listEndangeredAnimalsAboveAverage meanEndangeredHeight
        , endangeredchart
        , br [][]
        , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanEndangeredHeight)))
        , br [][]
        , Html.text ("Above meanheight "++ (String.fromInt (List.length aboveEndangeredHeight)))
        , pieview aboveEndangereddata
        , Html.text ("Below meanheight "++ (String.fromInt (List.length belowEndangeredHeight)) )
        , pieview belowEndangereddata
        ]
    }

pageSecured : Page msg
pageSecured =
    { title = "Secured"
    , content =
        [ Html.text "Bedrohungsstatus Secured"
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africasecured )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americasecured)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americasecured)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiasecured)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europesecured)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiasecured)))
         ] 
         , br [][]
         , scatterplotStatus animalFiltered listSecuredAnimalsBelowAverage listSecuredAnimalsAboveAverage meanSecuredHeight
         , securedchart
         , br [][]
         , Html.text " "
         , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanSecuredHeight)))
        , br [][]
         , Html.text ("Above meanheight "++ (String.fromInt (List.length aboveSecuredHeight)))
         , pieview aboveSecureddata
         , Html.text ("Below meanheight "++ (String.fromInt (List.length belowSecuredHeight)))
         , pieview belowSecureddata    
        ]
    }
pageNo_Information : Page msg
pageNo_Information =
    { title = "No_Information"
        , content =
        [ Html.text "Bedrohungsstatus No_Information"
        ,br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africainformation )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americainformation)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americainformation)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiainformation)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europeinformation)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiainformation)))
         ] 
         , br [][]
         ,scatterplotStatus animalFiltered listInformationAnimalsBelowAverage listInformationAnimalsAboveAverage meanInformationHeight
         , informationchart 
         , br [][]
         , Html.text " "
         , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanInformationHeight)))
        , br [][]
         , Html.text ("Above meanheight "++ (String.fromInt (List.length aboveInformationHeight)))
         , pieview aboveInformationdata
         , Html.text ("Below meanheight " ++ (String.fromInt (List.length belowInformationHeight)))
         , pieview belowInformationdata   
        ]
    }
pageRare : Page msg
pageRare =
    { title = "Rare"
        , content =
        [ Html.text "Bedrohungsstatus Rare"
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africarare )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americarare)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americarare)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiarare)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europerare)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiarare)))
         ] 
         , br [][]
         ,scatterplotStatus animalFiltered listRareAnimalsBelowAverage listRareAnimalsAboveAverage meanRareHeight
         ,rarechart     
        ]
    }
pageCritically : Page msg
pageCritically =
    { title = "Critically_Endangered"
        , content =
        [ Html.text "Bedrohungsstatus Critically_Endangered "
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africacritically)))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americacritically)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americacritically)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiacritically)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europecritically)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiacritically)))
         ] 
         , br [][]
         ,scatterplotStatus animalFiltered listCriticallyAnimalsBelowAverage listCriticallyAnimalsAboveAverage meanCriticallyHeight
         , criticallychart   
         , br [][]
         , Html.text " "
         , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanCriticallyHeight)))
         , br [][]
         , Html.text ("Above meanheight " ++ (String.fromInt (List.length aboveCriticallyHeight)))
         , pieview aboveCriticallydata
         , Html.text ("Below meanheight " ++ (String.fromInt (List.length belowCriticallyHeight)))
         , pieview belowCriticallydata  
        ]
    }
pageHardly : Page msg
pageHardly =
    { title = "Hardly_Endangered"
        , content =
        [ Html.text "Bedrohungsstatus Hardly_Endangered "
         , br [][]
         , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africahardly )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americahardly)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americahardly)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiahardly)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europehardly)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiahardly)))
         ] 
         , br [][]
         ,scatterplotStatus animalFiltered listHardlyAnimalsBelowAverage listHardlyAnimalsAboveAverage meanHardlyHeight
         ,hardlychart   
         , br [][]
         , Html.text " "
         , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanHardlyHeight)))
         , br [][]
         , Html.text ("Above meanheight " ++ (String.fromInt (List.length aboveHardlyHeight)))
         , pieview aboveHardlydata
         , Html.text ("Below meanheight " ++ (String.fromInt (List.length belowHardlyHeight)))
         , pieview belowHardlydata     
        ]
    }
pageThreatened : Page msg
pageThreatened =
    { title = "Threatened_with_Extinction"
        , content =
        [ Html.text "Bedrohungsstatus Threatened_with_Extinction "
        , br [][]
        , Html.div []
         [ Html.text ("Anzahl der Tiere für die verschiedenen Kontinente ")
         , br [][]
         , Html.text ("Africa          ->   " ++ (String.fromInt (List.length africathreatened )))
         , br [][]
         , Html.text ("South_America   ->   " ++ (String.fromInt (List.length south_Americathreatened)))
         , br [][]
         , Html.text ("North_America   ->   " ++ (String.fromInt (List.length north_Americathreatened)))
         , br [][]
         , Html.text ("Asia            ->   " ++ (String.fromInt (List.length asiathreatened)))
         , br [][]
         , Html.text ("Europe          ->   " ++ (String.fromInt (List.length europethreatened)))
         , br [][]
         , Html.text ("Australia       ->   " ++ (String.fromInt (List.length australiathreatened)))
         ] 
         , br [][]
        ,scatterplotStatus animalFiltered listThreatenedAnimalsBelowAverage listThreatenedAnimalsAboveAverage meanThreatenedHeight
        ,threatenedchart
        , br [][]
          , Html.text " "
          , Html.text (" Die allgemeine Durchschnittshöhe beträgt "++ (String.fromFloat (meanThreatenedHeight)))
        , br [][]
          ,Html.text ("Above meanheight "++(String.fromInt(List.length aboveThreatenedHeight)))
          , pieview abovethreateneddata
          ,Html.text ("Below meanheight " ++ (String.fromInt(List.length belowThreatenedHeight)))
          , pieview belowThreateneddata
        ]
    }

pageNotFound : Url.Url -> Page msg
pageNotFound url =
    { title = "Page not found"
    , content =
        [ Html.text "ERROR: Page "
        , b [] [ Html.text (Url.toString url) ]
        , Html.text " not found."
        ]
    }
--Filter Animals

type alias FilterAnimals =
    { status : Status
    , distributionArea : DistributionArea
    , height : Int
    , way_of_life : Int
    , age : Int
    }

filterAnimals : List Animal -> List FilterAnimals
filterAnimals animalList =
    let
        filterFunction : FilterAnimals -> Bool
        filterFunction animal =
            if animal.height == -1 then
                False

            else if animal.way_of_life == -1 then
                False

            else if animal.age == -1 then
                False

            else
                True

        removeMaybe : Maybe Int -> Int
        removeMaybe number =
            case number of
                Just int ->
                    int

                Nothing ->
                    -1

        filterFunctionRemoveMaybe : Animal -> FilterAnimals
        filterFunctionRemoveMaybe animal =
            FilterAnimals animal.status animal.distributionArea (removeMaybe animal.height) (removeMaybe animal.way_of_life) (removeMaybe animal.age)
    in
    List.map filterFunctionRemoveMaybe animalList |> List.filter filterFunction
specificAreaType : List FilterAnimals -> Status -> DistributionArea -> List FilterAnimals
specificAreaType animalList status distributionArea =
    List.filter (\animal -> animal.status == status && animal.distributionArea == distributionArea) animalList
specificAboveHeightType : List FilterAnimals -> Status -> DistributionArea -> Float -> List FilterAnimals
specificAboveHeightType animalList status distributionArea meanheight =
     List.filter (\animal -> animal.status == status && animal.distributionArea == distributionArea && toFloat(animal.height) > meanheight) animalList
specificBelowMeanHeightType : List FilterAnimals -> Status -> DistributionArea -> Float -> List FilterAnimals
specificBelowMeanHeightType animalList status distributionArea meanheight =
     List.filter (\animal -> animal.status == status && animal.distributionArea == distributionArea && toFloat(animal.height) < meanheight) animalList
specificAboveHeight : List FilterAnimals -> DistributionArea -> Float -> List FilterAnimals
specificAboveHeight animalList  distributionArea meanheight =
     List.filter (\animal ->  animal.distributionArea == distributionArea && toFloat(animal.height) > meanheight) animalList
specificBelowMeanHeight : List FilterAnimals ->  DistributionArea -> Float -> List FilterAnimals
specificBelowMeanHeight animalList distributionArea meanheight =
     List.filter (\animal -> animal.distributionArea == distributionArea && toFloat(animal.height) < meanheight) animalList

aboveHeight : List FilterAnimals -> Status -> Float -> List FilterAnimals
aboveHeight animalList status meanheight =
     List.filter (\animal -> animal.status == status && toFloat(animal.height)> meanheight) animalList
belowHeight : List FilterAnimals -> Status -> Float -> List FilterAnimals
belowHeight animalList status meanheight =
     List.filter (\animal -> animal.status == status && toFloat(animal.height)< meanheight) animalList
aboveMeanHeight : List FilterAnimals -> Float -> List FilterAnimals
aboveMeanHeight animalList meanheight =
    List.filter (\animal -> toFloat(animal.height) > meanheight) animalList
belowMeanHeight : List FilterAnimals -> Float -> List FilterAnimals
belowMeanHeight animalList meanheight =
    List.filter (\animal -> toFloat(animal.height) < meanheight ) animalList
countAllAnimals : List FilterAnimals -> DistributionArea -> List FilterAnimals
countAllAnimals  animalList distributionArea =
    List.filter (\animal -> animal.distributionArea == distributionArea) animalList
--all
aboveAllMean animal =
            animal.x > meanAllHeight
belowAllMean animal =
            animal.x < meanAllHeight
meanAllHeight = 
       getHeightAverage animalAllClassList
animalAllClassList =
            getAllClassList animalFiltered
listAllAnimalsAboveAverage =
            List.filter aboveAllMean animalAllClassList
listAllAnimalsBelowAverage =
            List.filter belowAllMean animalAllClassList
--Common

aboveCommonMean animal =
            animal.x > meanCommonHeight
belowCommonMean animal =
            animal.x < meanCommonHeight
meanCommonHeight =
            getHeightAverage animalCommonClassList
animalCommonClassList =
            getCommonClassList animalFiltered
listCommonAnimalsAboveAverage =
            List.filter aboveCommonMean animalCommonClassList
listCommonAnimalsBelowAverage =
            List.filter belowCommonMean animalCommonClassList
--Endangered
aboveEndangeredMean animal = 
            animal.x > meanEndangeredHeight
belowEndangeredMean animal = 
            animal.x < meanEndangeredHeight
meanEndangeredHeight = 
            getHeightAverage animalEndangeredClassList
animalEndangeredClassList =
            getEndangeredClassList animalFiltered
listEndangeredAnimalsAboveAverage =
            List.filter aboveEndangeredMean animalEndangeredClassList
listEndangeredAnimalsBelowAverage =
            List.filter belowEndangeredMean animalEndangeredClassList
aboveSecuredMean animal = 
            animal.x > meanSecuredHeight
belowSecuredMean animal = 
            animal.x < meanSecuredHeight
meanSecuredHeight = 
            getHeightAverage animalSecuredClassList 
animalSecuredClassList =
            getSecuredClassList animalFiltered            
listSecuredAnimalsAboveAverage =
            List.filter aboveSecuredMean animalSecuredClassList
listSecuredAnimalsBelowAverage =
            List.filter belowSecuredMean animalSecuredClassList  
            --Critically_Endangered
aboveCriticallyMean animal = 
            animal.x > meanCriticallyHeight
belowCriticallyMean animal = 
            animal.x < meanCriticallyHeight
meanCriticallyHeight = 
            getHeightAverage animalCriticallyClassList 
animalCriticallyClassList =
            getCriticallyClassList animalFiltered            
listCriticallyAnimalsAboveAverage =
            List.filter aboveCriticallyMean animalCriticallyClassList
listCriticallyAnimalsBelowAverage =
            List.filter belowCriticallyMean animalCriticallyClassList  
            --Threatened_with_Extinction
aboveThreatenedMean animal = 
            animal.x > meanThreatenedHeight
belowThreatenedMean animal = 
            animal.x < meanThreatenedHeight
meanThreatenedHeight = 
            getHeightAverage animalThreatenedClassList 
animalThreatenedClassList =
            getThreatenedClassList animalFiltered            
listThreatenedAnimalsAboveAverage =
            List.filter aboveThreatenedMean animalThreatenedClassList
listThreatenedAnimalsBelowAverage =
            List.filter belowThreatenedMean animalThreatenedClassList  
            --Hardly_Endangered
aboveHardlyMean animal = 
            animal.x > meanHardlyHeight
belowHardlyMean animal = 
            animal.x < meanHardlyHeight
meanHardlyHeight = 
            getHeightAverage animalHardlyClassList 
animalHardlyClassList =
            getHardlyClassList animalFiltered            
listHardlyAnimalsAboveAverage =
            List.filter aboveHardlyMean animalHardlyClassList
listHardlyAnimalsBelowAverage =
            List.filter belowHardlyMean animalHardlyClassList  
          --No_Information
aboveInformationMean animal = 
            animal.x > meanInformationHeight
belowInformationMean animal = 
            animal.x < meanInformationHeight
meanInformationHeight = 
            getHeightAverage animalInformationClassList 
animalInformationClassList =
            getInformationClassList animalFiltered            
listInformationAnimalsAboveAverage =
            List.filter aboveInformationMean animalInformationClassList
listInformationAnimalsBelowAverage =
            List.filter belowInformationMean animalInformationClassList  
          
            --Rare
aboveRareMean animal = 
            animal.x > meanRareHeight
belowRareMean animal = 
        animal.x < meanRareHeight
meanRareHeight = 
            getHeightAverage animalRareClassList 
animalRareClassList =
            getRareClassList animalFiltered            
listRareAnimalsAboveAverage =
            List.filter aboveRareMean animalRareClassList
listRareAnimalsBelowAverage =
            List.filter belowRareMean animalRareClassList    
--for all Scatterplots
animalFiltered =
            filterAndReduceAnimals animals
--Above meanHeight
aboveHeightAll =  aboveMeanHeight (filterAnimals animals) meanAllHeight
belowHeightAll = belowMeanHeight (filterAnimals animals) meanAllHeight
aboveCommonHeight = aboveHeight (filterAnimals animals ) Common meanCommonHeight
belowCommonHeight = belowHeight (filterAnimals animals) Common meanCommonHeight
aboveEndangeredHeight = aboveHeight (filterAnimals animals ) Endangered meanEndangeredHeight
belowEndangeredHeight = belowHeight (filterAnimals animals) Endangered meanEndangeredHeight
aboveHardlyHeight = aboveHeight (filterAnimals animals ) Hardly_Endangered meanHardlyHeight
belowHardlyHeight = belowHeight (filterAnimals animals) Hardly_Endangered meanHardlyHeight
aboveCriticallyHeight = aboveHeight (filterAnimals animals ) Critically_Endangered meanCriticallyHeight
belowCriticallyHeight = belowHeight (filterAnimals animals) Critically_Endangered meanCriticallyHeight
aboveSecuredHeight = aboveHeight (filterAnimals animals ) Secured meanSecuredHeight
belowSecuredHeight = belowHeight (filterAnimals animals) Secured meanSecuredHeight
aboveInformationHeight = aboveHeight (filterAnimals animals ) No_Information meanInformationHeight
belowInformationHeight = belowHeight (filterAnimals animals) No_Information meanInformationHeight
aboveThreatenedHeight = aboveHeight (filterAnimals animals ) Threatened_with_Extinction meanThreatenedHeight
belowThreatenedHeight = belowHeight (filterAnimals animals) Threatened_with_Extinction meanThreatenedHeight

--Piecharts
--piechartdata
pieAllafrica_above = specificAboveHeight (filterAnimals animals)  Africa meanAllHeight
pieAllsouth_America_above = specificAboveHeight (filterAnimals animals)  South_America meanAllHeight
pieAllnorth_America_above = specificAboveHeight (filterAnimals animals)  North_America meanAllHeight
pieAllasia_above = specificAboveHeight (filterAnimals animals)  Asia meanAllHeight
pieAllaustralia_above = specificAboveHeight (filterAnimals animals)  Australia meanAllHeight
pieAlleurope_above = specificAboveHeight (filterAnimals animals)  Europe meanAllHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

aboveAlldata : List ( String, Float )
aboveAlldata =
    [ ( "Africa ", toFloat(List.length pieAllafrica_above) )
    , ( "South_America",  toFloat(List.length pieAllsouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieAllnorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieAlleurope_above))
    , ( "Asia ",  toFloat(List.length pieAllasia_above) )
    , ( "Australia",  toFloat(List.length pieAllaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieAllafrica_below = specificBelowMeanHeight (filterAnimals animals)  Africa meanAllHeight
pieAllsouth_America_below = specificBelowMeanHeight(filterAnimals animals)  South_America meanAllHeight
pieAllnorth_America_below = specificBelowMeanHeight (filterAnimals animals)  North_America meanAllHeight
pieAllasia_below = specificBelowMeanHeight (filterAnimals animals)  Asia meanAllHeight
pieAllaustralia_below = specificBelowMeanHeight (filterAnimals animals)  Australia meanAllHeight
pieAlleurope_below = specificBelowMeanHeight (filterAnimals animals)  Europe meanAllHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowAlldata : List ( String, Float )
belowAlldata =
    [ ( "Africa ", toFloat(List.length pieCommonafrica_below) )
    , ( "South_America",  toFloat(List.length pieCommonsouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieCommonnorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieCommoneurope_below))
    , ( "Asia ",  toFloat(List.length pieCommonasia_below) )
    , ( "Australia",  toFloat(List.length pieCommonaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieCommonafrica_above = specificAboveHeightType (filterAnimals animals) Common Africa meanCommonHeight
pieCommonsouth_America_above = specificAboveHeightType (filterAnimals animals) Common South_America meanCommonHeight
pieCommonnorth_America_above = specificAboveHeightType (filterAnimals animals) Common North_America meanCommonHeight
pieCommonasia_above = specificAboveHeightType (filterAnimals animals) Common Asia meanCommonHeight
pieCommonaustralia_above = specificAboveHeightType (filterAnimals animals) Common Australia meanCommonHeight
pieCommoneurope_above = specificAboveHeightType (filterAnimals animals) Common Europe meanCommonHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

abovecommondata : List ( String, Float )
abovecommondata =
    [ ( "Africa ", toFloat(List.length pieCommonafrica_above) )
    , ( "South_America",  toFloat(List.length pieCommonsouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieCommonnorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieCommoneurope_above))
    , ( "Asia ",  toFloat(List.length pieCommonasia_above) )
    , ( "Australia",  toFloat(List.length pieCommonaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieCommonafrica_below = specificBelowMeanHeightType (filterAnimals animals) Common Africa meanCommonHeight
pieCommonsouth_America_below = specificBelowMeanHeightType(filterAnimals animals) Common South_America meanCommonHeight
pieCommonnorth_America_below = specificBelowMeanHeightType (filterAnimals animals) Common North_America meanCommonHeight
pieCommonasia_below = specificBelowMeanHeightType (filterAnimals animals) Common Asia meanCommonHeight
pieCommonaustralia_below = specificBelowMeanHeightType (filterAnimals animals) Common Australia meanCommonHeight
pieCommoneurope_below = specificBelowMeanHeightType (filterAnimals animals) Common Europe meanCommonHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowcommondata : List ( String, Float )
belowcommondata =
    [ ( "Africa ", toFloat(List.length pieCommonafrica_below) )
    , ( "South_America",  toFloat(List.length pieCommonsouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieCommonnorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieCommoneurope_below))
    , ( "Asia ",  toFloat(List.length pieCommonasia_below) )
    , ( "Australia",  toFloat(List.length pieCommonaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieThreatenedafrica_above = specificAboveHeightType (filterAnimals animals) Threatened_with_Extinction Africa meanThreatenedHeight
pieThreatenedsouth_America_above = specificAboveHeightType (filterAnimals animals) Threatened_with_Extinction South_America meanThreatenedHeight
pieThreatenednorth_America_above = specificAboveHeightType (filterAnimals animals) Threatened_with_Extinction North_America meanThreatenedHeight
pieThreatenedasia_above = specificAboveHeightType (filterAnimals animals) Threatened_with_Extinction Asia meanThreatenedHeight
pieThreatenedaustralia_above = specificAboveHeightType (filterAnimals animals) Threatened_with_Extinction Australia meanThreatenedHeight
pieThreatenedeurope_above = specificAboveHeightType (filterAnimals animals) Threatened_with_Extinction Europe meanThreatenedHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

abovethreateneddata : List ( String, Float )
abovethreateneddata =
    [ ( "Africa ", toFloat(List.length pieThreatenedafrica_above) )
    , ( "South_America",  toFloat(List.length pieThreatenedsouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieThreatenednorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieThreatenedeurope_above))
    , ( "Asia ",  toFloat(List.length pieThreatenedasia_above) )
    , ( "Australia",  toFloat(List.length pieThreatenedaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieThreatenedafrica_below = specificBelowMeanHeightType (filterAnimals animals) Threatened_with_Extinction Africa meanThreatenedHeight
pieThreatenedsouth_America_below = specificBelowMeanHeightType(filterAnimals animals) Threatened_with_Extinction South_America meanThreatenedHeight
pieThreatenednorth_America_below = specificBelowMeanHeightType (filterAnimals animals) Threatened_with_Extinction North_America meanThreatenedHeight
pieThreatenedasia_below = specificBelowMeanHeightType (filterAnimals animals) Threatened_with_Extinction Asia meanThreatenedHeight
pieThreatenedaustralia_below = specificBelowMeanHeightType (filterAnimals animals) Threatened_with_Extinction Australia meanThreatenedHeight
pieThreatenedeurope_below = specificBelowMeanHeightType (filterAnimals animals) Threatened_with_Extinction Europe meanThreatenedHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowThreateneddata : List ( String, Float )
belowThreateneddata =
    [ ( "Africa ", toFloat(List.length pieThreatenedafrica_below) )
    , ( "South_America",  toFloat(List.length pieThreatenedsouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieThreatenednorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieThreatenedeurope_below))
    , ( "Asia ",  toFloat(List.length pieThreatenedasia_below) )
    , ( "Australia",  toFloat(List.length pieThreatenedaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieEndangeredafrica_above = specificAboveHeightType (filterAnimals animals) Endangered Africa meanEndangeredHeight
pieEndangeredsouth_America_above = specificAboveHeightType (filterAnimals animals) Endangered South_America meanEndangeredHeight
pieEndangerednorth_America_above = specificAboveHeightType (filterAnimals animals) Endangered North_America meanEndangeredHeight
pieEndangeredasia_above = specificAboveHeightType (filterAnimals animals) Endangered Asia meanEndangeredHeight
pieEndangeredaustralia_above = specificAboveHeightType (filterAnimals animals) Endangered Australia meanEndangeredHeight
pieEndangeredeurope_above = specificAboveHeightType (filterAnimals animals) Endangered Europe meanEndangeredHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

aboveEndangereddata : List ( String, Float )
aboveEndangereddata =
    [ ( "Africa ", toFloat(List.length pieEndangeredafrica_above) )
    , ( "South_America",  toFloat(List.length pieEndangeredsouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieEndangerednorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieEndangeredeurope_above))
    , ( "Asia ",  toFloat(List.length pieEndangeredasia_above) )
    , ( "Australia",  toFloat(List.length pieEndangeredaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieEndangeredafrica_below = specificBelowMeanHeightType (filterAnimals animals) Endangered Africa meanEndangeredHeight
pieEndangeredsouth_America_below = specificBelowMeanHeightType(filterAnimals animals) Endangered South_America meanEndangeredHeight
pieEndangerednorth_America_below = specificBelowMeanHeightType (filterAnimals animals) Endangered North_America meanEndangeredHeight
pieEndangeredasia_below = specificBelowMeanHeightType (filterAnimals animals) Endangered Asia meanEndangeredHeight
pieEndangeredaustralia_below = specificBelowMeanHeightType (filterAnimals animals) Endangered Australia meanEndangeredHeight
pieEndangeredeurope_below = specificBelowMeanHeightType (filterAnimals animals) Endangered Europe meanEndangeredHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowEndangereddata : List ( String, Float )
belowEndangereddata =
    [ ( "Africa ", toFloat(List.length pieEndangeredafrica_below) )
    , ( "South_America",  toFloat(List.length pieEndangeredsouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieEndangerednorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieEndangeredeurope_below))
    , ( "Asia ",  toFloat(List.length pieEndangeredasia_below) )
    , ( "Australia",  toFloat(List.length pieEndangeredaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieInformationafrica_above = specificAboveHeightType (filterAnimals animals) No_Information Africa meanInformationHeight
pieInformationsouth_America_above = specificAboveHeightType (filterAnimals animals) No_Information South_America meanInformationHeight
pieInformationnorth_America_above = specificAboveHeightType (filterAnimals animals) No_Information North_America meanInformationHeight
pieInformationasia_above = specificAboveHeightType (filterAnimals animals) No_Information Asia meanInformationHeight
pieInformationaustralia_above = specificAboveHeightType (filterAnimals animals) No_Information Australia meanInformationHeight
pieInformationeurope_above = specificAboveHeightType (filterAnimals animals) No_Information Europe meanInformationHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

aboveInformationdata : List ( String, Float )
aboveInformationdata =
    [ ( "Africa ", toFloat(List.length pieInformationafrica_above) )
    , ( "South_America",  toFloat(List.length pieInformationsouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieInformationnorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieInformationeurope_above))
    , ( "Asia ",  toFloat(List.length pieInformationasia_above) )
    , ( "Australia",  toFloat(List.length pieInformationaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieInformationafrica_below = specificBelowMeanHeightType (filterAnimals animals) No_Information Africa meanInformationHeight
pieInformationsouth_America_below = specificBelowMeanHeightType(filterAnimals animals) No_Information South_America meanInformationHeight
pieInformationnorth_America_below = specificBelowMeanHeightType (filterAnimals animals) No_Information North_America meanInformationHeight
pieInformationasia_below = specificBelowMeanHeightType (filterAnimals animals) No_Information Asia meanInformationHeight
pieInformationaustralia_below = specificBelowMeanHeightType (filterAnimals animals) No_Information Australia meanInformationHeight
pieInformationeurope_below = specificBelowMeanHeightType (filterAnimals animals) No_Information Europe meanInformationHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowInformationdata : List ( String, Float )
belowInformationdata =
    [ ( "Africa ", toFloat(List.length pieInformationafrica_below) )
    , ( "South_America",  toFloat(List.length pieInformationsouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieInformationnorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieInformationeurope_below))
    , ( "Asia ",  toFloat(List.length pieInformationasia_below) )
    , ( "Australia",  toFloat(List.length pieInformationaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieHardlyafrica_above = specificAboveHeightType (filterAnimals animals) Hardly_Endangered Africa meanHardlyHeight
pieHardlysouth_America_above = specificAboveHeightType (filterAnimals animals) Hardly_Endangered South_America meanHardlyHeight
pieHardlynorth_America_above = specificAboveHeightType (filterAnimals animals) Hardly_Endangered North_America meanHardlyHeight
pieHardlyasia_above = specificAboveHeightType (filterAnimals animals) Hardly_Endangered Asia meanHardlyHeight
pieHardlyaustralia_above = specificAboveHeightType (filterAnimals animals) Hardly_Endangered Australia meanHardlyHeight
pieHardlyeurope_above = specificAboveHeightType (filterAnimals animals) Hardly_Endangered Europe meanHardlyHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

aboveHardlydata : List ( String, Float )
aboveHardlydata =
    [ ( "Africa ", toFloat(List.length pieHardlyafrica_above) )
    , ( "South_America",  toFloat(List.length pieHardlysouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieHardlynorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieHardlyeurope_above))
    , ( "Asia ",  toFloat(List.length pieHardlyasia_above) )
    , ( "Australia",  toFloat(List.length pieHardlyaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieHardlyafrica_below = specificBelowMeanHeightType (filterAnimals animals) Hardly_Endangered Africa meanHardlyHeight
pieHardlysouth_America_below = specificBelowMeanHeightType(filterAnimals animals) Hardly_Endangered South_America meanHardlyHeight
pieHardlynorth_America_below = specificBelowMeanHeightType (filterAnimals animals) Hardly_Endangered North_America meanHardlyHeight
pieHardlyasia_below = specificBelowMeanHeightType (filterAnimals animals) Hardly_Endangered Asia meanHardlyHeight
pieHardlyaustralia_below = specificBelowMeanHeightType (filterAnimals animals) Hardly_Endangered Australia meanHardlyHeight
pieHardlyeurope_below = specificBelowMeanHeightType (filterAnimals animals) Hardly_Endangered Europe meanHardlyHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowHardlydata : List ( String, Float )
belowHardlydata =
    [ ( "Africa ", toFloat(List.length pieHardlyafrica_below) )
    , ( "South_America",  toFloat(List.length pieHardlysouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieHardlynorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieHardlyeurope_below))
    , ( "Asia ",  toFloat(List.length pieHardlyasia_below) )
    , ( "Australia",  toFloat(List.length pieHardlyaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieSecuredafrica_above = specificAboveHeightType (filterAnimals animals) Secured Africa meanSecuredHeight
pieSecuredsouth_America_above = specificAboveHeightType (filterAnimals animals) Secured South_America meanSecuredHeight
pieSecurednorth_America_above = specificAboveHeightType (filterAnimals animals) Secured North_America meanSecuredHeight
pieSecuredasia_above = specificAboveHeightType (filterAnimals animals) Secured Asia meanSecuredHeight
pieSecuredaustralia_above = specificAboveHeightType (filterAnimals animals) Secured Australia meanSecuredHeight
pieSecuredeurope_above = specificAboveHeightType (filterAnimals animals) Secured Europe meanSecuredHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

aboveSecureddata : List ( String, Float )
aboveSecureddata =
    [ ( "Africa ", toFloat(List.length pieSecuredafrica_above) )
    , ( "South_America",  toFloat(List.length pieSecuredsouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieSecurednorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieSecuredeurope_above))
    , ( "Asia ",  toFloat(List.length pieSecuredasia_above) )
    , ( "Australia",  toFloat(List.length pieSecuredaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieSecuredafrica_below = specificBelowMeanHeightType (filterAnimals animals) Secured Africa meanSecuredHeight
pieSecuredsouth_America_below = specificBelowMeanHeightType(filterAnimals animals) Secured South_America meanSecuredHeight
pieSecurednorth_America_below = specificBelowMeanHeightType (filterAnimals animals) Secured North_America meanSecuredHeight
pieSecuredasia_below = specificBelowMeanHeightType (filterAnimals animals) Secured Asia meanSecuredHeight
pieSecuredaustralia_below = specificBelowMeanHeightType (filterAnimals animals) Secured Australia meanSecuredHeight
pieSecuredeurope_below = specificBelowMeanHeightType (filterAnimals animals) Secured Europe meanSecuredHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowSecureddata : List ( String, Float )
belowSecureddata =
    [ ( "Africa ", toFloat(List.length pieSecuredafrica_below) )
    , ( "South_America",  toFloat(List.length pieSecuredsouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieSecurednorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieSecuredeurope_below))
    , ( "Asia ",  toFloat(List.length pieSecuredasia_below) )
    , ( "Australia",  toFloat(List.length pieSecuredaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
pieCriticallyafrica_above = specificAboveHeightType (filterAnimals animals) Critically_Endangered Africa meanCriticallyHeight
pieCriticallysouth_America_above = specificAboveHeightType (filterAnimals animals)  Critically_Endangered South_America meanCriticallyHeight
pieCriticallynorth_America_above = specificAboveHeightType (filterAnimals animals)  Critically_Endangered North_America meanCriticallyHeight
pieCriticallyasia_above = specificAboveHeightType (filterAnimals animals)  Critically_Endangered Asia meanCriticallyHeight
pieCriticallyaustralia_above = specificAboveHeightType (filterAnimals animals)  Critically_Endangered Australia meanCriticallyHeight
pieCriticallyeurope_above = specificAboveHeightType (filterAnimals animals)  Critically_Endangered Europe meanCriticallyHeight
--pieCommonarctic_above = specificAboveHeightType (filterAnimals animals) Common Arctic meanCommonHeight

aboveCriticallydata : List ( String, Float )
aboveCriticallydata =
    [ ( "Africa ", toFloat(List.length pieCriticallyafrica_above) )
    , ( "South_America",  toFloat(List.length pieCriticallysouth_America_above) )
    , ( "North_America ",  toFloat(List.length pieCriticallynorth_America_above) )
    , ( "Europe ",  toFloat(List.length pieCriticallyeurope_above))
    , ( "Asia ",  toFloat(List.length pieCriticallyasia_above) )
    , ( "Australia",  toFloat(List.length pieCriticallyaustralia_above) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_above) )
    ]
pieCriticallyafrica_below = specificBelowMeanHeightType (filterAnimals animals)  Critically_Endangered Africa meanCriticallyHeight
pieCriticallysouth_America_below = specificBelowMeanHeightType(filterAnimals animals)  Critically_Endangered South_America meanCriticallyHeight
pieCriticallynorth_America_below = specificBelowMeanHeightType (filterAnimals animals)  Critically_Endangered North_America meanCriticallyHeight
pieCriticallyasia_below = specificBelowMeanHeightType (filterAnimals animals)  Critically_Endangered Asia meanCriticallyHeight
pieCriticallyaustralia_below = specificBelowMeanHeightType (filterAnimals animals)  Critically_Endangered Australia meanCriticallyHeight
pieCriticallyeurope_below = specificBelowMeanHeightType (filterAnimals animals)  Critically_Endangered Europe meanCriticallyHeight
--pieCommonarctic_below =specificBelowMeanHeightType (filterAnimals animals) Common Arctic meanCommonHeight

belowCriticallydata : List ( String, Float )
belowCriticallydata =
    [ ( "Africa ", toFloat(List.length pieCriticallyafrica_below) )
    , ( "South_America",  toFloat(List.length pieCriticallysouth_America_below) )
    , ( "North_America ",  toFloat(List.length pieCriticallynorth_America_below) )
    , ( "Europe ",  toFloat(List.length pieCriticallyeurope_below))
    , ( "Asia ",  toFloat(List.length pieCriticallyasia_below) )
    , ( "Australia",  toFloat(List.length pieCriticallyaustralia_below) )
  --  , ( "Artic ",  toFloat(List.length pieCommonarctic_below  ) )
    ]
w1 : Float
w1 =
     990

h1 : Float
h1 = 
    504
colors : Array Color 
colors =
    Array.fromList
    [ Color.rgb255 152 171 198
    , Color.rgb255 138 137 166
    , Color.rgb255 123 104 136
    , Color.rgb255 107  72 107
    , Color.rgb255 159  92  85
    , Color.rgb255 208 116  60
    , Color.rgb255 255  96   0
    ]
pieradius : Float
pieradius =
     Basics.min w1 h1 / 2
pieview : List(String, Float) -> Svg msg
pieview model =
    let 
       pieData =
        model |> List.map Tuple.second |> Shape.pie {defaultPieConfig | outerRadius = pieradius}
       makeSlice index datum = 
         Path.element (Shape.arc datum) [fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors, stroke <| Paint Color.white]
       makeLabel slice (label, value) = 
         let 
             (x, y) =
                Shape.centroid { slice | innerRadius = pieradius - 40, outerRadius = pieradius - 40}
         in 
          text_
           [ transform [Translate x y] 
           , dy (TypedSvg.Types.em 0.35)
           , textAnchor AnchorMiddle
           ]
           [TypedSvg.Core.text label]
    in 
     svg [viewBox 0 0 w1 h1]
      [g [transform [Translate (w1 / 2) (h1 / 2) ] ] 
         [ g [] <| List.indexedMap makeSlice pieData
         , g [] <| List.map2 makeLabel pieData model
         ]
      ]

accessor : Bar.Accessor ( String, Float )
accessor =
    Bar.Accessor (always "") Tuple.first Tuple.second

--common
commonchart : Html msg
commonchart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( commondata, accessor )




africac = specificAreaType (filterAnimals animals) Common Africa
south_Americac = specificAreaType (filterAnimals animals) Common South_America
north_Americac = specificAreaType (filterAnimals animals) Common North_America
asiac = specificAreaType (filterAnimals animals) Common Asia
australiac = specificAreaType (filterAnimals animals) Common Australia
europec = specificAreaType (filterAnimals animals) Common Europe
arcticc = specificAreaType (filterAnimals animals) Common Arctic

commondata : List ( String, Float )
commondata =
    [ ( "Africa ", toFloat(List.length africac) )
    , ( "South_America",  toFloat(List.length south_Americac) )
    , ( "North_America ",  toFloat(List.length north_Americac) )
    , ( "Europe ",  toFloat(List.length europec))
    , ( "Asia ",  toFloat(List.length asiac) )
    , ( "Australia",  toFloat(List.length australiac) )
    , ( "Artic ",  toFloat(List.length arcticc) )
    ]
africa = countAllAnimals (filterAnimals animals) Africa
south_America = countAllAnimals (filterAnimals animals) South_America
north_America = countAllAnimals (filterAnimals animals) North_America
europe = countAllAnimals (filterAnimals animals ) Europe
asia = countAllAnimals (filterAnimals animals ) Asia
australia = countAllAnimals (filterAnimals animals) Australia
--Endangered
endangeredchart : Html msg
endangeredchart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( endangereddata, accessor )




africae = specificAreaType (filterAnimals animals) Endangered Africa
south_Americae = specificAreaType (filterAnimals animals) Endangered South_America
north_Americae = specificAreaType (filterAnimals animals) Endangered North_America
asiae = specificAreaType (filterAnimals animals) Endangered Asia
australiae = specificAreaType (filterAnimals animals) Endangered Australia
europee = specificAreaType (filterAnimals animals) Endangered Europe
arctice = specificAreaType (filterAnimals animals) Endangered Arctic

endangereddata : List ( String, Float )
endangereddata =
    [ ( "Africa ", toFloat(List.length africae) )
    , ( "South_America",  toFloat(List.length south_Americae) )
    , ( "North_America ",  toFloat(List.length north_Americae) )
    , ( "Europe ",  toFloat(List.length europee))
    , ( "Asia ",  toFloat(List.length asiae) )
    , ( "Australia",  toFloat(List.length australiae) )
    , ( "Artic ",  toFloat(List.length arctice) )
    ]

--Hardly_Endangered
hardlychart : Html msg
hardlychart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( hardlydata, accessor )




africahardly = specificAreaType (filterAnimals animals) Hardly_Endangered Africa
south_Americahardly = specificAreaType (filterAnimals animals) Hardly_Endangered South_America
north_Americahardly = specificAreaType (filterAnimals animals) Hardly_Endangered North_America
asiahardly = specificAreaType (filterAnimals animals) Hardly_Endangered Asia
australiahardly = specificAreaType (filterAnimals animals) Hardly_Endangered Australia
europehardly = specificAreaType (filterAnimals animals) Hardly_Endangered Europe
arctichardly = specificAreaType (filterAnimals animals) Hardly_Endangered Arctic

hardlydata : List ( String, Float )
hardlydata =
    [ ( "Africa ", toFloat(List.length africahardly) )
    , ( "South_America",  toFloat(List.length south_Americahardly) )
    , ( "North_America ",  toFloat(List.length north_Americahardly) )
    , ( "Europe ",  toFloat(List.length europehardly))
    , ( "Asia ",  toFloat(List.length asiahardly) )
    , ( "Australia",  toFloat(List.length australiahardly) )
    , ( "Artic ",  toFloat(List.length arctichardly) )
    ]

--Critically_Endangered
criticallychart : Html msg
criticallychart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( criticallydata, accessor )




africacritically = specificAreaType (filterAnimals animals) Critically_Endangered Africa
south_Americacritically = specificAreaType (filterAnimals animals) Critically_Endangered South_America
north_Americacritically = specificAreaType (filterAnimals animals) Critically_Endangered North_America
asiacritically = specificAreaType (filterAnimals animals) Critically_Endangered Asia
australiacritically = specificAreaType (filterAnimals animals) Critically_Endangered Australia
europecritically = specificAreaType (filterAnimals animals) Critically_Endangered Europe
arcticcritically = specificAreaType (filterAnimals animals) Critically_Endangered Arctic

criticallydata : List ( String, Float )
criticallydata =
    [ ( "Africa ", toFloat(List.length africacritically) )
    , ( "South_America",  toFloat(List.length south_Americacritically) )
    , ( "North_America ",  toFloat(List.length north_Americacritically) )
    , ( "Europe ",  toFloat(List.length europecritically))
    , ( "Asia ",  toFloat(List.length asiacritically) )
    , ( "Australia",  toFloat(List.length australiacritically) )
    , ( "Artic ",  toFloat(List.length arcticcritically) )
    ]
--Secured
securedchart : Html msg
securedchart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( secureddata, accessor )




africasecured = specificAreaType (filterAnimals animals) Secured Africa
south_Americasecured = specificAreaType (filterAnimals animals) Secured South_America
north_Americasecured = specificAreaType (filterAnimals animals) Secured North_America
asiasecured = specificAreaType (filterAnimals animals) Secured Asia
australiasecured = specificAreaType (filterAnimals animals) Secured Australia
europesecured = specificAreaType (filterAnimals animals) Secured Europe
arcticsecured = specificAreaType (filterAnimals animals) Secured Arctic

secureddata : List ( String, Float )
secureddata =
    [ ( "Africa ", toFloat(List.length africasecured) )
    , ( "South_America",  toFloat(List.length south_Americasecured) )
    , ( "North_America ",  toFloat(List.length north_Americasecured) )
    , ( "Europe ",  toFloat(List.length europesecured))
    , ( "Asia ",  toFloat(List.length asiasecured) )
    , ( "Australia",  toFloat(List.length australiasecured) )
    , ( "Artic ",  toFloat(List.length arcticsecured) )
    ]
--Rare
rarechart : Html msg
rarechart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( raredata, accessor )




africarare = specificAreaType (filterAnimals animals) Rare Africa
south_Americarare = specificAreaType (filterAnimals animals) Rare South_America
north_Americarare = specificAreaType (filterAnimals animals) Rare North_America
asiarare = specificAreaType (filterAnimals animals) Rare Asia
australiarare = specificAreaType (filterAnimals animals) Rare Australia
europerare = specificAreaType (filterAnimals animals) Rare Europe
arcticrare = specificAreaType (filterAnimals animals) Rare Arctic

raredata : List ( String, Float )
raredata =
    [ ( "Africa ", toFloat(List.length africarare) )
    , ( "South_America",  toFloat(List.length south_Americarare) )
    , ( "North_America ",  toFloat(List.length north_Americarare) )
    , ( "Europe ",  toFloat(List.length europerare))
    , ( "Asia ",  toFloat(List.length asiarare) )
    , ( "Australia",  toFloat(List.length australiarare) )
    , ( "Artic ",  toFloat(List.length arcticrare) )
    ]
--No_Information
informationchart : Html msg
informationchart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( informationdata, accessor )




africainformation = specificAreaType (filterAnimals animals) No_Information Africa
south_Americainformation = specificAreaType (filterAnimals animals) No_Information South_America
north_Americainformation = specificAreaType (filterAnimals animals) No_Information North_America
asiainformation = specificAreaType (filterAnimals animals) No_Information Asia
australiainformation = specificAreaType (filterAnimals animals) No_Information Australia
europeinformation = specificAreaType (filterAnimals animals) No_Information Europe
arcticinformation = specificAreaType (filterAnimals animals) No_Information Arctic

informationdata : List ( String, Float )
informationdata =
    [ ( "Africa ", toFloat(List.length africainformation) )
    , ( "South_America",  toFloat(List.length south_Americainformation) )
    , ( "North_America ",  toFloat(List.length north_Americainformation) )
    , ( "Europe ",  toFloat(List.length europeinformation))
    , ( "Asia ",  toFloat(List.length asiainformation) )
    , ( "Australia",  toFloat(List.length australiainformation) )
    , ( "Artic ",  toFloat(List.length arcticinformation) )
    ]
--Threatened
threatenedchart : Html msg
threatenedchart =
    Bar.init
        |> Bar.setHeight 400
        |> Bar.setTitle "Continents"
        |> Bar.setDesc "This bar shows the distribution of the mammals"
        |> Bar.setDomainLinear ( 0, 100 )
        |> Bar.setAxisYTickCount 5
        |> Bar.setDimensions
            { margin = { top = 10, right = 10, bottom = 30, left = 30 }
            , width = 600
            , height = 400
            }
        |> Bar.render ( threateneddata, accessor )




africathreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction Africa
south_Americathreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction South_America
north_Americathreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction North_America
asiathreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction Asia
australiathreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction Australia
europethreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction Europe
arcticthreatened = specificAreaType (filterAnimals animals) Threatened_with_Extinction Arctic

threateneddata : List ( String, Float )
threateneddata =
    [ ( "Africa ", toFloat(List.length africathreatened) )
    , ( "South_America",  toFloat(List.length south_Americathreatened) )
    , ( "North_America ",  toFloat(List.length north_Americathreatened) )
    , ( "Europe ",  toFloat(List.length europethreatened))
    , ( "Asia ",  toFloat(List.length asiathreatened) )
    , ( "Australia",  toFloat(List.length australiathreatened) )
    , ( "Artic ",  toFloat(List.length arcticthreatened) )
    ]
--Scatterplots
w : Float
w =
    500


h : Float
h =
    500


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        scale : ( Float, Float )
        scale =
            Maybe.withDefault defaultExtent (Statistics.extent values)

        range : Float
        range =
            -(Tuple.first scale) + Tuple.second scale

        down : Float
        down =
            Tuple.first scale - range / toFloat (2 * tickCount)

        up : Float
        up =
            Tuple.second scale + range / toFloat (2 * tickCount)
    in
    if down < 0 then
        ( 0, up )

    else
        ( down, up )


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


type alias XyData =
    { xDescription : String
    , yDescription : String
    , uDescription : String
    , vDescription : String
    , data : List Point
    }

--Scatterplots
scatterplotStatus : XyData -> List Point -> List Point -> Float -> Svg msg
scatterplotStatus model aboveMean belowMean mean =
    let
        xValues : List Float
        xValues =
            List.map .u model.data

        yValues : List Float
        yValues =
            List.map .x model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
                .point text { display: none; }
                .point:hover text { display: inline;}
              """ ]
        , g
            [ transform [ Translate (padding - 1) (h - padding) ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x + 30)
                , y 30
                ]
                [ TypedSvg.Core.text "Age" ]
            ]
        , g
            [ transform [ Translate (padding - 1) padding ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [ yAxis yValues
            , text_
                [ x -30
                , y (Scale.convert yScaleLocal labelPositions.y - 30)
                ]
                [ TypedSvg.Core.text "Height" ]
            ]
        , g
            [ transform [ Translate padding padding ]
            , fill (TypedSvg.Types.Paint Color.grey)
            ]
            (List.map (pointRectangle xScaleLocal yScaleLocal) model.data)
        , g
            [ transform [ Translate padding padding ]
            , fill (TypedSvg.Types.Paint Color.blue)
            ]
            (List.map (pointColor xScaleLocal yScaleLocal) aboveMean)
        , g
            [ transform [ Translate padding padding ]
            , fill (TypedSvg.Types.Paint Color.red)
            ]
            (List.map (pointColor xScaleLocal yScaleLocal) belowMean)
        ]
getCommonClassList : XyData -> List Point
getCommonClassList listanimals =
    let
        dat : List Point
        dat =
            listanimals.data

        isTarget animalClass =
            animalClass.v == Common
    in
    List.filter isTarget dat
getEndangeredClassList: XyData -> List Point
getEndangeredClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Endangered
    in 
    List.filter isTarget dat

getCriticallyClassList: XyData -> List Point
getCriticallyClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Critically_Endangered
    in 
    List.filter isTarget dat
getThreatenedClassList: XyData -> List Point
getThreatenedClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Threatened_with_Extinction
    in 
    List.filter isTarget dat
getHardlyClassList: XyData -> List Point
getHardlyClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Hardly_Endangered
    in 
    List.filter isTarget dat
getInformationClassList: XyData -> List Point
getInformationClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == No_Information
    in 
    List.filter isTarget dat
getAllClassList : XyData -> List Point 
getAllClassList listanimals =
      let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Common ||
                               animalClass.v == Critically_Endangered ||  
                               animalClass.v == Endangered ||
                               animalClass.v == Secured ||
                               animalClass.v == Hardly_Endangered ||
                               animalClass.v == No_Information ||  
                               animalClass.v == Rare ||
                               animalClass.v == Threatened_with_Extinction
        
    in 
    List.filter isTarget dat
getSecuredClassList: XyData -> List Point
getSecuredClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Secured
    in 
    List.filter isTarget dat
 
getRareClassList: XyData -> List Point
getRareClassList listanimals =
    let 
        dat : List Point
        dat = listanimals.data

        isTarget animalClass = animalClass.v == Rare
    in 
    List.filter isTarget dat
  
type alias Point =
    { pointName : String, x : Float, y : Float, u : Float, v : Status }


pointColor : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
pointColor scaleX scaleY xyPoint =
    g
        [ TypedSvg.Attributes.class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]
        ]
        [ circle
            [ cx (Scale.convert scaleX xyPoint.u)
            , cy (Scale.convert scaleY xyPoint.x)
            , r radius
            ]
            []
        , text_
            [ transform [ Translate (Scale.convert scaleX xyPoint.u) (Scale.convert scaleY xyPoint.x - (2 * radius)) ]
            , textAnchor AnchorMiddle
            ]
            [ TypedSvg.Core.text xyPoint.pointName ]
        ]

pointCircle : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
pointCircle scaleX scaleY xyPoint =
    g
        [ TypedSvg.Attributes.class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]
        ]
        [ circle
            [ cx (Scale.convert scaleX xyPoint.u)
            , cy (Scale.convert scaleY xyPoint.x)
            , r radius
            ]
            []
        , text_
            [ transform [ Translate (Scale.convert scaleX xyPoint.u) (Scale.convert scaleY xyPoint.x - (2 * radius)) ]
            , textAnchor AnchorMiddle
            ]
            [ TypedSvg.Core.text xyPoint.pointName ]
        ]


pointRectangle : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
pointRectangle scaleX scaleY xyPoint =
    g
        [ TypedSvg.Attributes.class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]
        ]
        [ rect
            [ x (Scale.convert scaleX xyPoint.u - radius)
            , y (Scale.convert scaleY xyPoint.x - radius)
            , SAPX.width (2 * radius)
            , SAPX.height (2 * radius)
            ]
            []
        , text_
            [ transform [ Translate (Scale.convert scaleX xyPoint.u) (Scale.convert scaleY xyPoint.x - (2 * radius)) ]
            , textAnchor AnchorMiddle
            ]
            [ TypedSvg.Core.text xyPoint.pointName ]
        ]


filter : List Animal -> List Animal
filter listanimal =
    let
        filterFunction animal =
            animal.height
                /= Nothing
                && animal.way_of_life
                /= Nothing
                && animal.age
                /= Nothing
    in
    List.filter filterFunction listanimal


getName : String -> Maybe Int -> Maybe Int -> Maybe Int -> Status -> Point
getName n m p d t =
    let
        f x =
            Maybe.withDefault 0 x
    in
    Point (n ++ " (" ++ String.fromInt (f m) ++ ", " ++ String.fromInt (f d) ++ ")") (toFloat (f m)) (toFloat (f p)) (toFloat (f d)) t


animalToPoint : Animal -> Point
animalToPoint animal =
    getName animal.animalName animal.height animal.way_of_life animal.age animal.status


filterAndReduceAnimals : List Animal -> XyData
filterAndReduceAnimals my_animals =
    XyData "height" "way_of_life" "age" "status" (List.map animalToPoint (filter my_animals))

getHeightAverage : List Point -> Float
getHeightAverage listanimals =
    let
        getHeight animal =
            animal.x
    in
    List.sum (List.map getHeight listanimals) / toFloat (List.length listanimals)
--


animals : List Animal
animals =
   [ Animal "Bengalischer Tiger"  Critically_Endangered Asia (Just 100) (Just 1) (Just 260.0)  (Just 1)  (Just 26)
   , Animal "Grizzlybär" Secured  North_America (Just 105) (Just 1) (Just 450.0) (Just 1)  (Just 25)
   , Animal "Gepard" Critically_Endangered Africa (Just 80) (Just 1) (Just 60.0) (Just 1)  (Just 14)
   , Animal "Asiatischer Elefant" Critically_Endangered  Asia (Just 350) (Just 2) (Just 5000.0)  (Just 2)  (Just 70)
   , Animal "Wolf" Endangered  North_America (Just 96) (Just 4) (Just 60.0) (Just 1)    (Just 13)
   , Animal "Schimpanse" Threatened_with_Extinction Africa (Just 60) (Just 2) (Just 60.0) (Just 4)  (Just 40)
   , Animal "Großer Panda"  Threatened_with_Extinction Asia (Just 70) (Just 1) (Just 125.0) (Just 3)  (Just 15)
   , Animal "Löwe" Endangered Africa (Just 110) (Just 4) (Just 250.0) (Just 1) (Just 20)
   , Animal "Eisbär" Secured Arctic (Just 110) (Just 1)(Just 900.0) (Just 1)  (Just 25)
   , Animal "Erdmännchen" Common Africa (Just 35) (Just 2) (Just 0.9) (Just 3)  (Just 6)
   , Animal "Steppenzebra" Hardly_Endangered Africa (Just 140) (Just 5) (Just 385.0)  (Just 2)(Just 12)
   , Animal "Leopard" Secured Africa (Just 80) (Just 1)(Just 90.0) (Just 1)  (Just 20)
   , Animal "Rotes Riesenkänguru" Common Australia (Just 120) (Just 2) (Just 90.0) (Just 2) (Just 25)
   , Animal "Rotfuchs"  Common Europe (Just 40) (Just 3)  (Just 11.0) (Just 1)  (Just 10)
   , Animal "Orang-Utan" Threatened_with_Extinction Asia (Just 100) (Just 1) (Just 80.0) (Just 2)  (Just 50)
   , Animal "Eisfuchs"Secured North_America (Just 28) (Just 4) (Just 4.0) (Just 1)  (Just 10)
   , Animal "Großer Ameisenbär" Endangered South_America (Just 90) (Just 1) (Just 39.0) (Just 3)   (Just 20)
   , Animal "Thomsongazelle" Hardly_Endangered Africa (Just 65) (Just 5) (Just 30.0) (Just 2) (Just 13)
   , Animal "Spitzmaulnashorn" Threatened_with_Extinction Africa (Just 170) (Just 1) (Just 1300.0) (Just 2)  (Just 40)
   , Animal "Jaguar" Endangered South_America (Just 60) (Just 1)(Just 120.0) (Just 1)  (Just 22)
   , Animal "Totenkopfäffchen" Common South_America (Just 20) (Just 2) (Just 0.9) (Just 3)  (Just 20)
   , Animal "Giraffe" Endangered Africa (Just 400) (Just 2) (Just 1930.0)(Just 2) (Just 25)
   , Animal "Assapan" Common North_America (Just 15) (Just -1) (Just 0.1) (Just 2) (Just 10)
   , Animal "Steppengalago" Common Africa (Just 17) (Just -1) (Just 0.2) (Just 3) (Just 16)
   , Animal "Große Hufeisennase"Critically_Endangered Europe (Just 7) (Just 2) (Just 0.03) (Just 5) (Just 30)
   , Animal "Flachlandtapir"   Hardly_Endangered South_America (Just 100) (Just 1) (Just 250.0) (Just 2) (Just 30)
   , Animal "Matschie-Baumkänguru" Threatened_with_Extinction  Asia (Just 50) (Just 1) (Just 13.0) (Just 3) (Just 50)
   , Animal "Westeuropäischer Igel" Common Europe (Just 10) (Just 1)(Just 1.0) (Just 3)  (Just 10) 
   , Animal "Südafrikanisches Stachelschwein" Common Africa (Just 20)(Just -1) (Just 20.0) (Just 2)(Just 15)
   , Animal "Mandrill" Endangered Africa (Just 60)(Just 2) (Just 37.0)(Just 3)(Just 46)
   , Animal "Flusspferd" Secured Africa (Just 150)(Just 2) (Just 3200.0) (Just 2)(Just 50)
   , Animal "Schneeleopard" Critically_Endangered Asia (Just 60)(Just 1) (Just 55.0) (Just 1) (Just 18)
   , Animal "Katta" Endangered Africa (Just 40) (Just 2)  (Just 3.5) (Just 3)  (Just 20)
   , Animal "Damhirsch" Common Europe (Just 100) (Just 5)(Just 100.0) (Just 2) (Just 15)
   , Animal "Zwergmaus" Secured Europe (Just 1)(Just 1) (Just 0.07) (Just 3)(Just 2)
   , Animal "Siamang" Secured Asia (Just 90)(Just 3) (Just 15.0) (Just 2) (Just 40)
   , Animal "Dachs" Common Europe (Just 60) (Just 2) (Just 16.0)  (Just 4)(Just 15)
   , Animal "Fennek" No_Information Africa (Just 20) (Just 3)(Just 1.5)(Just 1) (Just 14)
   , Animal "Gaur" Endangered Asia (Just 220)(Just 5) (Just 1225.0) (Just 2) (Just 26)
   , Animal "Steppen-Schippentier"Secured Africa (Just 30) (Just 1) (Just 18.0) (Just 5) (Just 10)
   , Animal "Rotgesichtsmakak"No_Information Asia (Just 40) (Just 2) (Just 11.0) (Just 4) (Just 30)
   , Animal "Tüpelhyäne" Secured Africa (Just 90) (Just 4) (Just 85.0) (Just 1) (Just 25)
   , Animal "Schneeziege" Secured North_America (Just 122)(Just -1) (Just 140.0) (Just 2) (Just 18)
   , Animal "Wildschwein" Common Europe (Just 110) (Just 2) (Just 350.0) (Just 3)  (Just 27)
   , Animal "Sandkatze" Common Africa (Just 26) (Just 1)(Just 3.4) (Just 1)(Just 13)
 --  , Animal "Dromedar" Extinct_in_the_Wild Africa (Just 220)(Just -1) (Just 650.0) (Just 2) (Just 50)
   , Animal "Breitmaulnashorn" Secured Africa (Just 200)(Just 2) (Just 3600.0) (Just 2) (Just 45)
   , Animal "Koala" Secured Australia (Just 40)(Just 1) (Just 15.0) (Just 2) (Just 12)
   , Animal "Erdferkel" Secured Africa (Just 65) (Just 1) (Just 70.0) (Just 5)(Just 23)
   , Animal "Wasserschwein" Common South_America (Just 50) (Just 2) (Just 80.0) (Just 2) (Just 10)
   , Animal "Sibirischer Tiger" Threatened_with_Extinction Asia (Just 110)(Just 1) (Just 305.0)(Just 1) (Just 26)
   , Animal "Goldener Löwenaffe" Threatened_with_Extinction South_America (Just 20)(Just 2) (Just 0.8) (Just 3) (Just 15)
   , Animal "Dickhornschaf" Secured North_America (Just 100)(Just 2) (Just 143.0)(Just 2) (Just 24)
   , Animal "Gemeiner Vampir" Common South_America (Just 10) (Just 2) (Just 0.05) (Just 5) (Just 12)
   , Animal "Streifenschakal" No_Information Africa (Just 50)(Just 2) (Just 14.0) (Just 4) (Just 10)
   , Animal "Europäisches Wildkanninchen" Common Europe (Just 20)(Just 2) (Just 2.2) (Just 2)(Just 10)
   , Animal "Europäischer Mufflon" Endangered Europe (Just 75)(Just -1) (Just 55.0) (Just 2) (Just 20)
   , Animal "Afrikanischer Elefant" Critically_Endangered Africa (Just 400)(Just 2) (Just 6300) (Just 2) (Just 60)
   , Animal "Wollaffe" Endangered South_America (Just 60) (Just -1) (Just 11.0) (Just 3) (Just 25)
   , Animal "Springhase"Endangered Africa (Just 30)(Just 1) (Just 4.0) (Just 2) (Just 14)
   , Animal "Binturong" Common Asia (Just 60) (Just 1) (Just 14.0) (Just 1) (Just 20)
   , Animal "Kurzschnabeligel" Common  Australia (Just 10)(Just 1) (Just 7.0) (Just 5)(Just 50)
   , Animal "Östliches Graues Riesenkänguru" Secured Australia (Just 160)  (Just 2)(Just 90.0) (Just 2) (Just 20)
   , Animal "Großer Tanrek" Common Africa (Just 10)(Just 1) (Just 2.5) (Just 5)(Just 6)
   , Animal "Bartaffe" Critically_Endangered Asia (Just 40)(Just 2) (Just 9.0) (Just 3) (Just 20)
  -- , Animal "Polarhase" Common North_America (Just 20) (Just 2) (Just 7.0) (Just 2) (Just -1)
   , Animal "Alpaka" Common South_America (Just 105)(Just 2) (Just 65.0) (Just 2) (Just 20)
   , Animal "Nordluchs" Rare Asia (Just 75)(Just 1)  (Just 22.0)(Just 1) (Just 15)
   , Animal "Wanderratte" Common Europe (Just 10) (Just 2) (Just 0.4) (Just 4) (Just 3)
   , Animal "Kojote" Common North_America (Just 53)(Just 4) (Just 20.0)(Just 1)(Just 18)
   , Animal "Kragenbär" Endangered Asia (Just 100)(Just 1) (Just 150.0) (Just 4)(Just 25)
   , Animal "Streifenskunk" Common North_America (Just 30)(Just 1) (Just 6.0) (Just 3) (Just 3)
   , Animal "Streifengnu" Common Africa (Just 145)(Just 5) (Just 275.0) (Just 2)(Just 21)
   , Animal "Rothirsch" Common Europe (Just 150)(Just 5) (Just 340.0) (Just 2) (Just 15)
   , Animal "Schwarzschwanz-Präriehund" Hardly_Endangered North_America (Just 20)(Just 2) (Just 1.4)(Just 2) (Just 4)
   , Animal "Europäisches Eichhörnchen" Common Europe (Just 20)(Just 1) (Just 0.4) (Just 2)(Just 12)
   , Animal "Ringschwanzbeutler" Common Australia (Just 30)(Just 3) (Just 1.1)(Just 2) (Just 5)
   , Animal "Mauswiesel" Common Europe (Just 20)(Just 1) (Just 0.2) (Just 1)(Just 3)
   , Animal "Flachlandgorilla" Critically_Endangered Africa (Just 150)(Just 2) (Just 275.0)(Just 2)(Just 50)
   , Animal "Zwergfledermaus" Secured Europe (Just 5)(Just 2) (Just 0.003) (Just 5) (Just 6)
   , Animal "Nordopossum" Common North_America (Just 25)(Just 1) (Just 5.5) (Just 1) (Just 7)
   , Animal "Roter Uakari"Endangered South_America (Just 40)(Just 2) (Just 4.0) (Just 2) (Just 30)
   , Animal "Vielfraß" Endangered North_America (Just 43) (Just 1)(Just 32.0)(Just 4) (Just 10)
   , Animal "Wasserbock" Critically_Endangered Africa (Just 200)(Just 5) (Just 250.0) (Just 2) (Just 18)
   , Animal "Kaiserschnurrbarttamarin" Endangered South_America (Just 23)(Just 2) (Just 0.5) (Just 2) (Just 20)
   , Animal "Asiatischer Löwe" Critically_Endangered Asia (Just 125)(Just 4) (Just 250.0) (Just 1) (Just 21)
   , Animal "Schwarzbär" Common North_America (Just 90)(Just 1) (Just 275.0) (Just 4) (Just 35)
   , Animal "Großer Kaninchennasenbeutler" Endangered Australia (Just 30)(Just 1) (Just 2.5) (Just 3)(Just 8)
   , Animal "Mendesantilope" Threatened_with_Extinction Africa (Just 115)(Just 2) (Just 125.0) (Just 2) (Just 25)
   , Animal "Mungos" Common Africa (Just 25)(Just 1) (Just 4.0)(Just 4)(Just 12)
   , Animal "Riesenwalsschwein" Common Africa (Just 110)(Just 5) (Just 275.0) (Just 2) (Just 18)
   , Animal "Elch" Common North_America (Just 235)(Just 1) (Just 825.0) (Just 2) (Just 27)
   , Animal "Nasenaffe" Critically_Endangered Asia (Just 55)(Just 2) (Just 22.0) (Just 2) (Just 13)
   , Animal "Marderhund" Common Asia (Just 50)(Just 3) (Just 10.0) (Just 4) (Just 11)
   , Animal "Kleinfleck-Ginsterkatze" Common Africa (Just 20)(Just 1) (Just 3.0) (Just 1) (Just 13)
   , Animal "Hutias" Threatened_with_Extinction North_America (Just 10)(Just 3) (Just 8.5) (Just 2) (Just 11)
   , Animal "Dschelada"Endangered Africa (Just  50)(Just 2) (Just 20.0) (Just 2) (Just 30)
   , Animal "Europäische Wildkatze" Common Europe (Just 30)(Just 1) (Just 8.0) (Just 2)(Just 18)
   , Animal "Benettkänguru" Common Australia (Just 100)(Just 1) (Just 18.0) (Just 2) (Just 15)
   , Animal "Wickelbär" Common South_America (Just 25)(Just 1) (Just 4.5) (Just 3) (Just 24)
   , Animal "Wieselkatze" Common South_America (Just 33) (Just 1) (Just 9.0) (Just 1) (Just 15)
   , Animal "Große Braune Fledermaus" Common  North_America (Just 10)(Just 2) (Just 0.01) (Just 5)(Just 19)
   , Animal "Elenantilope" Critically_Endangered Africa (Just 200)(Just 5) (Just 1000.0) (Just 2) (Just 23)
   , Animal "Kuhantilope" Endangered Africa (Just 150)(Just 5) (Just 220.0) (Just 2) (Just 19)
   , Animal "Streifenhyäne" Secured Africa (Just 95)(Just 1) (Just 55.0) (Just 4) (Just 24)
   , Animal "Dorkasgazelle" Endangered Africa (Just 70)(Just 5) (Just 30.0) (Just 2) (Just 12)
   , Animal "Rötelmaus" Common Europe (Just 5)(Just 2) (Just 0.04)(Just 2) (Just 5)
   , Animal "Wildmeerschweinchen" Secured South_America (Just 10)(Just 2) (Just 1.5) (Just 2) (Just 5)
   , Animal "Taschenratte" Common North_America (Just 5)(Just 1) (Just 0.5) (Just 2) (Just 5)
   , Animal "Streifenbackenhörnchen" Common North_America (Just 10)(Just 1) (Just 0.1) (Just 2) (Just 3)
   , Animal "Vierhornantilope" Endangered Asia (Just 60)(Just 1) (Just 21.0) (Just 2) (Just 10)
   , Animal "Tayra" Common South_America (Just 30)(Just 1) (Just 5.0)(Just 4) (Just 18)
   , Animal "Ameisenbeutler" Endangered Australia (Just 30)(Just 1) (Just 0.6) (Just 5) (Just 5)
   , Animal "Trampeltier" Threatened_with_Extinction Asia (Just 230)(Just 1) (Just 700.0) (Just 2) (Just 50)
   , Animal "Kitfuchs" Common North_America (Just 35)(Just 3) (Just 2.2) (Just 1) (Just 4)
   , Animal "Hirschziegenantilope" Endangered Asia (Just 85)(Just 5) (Just 43.0) (Just 2) (Just 20)
   , Animal "Langschwanzkatze" Critically_Endangered South_America (Just 30)(Just 1) (Just 4.0) (Just 1) (Just 20)
   , Animal "Feldhase" Common Europe (Just 20)(Just 1) (Just 5.0) (Just 2) (Just 5)
   , Animal "Brillenbär" Endangered South_America (Just 80)(Just 1) (Just 175.0) (Just 4) (Just 25)
   , Animal "Larvensifaka" Endangered Africa (Just 30)(Just 2) (Just 7.0) (Just 2) (Just 23)
   , Animal "Siebenschläfer" Secured Europe (Just 10)(Just 2) (Just 0.1) (Just 2) (Just 4)
   , Animal "Waschbär" Common North_America (Just 30)(Just  -1) (Just 8.0) (Just 4) (Just 16)
   , Animal "Berberaffe" Endangered Africa (Just 40)(Just 2) (Just 16.0) (Just 2) (Just 30)
   , Animal "Braunes Langohr" Endangered Europe (Just 5)(Just 2) (Just 0.01) (Just 5) (Just 30)
   , Animal "Felskänguru" Endangered Australia  (Just 30)(Just 2) (Just 9.0) (Just 2) (Just 14)
   , Animal "Chinchilla" Threatened_with_Extinction South_America (Just 10)(Just 2) (Just 0.8) (Just 2) (Just 10)
   , Animal "Waldhund" Endangered South_America (Just 30)(Just 2) (Just 7.0) (Just 1) (Just 13)
   , Animal "Hermelin" Common Europe (Just 10)(Just 1)(Just 0.3) (Just 4) (Just 4)
   , Animal "Nebelparder" Endangered Asia (Just 40)(Just 1) (Just 25.0) (Just 1) (Just 19)
   , Animal "Mantelpavian" Secured Africa (Just 40)(Just 2) (Just 21.0) (Just 4) (Just 37)
   , Animal "Grauhörnchen" Common North_America (Just 20)(Just 1) (Just 0.5) (Just 2) (Just 12)
   , Animal "Fossa" Endangered Africa (Just 40)(Just 1) (Just 12.0) (Just 1) (Just 17)
   , Animal "Kaffernbüffel" Secured Africa (Just 170)(Just 5) (Just 900.0) (Just 2) (Just 18)
   , Animal "Berglemming" Common Europe (Just 5)(Just 2) (Just 0.1) (Just 2) (Just 2)
   , Animal "Weißwedelhirsch" Common North_America (Just 100)(Just 1) (Just 150.0) (Just 2) (Just 10)
   , Animal "Bonobo" Critically_Endangered Africa (Just 85)(Just 2) (Just 60.0) (Just 4) (Just 45)
   , Animal "Bergzebra" Critically_Endangered Africa (Just 150)(Just 5) (Just 375.0) (Just 2) (Just 29)
   , Animal "Katzenbär" Critically_Endangered Asia (Just 40)(Just 1) (Just 6.0) (Just 4) (Just 14)
   , Animal "Sambar" Common Asia (Just 150)(Just 2) (Just 260.0) (Just 4) (Just 20)
   , Animal "Neunbindengürteltier" Common South_America (Just 30)(Just 1) (Just 8.0) (Just 4)(Just 12)
   , Animal "Roter Brüllaffe"Common South_America (Just 30)(Just 2) (Just 9.0) (Just 2) (Just 15)
   , Animal "Großer Streifenbeutler" Secured Australia (Just 15)(Just 1) (Just 0.5) (Just 3) (Just 9)
   , Animal "Großer Kudu" Secured Africa (Just 150)(Just 2) (Just 315.0) (Just 2) (Just 23)
   --, Animal "Spitzhörnchen" Endangered Asia (Just 25)(Just 3) (Just 0.3) (Just 3) (Just -1)
   , Animal "Fischkatze" Endangered Asia (Just 40)(Just 1) (Just 14.0) (Just 1) (Just 12)
   , Animal "Hirschferkel"No_Information Africa (Just 35)(Just 2) (Just 15.0) (Just 2) (Just 13)
   , Animal "Vervetmeerkatze" Common Africa (Just 30)(Just 2) (Just 9.0) (Just 2) (Just 30)
   , Animal "Großes Mausohr" Secured Europe (Just 10)(Just 2) (Just 0.04) (Just 5) (Just 7)
   , Animal "Hübschgesichtwallaby" Common Australia (Just 40)(Just 2) (Just 25.0)(Just 2) (Just 9)
   , Animal "Berggorilla "Critically_Endangered Africa (Just 150)(Just 2) (Just 275.0) (Just 2) (Just 50)
   , Animal "Gämse" Secured Europe (Just 80)(Just 2) (Just 50.0) (Just 2) (Just 22)
   , Animal "Zorilla" Common Africa (Just 13)(Just 1) (Just 1.5) (Just 1) (Just 13)
   , Animal "Babirusa" Endangered Asia (Just 80)(Just 2) (Just 100.0) (Just 2) (Just 24)
   , Animal "Weißbüschelaffe" Common South_America (Just 10)(Just 2) (Just 0.5) (Just 3) (Just 10)
   , Animal "Wüstenigel" Common Africa (Just 10)(Just 1) (Just 0.5)  (Just 5) (Just 7)
   , Animal "Sibirisches Moschustier" Endangered Asia (Just 80)(Just 1) (Just 18.0) (Just 2) (Just 20)
   , Animal "Südlicher Tamandua" Common South_America (Just 40)(Just 1) (Just 7.0) (Just 5) (Just 9)
   , Animal "Fischermarder" Common North_America (Just 20)(Just 1) (Just 5.5) (Just 1) (Just 10)
   , Animal "Maleienbär" Critically_Endangered Asia (Just 70)(Just 1) (Just 65.0) (Just 4)(Just 31)
   , Animal "Saiga" Threatened_with_Extinction Asia (Just 100)(Just 2) (Just 70.0) (Just 2)(Just 12)
   , Animal "Rennmaus" Common Africa (Just 13)(Just 1) (Just 0.06) (Just 3) (Just 2)
   , Animal "Gewöhnliches Strachelschwein" Rare Africa (Just 40)(Just 2) (Just 30.0) (Just 4) (Just 15)
   , Animal "Großes Hasenmaul" Common South_America (Just 10)(Just 2) (Just 0.09) (Just 1) (Just 11)
   , Animal "Honigdachs" Common Africa (Just 30)(Just 1) (Just 13.0) (Just 4) (Just 26)
   , Animal "Muntjak" Endangered Asia (Just 78)(Just 1) (Just 33.0) (Just 2) (Just 19)
   , Animal "Moschusochse" Secured North_America (Just 150)(Just 5) (Just 410.0) (Just 2) (Just 24)
   , Animal "Fuchskusu" Secured Australia (Just 10)(Just 1) (Just 4.5) (Just 3) (Just 13)
   , Animal "Karakal" Common Africa (Just 50)(Just 1) (Just 20.0)(Just 1) (Just 12)
   , Animal "Goldschakal" Common Africa (Just 40)(Just 2) (Just 15.0) (Just 1) (Just 9)
   , Animal "Hammerkopf-Flughund" Secured Africa (Just 20)(Just -1) (Just 0.5) (Just 2) (Just 20)
   , Animal "Kuskus" Endangered Asia (Just 30)(Just 1) (Just 5.0) (Just 2) (Just 11)
   , Animal "Puma" Endangered South_America (Just 70)(Just 1) (Just 105.0)(Just 1) (Just 12)
   , Animal "Beutelteufel" Common Australia (Just 50) (Just 1) (Just 12.0) (Just 1) (Just 8)
   , Animal "Löffelhund" Common Africa (Just 40)(Just 2) (Just 5.5) (Just 1) (Just 13)
   , Animal "Dallschaf" Secured North_America (Just 100)(Just 5) (Just 115.0) (Just 2) (Just 24)
   , Animal "Nacktnaseneombat" Common Australia (Just 40)(Just 1) (Just 35.0) (Just 2) (Just 5)
   , Animal "Guanako" Endangered South_America (Just 130)(Just 2) (Just 120.0) (Just 2) (Just 28)
   , Animal "Potto" Secured Africa (Just 35)(Just 1) (Just 1.6) (Just 3) (Just 15)
   , Animal "Axishirsch" Secured Asia (Just 100)(Just 2) (Just 110.0) (Just 2) (Just 20)
   , Animal "Kugelgürteltier"Secured South_America (Just 30)(Just 1) (Just 1.5) (Just 5) (Just 15)
   , Animal "Fingertier" Critically_Endangered Africa (Just 30)(Just 1) (Just 3.0)(Just 5) (Just 23)
   , Animal "Waldmurmeltier" Secured North_America (Just 30)(Just 1) (Just 6.5) (Just 3) (Just 6)
   , Animal "Yak" Endangered Asia (Just 200)(Just 5) (Just 1000.0) (Just 2) (Just 25)
   , Animal "Dingo" Secured Australia (Just 65)(Just 4) (Just 20.0)  (Just 4) (Just 14)
   , Animal "Wüstenspringmaus" Common Africa (Just 5)(Just 1) (Just 0.05) (Just 3) (Just 3)
   , Animal "Gehaubter Kapuziner" Secured South_America (Just 20)(Just 2) (Just 4.5) (Just 3) (Just 45)
   , Animal "Springbock" Secured Africa (Just 90)(Just 5) (Just 50.0) (Just 2)  (Just 19)
   , Animal "Schabrackenhyäne" Secured Africa (Just 88)(Just 2) (Just 47.0) (Just 4) (Just 12)
   , Animal "Gabelbock" Common North_America (Just 105)(Just 5) (Just 70.0) (Just 2) (Just 10)
   , Animal "Mausmaki" Common Africa (Just 10)(Just 2) (Just 0.1) (Just 3) (Just 15)
   , Animal "Großer Langnasenbeutler" Secured Australia (Just 20)(Just 1)(Just 2.2) (Just 5) (Just 5)
   , Animal "Schwarzwedelhirsch" Common North_America (Just 110)(Just 2) (Just 150.0) (Just 2) (Just 20)
   , Animal "Erdwolf" Common Africa (Just 50)(Just 3) (Just 14.0) (Just 5) (Just 18)
   , Animal "Guamofledermaus" Endangered South_America (Just 10)(Just 2) (Just 0.04) (Just 5) (Just 8)
   , Animal "Südafrikanischer Spießbock" Secured Africa (Just 140)(Just 5) (Just 240.0) (Just 2)(Just 20)
   , Animal "Goldstumpfnase" Endangered Asia (Just 55)(Just 2) (Just 35.0) (Just 2) (Just 10)
   , Animal "Steinbock" Common Europe (Just 105)(Just 5) (Just 150.0) (Just 2) (Just 10)
   , Animal "Schabrackenschakal" Secured Africa (Just 50)(Just 3) (Just 14.0) (Just 1) (Just 14)
   , Animal "Dreifinger-Faultier" Secured South_America (Just 50)(Just 1) (Just 6.5) (Just 1) (Just 12)
   , Animal "Mähnenwolf" Secured South_America (Just 90)(Just 1) (Just 26.0) (Just 1) (Just 15)
   , Animal "Waldmaus" Secured Europe (Just 10)(Just -1) (Just 0.03) (Just 2) (Just 2)
   , Animal "Dianameerkatze" Critically_Endangered Africa (Just 50)(Just 2) (Just 7.0) (Just 2) (Just 22)
   , Animal "Wasserbüffel" Critically_Endangered Asia (Just 190)(Just 5) (Just 1200.0) (Just 2) (Just 25)
   , Animal "Zwergbeutelratte" Common South_America (Just 20)(Just 1) (Just 0.1) (Just 3) (Just 1)
   , Animal "Rentier" Common  Europe (Just 220)(Just 5) (Just 320.0) (Just 2) (Just 15)
   , Animal "Silberdachs"Secured North_America (Just 40)(Just 1) (Just 12.0) (Just 1) (Just 14)
   , Animal "Hanumanlangur" Secured Asia (Just 40)(Just 2) (Just 24.0) (Just 2) (Just 20)
   , Animal "Nilgauantilope" Secured Asia (Just 150)(Just 2) (Just 300.0) (Just 2)(Just 20)
   , Animal "Quokka" Endangered Australia (Just 35)(Just 2) (Just 5.0) (Just 2) (Just 10)
   , Animal "Lippenbär" Endangered Asia (Just 90)(Just 1) (Just 145.0) (Just 3) (Just 40)
   , Animal "Bürstenrattenkänguru" Secured Australia (Just 30)(Just 1) (Just 2.3) (Just 2) (Just 12)
   , Animal "Lisztaffe" Critically_Endangered South_America (Just 20)(Just 2) (Just 0.4) (Just 3) (Just 25)
   , Animal "Europäischer Maulwurf" Secured Europe (Just 13)(Just 1) (Just 0.1) (Just 5) (Just 4)
   , Animal "Panzernashorn" Critically_Endangered Asia (Just 175)(Just 1) (Just 2200.0)(Just 2) (Just 47)
   , Animal "Nordamerikanisches Katzenfrett" Secured North_America (Just 16)(Just 1) (Just 1.4) (Just 4) (Just 7)
   , Animal "Okapi" Secured Africa (Just 215)(Just 1) (Just 350.0) (Just 2) (Just 33)
   , Animal "Filander" Secured Australia (Just 45)(Just 1) (Just 12.0) (Just 2)(Just 6)
   , Animal "Gewöhnliches Warzenschwein" Common Africa (Just 85)(Just 2) (Just 150.0) (Just 2) (Just 10)
   , Animal "Serau" Endangered Asia (Just 95)(Just 1) (Just 140.0) (Just 2) (Just 18)
   , Animal "Agutis" No_Information South_America (Just 40)(Just 3) (Just 4.0) (Just 2) (Just 17)
   , Animal "Pferd" Secured North_America (Just 170)(Just 5) (Just 375.0) (Just 2) (Just 50)
   , Animal "Zwergseidenaffe" Secured South_America (Just 15)(Just 3) (Just 0.1)(Just 2) (Just 12)
   , Animal "Kronenducker" Secured Africa (Just 70)(Just 3) (Just 25.0) (Just 2) (Just 14)
   , Animal "Zwergflusspferd" Endangered Africa (Just 100)(Just 1) (Just 270.0) (Just 2)(Just 43)
   , Animal "Serval" Common Africa (Just 60)(Just 1) (Just 18.0)(Just 1) (Just 19)
   , Animal "Fleckenbeutelmarder" Common Australia (Just 20) (Just 1) (Just 3.0) (Just 1) (Just 6)
   , Animal "Riesengleiter" Endangered Asia (Just 35) (Just 1) (Just 1.7) (Just 2)(Just 17)
   , Animal "Große Kurzschwanzspitzmaus" Secured North_America (Just 10)(Just 1) (Just 0.03) (Just 1) (Just 1)
   , Animal "Rüsselspringer" Endangered Africa (Just 20) (Just 3) (Just 0.5) (Just 5) (Just 5)
   , Animal "Mantelaffe" Secured Africa (Just 40)(Just 2) (Just 14.5) (Just 2) (Just 22)
   , Animal "Buschbock" Secured Africa (Just 110) (Just 3) (Just 77.0) (Just 2)(Just 13)
   , Animal "Kalifornischer Eselhase" Secured North_America (Just 45)(Just 1) (Just 4.0) (Just 2) (Just 6)
   , Animal "Nacktmull" Secured Africa (Just 10)(Just 2) ( Just 0.08) (Just 2) (Just 25)
   , Animal "Asien-Linsangs" Secured Asia (Just 40)(Just 1) (Just 0.8)(Just 5) (Just 10)
   , Animal "Rothund" Endangered Asia (Just 60)(Just 4) (Just 20.0) (Just 1) (Just 11)
   , Animal "Baummarder" Secured Europe (Just 40)(Just 1) (Just 1.8) (Just 1) (Just 11)
   , Animal "Indri" Threatened_with_Extinction Africa (Just 50)(Just 2) (Just 10.0) (Just 2) (Just 20)
   , Animal "Baumstachler" Endangered South_America (Just 40)(Just 1) (Just 5.0) (Just 2) (Just 17)
   , Animal "Afrikanischer Wildhund"  Critically_Endangered Africa (Just 80)(Just 4) (Just 36.0) (Just 1)(Just 11)
   , Animal "Ozelot" Common South_America (Just 50)(Just 1) (Just 16.0) (Just 1) (Just 10)
   , Animal "Tahre" Endangered Asia (Just 100)(Just 2) (Just 100.0) (Just 2) (Just 10)
   , Animal "Süd-Zwergichneumon" Secured Africa (Just 20)(Just 2) (Just 0.6) (Just 5)(Just 12)
   , Animal "Rappenantilope" Endangered Africa (Just 160)(Just 5) (Just 300.0) (Just 2)(Just 22)
   , Animal "Europäischer Iltis" Secured  Europe (Just 30)(Just 1) (Just 0.9) (Just 1) (Just 6)
   , Animal "Rhesusaffe" Endangered Asia (Just 30)(Just 2) (Just 6.0) (Just 2) (Just 25)
   , Animal "Dikdik" Secured Africa (Just 50)(Just 2) (Just 7.0) (Just 2) (Just 10)
   , Animal "Grevyzebra" Critically_Endangered Africa (Just 160)(Just 5) (Just 450.0) (Just 2) (Just 18)
   , Animal "Gespenstfledermaus" Endangered Australia (Just 10)(Just 1) (Just 0.1)(Just 1) (Just 16)
   , Animal "Europäisches Reh"  Secured Europe (Just 100)(Just 2) (Just 50.0)(Just 2)(Just 17)
   , Animal "Koboldmaki" No_Information Asia (Just 10)(Just 1) (Just 0.1) (Just 5) (Just 13)
   , Animal "Halsbandpakari"Secured South_America (Just 50)(Just 5) (Just 30.0) (Just 4)(Just 24)
   , Animal "Riesenhörnchen" Endangered Asia (Just 30)(Just 1) (Just 3.0) (Just 2)(Just 5)
   , Animal "Rotluchs" Secured North_America (Just 60)(Just 1) (Just 15.0) (Just 1) (Just 12)
   , Animal "Moorantilope" Hardly_Endangered Africa (Just 105)(Just 5) (Just 120.0) (Just 2)(Just 17)
   , Animal "Springtamarin" Endangered South_America (Just 20)(Just 2) (Just 0.8)(Just 3) (Just 18)
   , Animal " Zebramanguste"Secured Africa (Just 30)(Just 2) (Just 2.2) (Just 3) (Just 12)
 --  , Animal "Zwergameisenbär" Secured South_America (Just 20)(Just 1) (Just 0.4) (Just 5) (Just -1)
   , Animal "Nyala" Hardly_Endangered Africa (Just 150)(Just 2) (Just 125.0) (Just 2) (Just 16)
   , Animal "Goldmull" Endangered Africa (Just 10)(Just 1) (Just 0.1) (Just 5) (Just 5)
   , Animal "Graufuchs" Secured North_America (Just 50)(Just  3) (Just 7.0) (Just 3)(Just 14)
   , Animal "Baumschliefer" Secured Africa (Just 40)(Just 1) (Just 4.0) (Just 2) (Just 12)
   , Animal "Klippspringer" Hardly_Endangered Africa (Just 60)(Just 3) (Just 18.0) (Just 2)(Just 17) 
   , Animal "Pinselohrschwein" Secured Africa (Just 95)(Just 2) (Just 130.0) (Just 2) (Just 21)
   , Animal "Buntbock" Hardly_Endangered Africa (Just 100)(Just 5) (Just 80.0) (Just 2) (Just 21)
   , Animal "Nordamerikanischer Pika" Secured North_America (Just 20)(Just 1) (Just 0.1) (Just 2)(Just 5)
   , Animal "Weißhandgibbon" Hardly_Endangered Asia (Just 40)(Just 3) (Just 8.0) (Just 3)(Just 25)
   , Animal "Kaninchenkänguru" Common Australia (Just 25)(Just 1) (Just 2.2) (Just 2) (Just 7) 
   , Animal "Sumatra-Nashorn" Threatened_with_Extinction Asia (Just 145)(Just 1) (Just 1000.0) (Just 2) (Just 32)
   , Animal "Plumplori" Common Asia (Just 30)(Just 1) (Just 2.0) (Just 3) (Just 26)
   , Animal "Lama" Secured South_America (Just 130)(Just 5) (Just 155.0) (Just 2)(Just 29)
   , Animal "Husarenaffe"Secured Africa (Just 50)(Just 2) (Just 13.0) (Just 4) (Just 21)
   , Animal "Bieberhörnchen" Hardly_Endangered North_America (Just 12)(Just 1) (Just 1.8) (Just 2)(Just 6)
   , Animal "Impala" Hardly_Endangered Africa (Just 100)(Just 5) (Just 65.0) (Just 2)(Just 13)
   , Animal "Alpenmurmeltier" Secured Europe (Just 30)(Just 2) (Just 7.5) (Just 3) (Just 15)
   , Animal "Mongozmaki" Endangered Africa (Just 40)(Just 2) (Just 3.0) (Just 2)(Just 30)
   , Animal "Giraffengazelle" No_Information Africa (Just 105)(Just 2) (Just 52.0) (Just 2)(Just 8)
   , Animal "Schabrackentapir" Endangered Asia (Just 120)(Just 1) (Just 320.0) (Just 2) (Just 30)
   , Animal "Kleinfleckkatze" No_Information South_America (Just 30)(Just 1) (Just 6.0) (Just 1)(Just 21)
   , Animal "Indischer Riesenflughund" Secured Asia (Just 30)(Just 2) (Just  1.6) (Just 2) (Just 31)
   , Animal "Wildesel" Threatened_with_Extinction Africa (Just 125)(Just 1) (Just 250.0) (Just 2) (Just 45)
   , Animal "Amerikanischer Nerz" Secured North_America (Just 40)(Just 1) (Just 2.3) (Just 1) (Just 10)
   , Animal "Nasenbär" Secured South_America (Just 30)(Just 2) (Just 6.0) (Just 3) (Just 8)
   , Animal "Rohrkatze" Secured Asia (Just 30)(Just 1) (Just 16.0) (Just 1) (Just 20)
   , Animal "Große Rohrratte" Common Africa (Just 20)(Just 2) (Just 9.0) (Just 2) (Just 4)
   , Animal "Afrika-Zibetkatze" Secured Africa (Just 40)(Just 1) (Just 20.0) (Just 4) (Just 20)
   , Animal "Arktisches Erdhörnchen" Common North_America (Just 30)(Just 2) (Just 0.8) (Just 2)(Just 11)
   , Animal "Äthiopischer Wolf" Threatened_with_Extinction Africa (Just 62)(Just 4) (Just 20.0) (Just 1) (Just 9)
   , Animal "Feldhamster" Secured Europe (Just 15)(Just 1) (Just 0.9) (Just 3) (Just 2)
   , Animal "Kurzkopfgleitbeutler" Common  Australia (Just 25)(Just 2) (Just 0.2) (Just 5)(Just 14)
   , Animal "Pudu" Endangered South_America (Just 45)(Just 1) (Just 13.5) (Just 2) (Just 12)
   , Animal "Pampashase" Hardly_Endangered South_America (Just 48)(Just 3) (Just 16.0) (Just 2) (Just 10)
   , Animal "Vari" Critically_Endangered Africa (Just 40)(Just 2) (Just 4.5) (Just 2) (Just 25)
  
    
   ]

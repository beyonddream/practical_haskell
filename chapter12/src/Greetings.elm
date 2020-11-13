module Greetings exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (Decoder, map2, field, string)
import Http

main = Browser.element { init = init, update = update, subscriptions = \_ -> Sub.none, view = view }

type alias Model = { productId : String, productStatus : ProductStatus }
type ProductStatus = JustStarted
                    | LoadingProduct
                    | Error
                    | ProductData Product

type alias Product = { name : String, description : String }


productDecoder : Decoder Product
productDecoder = map2  Product (field "name" string)
                               (field "description" string)

init : () -> (Model, Cmd Msg)
init _ = ( { productId = "", productStatus = JustStarted }, Cmd.none )

type Msg = TextboxChanged String | Load | ReceivedInfo (Result Http.Error Product)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
        TextboxChanged pid -> ({ model | productId = pid }, Cmd.none)
        Load -> ( {model | productStatus = LoadingProduct}
                , Http.get
                    { url = "http://localhost:3000/product/" ++ model.productId
                    , expect = Http.expectJson ReceivedInfo productDecoder
                    } )
        ReceivedInfo result -> case result of
                Ok p -> ({model | productStatus = ProductData p }, Cmd.none)
                Err _ -> ({ model | productStatus = Error }, Cmd.none)

view : Model -> Html Msg
view model = div []
                [ viewProduct model
                , input  [ placeholder "Product Id: "
                        , value model.productId
                        , onInput TextboxChanged ] []
                , button [ onClick Load ] [ text "Search" ]
                ]

viewProduct : Model -> Html Msg
viewProduct model =
        case model of
                {  productId, productStatus } ->
                        case productStatus of
                                ProductData p -> div [] [  label [] [ text ("Product Name = " ++ p.name) ]
                                                         , label [] [ text ("Product description = " ++ p.description) ]
                                                         ]
                                _ -> text ""


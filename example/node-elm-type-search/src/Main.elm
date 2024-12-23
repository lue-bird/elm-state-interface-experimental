port module Main exposing (main)

{-| Fuzzy-find declared values/functions from within your project by rough type
-}

import Ansi.Color
import Bytes
import Bytes.Decode
import Elm.Parser
import Elm.Project
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Elm.Type
import Json.Decode
import Json.Encode
import Node
import TypeDistance


type State
    = WaitingForElmJson
    | Running RunningState
    | ElmJsonReadFailed String
    | SourceDirectoryReadFailed { path : String, message : String }
    | SourceFileReadFailed { path : String, message : String }


type alias RunningState =
    { elmJson : Elm.Project.Project
    , typeToSearchFor : Maybe String
    , sourceDirectoriesToRead : List String
    , sourceFilesToRead : List String
    , signatures :
        List
            { path : String
            , location : Elm.Syntax.Range.Location
            , name : String
            , type_ : Elm.Type.Type
            }
    }


syntaxToType : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Type.Type
syntaxToType syntaxType =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.GenericType name ->
            Elm.Type.Var name

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( _, unqualified )) arguments ->
            Elm.Type.Type unqualified
                (arguments
                    |> List.map (\(Elm.Syntax.Node.Node _ argument) -> argument |> syntaxToType)
                )

        Elm.Syntax.TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            Elm.Type.Tuple
                (parts
                    |> List.map (\(Elm.Syntax.Node.Node _ part) -> part |> syntaxToType)
                )

        Elm.Syntax.TypeAnnotation.Record fields ->
            Elm.Type.Record
                (fields
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, Elm.Syntax.Node.Node _ fieldValue )) ->
                            ( fieldName, fieldValue |> syntaxToType )
                        )
                )
                Nothing

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ extendedRecordVariable) (Elm.Syntax.Node.Node _ fields) ->
            Elm.Type.Record
                (fields
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, Elm.Syntax.Node.Node _ fieldValue )) ->
                            ( fieldName, fieldValue |> syntaxToType )
                        )
                )
                (Just extendedRecordVariable)

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (Elm.Syntax.Node.Node _ inType) (Elm.Syntax.Node.Node _ outType) ->
            Elm.Type.Lambda (inType |> syntaxToType) (outType |> syntaxToType)


initialState : State
initialState =
    WaitingForElmJson


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForElmJson ->
            Node.fileRequest "elm.json"
                |> Node.interfaceFutureMap
                    (\elmJsonBytesOrError ->
                        case elmJsonBytesOrError of
                            Err fileReadError ->
                                ElmJsonReadFailed
                                    ("elm.json couldn't be read because "
                                        ++ fileReadError.message
                                        ++ " (code "
                                        ++ fileReadError.code
                                        ++ ")"
                                    )

                            Ok elmJsonBytes ->
                                case elmJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width elmJsonBytes)) of
                                    Nothing ->
                                        ElmJsonReadFailed "elm.json bytes could not be decoded into UTF-8 String"

                                    Just elmJsonString ->
                                        case elmJsonString |> Json.Decode.decodeString Elm.Project.decoder of
                                            Err jsonDecodeError ->
                                                ElmJsonReadFailed
                                                    ("elm.json failed to parse due to "
                                                        ++ (jsonDecodeError |> Json.Decode.errorToString)
                                                    )

                                            Ok elmJson ->
                                                Running
                                                    { elmJson = elmJson
                                                    , sourceDirectoriesToRead = elmJson |> elmJsonSourceDirectories
                                                    , sourceFilesToRead = []
                                                    , signatures = []
                                                    , typeToSearchFor = Nothing
                                                    }
                    )

        Running running ->
            runningInterface running

        ElmJsonReadFailed elmJsonDecodeError ->
            badExitWith elmJsonDecodeError

        SourceDirectoryReadFailed directoryReadError ->
            badExitWith
                ("failed to check the source directory "
                    ++ directoryReadError.path
                    ++ " because "
                    ++ directoryReadError.message
                )

        SourceFileReadFailed fileReadError ->
            badExitWith
                ("failed to read the source file "
                    ++ fileReadError.path
                    ++ " because "
                    ++ fileReadError.message
                )


badExitWith : String -> Node.Interface future_
badExitWith errorMessage =
    [ Node.standardErrWrite (errorMessage ++ "\n")
    , Node.exit 1
    ]
        |> Node.interfaceBatch


runningInterface : RunningState -> Node.Interface State
runningInterface running =
    [ case running.typeToSearchFor of
        Nothing ->
            [ Node.standardOutWrite "fuzzy type search from within your project: "
            , Node.standardInListen
                |> Node.interfaceFutureMap
                    (\typeToSearchFor ->
                        Running
                            { elmJson = running.elmJson
                            , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                            , sourceFilesToRead = running.sourceFilesToRead
                            , signatures = running.signatures
                            , typeToSearchFor = Just typeToSearchFor
                            }
                    )
            ]
                |> Node.interfaceBatch

        Just typeStringToSearchFor ->
            case typeStringToSearchFor |> syntaxTypeParse |> Result.map syntaxToType of
                Err _ ->
                    badExitWith "failed to parse the input as a type"

                Ok typeToSearchFor ->
                    case ( running.sourceDirectoriesToRead, running.sourceFilesToRead ) of
                        ( _ :: _, _ ) ->
                            Node.interfaceNone

                        ( _, _ :: _ ) ->
                            Node.interfaceNone

                        ( [], [] ) ->
                            Node.standardOutWrite
                                ("Here are the 6 closest matches I could find:\n\n"
                                    ++ (running.signatures
                                            |> List.sortBy (\a -> TypeDistance.distance a.type_ typeToSearchFor)
                                            |> List.take 6
                                            |> List.map
                                                (\signature ->
                                                    let
                                                        link : String
                                                        link =
                                                            signature.path
                                                                ++ ":"
                                                                ++ (signature.location.row |> String.fromInt)
                                                                ++ ":"
                                                                ++ (signature.location.column |> String.fromInt)
                                                    in
                                                    (signature.name |> Ansi.Color.fontColor Ansi.Color.yellow)
                                                        ++ " : "
                                                        ++ (signature.type_ |> typeToString)
                                                        ++ "\n"
                                                        ++ ("at " ++ link |> Ansi.Color.fontColor Ansi.Color.brightBlack)
                                                )
                                            |> String.join "\n\n"
                                       )
                                    ++ "\n"
                                )
    , running.sourceDirectoriesToRead
        |> List.map
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err directorySourcePathSubPathRequestError ->
                                    SourceDirectoryReadFailed
                                        { path = sourceDirectoryPath
                                        , message = directorySourcePathSubPathRequestError.message
                                        }

                                Ok subPaths ->
                                    Running
                                        { elmJson = running.elmJson
                                        , sourceDirectoriesToRead =
                                            running.sourceDirectoriesToRead
                                                |> List.filterMap
                                                    (\sourceDirectoryToRead ->
                                                        if sourceDirectoryToRead == sourceDirectoryPath then
                                                            Nothing

                                                        else
                                                            Just sourceDirectoryToRead
                                                    )
                                        , sourceFilesToRead =
                                            (subPaths
                                                |> List.filterMap
                                                    (\subPath ->
                                                        if subPath |> String.endsWith ".elm" then
                                                            Just (sourceDirectoryPath ++ "/" ++ subPath)

                                                        else
                                                            Nothing
                                                    )
                                            )
                                                ++ running.sourceFilesToRead
                                        , signatures = running.signatures
                                        , typeToSearchFor = running.typeToSearchFor
                                        }
                        )
            )
        |> Node.interfaceBatch
    , running.sourceFilesToRead
        |> List.map
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            case sourceBytesOrError of
                                Err sourceFileReadError ->
                                    SourceFileReadFailed
                                        { path = sourceFilePath
                                        , message = sourceFileReadError.message
                                        }

                                Ok sourceBytes ->
                                    case sourceBytes |> Bytes.Decode.decode (Bytes.Decode.string (sourceBytes |> Bytes.width)) of
                                        Nothing ->
                                            SourceFileReadFailed
                                                { path = sourceFilePath
                                                , message = "source bytes couldn't be decoded into UTF-8"
                                                }

                                        Just source ->
                                            case source |> Elm.Parser.parseToFile of
                                                Err _ ->
                                                    SourceFileReadFailed
                                                        { path = sourceFilePath
                                                        , message = "source couldn't be parsed (by elm-syntax). Check for compiler errors and try again."
                                                        }

                                                Ok syntax ->
                                                    Running
                                                        { elmJson = running.elmJson
                                                        , sourceDirectoriesToRead = running.sourceDirectoriesToRead
                                                        , sourceFilesToRead =
                                                            running.sourceFilesToRead
                                                                |> List.filterMap
                                                                    (\sourceFileToRead ->
                                                                        if sourceFileToRead == sourceFilePath then
                                                                            Nothing

                                                                        else
                                                                            Just sourceFileToRead
                                                                    )
                                                        , signatures =
                                                            (syntax |> syntaxModuleSignatures sourceFilePath)
                                                                ++ running.signatures
                                                        , typeToSearchFor = running.typeToSearchFor
                                                        }
                        )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


syntaxModuleSignatures :
    String
    -> Elm.Syntax.File.File
    ->
        List
            { path : String
            , location : Elm.Syntax.Range.Location
            , name : String
            , type_ : Elm.Type.Type
            }
syntaxModuleSignatures sourceFilePath syntaxModule =
    syntaxModule.declarations
        |> List.filterMap
            (\(Elm.Syntax.Node.Node declarationRange declaration) ->
                case declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration expressionDeclaration ->
                        case expressionDeclaration.signature of
                            Nothing ->
                                Nothing

                            Just (Elm.Syntax.Node.Node _ signature) ->
                                Just
                                    { path = sourceFilePath
                                    , location = declarationRange.start
                                    , name =
                                        expressionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .name
                                            |> Elm.Syntax.Node.value
                                    , type_ = signature.typeAnnotation |> Elm.Syntax.Node.value |> syntaxToType
                                    }

                    _ ->
                        Nothing
            )


typeToString : Elm.Type.Type -> String
typeToString type_ =
    case type_ of
        Elm.Type.Var name ->
            name |> Ansi.Color.fontColor Ansi.Color.red

        Elm.Type.Tuple parts ->
            case parts of
                [] ->
                    "()"

                [ inParens ] ->
                    "(" ++ (inParens |> typeToString) ++ ")"

                [ part0, part1 ] ->
                    "( " ++ (part0 |> typeToString) ++ ", " ++ (part1 |> typeToString) ++ " )"

                [ part0, part1, part2 ] ->
                    "( " ++ (part0 |> typeToString) ++ ", " ++ (part1 |> typeToString) ++ ", " ++ (part2 |> typeToString) ++ " )"

                invalidPart0 :: invalidPart1 :: invalidPart2 :: invalidPart3 :: invalidPart4Up ->
                    "( "
                        ++ ((invalidPart0 :: invalidPart1 :: invalidPart2 :: invalidPart3 :: invalidPart4Up)
                                |> List.map (\part -> part |> typeToString)
                                |> String.join ", "
                           )
                        ++ " )"

        Elm.Type.Lambda inType outType ->
            (if inType |> typeIsSpaceSeparated then
                "(" ++ (inType |> typeToString) ++ ")"

             else
                inType |> typeToString
            )
                ++ " -> "
                ++ (outType |> typeToString)

        Elm.Type.Type name arguments ->
            (name |> Ansi.Color.fontColor Ansi.Color.blue)
                ++ (arguments
                        |> List.map
                            (\argument ->
                                " "
                                    ++ (if argument |> typeIsSpaceSeparated then
                                            "(" ++ (argument |> typeToString) ++ ")"

                                        else
                                            argument |> typeToString
                                       )
                            )
                        |> String.concat
                   )

        Elm.Type.Record fields maybeExtendedRecordVariable ->
            case maybeExtendedRecordVariable of
                Nothing ->
                    case fields of
                        [] ->
                            "{}"

                        fieldHead :: fieldTail ->
                            "{ "
                                ++ ((fieldHead :: fieldTail)
                                        |> List.map
                                            (\( fieldName, fieldValue ) ->
                                                (fieldName |> Ansi.Color.fontColor Ansi.Color.green)
                                                    ++ " : "
                                                    ++ (fieldValue |> typeToString)
                                            )
                                        |> String.join ", "
                                   )
                                ++ " }"

                Just extendedRecordVariable ->
                    "{ "
                        ++ (extendedRecordVariable |> Ansi.Color.fontColor Ansi.Color.red)
                        ++ " | "
                        ++ (fields
                                |> List.map
                                    (\( fieldName, fieldValue ) ->
                                        fieldName ++ " : " ++ (fieldValue |> typeToString)
                                    )
                                |> String.join ", "
                           )
                        ++ " }"


typeIsSpaceSeparated : Elm.Type.Type -> Bool
typeIsSpaceSeparated type_ =
    case type_ of
        Elm.Type.Var _ ->
            False

        Elm.Type.Tuple parts ->
            case parts of
                [] ->
                    False

                [ inParens ] ->
                    inParens |> typeIsSpaceSeparated

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    False

        Elm.Type.Lambda _ _ ->
            True

        Elm.Type.Type _ arguments ->
            case arguments of
                [] ->
                    False

                _ :: _ ->
                    True

        Elm.Type.Record _ _ ->
            False


syntaxTypeParse : String -> Result String Elm.Syntax.TypeAnnotation.TypeAnnotation
syntaxTypeParse typeSource =
    let
        sourceWithDummyWrapperModule =
            "module A exposing (..)\ntype alias A = "
                ++ (typeSource
                        |> String.lines
                        |> List.map (\line -> "    " ++ line)
                        |> String.join "\n"
                   )
    in
    case Elm.Parser.parseToFile sourceWithDummyWrapperModule of
        Err _ ->
            Err "failed to parse type"

        Ok parsedFile ->
            case parsedFile.declarations of
                (Elm.Syntax.Node.Node _ (Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration)) :: _ ->
                    Ok (typeAliasDeclaration.typeAnnotation |> Elm.Syntax.Node.value)

                _ ->
                    Err "bug: dummy module for parsing has wrong first declaration kind"


elmJsonSourceDirectories : Elm.Project.Project -> List String
elmJsonSourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs

        Elm.Project.Package _ ->
            [ "src" ]


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Data.Aeson              (Value(..), toJSON)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           MCP.Server
import           MCP.Server.Derive
import           Network.URI             (parseURI)
import           Test.QuickCheck
import           TestTypes
import           Text.Read               (readMaybe)

-- =============================================================================
-- JSON Type Conversion Tests
-- =============================================================================

-- Test conversion from JSON Value to Text and back
prop_intRoundTrip :: Int -> Bool
prop_intRoundTrip i = 
    let jsonVal = toJSON i
        textVal = jsonValueToText jsonVal
        parsed = case readMaybe (T.unpack textVal) of
                   Just result -> result
                   Nothing -> error $ "Failed to parse Int from: " <> T.unpack textVal
    in parsed == i

prop_boolRoundTrip :: Bool -> Bool
prop_boolRoundTrip b = 
    let jsonVal = toJSON b
        textVal = jsonValueToText jsonVal
        parsed = case T.toLower textVal of
                   "true" -> True
                   "false" -> False
                   _ -> error $ "Failed to parse Bool from: " <> T.unpack textVal
    in parsed == b

prop_textRoundTrip :: Text -> Bool
prop_textRoundTrip t = 
    let jsonVal = toJSON t
        textVal = jsonValueToText jsonVal
    in textVal == t

-- Test the specific conversion functions we use in the derivation
testIntConversion :: IO Bool
testIntConversion = do
    let testCases = [0, 42, -17, 999999] :: [Int]
    results <- mapM (\i -> do
        let jsonVal = toJSON i
            textVal = jsonValueToText jsonVal
            parsed = case readMaybe (T.unpack textVal) of
                       Just result -> result
                       Nothing -> error $ "Failed to parse: " <> T.unpack textVal
        return (parsed == (i :: Int))
        ) testCases
    return (all id results)

testBoolConversion :: IO Bool
testBoolConversion = do
    let testCases = [True, False]
    results <- mapM (\b -> do
        let jsonVal = toJSON b
            textVal = jsonValueToText jsonVal
            parsed = case T.toLower textVal of
                       "true" -> True
                       "false" -> False
                       _ -> error $ "Failed to parse: " <> T.unpack textVal
        return (parsed == b)
        ) testCases
    return (all id results)

testTextConversion :: IO Bool
testTextConversion = do
    let testCases = ["hello", "world", "", "123", "true", "false"]
    results <- mapM (\t -> do
        let jsonVal = toJSON t
            textVal = jsonValueToText jsonVal
        return (textVal == t)
        ) testCases
    return (all id results)

-- Test JSON Value to Text conversion specifically
testJsonValueToText :: IO Bool
testJsonValueToText = do
    let tests = 
            [ (Number 42, "42")      -- Whole numbers become integers
            , (Number 42.5, "42.5")  -- Decimals stay as decimals
            , (String "hello", "hello")
            , (Bool True, "true")
            , (Bool False, "false")
            , (Null, "")
            ]
    
    let results = map (\(input, expected) -> jsonValueToText input == expected) tests
    return (all id results)

runJsonTypesTests :: IO Bool
runJsonTypesTests = do
    putStrLn "\n=== JSON Type Conversion Tests ==="
    
    -- QuickCheck property tests
    putStr "Testing Int round-trip property: "
    quickCheck prop_intRoundTrip
    
    putStr "Testing Bool round-trip property: "
    quickCheck prop_boolRoundTrip
    
    -- Manual conversion tests
    putStr "Testing Int conversion: "
    intResult <- testIntConversion
    putStrLn $ if intResult then "PASS" else "FAIL"
    
    putStr "Testing Bool conversion: "
    boolResult <- testBoolConversion
    putStrLn $ if boolResult then "PASS" else "FAIL"
    
    putStr "Testing Text conversion: "
    textResult <- testTextConversion
    putStrLn $ if textResult then "PASS" else "FAIL"
    
    putStr "Testing jsonValueToText function: "
    jsonResult <- testJsonValueToText
    putStrLn $ if jsonResult then "PASS" else "FAIL"
    
    let allPassed = intResult && boolResult && textResult && jsonResult
    putStrLn $ "JSON Types result: " ++ if allPassed then "PASS" else "FAIL"
    return allPassed

-- =============================================================================
-- End-to-End Derivation Tests
-- =============================================================================

-- Generate handlers using Template Haskell (importing types from TestTypes module)
testPromptHandlers :: (PromptListHandler IO, PromptGetHandler IO)
testPromptHandlers = $(derivePromptHandler ''TestPrompt 'handleTestPrompt)

testResourceHandlers :: (ResourceListHandler IO, ResourceReadHandler IO)
testResourceHandlers = $(deriveResourceHandler ''TestResource 'handleTestResource)

testToolHandlers :: (ToolListHandler IO, ToolCallHandler IO)
testToolHandlers = $(deriveToolHandler ''TestTool 'handleTestTool)

-- Test handlers with custom descriptions
testToolHandlersWithDescriptions :: (ToolListHandler IO, ToolCallHandler IO)
testToolHandlersWithDescriptions = $(deriveToolHandlerWithDescription ''TestTool 'handleTestTool testDescriptions)

-- End-to-end test functions
testPromptDerivation :: IO Bool
testPromptDerivation = do
    let (_, getHandler) = testPromptHandlers
    
    -- Test simple prompt
    result1 <- getHandler "simple_prompt" [("message", "hello")]
    let test1 = case result1 of
            Right (ContentText content) -> content == "Simple prompt: hello"
            _ -> False
    
    -- Test complex prompt with multiple types
    result2 <- getHandler "complex_prompt" [("title", "urgent task"), ("priority", "5"), ("urgent", "true")]
    let test2 = case result2 of
            Right (ContentText content) -> content == "Complex prompt: urgent task (priority=5, urgent=True)"
            _ -> False
    
    -- Test optional prompt with missing optional field
    result3 <- getHandler "optional_prompt" [("required", "test")]
    let test3 = case result3 of
            Right (ContentText content) -> content == "Optional prompt: test"
            _ -> False
    
    -- Test optional prompt with optional field present
    result4 <- getHandler "optional_prompt" [("required", "test"), ("optional", "42")]
    let test4 = case result4 of
            Right (ContentText content) -> content == "Optional prompt: test optional=42"
            _ -> False
    
    return (test1 && test2 && test3 && test4)

testResourceDerivation :: IO Bool
testResourceDerivation = do
    let (_, readHandler) = testResourceHandlers
    
    -- Test simple resource
    case parseURI "resource://config_file" of
        Just uri1 -> do
            result1 <- readHandler uri1
            let test1 = case result1 of
                    Right (ContentText content) -> T.isInfixOf "Config file contents" content
                    _ -> False
            
            -- Test parameterized resource (this would need to match actual schema)
            case parseURI "resource://database_connection" of
                Just uri2 -> do
                    result2 <- readHandler uri2
                    let test2 = case result2 of
                            Right (ContentText content) -> T.isInfixOf "Database at" content
                            _ -> False
                    return (test1 && test2)
                Nothing -> return False
        Nothing -> return False

testToolDerivation :: IO Bool
testToolDerivation = do
    let (_, callHandler) = testToolHandlers
    
    -- Test simple tool
    result1 <- callHandler "echo" [("text", "hello world")]
    let test1 = case result1 of
            Right (ContentText content) -> content == "Echo: hello world"
            _ -> False
    
    -- Test tool with multiple typed parameters
    result2 <- callHandler "calculate" [("operation", "add"), ("x", "10"), ("y", "5")]
    let test2 = case result2 of
            Right (ContentText content) -> content == "15"
            _ -> False
    
    -- Test tool with boolean parameter
    result3 <- callHandler "toggle" [("flag", "true")]
    let test3 = case result3 of
            Right (ContentText content) -> content == "Flag is now: False"
            _ -> False
    
    -- Test tool with optional parameters
    result4 <- callHandler "search" [("query", "test"), ("limit", "10")]
    let test4 = case result4 of
            Right (ContentText content) -> T.isInfixOf "Search results for 'test'" content && T.isInfixOf "(limit=10)" content
            _ -> False
    
    return (test1 && test2 && test3 && test4)

testCustomDescriptions :: IO Bool
testCustomDescriptions = do
    let (toolListHandler, _) = testToolHandlersWithDescriptions
    
    paginatedResult <- toolListHandler Nothing
    let toolDefs = paginatedItems paginatedResult
    
    -- Find the Echo tool definition and check its description
    let echoDef = filter (\def -> toolDefinitionName def == "echo") toolDefs
    let echoDescTest = case echoDef of
            [def] -> toolDefinitionDescription def == "Echoes the input text back to the user"
            _ -> False
    
    -- Find the Calculate tool definition and check its description and field descriptions
    let calculateDef = filter (\def -> toolDefinitionName def == "calculate") toolDefs
    let calculateTest = case calculateDef of
            [def] -> 
                let correctToolDesc = toolDefinitionDescription def == "Performs mathematical calculations"
                    correctFieldDescs = case toolDefinitionInputSchema def of
                        InputSchemaDefinitionObject props _ ->
                            let textDesc = case lookup "text" props of
                                    Just prop -> propertyDescription prop == "The text to echo back"
                                    Nothing -> True  -- text field doesn't exist in Calculate, that's fine
                                operationDesc = case lookup "operation" props of
                                    Just prop -> propertyDescription prop == "The mathematical operation to perform"
                                    Nothing -> False
                                xDesc = case lookup "x" props of
                                    Just prop -> propertyDescription prop == "The first number"
                                    Nothing -> False
                                yDesc = case lookup "y" props of
                                    Just prop -> propertyDescription prop == "The second number"
                                    Nothing -> False
                            in operationDesc && xDesc && yDesc
                in correctToolDesc && correctFieldDescs
            _ -> False
    
    return (echoDescTest && calculateTest)

testSchemaGeneration :: IO Bool
testSchemaGeneration = do
    let (toolListHandler, _) = testToolHandlers
    
    paginatedResult <- toolListHandler Nothing
    let toolDefs = paginatedItems paginatedResult
    
    -- Find the Calculate tool definition
    let calculateDef = filter (\def -> toolDefinitionName def == "calculate") toolDefs
    case calculateDef of
        [def] -> case toolDefinitionInputSchema def of
            InputSchemaDefinitionObject props required ->
                let hasXInt = case lookup "x" props of
                        Just prop -> propertyType prop == "integer"
                        Nothing -> False
                    hasYInt = case lookup "y" props of
                        Just prop -> propertyType prop == "integer"
                        Nothing -> False
                    hasOpString = case lookup "operation" props of
                        Just prop -> propertyType prop == "string"
                        Nothing -> False
                    hasRequiredFields = all (`elem` required) ["operation", "x", "y"]
                in return (hasXInt && hasYInt && hasOpString && hasRequiredFields)
        _ -> return False

runEndToEndTests :: IO Bool
runEndToEndTests = do
    putStrLn "\n=== End-to-End Derivation Tests ==="
    
    putStr "Testing prompt derivation: "
    promptResult <- testPromptDerivation
    putStrLn $ if promptResult then "PASS" else "FAIL"
    
    putStr "Testing resource derivation: "
    resourceResult <- testResourceDerivation
    putStrLn $ if resourceResult then "PASS" else "FAIL"
    
    putStr "Testing tool derivation: "
    toolResult <- testToolDerivation
    putStrLn $ if toolResult then "PASS" else "FAIL"
    
    putStr "Testing schema generation: "
    schemaResult <- testSchemaGeneration
    putStrLn $ if schemaResult then "PASS" else "FAIL"
    
    putStr "Testing custom descriptions: "
    descriptionResult <- testCustomDescriptions
    putStrLn $ if descriptionResult then "PASS" else "FAIL"
    
    let allPassed = promptResult && resourceResult && toolResult && schemaResult && descriptionResult
    putStrLn $ "End-to-End result: " ++ if allPassed then "PASS" else "FAIL"
    return allPassed

-- =============================================================================
-- Main Test Runner
-- =============================================================================

main :: IO ()
main = do
    putStrLn "MCP Server Test Suite"
    putStrLn "===================="
    
    jsonResult <- runJsonTypesTests
    endToEndResult <- runEndToEndTests
    
    let overallResult = jsonResult && endToEndResult
    putStrLn $ "\n===================="
    putStrLn $ "Overall result: " ++ if overallResult then "ALL TESTS PASSED" else "SOME TESTS FAILED"

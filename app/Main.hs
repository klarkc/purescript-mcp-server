{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
    -- Set UTF-8 encoding to handle Unicode characters properly
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    
    putStrLn "Hello, World!"

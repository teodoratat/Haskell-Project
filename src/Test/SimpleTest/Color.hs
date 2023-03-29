module Test.SimpleTest.Color where

revert = "\ESC[0m"

red s = "\ESC[31m" ++ s ++ revert

green s = "\ESC[32m" ++ s ++ revert

yellow s = "\ESC[33m" ++ s ++ revert

blue s = "\ESC[34m" ++ s ++ revert

color256 c s = "\ESC[38;5;" ++ show c ++ "m" ++ s ++ revert

bold s = "\ESC[1m" ++ s ++ revert

italics s = "\ESC[3m" ++ s ++ revert

underline s = "\ESC[4m" ++ s ++ revert
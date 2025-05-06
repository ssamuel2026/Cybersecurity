-- Author: Samuel Cho 
-- Version: GHCi, version 9.4.8 but used online compiler to compile by Tutorials Point
-- Project: This is a currency exchange rate project. I decided to make it since I will be studying abroad next year.

--These import statements are needed for this currency exchange rate converter to run properly.
import Data.Map (Map)  -- This imports the Map type which holds keys and values like a dictionary.
import qualified Data.Map as Map  -- Importing 'qualified' helps me to use functions that work directly with Map types
-- Import Text.Read is the general module for parsing strings into data types 
import Text.Read (readMaybe)  -- readMaybe is specific for returning a Maybe (might exist or might not exist) value type 
import Data.Char (toUpper)  -- toUpper converts all characters to uppercase 

-- CITE: Hackage 
-- URL: https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
-- HELP: I learned how to incorporate the Data.Map import statement

-- CITE: Hackage 
-- URL: https://hackage.haskell.org/package/base-4.19.1.0/docs/Text-Read.html
-- HELP: I learned how to incorporate the Text.Read import statement 

-- CITE: Hackage 
-- URL: https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Char.html
-- HELP: I learned how to incorporate the Data.Char import statement 


-- This defines a type called Currency Code which is used as a string.
type Currency = String

-- This defines a type called RespectiveRate which is map (dictionary) that contains strings as keys and 
-- numbers as values. The keys are the countries and the values are the exchange rates.
type RespectiveRate = Map (Currency, Currency) Double

-- This function lists out all of possible exchange rates for 
--(United States dollar (USD), European Euro (EUR), Great Britian Pound (GBP), 
--Japanese Yen (JPY), Canadian Dollar (CAD), New Zealand Dollar (NZD), South Korean Won (KRW).
-- The exchange rate numbers are all current values found using Google.com.
respectiveRate :: RespectiveRate
respectiveRate = Map.fromList [
    (("USD", "EUR"), 0.94),
    (("USD", "GBP"), 0.80),
    (("USD", "JPY"), 154.69),
    (("USD", "CAD"), 1.37),
    (("USD", "NZD"), 1.68),
    (("USD", "KRW"), 1374.30),
    (("EUR", "USD"), 1.07),
    (("EUR", "GBP"), 0.86),
    (("EUR", "JPY"), 165.41),
    (("EUR", "CAD"), 1.46),
    (("EUR", "NZD"), 1.80),
    (("EUR", "KRW"), 1469.77),
    (("GBP", "USD"), 1.24),
    (("GBP", "EUR"), 1.16),
    (("GBP", "JPY"), 192.30),
    (("GBP", "CAD"), 1.70),
    (("GBP", "NZD"), 2.09),
    (("GBP", "KRW"), 1707.18),
    (("JPY", "USD"), 0.0065),
    (("JPY", "EUR"), 0.0060),
    (("JPY", "GBP"), 0.0052),
    (("JPY", "CAD"), 0.0088),
    (("JPY", "NZD"), 0.011),
    (("JPY", "KRW"), 8.88),
    (("CAD", "USD"), 0.73),
    (("CAD", "EUR"), 0.68),
    (("CAD", "GBP"), 0.59),
    (("CAD", "JPY"), 113.10),
    (("CAD", "NZD"), 1.23),
    (("CAD", "KRW"), 1004.36),
    (("NZD", "USD"), 0.59),
    (("NZD", "EUR"), 0.56),
    (("NZD", "GBP"), 0.48),
    (("NZD", "JPY"), 91.83),
    (("NZD", "CAD"), 0.81),
    (("NZD", "KRW"), 815.60),
    (("KRW", "USD"), 0.00073),
    (("KRW", "EUR"), 0.00068),
    (("KRW", "GBP"), 0.00058),
    (("KRW", "JPY"), 0.11),
    (("KRW", "NZD"), 0.0012)
    ];

-- CITE:: Reddit 
-- URL: https://www.reddit.com/r/haskellquestions/comments/2xn9y2/whats_the_equivalent_of_a_python_dictionary_in/
-- HELP: I used this to learn how to create a dictionary in Haskell by looking at a python dictionary. 

-- CITE:: Google
-- URL: Googl.com
-- HELP: I used google to get all of the current (update) exchange rates for the 7 countries.


-- This function converts the inputted amount of money from one currency to another. 
changeCurrency :: Double -> Currency -> Currency -> Maybe Double
changeCurrency amount from to
  | from == to = Just amount -- This checks if the currency is the same or different 
  | otherwise = fmap (* amount) (Map.lookup (from, to) respectiveRate) -- Map.lookup looks for the respective currency of both the key and value 

  -- CITE: Learn You A Haskell for Great Good!
  -- URL: https://learnyouahaskell.com/syntax-in-functions#:~:text=otherwise%20is%20defined%20simply%20as,guards%20check%20for%20boolean%20conditions.
  -- HELP: I learned how to utilize a structure called guard that is commonly used in Haskell for boolean conditions.

-- This function converts and string to a number type.
letDouble :: String -> Maybe Double
letDouble = readMaybe

-- This function converts and string to have uppercase characters.
makeUpperCaseCode :: String -> String
makeUpperCaseCode = map toUpper

-- This function checks the (Map) dictionary to see if the key exists. If it exits, it returns the value. If it doesn't,
-- the error message is handled in the function below called inputCurrency.    
supportedCurrency :: Currency -> Bool
supportedCurrency currency = currency `elem` map fst (Map.keys respectiveRate)

-- This function is used for the user to input a correct currency code (a specific country) that exists inside
-- the dictionary. 
-- This also has error handling where if the user inputs a currecy code not in the directory, then
-- it gives the error message "Invalid or unsupported......."
inputCurrency :: String -> IO Currency
inputCurrency message = do
  putStrLn message
  currency <- getLine
  let upperCase= makeUpperCaseCode currency  -- 
  if length upperCase == 3 && all (`elem` ['A'..'Z']) upperCase && supportedCurrency upperCase
    then return upperCase
    else do
      putStrLn "Invalid or unsupported currency code. Please enter a valid 3-letter currency code."
      inputCurrency message
      
-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/156013/haskell-syntax-for-a-case-expression-in-a-do-block
-- HELP: I used this to learn how to use a do block in Haskell.

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- HELP: I learned how to use let inside the body of a block code 


-- This function is used for the user to input a correct amount of money they wish to convert to a different currency. 
-- This also has error hadnling where if the user inputs something other than a number, then it gives the error message 
-- "Invalid amount entered..... "
inputNumber :: String -> IO Double
inputNumber message = do
  putStrLn message
  amtInput <- getLine
  case letDouble amtInput of
    Just amount -> return amount
    Nothing -> do
      putStrLn "Error: Invalid amount entered. Please enter a numeric value."
      inputNumber message

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/156013/haskell-syntax-for-a-case-expression-in-a-do-block
-- HELP: I used this to learn how to use a do block in Haskell.

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/70415532/express-a-case-of-pattern-more-elegantly-in-haskell
-- HELP: I learned how to use 'case ... of' for case expressions and pattern matching 


-- This function displays the converted amount of money.
-- This function also does error handling by returning an error statement when an invalid currency code (country) is inputted. 
displayResult :: Maybe Double -> IO ()
displayResult (Just result) = putStrLn $ "Converted amount: " ++ show result
displayResult Nothing = putStrLn "Error: Invalid currency code or no exchange rate available."


-- This main function controls the operational flow of the whole program.
-- THe main functional also holds a lot of interactive components of input and output such as saying "welcome!" and
-- asks which two countries you would like to convert from and to. 
main :: IO ()
main = do
  putStrLn "Welcome to Cho's Airport Currency Converter!"
  putStrLn "We support the following currencies: USD, EUR, GBP, JPY, CAD, NZD, KRW"
  amount <- inputNumber "Enter the amount you would like to convert (e.g., 100.0):"
  fromCurrency <- inputCurrency "Enter the currency to convert from (e.g., USD):"
  toCurrency <- inputCurrency "Enter the currency to convert to (e.g., EUR):"
  let result = changeCurrency amount fromCurrency toCurrency
  displayResult result

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- HELP: I learned how to use let inside the body of a block code 


-- Not really needed, but it is good to have. 
run :: IO ()
run = main 
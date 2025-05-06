-- Author: Samuel Cho 
-- Version: GHCi, version 9.4.8 but used an online compiler to compile by Tutorials Point
-- Project: This is a simple calculator that can perform addition, subtraction, multiplication, division, 
-- square root, and absolute values. 

--These import statements are needed for this simple calculator to run properly.
-- Import Text.Read is the general module for parsing strings into data types 
import Text.Read (readMaybe) -- readMaybe is specific for returning a Maybe (might exist or might not exist) value type 
-- Import Data.Maybe is the general module for manipulating values from Haskell's Maybe type (specifically optional values)
import Data.Maybe (isJust, catMaybes) -- isJust is specifically for returning boolean values and catMaybe is for extracting the elements.
-- CITE: Hackage 
-- URL: https://hackage.haskell.org/package/base-4.19.1.0/docs/Text-Read.html
-- HELP: I learned how to incorporate the Text.Read import statement 

-- CITE: Hackage 
-- URL: https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Maybe.html
-- HELP: I learned how to incorporate the Data.Maybe import statement 

-- This data type called MathOperation takes in 6 unique values using type constructors. In this case,
-- it takes in mathematical operators as the constructors 
data MathOperation = Add | Subtract | Multiply | Divide | Sqrt | Abs
  deriving (Eq, Show, Read) -- Eq means equality 
                            -- Show and Read is converted to strings 

-- CITE: Monday Morning Haskell
-- URL: https://mmhaskell.com/blog/2016/12/17/making-your-own-data-types-in-haskell
-- HELP: Learned how to use type constructors in Haskell

-- This function takes a list of strings as the input to represent simple mathematical operations
-- The type Just is a constructor type for the Maybe type which returns a value or no value.
doOperation :: String -> Maybe MathOperation
doOperation "+" = Just Add 
doOperation "-" = Just Subtract
doOperation "*" = Just Multiply
doOperation "/" = Just Divide
doOperation "sqrt" = Just Sqrt
doOperation "abs" = Just Abs
doOperation _ = Nothing -- No value and returns nothing 

-- CITE: Haskell Language 
-- URL: https://wiki.haskell.org/Type#:~:text=The%20Just%20constructor%20takes%20one%20parameter%2C%20of%20type%20a.&text=Here%2C%20one%20of%20the%20constructors,very%20common%20pattern%20in%20Haskell.
-- HELP: Learned how to use the Just constructor data type 

-- This function performs the actual operations that is on a calculator. 
performCalculate :: MathOperation -> [Double] -> Maybe Double
performCalculate Add [x, y] = Just (x + y)
performCalculate Subtract [x, y] = Just (x - y)
performCalculate Multiply [x, y] = Just (x * y)
performCalculate Divide [x, y] = if y == 0 then Nothing else Just (x / y)
performCalculate Sqrt [x] = Just (sqrt x)
performCalculate Abs [x] = Just (abs x)
performCalculate _ _ = Nothing

-- CITE: Tutorials Point 
-- URL: https://www.tutorialspoint.com/haskell/haskell_basic_operators.htm
-- HELP: I learned how simple mathemtical operations work in Haskell


-- This function handles the input and output from the user. It asks for a mathematical operation. 
-- This function also does error handling for an mathematical operation that is inputted incorrectly. 
letOperation :: IO MathOperation
letOperation = do
  putStrLn "Enter an operation from the following (+, -, *, /, sqrt, abs):"
  opStr <- getLine
  let opMaybe = doOperation opStr
  case opMaybe of
    Just op -> return op
    Nothing -> do
      putStrLn "This is an invalid operation. Please try again!"
      letOperation

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/156013/haskell-syntax-for-a-case-expression-in-a-do-block
-- HELP: I used this to learn how to use a do block in Haskell.

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- HELP: I learned how to use let inside the body of a block code 

-- This function is primarily to handle error handling of incorrect arguments inputted. It produces a message reminding 
-- the user to input "exactly x number of arguments"
letNumbers :: MathOperation -> IO [Double]
letNumbers op = do
  putStrLn $ "Enter " ++ requiredInputs op ++ " separated by a space:"
  input <- getLine
  let numbersOrNothing = map readMaybe $ words input
  if all isJust numbersOrNothing && length numbersOrNothing == expectedArgs op
    then return $ catMaybes numbersOrNothing
    else do
      putStrLn $ "Invalid input. Please enter exactly " ++ requiredInputs op
      letNumbers op

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/156013/haskell-syntax-for-a-case-expression-in-a-do-block
-- HELP: I used this to learn how to use a do block in Haskell.

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- HELP: I learned how to use let inside the body of a block code 

-- CITE: Tutorials Point 
-- URL: https://www.tutorialspoint.com/haskell/haskell_nested_if_else_statement.htm
-- HELP: I learned how to use if-else in Haskell


-- This function represents how many arguments is needed in MathOperation. Then it is converted to a string.
requiredInputs :: MathOperation -> String
requiredInputs Sqrt = "one number"
requiredInputs Abs = "one number"
requiredInputs _ = "two numbers"

-- This function checks the number of arguments inputted by the user for  each math operation. It is specifically
-- useful for square roots and absolute values that can only take 1 argument. 
expectedArgs :: MathOperation -> Int
expectedArgs Sqrt = 1
expectedArgs Abs = 1
expectedArgs _ = 2

-- This main function controls the operational flow of the whole program
-- It also does error handling if no number is inputted. 
main :: IO ()
main = do
  op <- letOperation
  nums <- letNumbers op
  let result = performCalculate op nums
  case result of
    Just r -> print r
    Nothing -> putStrLn "Error: Invalid input or operation."


-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/156013/haskell-syntax-for-a-case-expression-in-a-do-block
-- HELP: I used this to learn how to use a do block in Haskell.

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- HELP: I learned how to use let inside the body of a block code 

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/70415532/express-a-case-of-pattern-more-elegantly-in-haskell
-- HELP: I learned how to use 'case ... of' for case expressions and pattern matching 
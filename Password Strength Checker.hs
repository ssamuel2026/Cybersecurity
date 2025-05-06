-- Author: Samuel Cho 
-- Version: GHCi, version 9.4.8 but used online compiler to compile by Tutorials Point
-- Project: Password Strength Checker. I decided to make this project because I am really interested in cybersecurity.

--This import statement is needed for the password strength checker to run properly.
import Data.Char (isDigit, isUpper, isLower, isPunctuation)

-- CITE: Hackage 
-- URL: https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Char.html
-- HELP: I learned how to incorporate the Data.Char import statement 

-- This chunk of the code shows a record field of different data types called PasswordRequirements. 
-- The purpose of the data type PasswordRequirements is specify the conditions for which the password being made needs to meet. 
data PasswordRequirements = PasswordRequirements
  { minLength       :: Int   -- This is a field type of int (number)
  , needUppercase   :: Bool  -- This is a field type of bool (true or false)
  , needLowercase   :: Bool  -- This is a field type of bool (true or false)
  , needDigits      :: Bool  -- This is a field type of bool (true or false)
  , needSpecialChars:: Bool   -- This is a field type of bool (true or false)
  } deriving (Show)

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/54589842/getting-haskell-record-fields-based-on-their-type
-- HELP: I used to learn how to do record field of data types neatly/concisely using this structure above.

-- This function checks if the inputted password by the user checks all of the boxes of the default requirements.
defaultRequirements :: PasswordRequirements
defaultRequirements = PasswordRequirements
  { minLength        = 8     -- Field type of int with a minimum length of 8
  , needUppercase    = True  -- Field type of bool set to true
  , needLowercase    = True  -- Field type of bool set to true
  , needDigits       = True  -- Field type of bool set to true
  , needSpecialChars = True  -- Field type of bool set to true
  }

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/54589842/getting-haskell-record-fields-based-on-their-type
-- HELP: I used to learn how to record field of data types neatly/concisely 


-- This function checks to see whether the user's inputted password meets 
-- at least the minimum length requirement.
checkLength :: PasswordRequirements -> String -> Bool
checkLength policy password = length password >= minLength policy


-- This function checks to see whether the user's inputted password has at 
-- least one upper case letter.
checkUppercase :: PasswordRequirements -> String -> Bool
checkUppercase policy password = not (needUppercase policy) || any isUpper password


-- This function checks to see whether the user's inputted password has at
-- least one lowercase letter.
checkLowercase :: PasswordRequirements -> String -> Bool
checkLowercase policy password = not (needLowercase policy) || any isLower password


-- This function checks to see whether the user's inputted password has at
-- least one digit in it.
checkDigits :: PasswordRequirements -> String -> Bool
checkDigits policy password = not (needDigits policy) || any isDigit password


-- This function checks to see whether the user's inputted password has at
-- least one special character in it. 
checkSpecialChars :: PasswordRequirements -> String -> Bool
checkSpecialChars policy password = not (needSpecialChars policy) || any isPunctuation password

-- Aggregates all validations and identifies password strength
-- This function assesses the strength of the password by checking if it satisfies all of the requirements 
-- provided by the record field data type PasswordRequirements.
checkPasswordStrength :: PasswordRequirements -> String -> Int
checkPasswordStrength policy password
  | not (checkLength policy password)      = 1   -- not = password doesn't have at least 8 characters.
  | not (checkUppercase policy password)   = 2   -- not = password doesn't have at least one uppercase character.
  | not (checkLowercase policy password)   = 3   -- not = password doesn't have at least one lowercase character.
  | not (checkDigits policy password)      = 4   -- not = password doesn't have at least one number.
  | not (checkSpecialChars policy password)= 5   -- not = password doesn't have at least one special character.
  | otherwise                              = 6   -- otherwise = presents error message for error handling 

  -- CITE: Learn You A Haskell for Great Good!
  -- URL: https://learnyouahaskell.com/syntax-in-functions#:~:text=otherwise%20is%20defined%20simply%20as,guards%20check%20for%20boolean%20conditions.
  -- HELP: I learned how to utilize a structure called guard that is commonly used in Haskell for boolean conditions.

-- This fucnction handles all of the error handling for this program. The error messages appear when a requirement 
-- that the user inputted does not satisfy the given requirements. 
passwordErrorMessage :: Int -> String
passwordErrorMessage strength = case strength of
  1 -> "Your password is too short (needs to be at least 8 characters)."
  2 -> "Your password must contain at least one uppercase letter."
  3 -> "Your password must contain at least one lowercase letter."
  4 -> "Your password must contain at least one digit."
  5 -> "Your password must contain at least one special character."
  _ -> "Invalid password strength value." 

-- Function to get password and validate it
-- This function covers the input and output of this code. It does this by asking the user to enter a password.
-- If it satisfies all 6 requirements, than it prints the message, "Your password is strong!" which terminates the program. 
getPasswordAndValidate :: PasswordRequirements -> IO ()
getPasswordAndValidate policy = do
  putStrLn "Enter a password: "
  password <- getLine
  let strength = checkPasswordStrength policy password
  if strength == 6
    then putStrLn "Your password is strong!"
    else do
      putStrLn $ passwordErrorMessage strength
      getPasswordAndValidate policy

-- CITE: Stack Overflow 
-- URL: https://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- HELP: I learned how to use let inside the body of a block code 


-- This main function controls the operational flow of the whole program.
main :: IO ()
main = getPasswordAndValidate defaultRequirements
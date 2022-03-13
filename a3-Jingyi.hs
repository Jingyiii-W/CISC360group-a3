-- CISC 360 a3, Winter 2022

-- SEE THE FILE a3.pdf
-- for instructions

module A3
where
import Data.List

-- Q1:
-- Add your student ID (if in a group of 2, write the second student's ID in a comment):
student_id :: Integer
student_id = 20155672  -- 

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.

{-
Q2: Truth Tables

To build a truth table for a formula, there are 4 steps:

  1) Traverse the formula to find all atomic propositions (propositional variables).

  2) Find all the possible valuations---combinations of True and False
      for the atomic propositions in the formula.

  3) Evaluate the formula for each valuation obtained in (2).

  4) Use the results of (1-3) to build the table.

In this question, you will implement steps (1-3).
-}

-- Variable is a synonym for String.
type Variable = String

-- In our simplified version of classical propositional logic,
-- we have the following definition for a Formula:
data Formula = Top                      -- truth (always true)
             | Bot                      -- falsehood (contradiction) (not used in Q3)
             | And Formula Formula      -- conjunction
             | Or Formula Formula       -- disjunction
             | Implies Formula Formula  -- implication
             | Not Formula              -- negation (not used in Q3)
             | Atom Variable            -- atomic proposition ("propositional variable")
             deriving (Eq, Show)

-- Some atoms, for convenience
vA = Atom "A"
vB = Atom "B"
vC = Atom "C"
vD = Atom "D"
vE = Atom "E"
vF = Atom "F"

-- Some example formulas that you can use to test your functions
formula1  = Implies (And vA vB) vC
formula2  = Implies Bot (And vA vB)
formula3  = Implies (And vA vB) Top
formula4  = And (Implies vA (And vB vC)) (And vD vE)
formula5  = And vA vB
formula6  = Not vA
formula7  = Implies vA vB
formula8  = Or vA (Not vA)
formula9  = Or vA (Not vB)

-- A Valuation is a list of pairs corresponding to a truth value (i.e. True or False)
--  for each atom in a formula
type Valuation = [(Variable, Bool)]

-- A TruthTable is an enumeration of the valuations for a given formula,
-- with each valuation paired with the corresponding evaluation of that formula.
-- (This corresponds to a truth table with no "intermediate columns".)
data TruthTable = TruthTable [(Valuation, Bool)]

{-
   This function is here so that when you print a TruthTable in GHCi, the table is nice and readable.
   You don't need to understand how this works to complete the assignment.
-}
instance Show TruthTable where
  show (TruthTable rows) =
    case rows of
      [] -> ""
      ([], result) : _ -> "   result is " ++ pad_show result ++ "\n"
      ((c,b) : valu, result) : xs -> 
        c ++ "=" ++ (pad_show b) ++ "   "
          ++ show (TruthTable [(valu,result)])
          ++ show (TruthTable xs)
    where
      pad_show True  = "True "
      pad_show False = "False"

{-
  Q2a: getAtoms:

  Traverse a formula and build a list of all Atoms in the formula, without duplicates.

  You may use the built-in function "nub", which takes a list and returns the list
  without duplicates.
-}
getAtoms :: Formula -> [Variable] -> Formula -> [Variable]

getAtoms vList Top               = vList
getAtoms vList Bot               = vList

getAtoms vList (ATOM v)          = nub (v:vList)

getAtoms vList (Not phi)         = getAtoms vList phi
getAtoms vList (And phi1 phi2)   = getAtoms (getAtoms vList phi1) phi2
getAtoms vList (Or phi1 phi2)    = getAtoms (getAtoms vList phi1) phi2
getAtoms vList (Implies phi psi) = getAtoms (getAtoms vList phi) psi

{-
   Q2b: getValuations:

   Build a list of all possible valuations for a set of atomic propositions
-}
getValuations :: [Variable] -> [Valuation]
getValuations []       = [[]]
getValuations (c : cs) = let f xs = [(c, True) : xs, (c,False) : xs] in concat (map f (getValuations cs))

{-
  Hint: To apply a function f to every element of a list xs,
   write  map f xs.
  For example, the following adds 1 to the start of every list
   in a list of lists [[2,3], [2,4]]:
   map (\ys -> 1 : ys) [[2,3], [2,4]]  ==  [[1,2,3], [1,2,4]]
-}

{-
   Q2c: evalF:
    Evaluate a formula with a particular valuation,
     returning the resulting boolean value
-}
evalF :: Valuation -> Formula -> Bool
evalF _    Top                 = True
evalF _    Bot                 = False
evalF valu (Not phi1)          = not (evalF valu phi1)
evalF valu (ATOM c)            = case (lookup c valu) of {(Just a) -> a; _ -> error "This is not valid valuations"}
evalF valu (And phi1 phi2)     = (evalF valu phi1) && (evalF valu phi2)
evalF valu (Or phi1 phi2)      = (evalF valu phi1) || (evalF valu phi2)
evalF valu (Implies phi1 phi2) = (not (evalF valu phi1)) || (evalF valu phi2)

-- buildTable:
--  Build a truth table for a given formula.
--  You can use this function to help check your definitions
--  of getAtoms, getValuations and evalF.
buildTable :: Formula -> TruthTable
buildTable psi =
  let valuations = getValuations (getAtoms psi)
  in
    TruthTable (zip valuations
                    (map (\valu -> evalF valu psi) valuations))


{-
Q3: Tiny Theorem Prover

    In this question, you will complete an implementation of a
    continuation-based backtracking theorem prover that supports the rules
    given in a3.pdf.
 
    The prover is structured using two functions that do all the work,
    and one function that "kicks off" the process by passing continuations:

       prove'      looks at the goal formula (the formula we're trying to prove),
                     trying the -Right rules;
 
       prove_left  looks at the assumptions, trying the -Left rules;

       prove       kicks off the process by calling prove'.

    [X] Use-Assumption
    [ ] Top-Right
    [X] Implies-Right
    [ ] And-Right
    [ ] Or-Right-1 and Or-Right-2
    
    [X] Implies-Left
    [X] And-Left
    [ ] Or-Left

    You do not need to handle the Bot and Not formulas in this question.
-}

-- a Context is a list of Formulas, representing assumptions
type Context = [Formula]

{-
  prove': Given a context, a formula, and two continuations representing success and failure,
          call the appropriate continuation.
-}
prove' :: Context         -- formulas being assumed (to the left of the turnstile  |-  )
       -> Formula         -- goal formula to be proved (to the right of the turnstile)
       -> (() -> b,       -- kSucceed: call this if we proved the formula
           () -> b)       -- kFail: call this if we can't prove the formula
       -> b
prove' ctx goal (kSucceed, kFail) =
  let call_prove_left () = prove_left ctx ([], ctx) goal (kSucceed, kFail)
  in
    if elem goal ctx then  -- Use-Assumption rule
      kSucceed ()
    else
      case goal of
        Top               -> -- Top-Right rule
                             undefined
                             
        Implies phi psi   -> -- Implies-Right rule
                             prove' (phi : ctx) psi (kSucceed, kFail)
        
        And phi1 phi2     -> -- And-Right rule
                             undefined
                                              
        Or phi1 phi2      -> -- Or-Right rules (try Or-Right-1, if it fails, try Or-Right-2)
                             undefined
        
        _                 -> -- Can't use any of the -Right rules at this moment, so:
                             call_prove_left ()

{-
  prove_left: Given an original context,
                    a context that prove_left has already processed,
                    a context that prove_left has not processed,
                    a goal formula,
                    and two continuations,
                    call the appropriate continuation.
-}
prove_left :: Context              -- the original context
           -> (Context, Context)   -- the "done" context, and the "to do" context
           -> Formula              -- the goal formula
           -> (() -> b,            -- kSucceed: call this if we proved the formula
               () -> b)            -- kFail: call this if we can't prove the formula
           -> b
           
prove_left original (done, []) goal (kSucceed, kFail) =
                       --  ^^ the "to do" context is empty, so there's nothing remaining to examine
    if original == done then
        -- prove_left looked at everything but didn't change the context at all, so fail
        kFail ()
    else
        -- prove_left changed something, so we are giving prove' stronger assumptions
        prove' done goal (kSucceed, kFail)
        
prove_left original (done, focus : rest) goal (kSucceed, kFail) =

    let leave_alone () =   -- leave_alone: Call this to move "focus", the head of the to-do list,
                           --              into the "done" list without changing "focus"
            prove_left original (done ++ [focus], rest) goal (kSucceed, kFail)
    in
      case focus of
        Implies phi1 phi2 ->   -- Implies-Left rule
            prove' (done ++ rest) phi1 (-- kSucceed:
                                        \() -> prove' (done ++ [phi2] ++ rest)
                                                       goal
                                                       (kSucceed, kFail),
                                        -- kFail:
                                        leave_alone)
        
        And phi1 phi2 ->       -- And-Left rule
            prove_left original (done, [phi1, phi2] ++ rest) goal (kSucceed, kFail)
        
        Or phi1 phi2 ->        -- Or-Left rule
            undefined
        
        focus -> leave_alone ()

{-1
  prove: Given a context and a formula,
         return True if the formula is provable using the rules given in a3.pdf,
            and False otherwise.
-}
prove :: Context -> Formula -> Bool
prove ctx goal = prove' ctx goal (\() -> True,   -- On success, return True
                                  \() -> False)  -- On failure, return False


test_imp1 = prove [Implies vB vC] (Implies vB vC)
test_imp2 = prove [Implies vB vC] (Implies (And vB vB) vC)
test_imp3 = not (prove [Implies (And vB vD) vC] (Implies vB vC))

test_imps = test_imp1 && test_imp2 && test_imp3

test_or1 = prove [Or vA vB] (Or vB vA)
test_or2 = prove [Or vC (And vA (Implies vA vB))] (Or vB vC)
test_or3 = prove [vA, Or (Implies vA vB) (Implies vA vC)] (Or vB (Or vB vC))
test_or4 = not (prove [Or vC (And vA (Implies vA vD))] (Or vB vC))
test_ors = test_or1 && test_or2 && test_or3 && test_or4

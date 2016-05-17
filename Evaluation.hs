module Evaluation where

import Data.Map as M
import Abssimp

data Side = L | R
type State = M.Map SIdent Integer
type Cont = State -> State
type SCont = Side -> Cont
type ECont = Integer -> State

emptyCont :: Cont
emptyCont = id

emptyState :: State
emptyState = M.empty


showprog :: Program -> String
showprog (PAll s) = show $ prog s
prog :: Stmt -> State
prog s = stmt s emptyCont emptyCont R emptyState 

c :: Side -> Cont -> Cont -> Cont
c R _ cont = cont
c L cont _ = cont

stmt :: Stmt -> Cont -> Cont -> Side -> State -> State
stmt SSkip cleft cright L state 	= cleft state
stmt SSkip cleft cright R state 	= cright state

stmt SRight _ cright _ state 	= cright state
stmt SLeft cleft _ _ state 	= cleft state

stmt (SAss x e) cl cr side state 	= 
	expr e (\i -> c side cl cr $ M.insert x i state) state

stmt (SIf e s) cl cr side state		=
	expr e (\i -> if i == 0 then c side cl cr state else stmt s cl cr side state) state

stmt (SIfElse e s1 s2) cl cr side state		=
	expr e (\i -> if i == 0 then stmt s2 cl cr side state else stmt s1 cl cr side state) state

--stmt (STwo s1 s2) cl cr R state 	= 
--	stmt s1 cl (\state' -> stmt (STwo s1 s2) cl cr R state') R state
--stmt (STwo s1 s2) cl cr L state	=
--	stmt s2 (\state' -> stmt (STwo s1 s2) cl cr R state') cr R state

stmt (STwo s1 s2) cl cr R state		=
	stmt s1 cl cr' R state where
		cr' = stmt s2 cl' cr R where
			cl' = stmt s1 cl cr' R
	 
stmt (STwo s1 s2) cl cr L state		=
	stmt s2 cl' cr L state where
		cl' = stmt s1 cl cr' L where
			cr' = stmt s2 cl' cr L
	
expr :: Exp -> ECont -> State-> State
expr (EAdd e1 e2) ec s 	= ebin e1 e2 ec (+) s
expr (ESub e1 e2) ec s 	= ebin e1 e2 ec (-) s
expr (EMul e1 e2) ec s 	= ebin e1 e2 ec (*) s
expr (EMin e) ec s 	= expr e (\i -> ec $ 0 - i) s
expr (EId x) ec s 	= ec $ val where
	val = case M.lookup x s of
		Just v -> v
		Nothing -> 0
expr (EInt i) ec _ 	= ec i


ebin :: Exp -> Exp -> ECont -> (Integer -> Integer -> Integer) -> State -> State
ebin e1 e2 ec fun s = expr e1 (\i1 -> expr e2 (\i2 -> ec $ fun i1 i2) s) s


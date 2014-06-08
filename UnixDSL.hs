--{-# LANGUAGE FlexibleInstances #-} 

module UnixDSL where
-- cabal install split -- for spliting strings
-- FlexibleInstances to define list instance class

import Control.Monad.State
import Data.List.Split as S
import Data.Maybe
import Data.List 
import Control.Monad.Error
import Control.Monad.Writer  




type Name    = String
data Tree    = T Name [Tree]
             | F Name String
             deriving (Show,Eq) 
root = ["/"]



toString :: Tree -> Int -> String
toString t n = concat[(concat$ take n  (repeat " ")) ,getType t ++ " " ++   getName t, "\n" ,concat$ map (\x -> toString x (n+2)) (getChildren t)]
  where 
    getChildren (T _ l) = l
    getChildren (F _ _) = []

printT t =   liftIO  $ putStr $ toString  t 0

isFile :: Tree -> Bool
isFile (F _ _) = True
isFile _ = False

files :: Tree -> [Tree]
files (T _ l) = filter (isFile) l

dirs  :: Tree -> [Tree]
dirs (T _ l) = filter (not. isFile) l

getName :: Tree -> String
getName (T n _) = n
getName (F n _) = n

getType :: Tree -> String
getType (T n _) = "+"
getType (F n _) = "-"

parent x = reverse $ drop 1 $ reverse x

extractNames :: [Tree] -> [String]
extractNames [] = []
extractNames (h:t) = (getName h) : extractNames t


copy :: DST -> Tree -> Tree -> Tree
copy (s:l:[]) src t = if getName t /= s then t
                      else addChild src2 t
  where src2 = rename src l
copy (s:l)    src t = if getName t /= s
                      then t
                      else case t of
                          T name children -> T name (map (copy l src) children)
                          F _ _  -> t

addChild :: Tree -> Tree -> Tree
addChild src t = let t' = deleteN (getName src) t
              in case t' of
                T n l -> T n (src:l)

  


deleteR :: DST -> Tree -> Tree
deleteR (s:l:[]) t = if getName t /= s then t
                     else deleteN l t
deleteR (s:l)  t = if getName t /= s
                      then t
                      else case t of
                          T name children -> T name (map (deleteR l) children)
                          F _ _  -> t
                     
                   

deleteN :: String -> Tree -> Tree 
deleteN  s t@(T name l)  =  T name (filter (\x-> getName x /= s) l)


lookUp :: String -> [Tree] -> Tree
lookUp n t =let k = elemIndex n (extractNames t)
                in case k of
                  Just k' -> t !! k'
                  Nothing -> T  "" []
fetch :: [String] -> Tree -> Maybe Tree
fetch (s:[]) f@(F n _) | n == s =  Just f
                       | otherwise = Nothing
fetch (s:[]) t@(T n _) | n == s =  Just t 
                       | otherwise = Nothing
fetch (s:l:x) t@(T n ll) | n == s = fetch (l:x) $ lookUp (l)  ll 
                         | otherwise = Nothing
fetch _ _ = Nothing                 


rename :: Tree -> String -> Tree
rename node name = case node of
   T n l -> T name l
   F n c -> F name c


type OptionalArgs =  [Predicate]
type SRC = [String]
type DST = [String]

-- Effectful and Effectless commands
data CMD  = LS OptionalArgs DST 
          | MV SRC DST  
          | CP SRC DST
          | CD DST
          | RM DST
          | PWD  
          | Pipe CMD CMD
          | MKDIR DST
          | MKFile DST String
          | Find OptionalArgs DST 
          | Backup
          | Restore
          | Fetch SRC  -- internal use
          | SHOW       -- internal use
          deriving (Show)

data Predicate = Pr (Tree -> Bool)
instance Show Predicate where
  show x = "A predicate function"

type Action        = (CMD, BaseState)
type Log = [Action]
type BaseState = (Tree, [String])
type FState = [BaseState]

sep = "."
canonicalize :: DST -> DST -> DST
canonicalize  cwd path = concat $  map (\x -> if x == sep then cwd else [x]) path


andize :: [Predicate] -> Tree -> Bool
andize [] _ = True
andize ((Pr fun):ls) c = fun c && andize ls c


type EVAL a = ErrorT String  (WriterT Log (StateT FState IO)) a


runEVAL :: FState -> EVAL a -> IO ((Either String a, Log), FState) 
runEVAL st ev = runStateT (runWriterT (runErrorT ev)) st



sem :: CMD -> EVAL Tree
sem (Pipe cmd1 cmd2) = do
  sem cmd1
  sem cmd2


sem (LS options l) = do 
  ((t,cwd):prev) <- get
  let
    n | l == [] = cwd
      | otherwise = canonicalize cwd l
    in do node <- sem (Fetch n)
--          liftIO $ print $ "under " ++ (toStr cwd) ++ "\n"
          liftIO $ print $ extractNames $ filter (andize options)  (getChildrenOrSelf node)
          return t
  
  
 {- let
    n | l == [] = cwd
      | otherwise = canonicalize cwd l
    fun | ("d" `elem` options) && (not $ "f" `elem` options) = dirs
        | (not$ "d" `elem` options) &&  ("f" `elem` options) = files
        | otherwise  = getChildren
    in do  node <- sem (Fetch n)
           case node of
             T _ _ -> liftIO $ print (extractNames (fun node))
             F _ _ -> throwError "Cannot perform ls on a file"
           return t
-}
  where
   getChildrenOrSelf :: Tree -> [Tree]
   getChildrenOrSelf node = case node of
      T n c -> c
      f@(F n c) -> [f]
   
  
sem (CD dir) = do
  ((t,cwd):prev) <- get
  if null $ canonicalize cwd dir
    then sem $ CD root
    else  let
             dir' = canonicalize cwd dir
             t' =  fetch  dir t 
           in case t' of 
             Just n@(T _ _) -> do put $ (t, dir'):prev
                                  return n
             Nothing -> do liftIO $ putStrLn $"directory not found: " ++ toStr dir
                           throwError $ "directory not found: " ++ toStr dir 
                           return t

    
sem PWD     = do
  ((t,cwd):prev) <- get
  liftIO $ print   cwd
  return t

sem cmd@(RM target) = do
  ((t,cwd):prev) <- get
  node <- sem (Fetch $ canonicalize cwd target)
  let
    target' = canonicalize cwd target
    k = deleteR target' t
    cur = (k, cwd)
    in do 
          put $ cur:prev
          tell [(cmd, cur)]
          return k
sem cmd@(CP src dst) = do
  ((t,cwd):prev) <- get
  srcNode <- sem (Fetch $ canonicalize cwd src)
  let
    dst' = canonicalize cwd dst
    parentNode = fetch (parent dst') t
    in if parentNode == Nothing || isFile (fromJust parentNode )
      then do liftIO $ putStrLn $ "Could not find :" ++ show dst
              throwError "could not find the parent direcory of destination"
      else let t' = if fetch dst' t /= Nothing then deleteR dst' t else t
               t'' = copy dst' srcNode t
               cur = (t'',cwd)
           in do -- liftIO $ print $ "intermediate " ++ show t'
                 put $ cur:prev
                 tell [(cmd, cur)]
                 return t''
sem SHOW = do
  ((t,cwd):prev) <- get
  printT t
  return t

sem (MV src dst) = do sem (CP src dst)
                      sem (RM src)

sem cmd@(Backup) = do
  (cur@(t,cwd):prev) <- get
  put $ (t,cwd):(t, root):prev
  tell [(cmd, cur)]
  return t
       
sem cmd@(Restore) = do
  (h:prev)<- get
  if null prev
    then do 
       liftIO $ putStrLn "No prior backup exists!"
       throwError "No prior backup exists!"
    else let
            cur@(t,root) = head prev
         in do put prev
               tell [(cmd, cur)]
               return t
          

sem (Fetch node) = do
  ((t,cwd):prev) <- get
  let 
     n = fetch (canonicalize cwd node) t
    
     in do case n of
            Just node' -> do 
--              liftIO $ print node
              return node'
            Nothing    -> do 
              liftIO $ putStrLn $ "Unable to find the node!" ++ show node
              throwError $ "Unable to find the node!"
            

sem cmd@(MKDIR dst) = do
  ((t,cwd):prev) <- get
  let
    dst' = canonicalize cwd dst
    n = fetch dst' t
    in case n of
       Just _ -> do
         liftIO $ putStrLn $ "File or Dir with sama name exists! " ++ show dst
         throwError  "File or Dir with sama name exists!" 
       Nothing ->  let t' = copy dst' (T (last dst') []) t
                       cur = (t', cwd)
                   in do put $ cur:prev
                         tell [(cmd, cur)]
                         return t'

sem cmd@(MKFile dst content) = do
  ((t,cwd):prev) <- get
  let
    dst' = canonicalize cwd dst
    n = fetch dst' t
    in case n of
        Just _ ->  do
          liftIO $ putStr "File or Dir with sama name exists!"
          throwError "File or Dir with sama name exists!"
        Nothing -> let t' = copy dst' (F (last dst') content) t
                       cur = (t',cwd)
                   in do put $ (t', cwd):prev
                         tell [(cmd,cur)]
                         return t'

-- experimental
sem cmd@(Find fun dst) = do
   liftIO $ putStrLn $ "under:" ++ show dst  
   ((t,cwd):pwd) <- get
   sem $ LS fun dst
   t <- sem $ Fetch dst
   sem $ array $(map (\x -> Find fun (dst ++ [getName x]))) (getSubTree t)
   return t 
     where
       getSubTree :: Tree -> [Tree]
       getSubTree (T _ l) = filter (not. isFile) l
       getSubTree _ = []



array :: [CMD] -> CMD
array (x:[]) = x
array (x:y)  =  Pipe x (array y)


      
f1 = T "/" [T "bin" [F "ghci" "some stuff"], T "var" [T "mail" [F "alipour" "some emails!"], F "t2" "something"], F "t.c" "something"]


initState = [(f1, root)]



-- syntactic sugar

eval x =  runEVAL initState $ sem  x


  
(|||) =  Pipe 

ls = LS [] ["."]


toStr ::[String] -> String
toStr l = concat$ map ("/"++) (drop 1 l)

toList :: String -> [String]
toList "/" = ["/"]
toList x = if x!!0 == '.' then splitOn "/" x else "/" : drop 1 (splitOn "/" x)

cp :: String -> String -> CMD
cp src dst = CP  (toList src) (toList dst)

mv :: String -> String -> CMD
mv src dst = MV  (toList src) (toList dst)
rm dst = RM (toList dst)
cd dst = CD (toList dst)
pwd = PWD


restore = Restore
backup = Backup

view = SHOW
mkfile l  = MKFile(toList l)
mkdir l  = MKDIR (toList l)
fetchN l  = Fetch (toList l)
-- use syntactic suger ls = LS, or parametrize it
-- undo
-- scripting: go there and get *.txt files
-- change the cwd to list of strings



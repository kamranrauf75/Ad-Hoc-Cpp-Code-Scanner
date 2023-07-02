import System.IO
import Control.Monad
import Data.Char


main = do 
    theFile2 <- openFile "test.txt" ReadMode
    contents <- hGetContents theFile2
    let singlewords = words contents
    let f = func2 singlewords nums lalph ualph ""
    -- theFile <- openFile "test1.txt" WriteMode
    writeFile "test1.txt" (f)
    -- hClose "test1.txt" 

-- readFromFile = do
--     theFile2 <- openFile "test.txt" ReadMode
--     contents <- hGetContents theFile2
--     let singlewords = words contents
--     -- print singlewords
    
--     let f = func2 singlewords nums lalph ualph ""
--     print f
--     -- print contents
--     -- putStr contents
--     hClose theFile2

-- conts = readFromFile



-- func3 "" _ = ""
-- func3 conts line = if conts == "\n" then line else conts ++ line ++ func3 (drop 1 readFromFile) line

 
nums = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

lalph = ['a'..'z']

ualph = ['A'..'Z']

checkf [] _ = False
checkf (x:xs) elem = if [x] == elem then True else checkf (xs) elem

checkn [] _ = False
checkn (x:xs) elem = if x == elem then True else checkn (xs) elem

-- func2 [] _ _ _ = ""
-- func2 (x: y: z: xs) nums lalph ualph = 
--                     if x == "int" && (length y) > 1 && (checkn nums (take 1 y)) /= True && ((checkf lalph (take 1 y)) == True || (checkf ualph (take 1 y)) == True) && (z == ";")
--                     then "<keyword,int>, <identifier," ++ y ++ ">," ++ "<operator," ++ z ++ ">"
--                     else if (length x) > 1 && (checkn nums (take 1 x)) /= True && ((checkf lalph (take 1 x)) == True || (checkf ualph (take 1 x)) == True) && y == "=" && (all isDigit z)
--                         then "<identifier," ++ x ++ ">," ++ "<operator," ++ y ++ ">,<intConstant," ++ z ++ ">"

--                     else func2 xs nums lalph ualph



funckeys word = if word == "int"
                then "<keyword,int>"
                else if word == "float"
                    then "<keyword,float>"
                else if word == "bool"
                    then "<keyword,bool>"
                else if word == "double"
                    then "<keyword,double>"
                else if word == "void"
                    then "<keyword,void>"
                else if word == "while"
                    then "<keyword,while>"
                else if word == "for"
                    then "<keyword,for>"
                else if word == "if"
                    then "<keyword,if>"
                else if word == "else"
                    then "<keyword,else>"
                else if word == "char"
                    then "<keyword,char>"
                else if word == "array"
                    then "<keyword,array>"
                else if word == "struct"
                    then "<keyword,struct>"
                else if word == "class"
                    then "<keyword,class>"
                else if word == "break"
                    then "<keyword,break>"
                else if word == "case"
                    then "<keyword,case>"
                else if word == "return"
                    then "<keyword,return>"
                else if word == "cout"
                    then "<keyword,cout>"
                else if word == "string"
                    then "<keyword,string>"
                else if word == "cin"
                    then "<keyword,cin>"
                else if word == "true"
                    then "<keyword,true>"
                else if word == "false"
                    then "<keyword,false>"
                else if word == "endl"
                    then "<keyword,endl>"
                else "f"


upcheck _ [] = False
upcheck x (y:ys) = if x == y then True else upcheck x ys

uppercheck "" _ = False
uppercheck (x:xs) ualph = if upcheck x (ualph) == True then True else uppercheck xs ualph

lowercheck "" _ = False
lowercheck (x:xs) lalph = if upcheck x (lalph) == True then True else uppercheck xs lalph

funcstr string = if ((checkn nums (take 1 string)) /= True && ((checkf lalph (take 1 string)) == True || (checkf ualph (take 1 string)) == True)) == True
                    then (if (length string) > 1 then (if (uppercheck string ualph) == True then "<identifier, " ++ string ++ ">" else  "<error, " ++ string ++ ">") else  "<error, " ++ string ++ ">")
                else
                    "f"

 
funcparsedec [] = ""
funcparsedec (x:y:z:xs) = 
    if (x /= ")") then 
        (if ((funckeys x /= "f") && (fundems x /= "f")) 
            then (if ((funcstr x /= "f") && (funcstr x /= "<error, " ++ x ++ ">")) 
                    then (funckeys x ++ funcstr y ++ fundems z ++ funcparsedec xs) 
                    else (funckeys x ++ funcstr y ++ fundems z ++ funcparsedec xs))
            else funckeys x ++ funcstr y ++ fundems z ++ funcparsedec xs)    
    else "" 

funcops word = if word == "+"
                then "<operator,+>"
                else if word == "-"
                    then "<operator,->"
                else if word == "*"
                    then "<operator,*>"
                else if word == "/"
                    then "<operator,/>"
                else if word == "%"
                    then "<operator,%>"
                else if word == "<"
                    then "<operator,<>"
                else if word == "<="
                    then "<operator,<=>"
                else if word == ">"
                    then "<operator,>>"
                else if word == ">="
                    then "<operator,>=>"
                else if word == "="
                    then "<operator,=>"
                else if word == "=="
                    then "<operator,==>"
                else if word == "!="
                    then "<operator,!=>"
                else if word == "&&"
                    then "<operator,&&>"
                else if word == "||"
                    then "<operator,||>"
                else if word == "]"
                    then "<operator,]>"
                else if word == "["
                    then "<operator,[>"
                else if word == "("
                    then "<operator,(>"
                else if word == ")"
                    then "<operator,)>"
                else if word == "{"
                    then "<operator,{>"
                else if word == "}"
                    then "<operator,}>"
                else if word == "--"
                    then "<operator,-->"
                else if word == "++"
                    then "<operator,++>"
                else if word == "<<"
                    then "<operator,<<>"
                else if word == ">>"
                    then "<operator,>>>"
                else "f"


fundems word = if word == ":"
                then "<delimiter,:>"
                else if word == ";"
                    then "<delimiter,;>"
                else if word == ","
                    then "<delimiter,,>"
                else "f"

funcomms word = if word == "//"
                    then "scomm"
                else if word == "/*"
                    then "mcomm"
                else "f"

heptalcheck num = if num > 6 && num < 0 then False else True

fmulcomm [] = ""
fmulcomm (x:xs) = if x /= "*/" then x ++ " " ++ fmulcomm xs else "" 

strcol [] = ""
strcol (x:xs) = if x /= "\"" then x ++ " " ++ strcol xs else "" 

pointfinder "" = False
pointfinder (x:xs) = if [x] /= "." then True else pointfinder xs

 


-- func2 [x:_:_:_] _ _ _ _ = ""
-- func2 [x:y:_:_] _ _ _ _ = ""
-- func2 [x:y:z:_] _ _ _ _ = ""

checksss [] = False
checksss (x:xs) = if [x] == "\"" then True else checksss xs

func2 [] _ _ _ res = res
func2 (x: xs) nums lalph ualph res = 
                    -- if ((funckeys x /= "f") && (fundems z /= "f")) 
                    --     then (if ((funcstr y /= "f") && (funcstr y /= "<error, " ++ y ++ ">")) 
                    --         then res ++ funckeys x ++ funcstr y ++ fundems z ++ "\n" 
                    --         else res ++ funckeys x ++ funcstr y ++ fundems z ++ "\n") 
                    
                    if x == "/*" then (res ++ "<multiline-comment,/" ++ fmulcomm xs ++ "/>" ++ func2 (xs) nums lalph ualph res)

                        -- (if x /= "*/" then x ++ y ++ z ++ (func2 xs nums lalph ualph) else "error") 

                    -- else if ((funckeys x /= "f") && ((uppercheck y ualph == True) || (lowercheck y lalph == True)) && z == "(")
                    --     then ( res ++ funckeys x ++ funcstr y ++ fundems z ++ funcparsedec xs)
                   
                    -- else if x == "("
                    --     then ( res ++ funckeys x ++ funcstr y ++ fundems z ++ funcparsedec xs)

                    -- else if (x == "{") then res ++ funcops x

                    -- else if (x == "}") then res ++ funcops x

                    else if (funckeys x /= "f") then res ++ funckeys x ++ func2 (xs) nums lalph ualph res

                    else if (pointfinder x == False) && (all isDigit x) then res ++ "<intConstant,"++ x ++ ">" ++ func2 (xs) nums lalph ualph res
                    
                    -- else if (pointfinder x == True) then res ++ "<floatConstant,"++ x ++ ">" ++ func2 (xs) nums lalph ualph res

                    -- else if (checksss x == True) then (res ++ "<stringConstant,/" ++ x ++ " " ++ strcol xs ++ "/>" ++ func2 (xs) nums lalph ualph res)

                    else if (funcstr x /= "f") then res ++ funcstr x ++ func2 (xs) nums lalph ualph res


                    else if (fundems x /= "f") then res ++ fundems x ++ func2 (xs) nums lalph ualph res

                    else if (funcops x /= "f") then res ++ funcops x ++ func2 (xs) nums lalph ualph res


                    -- else if (funckeys x /= "f") then funckeys x
                    -- else if ()

                    -- else if (funckeys x /= "f") then funckeys x

                    else res ++ func2 (xs) nums lalph ualph res

-- func2 (x: y: z: xs) nums lalph ualph res = 
--                     if ((funckeys x /= "f") && (fundems z /= "f")) 
--                         then (if ((funcstr y /= "f") && (funcstr y /= "<error, " ++ y ++ ">")) 
--                             then res ++ funckeys x ++ funcstr y ++ fundems z ++ "\n" 
--                             else res ++ funckeys x ++ funcstr y ++ fundems z ++ "\n") 
                    
--                     else if x == "/*" then (res ++ "<multiline-comment,/" ++ y ++ " " ++ z ++ " " ++ fmulcomm xs ++ "/>\n")

--                         -- (if x /= "*/" then x ++ y ++ z ++ (func2 xs nums lalph ualph) else "error") 

--                     else if ((funckeys x /= "f") && ((uppercheck y ualph == True) || (lowercheck y lalph == True))  && z == "(")
--                         then ( res ++ funckeys x ++ funcstr y ++ fundems z ++ funcparsedec xs)


--                     else if (x == "{") then res ++ funcops x

--                     else if (x == "}") then res ++ funcops x

--                     else if (funckeys x /= "f") then res ++ funckeys x

--                     else if (pointfinder x == False) && (all isDigit x) then res ++ "<intConstant,"++ x ++ ">" 
                    
--                     else if (pointfinder x == True) then res ++ "<floatConstant,"++ x ++ ">"

--                     else if (funcstr x /= "f") then res ++ funcstr y

--                     else if (fundems x /= "f") then res ++ fundems x

--                     else if (funcops x /= "f") then res ++ funcops x


--                     -- else if (funckeys x /= "f") then funckeys x
--                     -- else if ()

--                     -- else if (funckeys x /= "f") then funckeys x

--                     else func2
                                                     

                    --     && (length y) > 1 && (checkn nums (take 1 y)) /= True && ((checkf lalph (take 1 y)) == True || (checkf ualph (take 1 y)) == True) && (z == ";")
                    -- then "<keyword,int>, <identifier," ++ y ++ ">," ++ "<operator," ++ z ++ ">"
                    -- else if (length x) > 1 && (checkn nums (take 1 x)) /= True && ((checkf lalph (take 1 x)) == True || (checkf ualph (take 1 x)) == True) && y == "=" && (all isDigit z)
                    --     then "<identifier," ++ x ++ ">," ++ "<operator," ++ y ++ ">,<intConstant," ++ z ++ ">"
                    -- else func2 xs nums lalph ualph

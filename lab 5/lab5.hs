import System.IO

import qualified Text.Parsec.Token as Token
import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Expr
    (
        buildExpressionParser,
        Assoc( ..) ,
        Operator( .. )
    )

import Text.ParserCombinators.Parsec.Language ( emptyDef, GenLanguageDef ( .. ), LanguageDef )
import Text.Parsec ( alphaNum, letter, (<|>), eof, parse, char )
import Text.Parsec.String ( parseFromFile )
import Imp

impLanguageDef :: LanguageDef ()
impLanguageDef =
    emptyDef
    {
        commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        nestedComments = False,
        caseSensitive = True,
        identStart = letter,
        identLetter = alphaNum,
        reservedNames =
            [
                "while", "if", "else", "int", "bool",
                "true", "false", "read", "print"
            ],
        reservedOpNames =
            [
                "+", "-", "*", "/", "%",
                "==", "!=", "<", "<=",
                ">=", ">", "&&", "||", "!", "="
            ]
    }

impLexer :: Token.TokenParser ()
impLexer = Token.makeTokenParser impLanguageDef

identifier :: Parser String
identifier = Token.identifier impLexer

reserved :: String -> Parser ()
reserved = Token.reserved impLexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp impLexer

parens :: Parser a -> Parser a
parens = Token.parens impLexer

braces :: Parser a -> Parser a
braces = Token.braces impLexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep impLexer

string :: Parser String
string = Token.stringLiteral impLexer

comma :: Parser String
comma = Token.comma impLexer

integer :: Parser Integer
integer = Token.integer impLexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace impLexer

bool :: Parser Bool
bool = (reserved "true" >> return True) <|> (reserved "false" >> return False)

term :: Parser Exp
term =
    parens expression
    <|> (I <$> integer)
    <|> (Id <$> identifier)
    <|> (B <$> bool)

expression :: Parser Exp
expression = buildExpressionParser operators term
    where
        operators =

            [ [ prefix "!" Not
            ]
            , [ binary "*" (BinA Mul) AssocLeft
            ]
            , [ binary "+" (BinA Add) AssocLeft
            ]
            , [ binary "==" (BinE Eq) AssocNone
            , binary "<=" (BinC Lte) AssocNone
            ]
            , [ binary "&&" (BinL And) AssocLeft
            , binary "||" (BinL Or) AssocLeft
            ]
            ]
        binary name fun = Infix ( reservedOp name >> return fun)
        prefix name fun = Prefix ( reservedOp name >> return fun)

statement :: Parser Stmt
statement = ifStmt <|> whileStmt <|> asgnStmt <|> intStmt <|> boolStmt
            <|> readStmt <|> printStmt



intStmt :: Parser Stmt
intStmt = do
    reserved "int"
    ident <- identifier
    return (Decl TInt ident)

boolStmt :: Parser Stmt
boolStmt = do
    reserved "bool"
    ident <- identifier
    return (Decl TBool ident)

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- parens expression
    while <- statement
    return (While cond while)

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- parens expression
    thenS <- statement
    reserved "else"
    elseS <- statement
    return (If cond thenS elseS)

asgnStmt :: Parser Stmt
asgnStmt = do
    name <- identifier
    reservedOp "="
    exp <- expression
    return $ Asgn name exp

lineParser :: Parser [Stmt]
lineParser = do semiSep statement

printStmt :: Parser Stmt
printStmt = do
    reserved "print"
    parens $ do
        arg <- string
        comma
        exp <- expression
        return $ Print arg exp

readStmt :: Parser Stmt
readStmt = do
    reserved "read"
    parens $ do
        arg <- string
        comma
        ident <- identifier
        return $ Read arg ident

inBracesStmt = return braces statement

parseString :: String -> [Stmt]
parseString str =
    case parse lineParser "" str of
        Left e -> error $ show e
        Right r -> r

main = do
    result <- parseFromFile lineParser "1.imp"
    case result of
        Left err -> print err
        Right xs -> print xs
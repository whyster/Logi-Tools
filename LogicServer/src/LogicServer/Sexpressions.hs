module LogicServer.Sexpressions(Sexpr(..), m_parens, m_ident, sexpParser) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

sexprDef = emptyDef{
                     identStart = letter <|> char '\\'
                   , identLetter = alphaNum
                   }


TokenParser { parens        = m_parens
            , identifier    = m_ident
            , stringLiteral = m_literal
            } = makeTokenParser sexprDef


data Sexpr = Atom String
           | Func String [Sexpr]
          deriving (Show, Eq)

funcParser :: Parsec String u Sexpr
funcParser = m_parens $ do
    funcIdent <- m_ident <|> m_literal
    args <- sexpParser `sepBy` spaces
    return $ Func funcIdent args

atomParser :: Parsec String u Sexpr
atomParser = Atom <$> (m_literal <|> m_ident)

sexpParser :: Parsec String u Sexpr 
sexpParser = atomParser <|> funcParser


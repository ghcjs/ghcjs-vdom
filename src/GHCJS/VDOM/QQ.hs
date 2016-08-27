{-# LANGUAGE QuasiQuotes, DeriveDataTypeable, TemplateHaskell #-}
{-
  More efficient JavaScript literals with QuasiQuoters

  mostly experimental, might not stay
 -}
module GHCJS.VDOM.QQ (ch, children, att, attributes) where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           GHCJS.VDOM.Internal.Types

import           GHCJS.Types
import           GHCJS.Marshal

import           Control.Applicative

import           Data.Char
import qualified Data.List as L
import           Data.List.Split
import           Data.Typeable

import           System.IO.Unsafe

att :: QuasiQuoter
att = attributes

ch :: QuasiQuoter
ch = children

-- example: [props|a:1, b: null, c: x, d: y |]
-- every value is either a literal or a variable referring to a convertible Haskell name
-- fixme, this does not have a proper parser
attributes :: QuasiQuoter
attributes = QuasiQuoter { quoteExp = quoteProps }

quoteProps :: String -> Q Exp
quoteProps pat = jsExpQQ ('{':ffiPat++"}") (map mkName names)
                 (\x -> AppE (VarE 'unsafePerformIO) (AppE (VarE 'toJSVal) x))
                 (AppE (ConE 'Attributes'))
  where
    (names, ffiPat) = genpat 1 $ map (break (==':') . trim) (linesBy (==',') pat)
    isName [] = False
    isName (x:xs) = isAlpha x && all isAlphaNum xs
    genpat :: Int -> [(String,String)] -> ([String], String)
    genpat _ [] = ([], "")
    genpat k ((x,':':n):xs)
      | isName n' = (n':ns, x ++ ": $" ++ (show k) ++ p)
      | otherwise = (ns, x ++ ':' : n ++ sep ++ p)
      where
        n'       = trim n
        ~(ns, p) = genpat (k+1) xs
        sep      = if null xs then "" else ","
    genpat _ _ = error "invalid pattern"


-- example: [children|x,y,z|] for haskell names x,y,z :: VNode
children :: QuasiQuoter
children = QuasiQuoter { quoteExp = quoteChildren }

quoteChildren :: String -> Q Exp
quoteChildren pat = jsExpQQ ffiPat names (AppE (VarE 'unVNode)) (AppE (ConE 'Children'))
  where
    names  = map (mkName.trim) (linesBy (==',') pat)
    ffiPat = '[' : L.intercalate "," (map (('$':).show) (take (length names) [(1::Int)..])) ++ "]"

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

newtype QQCounter = QQCounter { getCount :: Int } deriving (Typeable, Show)

jsExpQQ :: String -> [Name] -> (Exp -> Exp) -> (Exp -> Exp) -> Q Exp
jsExpQQ pat args unwrap wrap = do
  c <- maybe 0 getCount <$> qGetQ
  n <- newName ("__ghcjs_vdom_qq_spliced_" ++ show c)
  let ffiDecl      = ForeignD (ImportF CCall Unsafe pat' n (ty $ length args))
      ty :: Int -> Type
      ty 0         = ref
      ty n         = AppT (AppT ArrowT ref) (ty (n-1))
      ref          = ConT ''JSVal
      ffiCall []     = (VarE n)
      ffiCall (y:ys) = AppE (ffiCall ys) (unwrap (VarE y))
      pat'           = "__ghcjs_javascript_" ++ L.intercalate "_" (map (show . ord) pat)
  qAddTopDecls [ffiDecl]
  qPutQ (QQCounter (c+1))
  return $ wrap (ffiCall $ reverse args)

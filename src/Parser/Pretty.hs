{-# LANGUAGE FlexibleInstances #-}

module Parser.Pretty where

import Data.List (intercalate)
import Parser.Syntax
import Text.PrettyPrint

class Pretty a where
  pp :: a -> Doc

maybe' :: Maybe a -> (a -> Doc) -> Doc
maybe' Nothing  _ = empty
maybe' (Just a) f = f a

instance Pretty NescFile where
  pp (I i) = text "not implemented"
  pp (C c) = pp c
-- 1

instance Pretty Component where
  pp (Component kind iden para attr spec imp) =
    vcat [ pp kind <+> pp iden <> maybe' para pp <+> maybe' attr pp <+> lbrace
         , nest 2 $ pp spec
         , rbrace
         , maybe' imp pp
         , text ""
         ]
-- 5

instance Pretty CompKind where
  pp Module = text "module"
  pp Configuration = text "configuration"
  pp GenericModule = text "generic module"
  pp GenericConfiguration = text "generic configuration"
-- 6

instance Pretty Implementation where
  pp (CI ci) = pp ci
  pp (MI mi) = text "implementation" <+> text mi
-- 7

instance Pretty CompParameters where
  pp (CompParameters xs) = parens $ text xs
-- 8

instance Pretty ConfigurationImplementation where
  pp xs =
    nest 2
      (vcat [ text "implementation" <+> lbrace
            , vcat $ map (\x -> pp x <> semi) xs ])
    $$ rbrace
-- 12

instance Pretty ConfigurationElement where
  pp (CC c) = pp c
  pp (CN c) = pp c
  pp (CD c) = text "not implemented CD"
-- 14

instance Pretty Components where
  pp x = text "components" <+> cs
    where cs = hcat $ punctuate (comma <> space) (map pp x)
-- 15

instance Pretty ComponentLineSingle where
  pp (ComponentLineSingle ref name) = pp ref
-- 16

instance Pretty ComponentRef where
  pp (CRI name) = text name
  pp (CRNew name args) =
    text "new" <+> text name <> parens (text "")
-- 18

instance Pretty Connection where
  pp (Equate a b) = pp a <+> text "=" <+> pp b
  pp (LeftLink a b) = pp a <+> text "<-" <+> pp b
  pp (RightLink a b) = pp a <+> text "->" <+> pp b
-- 21

instance Pretty EndPoint where
  pp (EndPoint xs arg_list)
    = text (intercalate "." xs)
    <> maybe' arg_list (brackets . pp)
-- 22

instance Pretty ComponentSpecification where
  pp = vcat . map pp
-- 24

instance Pretty UsesProvides where
  pp (Uses s) = text "uses" <+> pp s
  pp (Provides s) = text "provides" <+> pp s
-- 26

instance Pretty SpecificationElementList where
  pp (SpecElem s) =  pp s <> semi
  pp (SpecElems s) = text ""
-- 27

instance Pretty Attributes where
  pp = vcat . map pp
-- 34

instance Pretty Attribute where
  pp (Attribute iden xs) = text $ "@" ++ iden ++ "()"
-- 35

instance Pretty Identifier where
  pp = text
-- C.10

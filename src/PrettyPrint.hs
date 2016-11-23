{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- based on https://github.com/jswebtools/language-ecmascript/blob/master/src/Language/ECMAScript3/PrettyPrint.hs

module PrettyPrint where

import Parser
import Data.Char (toLower)
import Data.List (intercalate)
import Text.PrettyPrint

class Pretty a where
  prettyPrint :: a -> Doc

instance Pretty NescFile where
  prettyPrint (I i) = text "not implemented"
  prettyPrint (C c) = prettyPrint c

instance Pretty Identifier where
  prettyPrint = text

instance Pretty Component where
  prettyPrint (Component kind iden param attr spec imp) =
    vcat [ prettyPrint kind <+> prettyPrint iden <> prettyPrint param <+> lbrace
         , nest 2 $ prettyPrint spec <> semi
         , rbrace
         , prettyPrint imp
         , text ""
         ]
instance Pretty ComponentSpecification where
  prettyPrint = hcat . map prettyPrint

instance Pretty UsesProvides where
  prettyPrint (Uses s) = text "uses" <+> prettyPrint s
  prettyPrint (Provides s) = text "provides" <+> prettyPrint s

instance Pretty SpecificationElementList where
  prettyPrint (SpecElem s) =  prettyPrint s
  prettyPrint (SpecElems s) = text ""

instance Pretty CompKind where
  prettyPrint Module = text "module"
  prettyPrint ComponentKeyword = text "component"
  prettyPrint Configuration = text "configuration"
  prettyPrint GenericModule = text "generic module"
  prettyPrint GenericConfiguration = text "generic configuration"

instance Pretty (Maybe CompParameters) where
  prettyPrint Nothing = text ""
  prettyPrint (Just x) = parens $ text x

instance Pretty Implementation where
  prettyPrint (CI ci) = prettyPrint ci
  prettyPrint (MI mi) = text "implementation"
                          <+> lbrace
                          <+> rbrace


instance Pretty ConfigurationImplementation where
  prettyPrint xs =
    nest 2
      (vcat [ text "implementation" <+> lbrace
            , vcat $ map prettyPrint xs ])
    $$ rbrace

instance Pretty ConfigurationElement where
  prettyPrint (CC c) = prettyPrint c
  prettyPrint (CN c) = prettyPrint c
  prettyPrint (CD c) = text "not implemented"

instance Pretty Components where
  prettyPrint = vcat . map prettyPrint

instance Pretty ComponentLine where
  prettyPrint (ComponentLine ref name) =
    text "components" <+> prettyPrint ref <> semi

instance Pretty ComponentRef where
  prettyPrint (CRI name) = text name
  prettyPrint (CRNew name args) =
    text "new" <+> text name <> parens (text "")

instance Pretty Connection where
  prettyPrint (Equate a b) = prettyPrint a <+> text "=" <+> prettyPrint b
  prettyPrint (LeftLink a b) = prettyPrint a <+> text "<-" <+> prettyPrint b
  prettyPrint (RightLink a b) = prettyPrint a <+> text "->" <+> prettyPrint b

instance Pretty EndPoint where
  prettyPrint (EndPoint xs arg_list) = text $ intercalate "." xs
